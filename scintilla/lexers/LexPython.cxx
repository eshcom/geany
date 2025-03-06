// Scintilla source code edit control
/** @file LexPython.cxx
 ** Lexer for Python.
 **/
// Copyright 1998-2002 by Neil Hodgson <neilh@scintilla.org>
// The License.txt file describes the conditions under which this software may be distributed.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>

#include <string>
#include <vector>
#include <map>
#include <algorithm>

#include "ILexer.h"
#include "Scintilla.h"
#include "SciLexer.h"

#include "StringCopy.h"
#include "WordList.h"
#include "LexAccessor.h"
#include "Accessor.h"
#include "StyleContext.h"
#include "CharacterSet.h"
#include "CharacterCategory.h"
#include "LexerModule.h"
#include "LexerCommon.h"
#include "OptionSet.h"
#include "SubStyles.h"
#include "DefaultLexer.h"

using namespace Scintilla;

namespace {
// Use an unnamed namespace to protect the functions and classes from name conflicts

/* Notes on f-strings: f-strings are strings prefixed with f (e.g. f'') that may
   have arbitrary expressions in {}.  The tokens in the expressions are lexed as if
   they were outside of any string.  Expressions may contain { and } characters as
   long as there is a closing } for every {, may be 2+ lines in a triple quoted
   string, and may have a formatting specifier following a ! or :, but both !
   and : are valid inside of a bracketed expression and != is a valid
   expression token even outside of a bracketed expression.

   When in an f-string expression, the lexer keeps track of the state value of
   the f-string and the nesting count for the expression (# of [, (, { seen - # of
   }, ), ] seen).  f-strings may be nested (e.g. f'{ a + f"{1+2}"') so a stack of
   states and nesting counts is kept.  If a f-string expression continues beyond
   the end of a line, this stack is saved in a std::map that maps a line number to
   the stack at the end of that line.  std::vector is used for the stack.

   The PEP for f-strings is at https://www.python.org/dev/peps/pep-0498/
*/
struct SingleFStringExpState {
	int state;
	int nestingCount;
};

/* kwCDef, kwCTypeName only used for Cython */
enum kwType { kwOther, kwClass, kwDef, kwImport, kwCDef, kwCTypeName, kwCPDef };

enum literalsAllowed { litNone = 0, litU = 1, litB = 2, litF = 4 };

const int indicatorWhitespace = 1;

bool IsPyComment(Accessor &styler, Sci_Position pos, Sci_Position len) {
	return len > 0 && styler[pos] == '#';
}

bool IsPyCommentState(int st) {
	return (st == SCE_P_TASKMARKER ||
			st == SCE_P_COMMENTLINE ||
			st == SCE_P_COMMENTBLOCK);
}

bool IsPyStringTypeChar(int ch, literalsAllowed allowed) {
	return ((allowed & litB) && (ch == 'b' || ch == 'B')) ||
		   ((allowed & litU) && (ch == 'u' || ch == 'U')) ||
		   ((allowed & litF) && (ch == 'f' || ch == 'F'));
}

bool IsPyStringStart(int ch, int chNext, int chNext2, literalsAllowed allowed) {
	if (ch == '\'' || ch == '"')
		return true;
	if (IsPyStringTypeChar(ch, allowed)) {
		if (chNext == '"' || chNext == '\'')
			return true;
		if ((chNext == 'r' || chNext == 'R') && (chNext2 == '"' || chNext2 == '\''))
			return true;
	}
	if ((ch == 'r' || ch == 'R') && (chNext == '"' || chNext == '\''))
		return true;
	
	return false;
}

bool IsPyNestedStringState(int st) {
	return (st == SCE_P_ESCAPESEQ ||
			st == SCE_P_FORMATSEQ ||
			st == SCE_P_STRINGEOL ||
			st == SCE_P_STRING_CONTINUED);
}

int GetSaveStringState(int st, int stringState) {
	return (st == SCE_P_ESCAPESEQ ||
			st == SCE_P_FORMATSEQ ||
			st == SCE_P_STRING_CONTINUED) ? stringState : st;
}

bool IsPyFStringState(int st, int stringState) {
	int saveSt = GetSaveStringState(st, stringState);
	return (saveSt == SCE_P_FCHARSTR || saveSt == SCE_P_FSTRING ||
			saveSt == SCE_P_FCHARSTRTRIPLE || saveSt == SCE_P_FSTRINGTRIPLE);
}

bool IsPySingleQuoteStringState(int st) {
	return (st == SCE_P_CHARSTR || st == SCE_P_STRING ||
			st == SCE_P_FCHARSTR || st == SCE_P_FSTRING);
}

bool IsPyTripleQuoteStringState(int st) {
	return (st == SCE_P_CHARSTRTRIPLE || st == SCE_P_STRINGTRIPLE ||
			st == SCE_P_FCHARSTRTRIPLE || st == SCE_P_FSTRINGTRIPLE);
}

bool IsPyStringStateForFold(int st) {
	return (IsPySingleQuoteStringState(st) ||
			IsPyTripleQuoteStringState(st) ||
			IsPyNestedStringState(st) ||
			st == SCE_P_FSTRING_SUBOPER ||
			st == SCE_P_FSTRING_OPTION);
}

char GetPyStringQuoteChar(int st) {
	if ((st == SCE_P_CHARSTR) || (st == SCE_P_FCHARSTR) ||
		(st == SCE_P_CHARSTRTRIPLE) || (st == SCE_P_FCHARSTRTRIPLE))
		return '\'';
	if ((st == SCE_P_STRING) || (st == SCE_P_FSTRING) ||
		(st == SCE_P_STRINGTRIPLE) || (st == SCE_P_FSTRINGTRIPLE))
		return '"';
	
	return '\0';
}

const char *GetPyTripleQuote(int st) {
	if ((st == SCE_P_CHARSTRTRIPLE) || (st == SCE_P_FCHARSTRTRIPLE))
		return R"(''')";
	if ((st == SCE_P_STRINGTRIPLE) || (st == SCE_P_FSTRINGTRIPLE))
		return R"(""")";
	
	return "\0";
}

void PushStateToStack(int state, std::vector<SingleFStringExpState> &stack,
					  SingleFStringExpState *&currentFStringExp) {
	SingleFStringExpState single = {state, 0};
	stack.push_back(single);
	
	currentFStringExp = &stack.back();
}

int PopFromStateStack(std::vector<SingleFStringExpState> &stack,
					  SingleFStringExpState *&currentFStringExp) {
	int state = 0;
	
	if (!stack.empty()) {
		state = stack.back().state;
		stack.pop_back();
	}
	if (stack.empty()) {
		currentFStringExp = NULL;
	} else {
		currentFStringExp = &stack.back();
	}
	return state;
}

/* Return the state to use for the string starting at i;
 * *nextIndex will be set to the first index following the quote(s) */
int GetPyStringState(Accessor &styler, Sci_Position i, Sci_PositionU *nextIndex,
					 literalsAllowed allowed) {
	char ch = styler.SafeGetCharAt(i);
	char chNext = styler.SafeGetCharAt(i + 1);
	const int firstIsF = (ch == 'f' || ch == 'F');
	
	// Advance beyond r, u, or ur prefix (or r, b, or br in Python 2.7+ and r, f, or fr in Python 3.6+),
	// but bail if there are any unexpected chars
	if (ch == 'r' || ch == 'R') {
		i++;
		ch = styler.SafeGetCharAt(i);
		chNext = styler.SafeGetCharAt(i + 1);
	} else if (IsPyStringTypeChar(ch, allowed)) {
		if (chNext == 'r' || chNext == 'R')
			i += 2;
		else
			i += 1;
		ch = styler.SafeGetCharAt(i);
		chNext = styler.SafeGetCharAt(i + 1);
	}
	if (ch != '"' && ch != '\'') {
		*nextIndex = i + 1;
		return SCE_P_DEFAULT;
	}
	if (ch == chNext && ch == styler.SafeGetCharAt(i + 2)) {
		*nextIndex = i + 3;
		
		if (ch == '"')
			return (firstIsF ? SCE_P_FSTRINGTRIPLE : SCE_P_STRINGTRIPLE);
		else
			return (firstIsF ? SCE_P_FCHARSTRTRIPLE : SCE_P_CHARSTRTRIPLE);
	} else {
		*nextIndex = i + 1;
		
		if (ch == '"')
			return (firstIsF ? SCE_P_FSTRING : SCE_P_STRING);
		else
			return (firstIsF ? SCE_P_FCHARSTR : SCE_P_CHARSTR);
	}
}

inline bool IsAWordChar(int ch, bool unicodeIdentifiers) {
	if (ch < 0x80)
		return IsWordChar(ch);
	
	if (!unicodeIdentifiers)
		return false;
	
	// Python uses the XID_Continue set from unicode data
	return IsXidContinue(ch);
}

inline bool IsAWordStart(int ch, bool unicodeIdentifiers) {
	if (ch < 0x80)
		return IsAlphaWordChar(ch);
	
	if (!unicodeIdentifiers)
		return false;
	
	// Python uses the XID_Start set from unicode data
	return IsXidStart(ch);
}

inline bool IsAConstWord(const char *s) {
	bool upper_exists = false;
	
	while (*s) {
		if (islower(*s))
			return false;
		else if (isupper(*s))
			upper_exists = true;
		s++;
	}
	return upper_exists;
}

static bool IsFirstNonWhitespace(Sci_Position pos, Accessor &styler) {
	Sci_Position line = styler.GetLine(pos);
	Sci_Position start_pos = styler.LineStart(line);
	for (Sci_Position i = start_pos; i < pos; i++) {
		const char ch = styler[i];
		if (!IsSpaceOrTab(ch))
			return false;
	}
	return true;
}

// Options used for LexerPython
struct OptionsPython {
	int whingeLevel;
	bool base2or8Literals;
	bool stringsU;
	bool stringsB;
	bool stringsF;
	bool stringsOverNewline;
	bool stdIdentsNoSubIdentifiers;
	bool fold;
	bool foldQuotes;
	bool foldCompact;
	bool unicodeIdentifiers;
	bool escapeSequence;
	bool formatSequence;
	
	OptionsPython() {
		whingeLevel = 0;
		base2or8Literals = true;
		stringsU = true;
		stringsB = true;
		stringsF = true;
		stringsOverNewline = false;
		stdIdentsNoSubIdentifiers = false;
		fold = false;
		foldQuotes = false;
		foldCompact = false;
		unicodeIdentifiers = true;
		escapeSequence = false;
		formatSequence = false;
	}
	
	literalsAllowed AllowedLiterals() const {
		literalsAllowed allowedLiterals = stringsU ? litU : litNone;
		if (stringsB)
			allowedLiterals = static_cast<literalsAllowed>(allowedLiterals | litB);
		if (stringsF)
			allowedLiterals = static_cast<literalsAllowed>(allowedLiterals | litF);
		return allowedLiterals;
	}
};

static const char *const pythonWordListDesc[] = {
	"Standard keywords",
	"Additional keywords (eg. import)",
	"Common keywords (eg. True/False/None)",
	"References to the current instance of a class (eg. self)",
	"Standard functions (BIFs)",
	"Standard identifiers",
	"Standard exceptions (eg. BaseException)",
	"Task marker and error marker keywords",
	0
};

struct OptionSetPython : public OptionSet<OptionsPython> {
	OptionSetPython() {
		DefineProperty("tab.timmy.whinge.level", &OptionsPython::whingeLevel,
				   "For Python code, checks whether indenting is consistent. "
				   "The default, 0 turns off indentation checking, "
				   "1 checks whether each line is potentially inconsistent with the previous line, "
				   "2 checks whether any space characters occur before a tab character in the indentation, "
				   "3 checks whether any spaces are in the indentation, and "
				   "4 checks for any tab characters in the indentation. "
				   "1 is a good level to use.");
		
		DefineProperty("lexer.python.literals.binary", &OptionsPython::base2or8Literals,
				   "Set to 0 to not recognise Python 3 binary and octal literals: 0b1011 0o712.");
		
		DefineProperty("lexer.python.strings.u", &OptionsPython::stringsU,
				   "Set to 0 to not recognise Python Unicode literals u\"x\" as used before Python 3.");
		
		DefineProperty("lexer.python.strings.b", &OptionsPython::stringsB,
				   "Set to 0 to not recognise Python 3 bytes literals b\"x\".");
		
		DefineProperty("lexer.python.strings.f", &OptionsPython::stringsF,
				   "Set to 0 to not recognise Python 3.6 f-string literals f\"var={var}\".");
		
		DefineProperty("lexer.python.strings.over.newline", &OptionsPython::stringsOverNewline,
				   "Set to 1 to allow strings to span newline characters.");
		
		DefineProperty("lexer.python.stdidents.no.sub.identifiers", &OptionsPython::stdIdentsNoSubIdentifiers,
				   "When enabled, it will not style stdIdents/stdFuncs/stdExcepts items "
				   "that are used as a sub-identifier. "
				   "Example: when set, will not highlight \"foo.open\" when \"open\" "
				   "is a stdIdents/stdFuncs/stdExcepts item.");
		
		DefineProperty("fold", &OptionsPython::fold);
		
		DefineProperty("fold.quotes.python", &OptionsPython::foldQuotes,
				   "This option enables folding multi-line quoted strings when using the Python lexer.");
		
		DefineProperty("fold.compact", &OptionsPython::foldCompact);
		
		DefineProperty("lexer.python.unicode.identifiers", &OptionsPython::unicodeIdentifiers,
				   "Set to 0 to not recognise Python 3 unicode identifiers.");
		
		DefineProperty("lexer.python.escape.sequence", &OptionsPython::escapeSequence,
					"Set to 1 to enable highlighting of escape sequences in strings");
		
		DefineProperty("lexer.python.format.sequence", &OptionsPython::formatSequence,
					"Set to 1 to enable highlighting of format sequences in strings");
		
		DefineWordListSets(pythonWordListDesc);
	}
};

const char styleSubable[] = { SCE_P_IDENTIFIER, 0 };

LexicalClass lexicalClasses[] = {
	// Lexer Python SCLEX_PYTHON SCE_P_:
	0,	"SCE_P_DEFAULT", "default", "White space",
	1,	"SCE_P_STD_WORD", "keyword", "Standard keywords",
	2,	"SCE_P_ADD_WORD", "keyword", "Additional keywords (eg. import)",
	3,	"SCE_P_COM_WORD", "keyword", "Common keywords (eg. True/False/None)",
	4,	"SCE_P_REF_WORD", "identifier", "References to the current instance of a class (eg. self)",
	5,	"SCE_P_STD_FUNC", "identifier", "Standard functions (BIFs)",
	6,	"SCE_P_STD_IDENT", "identifier", "Standard identifiers",
	7,	"SCE_P_STD_EXCEPT", "identifier", "Standard exceptions (eg. BaseException)",
	8,	"SCE_P_IDENTIFIER", "identifier", "Identifiers",
	9,	"SCE_P_SAME_STD_FUNC", "identifier", "Highlighted BIFs, but w/o opening brace",
	10,	"SCE_P_DECORATOR", "preprocessor", "Decorators",
	11,	"SCE_P_DEFCLASS", "identifier", "Class name definition",
	12,	"SCE_P_DEFFUNC", "identifier", "Function or method name definition",
	13,	"SCE_P_FUNCTION", "identifier", "Function or method name",
	14,	"SCE_P_CONSTANT", "identifier", "Constant name",
	15,	"SCE_P_OPERATOR", "operator", "Operators",
	16,	"SCE_P_NUMBER", "literal numeric", "Number",
	17,	"SCE_P_STRING", "literal string", "String",
	18,	"SCE_P_STRINGTRIPLE", "literal string", "Triple-quote string",
	19,	"SCE_P_CHARSTR", "literal string", "Charstring",
	20,	"SCE_P_CHARSTRTRIPLE", "literal string", "Triple-quote charstring",
	21,	"SCE_P_STRINGEOL", "error literal string", "End of line where string is not closed",
	22,	"SCE_P_ESCAPESEQ", "literal string escapesequence", "Escape sequence",
	23,	"SCE_P_FORMATSEQ", "literal string formatsequence", "Format sequence",
	24,	"SCE_P_FSTRING", "literal string interpolated", "F-String",
	25,	"SCE_P_FSTRINGTRIPLE", "literal string interpolated", "Triple-quote f-string",
	26,	"SCE_P_FCHARSTR", "literal string interpolated", "F-Charstring",
	27,	"SCE_P_FCHARSTRTRIPLE", "literal string interpolated", "Triple-quote f-charstring",
	28,	"SCE_P_FSTRING_SUBOPER", "literal string", "F-String sub-oper",
	29,	"SCE_P_FSTRING_OPTION", "literal string", "F-String option: !s, !r, !a",
	30,	"SCE_P_STRING_CONTINUED", "literal string", "String continuation symbol",
	31,	"SCE_P_LINE_CONTINUED", "preprocessor", "Line continuation symbol",
	40,	"SCE_P_TASKMARKER", "comment taskmarker", "Task Marker",
	41,	"SCE_P_COMMENTLINE", "comment line", "Comment-line",
	42,	"SCE_P_COMMENTBLOCK", "comment", "Comment-block",
};

}

class LexerPython : public DefaultLexer {
	WordList stdWords;
	WordList addWords;
	WordList comWords;
	WordList refWords;
	WordList stdFuncs;
	WordList stdIdents;
	WordList stdExcepts;
	WordList taskMarkers;
	OptionsPython options;
	OptionSetPython osPython;
	EscapeSequence escapeSeq;
	FormatSequence formatSeq;
	enum { ssIdentifier };
	SubStyles subStyles;
	std::map<Sci_Position, std::vector<SingleFStringExpState>> ftripleStateAtEol;
public:
	explicit LexerPython() :
		DefaultLexer(lexicalClasses, ELEMENTS(lexicalClasses)),
		subStyles(styleSubable, 0x80, 0x40, 0) {
	}
	~LexerPython() override {
	}
	void SCI_METHOD Release() override {
		delete this;
	}
	int SCI_METHOD Version() const override {
		return lvSubStyles;
	}
	const char *SCI_METHOD PropertyNames() override {
		return osPython.PropertyNames();
	}
	int SCI_METHOD PropertyType(const char *name) override {
		return osPython.PropertyType(name);
	}
	const char *SCI_METHOD DescribeProperty(const char *name) override {
		return osPython.DescribeProperty(name);
	}
	Sci_Position SCI_METHOD PropertySet(const char *key, const char *val) override;
	const char *SCI_METHOD DescribeWordListSets() override {
		return osPython.DescribeWordListSets();
	}
	Sci_Position SCI_METHOD WordListSet(int n, const char *wl) override;
	void SCI_METHOD Lex(Sci_PositionU startPos, Sci_Position length,
						int initStyle, IDocument *pAccess) override;
	void SCI_METHOD Fold(Sci_PositionU startPos, Sci_Position length,
						 int initStyle, IDocument *pAccess) override;
	
	void *SCI_METHOD PrivateCall(int, void *) override {
		return 0;
	}
	int SCI_METHOD LineEndTypesSupported() override {
		return SC_LINE_END_TYPE_UNICODE;
	}
	int SCI_METHOD AllocateSubStyles(int styleBase, int numberStyles) override {
		return subStyles.Allocate(styleBase, numberStyles);
	}
	int SCI_METHOD SubStylesStart(int styleBase) override {
		return subStyles.Start(styleBase);
	}
	int SCI_METHOD SubStylesLength(int styleBase) override {
		return subStyles.Length(styleBase);
	}
	int SCI_METHOD StyleFromSubStyle(int subStyle) override {
		const int styleBase = subStyles.BaseStyle(subStyle);
		return styleBase;
	}
	int SCI_METHOD PrimaryStyleFromStyle(int style) override {
		return style;
	}
	void SCI_METHOD FreeSubStyles() override {
		subStyles.Free();
	}
	void SCI_METHOD SetIdentifiers(int style, const char *identifiers) override {
		subStyles.SetIdentifiers(style, identifiers);
	}
	int SCI_METHOD DistanceToSecondaryStyles() override {
		return 0;
	}
	const char *SCI_METHOD GetSubStyleBases() override {
		return styleSubable;
	}
	static ILexer *LexerFactoryPython() {
		return new LexerPython();
	}

private:
	void ProcessLineEnd(StyleContext &sc, std::vector<SingleFStringExpState> &fstringStateStack,
						SingleFStringExpState *&currentFStringExp, bool &inContinuedString,
						int &stringState);
};

Sci_Position SCI_METHOD LexerPython::PropertySet(const char *key, const char *val) {
	if (osPython.PropertySet(&options, key, val)) {
		return 0;
	}
	return -1;
}

Sci_Position SCI_METHOD LexerPython::WordListSet(int n, const char *wl) {
	WordList *wordListN = 0;
	switch (n) {
	case 0:
		wordListN = &stdWords;
		break;
	case 1:
		wordListN = &addWords;
		break;
	case 2:
		wordListN = &comWords;
		break;
	case 3:
		wordListN = &refWords;
		break;
	case 4:
		wordListN = &stdFuncs;
		break;
	case 5:
		wordListN = &stdIdents;
		break;
	case 6:
		wordListN = &stdExcepts;
		break;
	case 7:
		wordListN = &taskMarkers;
		break;
	}
	Sci_Position firstModification = -1;
	if (wordListN) {
		WordList wlNew;
		wlNew.Set(wl);
		if (*wordListN != wlNew) {
			wordListN->Set(wl);
			firstModification = 0;
		}
	}
	return firstModification;
}

void LexerPython::ProcessLineEnd(StyleContext &sc, std::vector<SingleFStringExpState> &fstringStateStack,
								 SingleFStringExpState *&currentFStringExp, bool &inContinuedString,
								 int &stringState) {
	// Find the deepest single quote state because that string will end
	for (unsigned long i = 0; i < fstringStateStack.size(); i++) {
		if (IsPySingleQuoteStringState(fstringStateStack[i].state)) {
			sc.SetState(fstringStateStack[i].state);
			stringState = sc.state; // esh: fix fstring highlighting with nested {""}/{''}
			break;
		}
	}
	if (!fstringStateStack.empty()) {
		std::pair<Sci_Position, std::vector<SingleFStringExpState>> val;
		val.first = sc.currentLine;
		val.second = fstringStateStack;
		
		ftripleStateAtEol.insert(val);
	}
	
	int saveState = GetSaveStringState(sc.state, stringState);
	if ((sc.state == SCE_P_DEFAULT) || IsPyTripleQuoteStringState(saveState)) {
		// Perform colourisation of white space and triple quoted strings at end of each line
		// to allow tab marking to work inside white space and triple quoted strings
		sc.SetState(sc.state);
		
	} else if (IsPySingleQuoteStringState(saveState)) {
		if (inContinuedString || options.stringsOverNewline) {
			sc.SetState(saveState);
			inContinuedString = false;
		} else {
			sc.ChangeState(SCE_P_STRINGEOL);
			sc.ForwardSetState(SCE_P_DEFAULT);
		}
	}
}


#define CHECK_ESCAPE_SEQUENCE												\
	if (!strchr("{}", sc.chNext)) {											\
		if (options.escapeSequence) {										\
			sc.SetState(SCE_P_ESCAPESEQ);									\
			escapeSeq.initEscapeState(sc.chNext);							\
		}																	\
		sc.Forward(); /* Skip any character after the backslash */			\
	}

#define CHECK_FORMAT_SEQUENCE												\
	} else if (sc.ch == '%') {												\
		if (options.formatSequence) {										\
			sc.SetState(SCE_P_FORMATSEQ);									\
			formatSeq.initFormatState();									\
		}

#define CHECK_END_FSTRING_SUBOPER											\
	} else if (sc.ch == '}' && IsPyFStringState(sc.state, stringState)) {	\
		sc.SetState(GetSaveStringState(sc.state, stringState));				\
		if (currentFStringExp != NULL)										\
			sc.ChangeState(SCE_P_FSTRING_SUBOPER);							\
		else if (sc.chNext == '}')											\
			sc.Forward();

#define PROCESS_END_SEQUENCE												\
	if (sc.ch == '\\') {													\
		if (IsPySingleQuoteStringState(stringState) &&						\
			(IsCRLF(sc.chNext) || (sc.currentPos + 1) == endPos)) {		\
			sc.SetState(SCE_P_STRING_CONTINUED);							\
			inContinuedString = true;										\
			if ((sc.chNext == '\r') && (sc.GetRelative(2) == '\n'))			\
				sc.Forward();												\
		} else {															\
			CHECK_ESCAPE_SEQUENCE											\
			else {															\
				sc.SetState(stringState);									\
			}																\
		}																	\
	CHECK_FORMAT_SEQUENCE													\
																			\
	} else if (sc.ch == GetPyStringQuoteChar(stringState) &&				\
			   (IsPySingleQuoteStringState(stringState) ||					\
				sc.Match(GetPyTripleQuote(stringState)))) {					\
		sc.SetState(stringState);											\
		if (IsPyTripleQuoteStringState(stringState))						\
			sc.Forward(2);													\
		sc.ForwardSetState(SCE_P_DEFAULT);									\
		needEOLCheck = true;												\
																			\
	CHECK_END_FSTRING_SUBOPER												\
																			\
	} else {																\
		if (IsPySingleQuoteStringState(stringState)) {						\
			Sci_PositionU i = sc.currentPos;								\
			while (i < endPos && IsSpaceOrTab(styler[i]))					\
				i++;														\
			if (i == endPos || IsCRLF(styler[i]))							\
				sc.ChangeState(SCE_P_STRINGEOL);							\
			else															\
				sc.SetState(stringState);									\
		} else {															\
			sc.SetState(stringState);										\
		}																	\
	}

#define MOVE_INDEX_TO_NONSPACE							\
	Sci_PositionU i = sc.currentPos;					\
	while (i < endPos && IsSpaceOrTab(styler[i]))		\
		i++;


void SCI_METHOD LexerPython::Lex(Sci_PositionU startPos, Sci_Position length,
								 int initStyle, IDocument *pAccess) {
	Accessor styler(pAccess, NULL);
	
	// Track whether in f-string expression; vector is used for a stack to
	// handle nested f-strings such as f"""{f'''{f"{f'{1}'}"}'''}"""
	std::vector<SingleFStringExpState> fstringStateStack;
	SingleFStringExpState *currentFStringExp = NULL;
	
	Sci_Position lineCurrent = styler.GetLine(startPos);
	
	const literalsAllowed allowedLiterals = options.AllowedLiterals();
	const Sci_PositionU endPos = startPos + length;
	
	initStyle = initStyle & 31;
	
	// esh: added stringState for escape/format sequences highlighting
	int stringState = -1;
	
	// esh: define stringState
	if (IsPySingleQuoteStringState(initStyle)
		|| IsPyTripleQuoteStringState(initStyle)) {
		stringState = initStyle;
		
	} else if (IsPyNestedStringState(initStyle)) {
		Sci_Position back = startPos;
		int backStyle;
		while (--back) {
			backStyle = styler.StyleAt(back);
			if (IsPyNestedStringState(backStyle)) {
				continue;
				
			} else if (backStyle == SCE_P_FSTRING_SUBOPER) {
				int nestingCount = 1;
				char ch;
				while (--back) {
					if (styler.StyleAt(back) == SCE_P_FSTRING_SUBOPER) {
						ch = styler.SafeGetCharAt(back);
						if (ch == '}')
							nestingCount++;
						else if (ch == '{')
							nestingCount--;
					}
					if (nestingCount == 0)
						break;
				}
				continue;
				
			} else if (IsPySingleQuoteStringState(backStyle)
					   || IsPyTripleQuoteStringState(backStyle)) {
				stringState = backStyle;
				
				if (IsPyFStringState(stringState, -1)) {
					while (back < endPos && styler.SafeGetCharAt(back) != '}') {
						int style = styler.StyleAt(back);
						if (style == backStyle || IsPyNestedStringState(style))
							back++;
						else
							break;
					}
					
					if (styler.SafeGetCharAt(back) == '{') {
						char ch;
						while (++back < endPos) {
							ch = styler.SafeGetCharAt(back);
							if (IsCRLF(ch)) {
								//~ f"""{a:\
								//~ }"""
								PushStateToStack(stringState, fstringStateStack,
												 currentFStringExp);
								break;
							} else if (ch == '}') {
								//~ f"""asdfsdf{{{1}}}"""
								break;
							}
						}
					} else {
						while (back < endPos && IsSpace(styler.SafeGetCharAt(back)))
							back++;
						
						int braceCnt = 0;
						while (back < endPos && styler.SafeGetCharAt(back++) == '}')
							braceCnt++;
						
						if ((braceCnt % 2) == 1)
							PushStateToStack(stringState, fstringStateStack,
											 currentFStringExp);
					}
				}
			} else {
				Sci_PositionU nextIndex = 0;
				stringState = GetPyStringState(styler, ++back, &nextIndex,
											   allowedLiterals);
			}
			break;
		}
	}
	
	// Set up fstate stack from last line and remove any subsequent ftriple at eol states
	std::map<Sci_Position, std::vector<SingleFStringExpState>>::iterator it;
	it = ftripleStateAtEol.find(lineCurrent - 1);
	if (it != ftripleStateAtEol.end() && !it->second.empty()) {
		fstringStateStack = it->second;
		currentFStringExp = &fstringStateStack.back();
	}
	it = ftripleStateAtEol.lower_bound(lineCurrent);
	if (it != ftripleStateAtEol.end()) {
		ftripleStateAtEol.erase(it, ftripleStateAtEol.end());
	}
	
	kwType kwLast = kwOther;
	int styleLast;
	int spaceFlags = 0;
	styler.IndentAmount(lineCurrent, &spaceFlags, IsPyComment);
	bool base_n_number = false;
	
	const WordClassifier &classifierIdentifiers =
							subStyles.Classifier(SCE_P_IDENTIFIER);
	
	StyleContext sc(startPos, length, initStyle, styler);
	
	bool indentGood = true;
	Sci_Position startIndicator = sc.currentPos;
	bool inContinuedString = false;
	
	Sci_PositionU lineEndNext = styler.LineEnd(lineCurrent);
	
	while (sc.More()) {
		if (sc.state == SCE_P_FSTRING_SUBOPER) {
			int state = (sc.chPrev == '}')
							? PopFromStateStack(fstringStateStack, currentFStringExp)
							: fstringStateStack.back().state; // sc.chPrev == ':'
			sc.SetState(state);
			stringState = sc.state; // esh: fix fstring highlighting with nested {""}/{''}
		}
		
		if (sc.atLineStart) {
			styler.IndentAmount(lineCurrent, &spaceFlags, IsPyComment);
			indentGood = true;
			if (options.whingeLevel == 1) {
				indentGood = (spaceFlags & wsInconsistent) == 0;
			} else if (options.whingeLevel == 2) {
				indentGood = (spaceFlags & wsSpaceTab) == 0;
			} else if (options.whingeLevel == 3) {
				indentGood = (spaceFlags & wsSpace) == 0;
			} else if (options.whingeLevel == 4) {
				indentGood = (spaceFlags & wsTab) == 0;
			}
			if (!indentGood) {
				styler.IndicatorFill(startIndicator, sc.currentPos,
									 indicatorWhitespace, 0);
				startIndicator = sc.currentPos;
			}
			// esh: taken from LexCPP.cxx
			if (IsPySingleQuoteStringState(
							GetSaveStringState(sc.state, stringState))) {
				// Prevent SCE_P_STRINGEOL from leaking back to previous line which
				// ends with a line continuation by locking in the state up to this position.
				sc.SetState(sc.state);
			}
		}
		
		if (sc.atLineEnd) {
			ProcessLineEnd(sc, fstringStateStack, currentFStringExp,
						   inContinuedString, stringState);
			if (!sc.More()) break;
			lineCurrent++;
			lineEndNext = styler.LineEnd(lineCurrent);
		}
		
		// esh: Handle line continuation generically (taken from LexCPP.cxx)
		if (sc.ch == '\\') {
			if ((sc.currentPos + 1) >= lineEndNext) { // esh: end of line
				if (!IsPyStringStateForFold(sc.state) &&
					!IsPyCommentState(sc.state)) {
					// backslash - line continuation symbol
					sc.SetState(SCE_P_LINE_CONTINUED);
					sc.ForwardSetState(SCE_P_DEFAULT);
					continue;
				}
			} else if (sc.state == SCE_P_DEFAULT) { // esh: undefined backslash
				sc.SetState(SCE_P_STRINGEOL);
				sc.Forward();
				continue;
			}
		}
		
		bool needEOLCheck = false;
		
		if (sc.state == SCE_P_OPERATOR) {
			kwLast = kwOther;
			sc.SetState(SCE_P_DEFAULT);
		} else if (sc.state == SCE_P_NUMBER) {
			if (!IsAWordChar(sc.ch, false) &&
				!(!base_n_number && ((sc.ch == '+' || sc.ch == '-') &&
									 (sc.chPrev == 'e' || sc.chPrev == 'E')))) {
				sc.SetState(SCE_P_DEFAULT);
			}
		} else if (sc.state == SCE_P_IDENTIFIER) {
			if ((sc.ch == '.') || (!IsAWordChar(sc.ch, options.unicodeIdentifiers))) {
				char s[100];
				sc.GetCurrent(s, sizeof(s));
				int style = SCE_P_IDENTIFIER;
				if ((kwLast == kwImport) && (strcmp(s, "as") == 0)) {
					style = styleLast;
				} else if (stdWords.InList(s)) {
					style = SCE_P_STD_WORD;
				} else if (addWords.InList(s)) {
					style = SCE_P_ADD_WORD;
				} else if (comWords.InList(s)) {
					style = SCE_P_COM_WORD;
				} else if (refWords.InList(s)) {
					style = SCE_P_REF_WORD;
				} else if (kwLast == kwClass) {
					style = SCE_P_DEFCLASS;
				} else if (kwLast == kwDef) {
					style = SCE_P_DEFFUNC;
				} else if (kwLast == kwCDef || kwLast == kwCPDef) {
					MOVE_INDEX_TO_NONSPACE
					if (styler[i] == '(') {
						style = SCE_P_DEFFUNC;
					} else if (styler[i] == ':') {
						style = SCE_P_DEFCLASS;
					}
				} else if (stdIdents.InList(s)) {
					if (options.stdIdentsNoSubIdentifiers) {
						// We don't want to highlight stdIdents
						// that are used as a sub-identifier,
						// i.e. not open in "foo.open".
						Sci_Position pos = styler.GetStartSegment() - 1;
						if (pos < 0 || (styler.SafeGetCharAt(pos, '\0') != '.'))
							style = SCE_P_STD_IDENT;
					} else {
						style = SCE_P_STD_IDENT;
					}
				} else if (stdFuncs.InList(s)) {
					if (options.stdIdentsNoSubIdentifiers) {
						// We don't want to highlight stdFuncs
						// that are used as a sub-identifier,
						// i.e. not open in "foo.open".
						Sci_Position pos = styler.GetStartSegment() - 1;
						if (pos < 0 || (styler.SafeGetCharAt(pos, '\0') != '.'))
							style = SCE_P_STD_FUNC;
					} else {
						style = SCE_P_STD_FUNC;
					}
					if (style == SCE_P_STD_FUNC) {
						MOVE_INDEX_TO_NONSPACE
						if (styler[i] != '(') {
							style = SCE_P_SAME_STD_FUNC;
						}
					}
				} else if (stdExcepts.InList(s)) {
					if (options.stdIdentsNoSubIdentifiers) {
						// We don't want to highlight stdExcepts
						// that are used as a sub-identifier,
						// i.e. not open in "foo.open".
						Sci_Position pos = styler.GetStartSegment() - 1;
						if (pos < 0 || (styler.SafeGetCharAt(pos, '\0') != '.'))
							style = SCE_P_STD_EXCEPT;
					} else {
						style = SCE_P_STD_EXCEPT;
					}
				} else {
					MOVE_INDEX_TO_NONSPACE
					if (styler[i] == '(') {
						style = SCE_P_FUNCTION;
					} else if (IsAConstWord(s)) {
						style = SCE_P_CONSTANT;
					} else {
						int subStyle = classifierIdentifiers.ValueFor(s);
						if (subStyle >= 0) {
							style = subStyle;
						}
					}
				}
				sc.ChangeState(style);
				sc.SetState(SCE_P_DEFAULT);
				if (style == SCE_P_STD_WORD || style == SCE_P_ADD_WORD) {
					if (0 == strcmp(s, "class"))
						kwLast = kwClass;
					else if (0 == strcmp(s, "def"))
						kwLast = kwDef;
					else if (0 == strcmp(s, "import"))
					{
						kwLast = kwImport;
						styleLast = style;
					}
					else if (0 == strcmp(s, "cdef"))
						kwLast = kwCDef;
					else if (0 == strcmp(s, "cpdef"))
						kwLast = kwCPDef;
					else if (0 == strcmp(s, "cimport"))
					{
						kwLast = kwImport;
						styleLast = style;
					}
					else if (kwLast != kwCDef && kwLast != kwCPDef)
						kwLast = kwOther;
				} else if (kwLast != kwCDef && kwLast != kwCPDef &&
						   kwLast != kwImport) {
					kwLast = kwOther;
				}
			}
		} else if ((sc.state == SCE_P_COMMENTLINE) ||
				   (sc.state == SCE_P_COMMENTBLOCK)) {
			HighlightTaskMarker(sc, styler, taskMarkers, true,
								SCE_P_TASKMARKER);
			if (IsCRLF(sc.ch)) {
				sc.SetState(SCE_P_DEFAULT);
			}
		} else if (sc.state == SCE_P_DECORATOR) {
			if (!IsAWordStart(sc.ch, options.unicodeIdentifiers)) {
				sc.SetState(SCE_P_DEFAULT);
			}
		} else if (IsPySingleQuoteStringState(sc.state)) {
			if (sc.ch == '\\') {
				if (IsCRLF(sc.chNext) || (sc.currentPos + 1) == endPos) {
					stringState = sc.state;
					sc.SetState(SCE_P_STRING_CONTINUED);
					inContinuedString = true;
					if ((sc.chNext == '\r') && (sc.GetRelative(2) == '\n'))
						sc.Forward();
				} else {
					CHECK_ESCAPE_SEQUENCE
				}
			CHECK_FORMAT_SEQUENCE
			
			} else if (sc.ch == GetPyStringQuoteChar(stringState)) {
				sc.ForwardSetState(SCE_P_DEFAULT);
				needEOLCheck = true;
			CHECK_END_FSTRING_SUBOPER
			}
		} else if (IsPyTripleQuoteStringState(sc.state)) {
			if (sc.ch == '\\') {
				CHECK_ESCAPE_SEQUENCE
				
			CHECK_FORMAT_SEQUENCE
				
			} else if (sc.Match(GetPyTripleQuote(sc.state))) {
				sc.Forward(2);
				sc.ForwardSetState(SCE_P_DEFAULT);
				needEOLCheck = true;
			CHECK_END_FSTRING_SUBOPER
			}
		} else if (sc.state == SCE_P_ESCAPESEQ) {
			escapeSeq.digitsLeft--;
			if (escapeSeq.atEscapeEnd(sc.ch)) {
				PROCESS_END_SEQUENCE
			}
		} else if (sc.state == SCE_P_FORMATSEQ) {
			if (formatSeq.atFormatEnd(sc.ch)) {
				if (formatSeq.atFormatNone()) {
					sc.ChangeState(stringState);
				}
				PROCESS_END_SEQUENCE
			}
		} else if (sc.state == SCE_P_STRINGEOL) {
			if (sc.atLineStart) {
				sc.SetState(SCE_P_DEFAULT);
			}
		}
		
		// Note if used and not if else because string states also match
		// some of the above clauses
		if (IsPyFStringState(sc.state, stringState) && sc.ch == '{') {
			if (sc.chNext == '{') {
				sc.Forward();
			} else {
				PushStateToStack(GetSaveStringState(sc.state, stringState),
								 fstringStateStack, currentFStringExp);
				sc.SetState(SCE_P_FSTRING_SUBOPER);
				sc.ForwardSetState(SCE_P_DEFAULT);
			}
			needEOLCheck = true;
		}
		
		// If in an f-string expression, check for the ending quote(s)
		// and end f-string to handle syntactically incorrect cases like
		// f'{' and f"""{"""
		if (!fstringStateStack.empty() && (sc.ch == '\'' || sc.ch == '"')) {
			long matching_stack_i = -1;
			for (unsigned long stack_i = 0; stack_i < fstringStateStack.size() &&
				 matching_stack_i == -1; stack_i++) {
				const int stack_state = fstringStateStack[stack_i].state;
				if (sc.ch == GetPyStringQuoteChar(stack_state) &&
					(IsPySingleQuoteStringState(stack_state) ||
					 sc.Match(GetPyTripleQuote(stack_state)))) {
					matching_stack_i = stack_i;
				}
			}
			if (matching_stack_i != -1) {
				const int stack_state = fstringStateStack[matching_stack_i].state;
				sc.SetState(stack_state);
				stringState = sc.state; // esh: fix fstring highlighting with nested {""}/{''}
				if (IsPyTripleQuoteStringState(stack_state)) {
					sc.Forward(2);
				}
				sc.ForwardSetState(SCE_P_DEFAULT);
				needEOLCheck = true;
				
				while (fstringStateStack.size() >
					   static_cast<unsigned long>(matching_stack_i)) {
					PopFromStateStack(fstringStateStack, currentFStringExp);
				}
			}
		}
		// End of code to find the end of a state
		
		if (!indentGood && !IsSpaceOrTab(sc.ch)) {
			styler.IndicatorFill(startIndicator, sc.currentPos,
								 indicatorWhitespace, 1);
			startIndicator = sc.currentPos;
			indentGood = true;
		}
		
		// One cdef or cpdef line, clear kwLast only at end of line
		if ((kwLast == kwCDef || kwLast == kwCPDef) && sc.atLineEnd) {
			kwLast = kwOther;
		}
		
		// State exit code may have moved on to end of line
		if (needEOLCheck && sc.atLineEnd) {
			ProcessLineEnd(sc, fstringStateStack, currentFStringExp,
						   inContinuedString, stringState);
			if (!sc.More()) break;
			lineCurrent++;
			lineEndNext = styler.LineEnd(lineCurrent);
			styler.IndentAmount(lineCurrent, &spaceFlags, IsPyComment);
		}
		
		// If in f-string expression, check for }, :, !
		// to resume f-string state or update nesting count
		int saveState = GetSaveStringState(sc.state, stringState);
		if (currentFStringExp != NULL && !IsPySingleQuoteStringState(saveState)
									  && !IsPyTripleQuoteStringState(saveState)) {
			if (currentFStringExp->nestingCount == 0 &&
				(sc.ch == '}' || sc.ch == ':' || (sc.ch == '!' && sc.chNext != '='))) {
				if (sc.ch == '}' || sc.ch == ':') {
					sc.SetState(SCE_P_FSTRING_SUBOPER);
				} else if (sc.ch == '!') {
					if (strchr("sra", sc.chNext)) {
						sc.SetState(SCE_P_FSTRING_OPTION);
						sc.Forward();
						if (!strchr("}:", sc.chNext))
							sc.ChangeState(SCE_P_STRINGEOL);
					} else {
						sc.SetState(SCE_P_STRINGEOL);
					}
				}
			} else {
				if (sc.ch == '{' || sc.ch == '[' || sc.ch == '(') {
					currentFStringExp->nestingCount++;
				} else if (sc.ch == '}' || sc.ch == ']' || sc.ch == ')') {
					currentFStringExp->nestingCount--;
				}
			}
		}
		
		// Check for a new state starting character
		if (sc.state == SCE_P_DEFAULT) {
			if (sc.ch == '\\') {
				// will be processed in the section "Handle line continuation generically"
				continue;
				
			} else if (IsDigit(sc.ch) || (sc.ch == '.' && IsDigit(sc.chNext))) {
				if (sc.ch == '0' && (sc.chNext == 'x' || sc.chNext == 'X')) {
					base_n_number = true;
					sc.SetState(SCE_P_NUMBER);
				} else if (sc.ch == '0' &&
						   (sc.chNext == 'o' || sc.chNext == 'O' ||
							sc.chNext == 'b' || sc.chNext == 'B')) {
					if (options.base2or8Literals) {
						base_n_number = true;
						sc.SetState(SCE_P_NUMBER);
					} else {
						sc.SetState(SCE_P_NUMBER);
						sc.ForwardSetState(SCE_P_IDENTIFIER);
					}
				} else {
					base_n_number = false;
					sc.SetState(SCE_P_NUMBER);
				}
			} else if ((IsASCII(sc.ch) && IsOperator(sc.ch)) || sc.ch == '`') {
				sc.SetState(SCE_P_OPERATOR);
			} else if (sc.ch == '#') {
				sc.SetState(sc.chNext == '#' ? SCE_P_COMMENTBLOCK
											 : SCE_P_COMMENTLINE);
			} else if (sc.ch == '@') {
				if (IsFirstNonWhitespace(sc.currentPos, styler))
					sc.SetState(SCE_P_DECORATOR);
				else
					sc.SetState(SCE_P_OPERATOR);
			} else if (IsPyStringStart(sc.ch, sc.chNext, sc.GetRelative(2),
									   allowedLiterals)) {
				Sci_PositionU nextIndex = 0;
				sc.SetState(GetPyStringState(styler, sc.currentPos,
											 &nextIndex, allowedLiterals));
				// esh: save string state for escape/format sequences highlighting
				stringState = sc.state;
				while (nextIndex > (sc.currentPos + 1) && sc.More()) {
					sc.Forward();
				}
			} else if (IsAWordStart(sc.ch, options.unicodeIdentifiers)) {
				sc.SetState(SCE_P_IDENTIFIER);
			}
		}
		sc.Forward();
	}
	styler.IndicatorFill(startIndicator, sc.currentPos,
						 indicatorWhitespace, 0);
	sc.Complete();
}

static bool IsCommentLine(Sci_Position line, Accessor &styler) {
	Sci_Position pos = styler.LineStart(line);
	const Sci_Position eol_pos = styler.LineStart(line + 1) - 1;
	for (Sci_Position i = pos; i < eol_pos; i++) {
		const char ch = styler[i];
		if (ch == '#')
			return true;
		else if (!IsSpaceOrTab(ch))
			return false;
	}
	return false;
}

static bool IsQuoteLine(Sci_Position line, const Accessor &styler) {
	const int style = styler.StyleAt(styler.LineStart(line)) & 31;
	return IsPyStringStateForFold(style);
}


void SCI_METHOD LexerPython::Fold(Sci_PositionU startPos, Sci_Position length,
								  int /*initStyle - unused*/, IDocument *pAccess) {
	if (!options.fold)
		return;
	
	Accessor styler(pAccess, NULL);
	
	const Sci_Position maxPos = startPos + length;
	const Sci_Position maxLines = (maxPos == styler.Length())
											? styler.GetLine(maxPos)
											: styler.GetLine(maxPos - 1);	// Requested last line
	const Sci_Position docLines = styler.GetLine(styler.Length());			// Available last line
	
	// Backtrack to previous non-blank line so we can determine indent level
	// for any white space lines (needed esp. within triple quoted strings)
	// and so we can fix any preceding fold level (which is why we go back
	// at least one line in all cases)
	int spaceFlags = 0;
	Sci_Position lineCurrent = styler.GetLine(startPos);
	int indentCurrent = styler.IndentAmount(lineCurrent, &spaceFlags, NULL);
	while (lineCurrent > 0) {
		lineCurrent--;
		indentCurrent = styler.IndentAmount(lineCurrent, &spaceFlags, NULL);
		if (!(indentCurrent & SC_FOLDLEVELWHITEFLAG) &&
			!IsCommentLine(lineCurrent, styler) &&
			!IsQuoteLine(lineCurrent, styler))
			break;
	}
	int indentCurrentLevel = indentCurrent & SC_FOLDLEVELNUMBERMASK;
	
	// Set up initial loop state
	startPos = styler.LineStart(lineCurrent);
	int prev_state = SCE_P_DEFAULT & 31;
	if (lineCurrent >= 1)
		prev_state = styler.StyleAt(startPos - 1) & 31;
	int prevQuote = options.foldQuotes && IsPyStringStateForFold(prev_state);
	
	// Process all characters to end of requested range or end of any triple quote
	// that hangs over the end of the range.  Cap processing in all cases
	// to end of document (in case of unclosed quote at end).
	while ((lineCurrent <= docLines) && ((lineCurrent <= maxLines) || prevQuote)) {
		// Gather info
		int lev = indentCurrent;
		Sci_Position lineNext = lineCurrent + 1;
		int indentNext = indentCurrent;
		int quote = false;
		if (lineNext <= docLines) {
			// Information about next line is only available if not at end of document
			indentNext = styler.IndentAmount(lineNext, &spaceFlags, NULL);
			Sci_Position lookAtPos = (styler.LineStart(lineNext) == styler.Length())
													? styler.Length() - 1
													: styler.LineStart(lineNext);
			const int style = styler.StyleAt(lookAtPos) & 31;
			quote = options.foldQuotes && IsPyStringStateForFold(style);
		}
		const int quote_start = (quote && !prevQuote);
		const int quote_continue = (quote && prevQuote);
		if (!quote || !prevQuote)
			indentCurrentLevel = indentCurrent & SC_FOLDLEVELNUMBERMASK;
		if (quote)
			indentNext = indentCurrentLevel;
		if (indentNext & SC_FOLDLEVELWHITEFLAG)
			indentNext = SC_FOLDLEVELWHITEFLAG | indentCurrentLevel;
		
		if (quote_start) {
			// Place fold point at start of triple quoted string
			lev |= SC_FOLDLEVELHEADERFLAG;
		} else if (quote_continue || prevQuote) {
			// Add level to rest of lines in the string
			lev = lev + 1;
		}
		
		// Skip past any blank lines for next indent level info; we skip also
		// comments (all comments, not just those starting in column 0)
		// which effectively folds them into surrounding code rather
		// than screwing up folding.  If comments end file, use the min
		// comment indent as the level after
		
		int minCommentLevel = indentCurrentLevel;
		while (!quote && (lineNext < docLines) &&
			   ((indentNext & SC_FOLDLEVELWHITEFLAG) ||
				IsCommentLine(lineNext, styler))) {
			
			if (IsCommentLine(lineNext, styler) && indentNext < minCommentLevel) {
				minCommentLevel = indentNext;
			}
			lineNext++;
			indentNext = styler.IndentAmount(lineNext, &spaceFlags, NULL);
		}
		
		const int levelAfterComments = ((lineNext < docLines)
											? indentNext & SC_FOLDLEVELNUMBERMASK
											: minCommentLevel);
		const int levelBeforeComments = std::max(indentCurrentLevel,
												 levelAfterComments);
		
		// Now set all the indent levels on the lines we skipped
		// Do this from end to start.  Once we encounter one line
		// which is indented more than the line after the end of
		// the comment-block, use the level of the block before
		
		Sci_Position skipLine = lineNext;
		int skipLevel = levelAfterComments;
		
		while (--skipLine > lineCurrent) {
			const int skipLineIndent = styler.IndentAmount(skipLine, &spaceFlags, NULL);
			
			if (options.foldCompact) {
				if ((skipLineIndent & SC_FOLDLEVELNUMBERMASK) > levelAfterComments)
					skipLevel = levelBeforeComments;
				
				int whiteFlag = skipLineIndent & SC_FOLDLEVELWHITEFLAG;
				
				styler.SetLevel(skipLine, skipLevel | whiteFlag);
			} else {
				if ((skipLineIndent & SC_FOLDLEVELNUMBERMASK) > levelAfterComments &&
					!(skipLineIndent & SC_FOLDLEVELWHITEFLAG) &&
					!IsCommentLine(skipLine, styler))
					skipLevel = levelBeforeComments;
				
				styler.SetLevel(skipLine, skipLevel);
			}
		}
		// Set fold header on non-quote line
		if (!quote && !(indentCurrent & SC_FOLDLEVELWHITEFLAG)) {
			if ((indentCurrent & SC_FOLDLEVELNUMBERMASK) <
				(indentNext & SC_FOLDLEVELNUMBERMASK))
				lev |= SC_FOLDLEVELHEADERFLAG;
		}
		// Keep track of triple quote state of previous line
		prevQuote = quote;
		// Set fold level for this line and move to next line
		styler.SetLevel(lineCurrent,
						options.foldCompact ? lev : lev & ~SC_FOLDLEVELWHITEFLAG);
		indentCurrent = indentNext;
		lineCurrent = lineNext;
	}
	// NOTE: Cannot set level of last line here because indentCurrent doesn't have
	// header flag set; the loop above is crafted to take care of this case!
	//styler.SetLevel(lineCurrent, indentCurrent);
}

LexerModule lmPython(SCLEX_PYTHON, LexerPython::LexerFactoryPython,
					 "python", pythonWordListDesc);
