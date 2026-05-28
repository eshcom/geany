// Scintilla source code edit control
// Encoding: UTF-8
// Copyright 1998-2001 by Neil Hodgson <neilh@scintilla.org>
// The License.txt file describes the conditions under which this software may be distributed.
/** @file LexElixir.cxx
 ** Lexer for Elixir.
 ** Originally wrote by esh, based on Erlang lexer.
 **/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>

#include <string>
#include <vector>
#include <map>

#include "ILexer.h"
#include "Scintilla.h"
#include "SciLexer.h"

#include "StringCopy.h"
#include "WordList.h"
#include "LexAccessor.h"
#include "Accessor.h"
#include "StyleContext.h"
#include "CharacterSet.h"
#include "LexerModule.h"
#include "LexerCommon.h"
#include "OptionSet.h"
#include "SubStyles.h"
#include "DefaultLexer.h"

using namespace Scintilla;

namespace {

struct SingleStringExpState {
	int state;
	char closingChar;
	int nestingCount;
};

struct ModuleAliases {
	std::string module;
	std::map<std::string, std::string> aliases;
};

struct AtomPunctSequence {
	int charsLeft;
	char validChar;
	AtomPunctSequence() {
		charsLeft = 0;
		validChar = ' ';
	}
	void initAtomPunctState(int ch) {
		if (strchr("&-+=.|", ch)) {
			charsLeft = 3;
			validChar = ch;
		} else if (ch == '!') {
			charsLeft = 3;
			validChar = '=';
		} else if (ch == '*' || ch == '\\') {
			charsLeft = 2;
			validChar = ch;
		} else if (ch == '<') {
			charsLeft = 2;
			validChar = '>';
		} else if (ch == '{') {
			charsLeft = 2;
			validChar = '}';
		} else {
			charsLeft = 1;
		}
	}
	bool atAtomPunctBeg(int ch) const {
		return strchr("!@%^|/\\<>{}*&-+=.", ch) != NULL;
	}
	bool atAtomPunctEnd(int ch) const {
		return (charsLeft <= 0) || (ch != validChar);
	}
};

static inline bool isWordEnd(const int ch) {
	return (ch == '!' || ch == '?');
}

static bool isRadix(int radix, int ch) {
	int digit;
	
	if (radix < 2 || radix > 36)
		return false;
	
	if (IsDigit(ch)) {
		digit = ch - '0';
	} else if (IsAlnum(ch)) {
		digit = ToUpper(ch) - 'A' + 10;
	} else {
		return false;
	}
	return (digit < radix);
}

typedef enum {
	NUMERAL_START,
	NUMERAL_BASE_VALUE,
	NUMERAL_FLOAT,
	NUMERAL_EXPONENT
} number_state_t;

typedef enum {
	NONE_STATE,
	DEFNAME_STATE,		// def, defp, defmacro, defmacrop, ...
	TYPEDEF_STATE,		// @type, @spec, @callback, @macrocallback
	PIPEOPER_STATE,		// |>
	ALIAS_STATE,
	ALIAS_AS_STATE,
	ALIAS_GRP_STATE
} ident_state_t;

typedef enum {
	NONE_MODULE,
	OTHER_MODULE,
	KERNEL_MODULE
} module_type_t;

static inline bool IsCommentStyle(int style) {
	return (style == SCE_ELIXIR_COMMENT ||
			style == SCE_ELIXIR_TASKMARKER);
}

static inline bool IsSpaceEquivStyle(int style) {
	return (IsCommentStyle(style) ||
			style == SCE_ELIXIR_DEFAULT ||
			style == SCE_ELIXIR_LINE_CONTINUED);
}

static inline bool IsOperatorStyle(int style) {
	return (style == SCE_ELIXIR_OPERATOR ||
			style == SCE_ELIXIR_EEX_OPER ||
			style == SCE_ELIXIR_PIN_OPER ||
			style == SCE_ELIXIR_MAP_OPER ||
			style == SCE_ELIXIR_CAPTURE_OPER ||
			style == SCE_ELIXIR_LINE_CONTINUED);
}

static inline bool IsFoldingStyle(int style) {
	return (style == SCE_ELIXIR_STD_WORD ||
			style == SCE_ELIXIR_USR_WORD);
}

static inline bool IsKeywordStyle(int style) {
	return (style == SCE_ELIXIR_STD_WORD ||
			style == SCE_ELIXIR_ADD_WORD ||
			style == SCE_ELIXIR_USR_WORD);
}

static inline bool IsStdWordOrAttrStyle(int style) {
	return (IsKeywordStyle(style) ||
			style == SCE_ELIXIR_STD_MODULE_ATTR);
}

static inline bool IsStringValStyle(int style) {
	return (style == SCE_ELIXIR_TRIPLEVAL ||
			style == SCE_ELIXIR_STRINGVAL ||
			style == SCE_ELIXIR_CHARSTRVAL ||
			style == SCE_ELIXIR_LITERALVAL ||
			style == SCE_ELIXIR_LITERALTRIPLEVAL);
}

static inline bool IsStringStyle(int style) {
	return (IsStringValStyle(style) ||
			style == SCE_ELIXIR_TRIPLE ||
			style == SCE_ELIXIR_STRING ||
			style == SCE_ELIXIR_CHARSTR ||
			style == SCE_ELIXIR_LITERAL ||
			style == SCE_ELIXIR_LITERALTRIPLE);
}

static inline bool IsNestedStringStyle(int style) {
	return (style == SCE_ELIXIR_ESCAPESEQ ||
			style == SCE_ELIXIR_FORMATSEQ);
}

static inline int GetSaveStringStyle(int style, int stringStyle) {
	return IsNestedStringStyle(style) ? stringStyle : style;
}

static inline const char *GetTripleQuote(char closing_char) {
	if (closing_char == '\"')
		return R"(""")";
	if (closing_char == '\'')
		return R"(''')";
	
	return "\0";
}

static inline char GetClosingChar(char opening_char) {
	if (opening_char == '\"')
		return '\"';
	else if (opening_char == '\'')
		return '\'';
	else if (opening_char == '<')
		return '>';
	else if (opening_char == '{')
		return '}';
	else if (opening_char == '[')
		return ']';
	else if (opening_char == '(')
		return ')';
	else if (opening_char == '|')
		return '|';
	else if (opening_char == '/')
		return '/';
	else
		return ' ';
}

void PushStateToStack(int state, char closingChar,
					  std::vector<SingleStringExpState> &stack,
					  SingleStringExpState *&currentStringExp) {
	SingleStringExpState expState = {state, closingChar, 0};
	stack.push_back(expState);
	
	currentStringExp = &stack.back();
}

SingleStringExpState PopFromStateStack(std::vector<SingleStringExpState> &stack,
									   SingleStringExpState *&currentStringExp) {
	SingleStringExpState expState = {0, ' ', 0};
	
	if (!stack.empty()) {
		expState = stack.back();
		stack.pop_back();
	}
	if (stack.empty()) {
		currentStringExp = NULL;
	} else {
		currentStringExp = &stack.back();
	}
	return expState;
}

// esh: taken from LexAccessor.h/Match() with minor modifications
bool MatchWord(Sci_Position pos, Accessor &styler, const char *s) {
	int i = 0;
	for (; *s; i++) {
		if (*s != styler.SafeGetCharAt(pos + i))
			return false;
		s++;
	}
	return !IsAlnumWordChar(styler.SafeGetCharAt(pos + i));
}

bool IsQuotedField(Sci_Position pos, Sci_PositionU endPos, Accessor &styler) {
	char closing_char = styler[pos];
	
	while (++pos < endPos) {
		if (styler[pos] == '\\') {
			pos++; // Skip any character after the backslash
			continue;
		} else if (styler[pos] == closing_char) {
			if (++pos < endPos && styler[pos] == ':')
				return true;
			break;
		}
	}
	return false;
}

// Options used for LexerElixir
struct OptionsElixir {
	bool escapeSequence;
	bool formatSequence;
	
	OptionsElixir() {
		escapeSequence = false;
		formatSequence = false;
	}
};

static const char *const elixirWordListDesc[] = {
	"Standard keywords",
	"Additional keywords",
	"Standard atoms",
	"Standard functions (BIFs)",
	"Standard modules (BIMs)",
	"Standard module attributes",
	"Standard Erlang modules (Erlang BIMs)",
	"Standard exceptions",
	"Standard macros",
	"Built-in type functions",
	"Lib macros (Bureaucrat/Ecto/ExMachina/ExUnit/Phoenix/Plug/...)",
	"Exclude lib macros (these names are often used as var names)",
	"Exclude lib funcs (these names are often used as user-func names)",
	"Task marker and error marker keywords",
	0
};

struct OptionSetElixir : public OptionSet<OptionsElixir> {
	OptionSetElixir() {
		DefineProperty("lexer.elixir.escape.sequence", &OptionsElixir::escapeSequence,
					"Set to 1 to enable highlighting of escape sequences in strings");
		
		DefineProperty("lexer.elixir.format.sequence", &OptionsElixir::formatSequence,
					"Set to 1 to enable highlighting of format sequences in strings");
		
		DefineWordListSets(elixirWordListDesc);
	}
};

const char styleSubable[] = { 0 };

LexicalClass lexicalClasses[] = {
	// Lexer Python SCLEX_ELIXIR SCE_ELIXIR_:
	0,	"SCE_ELIXIR_DEFAULT", "default", "White space",
	1,	"SCE_ELIXIR_UNKNOWN", "erroneous expression", "Erroneous expression",
	2,	"SCE_ELIXIR_STD_WORD", "keyword", "Standard keywords",
	3,	"SCE_ELIXIR_ADD_WORD", "keyword", "Additional keywords",
	4,	"SCE_ELIXIR_STD_ATOM", "identifier", "Standard atoms",
	5,	"SCE_ELIXIR_STD_FUNC", "identifier", "Standard functions (BIFs)",
	6,	"SCE_ELIXIR_STD_MODULE", "identifier", "Standard modules (BIMs)",
	7,	"SCE_ELIXIR_STD_MODULE_ATTR", "preprocessor", "Standard module attributes",
	8,	"SCE_ELIXIR_STD_ERL_MODULE", "identifier", "Standard Erlang modules (Erlang BIMs)",
	9,	"SCE_ELIXIR_STD_EXCEPT", "identifier", "Standard exceptions",
	10,	"SCE_ELIXIR_STD_MACRO", "identifier", "Standard macros",
	11,	"SCE_ELIXIR_TYPE_FUNC", "identifier", "Built-in type functions",
	12,	"SCE_ELIXIR_LIB_MACRO", "identifier", "Lib macros (Bureaucrat/Ecto/ExMachina/ExUnit/Phoenix/Plug/...)",
	13,	"SCE_ELIXIR_LIB_FUNC", "identifier", "Lib functions",
	14,	"SCE_ELIXIR_USR_WORD", "identifier", "User-defined keywords",
	15,	"SCE_ELIXIR_OPERATOR", "operator", "Operators",
	16,	"SCE_ELIXIR_EEX_OPER", "operator", "EEx-operator",
	17,	"SCE_ELIXIR_PIN_OPER", "operator", "^-operator",
	18,	"SCE_ELIXIR_MAP_OPER", "operator", "%-operator",
	19,	"SCE_ELIXIR_CAPTURE_OPER", "operator", "&-operator",
	20,	"SCE_ELIXIR_FUNCTION", "identifier", "Functions",
	21,	"SCE_ELIXIR_DEFNAME", "identifier", "Object name definition",
	22,	"SCE_ELIXIR_MODULE", "identifier", "Modules",
	23,	"SCE_ELIXIR_MODULE_ATTR", "preprocessor", "Module attributes",
	24,	"SCE_ELIXIR_ERL_MODULE", "identifier", "Erlang modules",
	25,	"SCE_ELIXIR_IDENTIFIER", "identifier", "Other identifiers",
	26,	"SCE_ELIXIR_ATOM", "identifier", "Atoms",
	27,	"SCE_ELIXIR_NODE", "identifier", "Nodes",
	28,	"SCE_ELIXIR_FIELD", "identifier", "Field name of map/struct",
	29,	"SCE_ELIXIR_NUMBER", "numeric", "Number",
	30,	"SCE_ELIXIR_TRIPLE", "string", "Triple-quote string",
	31,	"SCE_ELIXIR_TRIPLEVAL", "string", "Triple-quote string used as the value of the map-field",
	40,	"SCE_ELIXIR_STRING", "string", "String",
	41,	"SCE_ELIXIR_STRINGVAL", "string", "String used as the value of the map-field",
	42,	"SCE_ELIXIR_CHARSTR", "string", "Charstring",
	43,	"SCE_ELIXIR_CHARSTRVAL", "string", "Charstring used as the value of the map-field",
	44,	"SCE_ELIXIR_LITERAL", "literal string", "Literal string (with prefix ~)",
	45,	"SCE_ELIXIR_LITERALVAL", "literal string", "Literal string used as the value of the map-field",
	46,	"SCE_ELIXIR_LITERALTRIPLE", "literal string", "Triple-quote literal string",
	47,	"SCE_ELIXIR_LITERALTRIPLEVAL", "literal string", "Triple-quote literal string used as the value of the map-field",
	48,	"SCE_ELIXIR_CHARACTER", "character", "Single character",
	49,	"SCE_ELIXIR_ESCAPESEQ", "string escapesequence", "Escape sequence",
	50,	"SCE_ELIXIR_FORMATSEQ", "string formatsequence", "Format sequence",
	51,	"SCE_ELIXIR_STRING_SUBOPER", "operator", "#{}-operator inside string",
	52,	"SCE_ELIXIR_ATOM_PUNCT", "identifier", "Atoms",
	53,	"SCE_ELIXIR_ATOM_QUOTED", "identifier", "Quoted atoms",
	54,	"SCE_ELIXIR_NODE_QUOTED", "comment line", "Quoted nodes",
	55,	"SCE_ELIXIR_LINE_CONTINUED", "preprocessor", "Line continuation symbol",
	56,	"SCE_ELIXIR_TASKMARKER", "comment taskmarker", "Task Marker",
	57,	"SCE_ELIXIR_COMMENT", "comment line", "Comment-line",
};

}

class LexerElixir : public DefaultLexer {
	WordList stdWords;
	WordList addWords;
	WordList stdAtoms;
	WordList stdFuncs;
	WordList stdModules;
	WordList stdModuleAttrs;
	WordList stdErlModules;
	WordList stdExcepts;
	WordList stdMacros;
	WordList typeFuncs;
	WordList libMacros;
	WordList exclLibMacros;
	WordList exclLibFuncs;
	WordList usrWords;
	WordList taskMarkers;
	OptionsElixir options;
	OptionSetElixir osElixir;
	EscapeSequence escapeSeq;
	ErlFormatSequence formatSeq;
	AtomPunctSequence atomPunctSeq;
	enum { ssIdentifier };
	SubStyles subStyles;
	std::map<Sci_Position, std::vector<SingleStringExpState>> stringStateAtEol;
	std::map<Sci_Position, ModuleAliases> moduleAliasesAtEol;
public:
	explicit LexerElixir() :
		DefaultLexer(lexicalClasses, ELEMENTS(lexicalClasses)),
		subStyles(styleSubable, 0x80, 0x40, 0) {
	}
	~LexerElixir() override {
	}
	void SCI_METHOD Release() override {
		delete this;
	}
	int SCI_METHOD Version() const override {
		return lvSubStyles;
	}
	const char * SCI_METHOD PropertyNames() override {
		return osElixir.PropertyNames();
	}
	int SCI_METHOD PropertyType(const char *name) override {
		return osElixir.PropertyType(name);
	}
	const char * SCI_METHOD DescribeProperty(const char *name) override {
		return osElixir.DescribeProperty(name);
	}
	Sci_Position SCI_METHOD PropertySet(const char *key, const char *val) override;
	const char * SCI_METHOD DescribeWordListSets() override {
		return osElixir.DescribeWordListSets();
	}
	Sci_Position SCI_METHOD WordListSet(int n, const char *wl) override;
	void SCI_METHOD Lex(Sci_PositionU startPos, Sci_Position length,
						int initStyle, IDocument *pAccess) override;
	void SCI_METHOD Fold(Sci_PositionU startPos, Sci_Position length,
						 int initStyle, IDocument *pAccess) override;
	
	void * SCI_METHOD PrivateCall(int, void *) override {
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
	const char * SCI_METHOD GetSubStyleBases() override {
		return styleSubable;
	}
	static ILexer *LexerFactoryElixir() {
		return new LexerElixir();
	}

private:
	void InsertModule(StyleContext &sc, const char *module);
	const char *InsertAlias(const char *alias);
	const char *GetModule(const char *ident, Sci_Position currentLine);
};

Sci_Position SCI_METHOD LexerElixir::PropertySet(const char *key, const char *val) {
	if (osElixir.PropertySet(&options, key, val)) {
		return 0;
	}
	return -1;
}

Sci_Position SCI_METHOD LexerElixir::WordListSet(int n, const char *wl) {
	WordList *wordListN = 0;
	switch (n) {
	case 0:
		wordListN = &stdWords;
		break;
	case 1:
		wordListN = &addWords;
		break;
	case 2:
		wordListN = &stdAtoms;
		break;
	case 3:
		wordListN = &stdFuncs;
		break;
	case 4:
		wordListN = &stdModules;
		break;
	case 5:
		wordListN = &stdModuleAttrs;
		break;
	case 6:
		wordListN = &stdErlModules;
		break;
	case 7:
		wordListN = &stdExcepts;
		break;
	case 8:
		wordListN = &stdMacros;
		break;
	case 9:
		wordListN = &typeFuncs;
		break;
	case 10:
		wordListN = &libMacros;
		break;
	case 11:
		wordListN = &exclLibMacros;
		break;
	case 12:
		wordListN = &exclLibFuncs;
		break;
	case 13:
		wordListN = &usrWords;
		break;
	case 14:
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

void LexerElixir::InsertModule(StyleContext &sc, const char *module) {
	std::pair<Sci_Position, ModuleAliases> val;
	val.first = sc.currentLine;
	val.second = {std::string(module), std::map<std::string, std::string>()};
	
	moduleAliasesAtEol.insert(val);
}

const char *LexerElixir::InsertAlias(const char *alias) {
	if (moduleAliasesAtEol.empty()) return alias;
	
	std::map<Sci_Position, ModuleAliases>::iterator iter =
		std::prev(moduleAliasesAtEol.end());
	
	std::pair<std::string, std::string> val;
	
	const char *shortAlias;
	if (alias) {
		const char *search = strrchr(alias, '.');
		std::string module = std::string(iter->second.module);
		
		if (search && search[1]) {
			if (search > alias) {
				module += '.';
				module.append(alias, search - alias);
			}
			shortAlias = ++search;
		} else {
			shortAlias = alias;
		}
		val.second = module;
	} else {
		const char *module = iter->second.module.c_str();
		const char *search = strrchr(module, '.');
		shortAlias = (search && search[1]) ? ++search : module;
		val.second = std::string(module);
	}
	val.first = std::string(shortAlias);
	iter->second.aliases.insert(val);
	return shortAlias;
}

const char *LexerElixir::GetModule(const char *alias, Sci_Position currentLine) {
	if (!moduleAliasesAtEol.empty()) {
		std::map<Sci_Position, ModuleAliases>::iterator mIter =
			std::prev(moduleAliasesAtEol.end());
		
		while (true) {
			if (mIter->first <= currentLine) {
				std::map<std::string, std::string>::iterator aIter =
					mIter->second.aliases.find(alias);
				if (aIter != mIter->second.aliases.end())
					return aIter->second.c_str();
			}
			
			if (mIter == moduleAliasesAtEol.begin())
				break;
			else
				mIter = std::prev(mIter);
		}
	}
	return alias;
}


#define L_LITERAL_PREFIX "scrwp"
#define U_LITERAL_PREFIX "SCRWNUDT"

#define CHECK_LINE_END															\
	if (sc.atLineEnd) {															\
		if (!stringStateStack.empty()) {										\
			std::pair<Sci_Position, std::vector<SingleStringExpState>> val;		\
			val.first = sc.currentLine;											\
			val.second = stringStateStack;										\
			stringStateAtEol.insert(val);										\
		}																		\
		if (!sc.More()) break;													\
		lineEndCurr = styler.LineEnd(sc.currentLine + 1);						\
	}

#define MOVE_INDEX_TO_NONSPACE													\
	Sci_PositionU i = sc.currentPos + 1;										\
	while (i < endPos && IsSpaceOrTab(styler[i]))								\
		i++;

#define SKIP_SPACES																\
	while (sc.More() && IsSpaceOrTab(sc.ch))									\
		sc.Forward();

#define SKIP_NEXT_SPACES														\
	while (sc.More() && IsSpaceOrTab(sc.chNext))								\
		sc.Forward();

#define CHANGE_STATE_BY_MODULE													\
	module_type == KERNEL_MODULE && stdFuncs.InList(ident)						\
		? sc.ChangeState(SCE_ELIXIR_STD_FUNC)									\
		: sc.ChangeState(SCE_ELIXIR_FUNCTION);

#define CHANGE_STATE_BY_ERLMODULE												\
	stdErlModules.InList(ident)													\
		? sc.ChangeState(SCE_ELIXIR_STD_ERL_MODULE)								\
		: sc.ChangeState(SCE_ELIXIR_ERL_MODULE);

#define CHANGE_STATE_BY_FUNCLIST												\
	if (stdFuncs.InList(ident)) {												\
		sc.ChangeState(SCE_ELIXIR_STD_FUNC);									\
	} else if (!exclLibFuncs.InList(ident) && libMacros.InList(ident)) {		\
		sc.ChangeState(SCE_ELIXIR_LIB_FUNC);									\
	} else {																	\
		sc.ChangeState(SCE_ELIXIR_FUNCTION);									\
	}

#define CHECK_LIB_MACROS														\
	if (IsKeywordStyle(last_state) || !IsSpace(sc.chPrev) ||					\
		sc.Match('!', '=') || sc.Match(':', ':') ||								\
		MatchWord(sc.currentPos, styler, "and") ||								\
		MatchWord(sc.currentPos, styler, "or") ||								\
		MatchWord(sc.currentPos, styler, "in") ||								\
		MatchWord(sc.currentPos, styler, "not in") ||							\
		MatchWord(sc.currentPos, styler, "when") ||								\
		MatchWord(sc.currentPos, styler, "end")) {								\
		/* do not change the state */											\
	} else if ((IsAlnumWordChar(sc.ch) || sc.Match('<', '<')					\
				|| strchr("{[%@:~\"'^!?", sc.ch))								\
			   && !exclLibMacros.InList(ident) && libMacros.InList(ident)) {	\
		/* { - tuple, [ - list, % - map/struct, @ - attribute, : - atom,
		 * ~ - string, " - string, ' - charlist, << - binary string,
		 * ^ - pin oper, ! - "not" oper ? - char */								\
		sc.ChangeState(SCE_ELIXIR_LIB_MACRO);									\
	}

#define SET_LITERAL_STATE														\
	closing_char = GetClosingChar(sc.GetRelative(2));							\
	if (closing_char == ' ') {													\
		sc.SetState(SCE_ELIXIR_UNKNOWN);										\
		sc.Forward();															\
	} else {																	\
		sc.SetState(assign_to_strfield ? SCE_ELIXIR_LITERALVAL					\
									   : SCE_ELIXIR_LITERAL);					\
		sc.Forward(2);															\
		if (sc.Match(R"(""")") || sc.Match(R"(''')")) {							\
			sc.ChangeState(assign_to_strfield ? SCE_ELIXIR_LITERALTRIPLEVAL		\
											  : SCE_ELIXIR_LITERALTRIPLE);		\
			sc.Forward(2);														\
		}																		\
		string_state = sc.state;												\
	}

#define CHECK_INTERPOLATE_STRING												\
	} else if (canbe_interpolate && sc.Match('#', '{')) {						\
		PushStateToStack(GetSaveStringStyle(sc.state, string_state),			\
						 closing_char, stringStateStack, currentStringExp);		\
		sc.SetState(SCE_ELIXIR_STRING_SUBOPER);									\
		sc.Forward();															\
		sc.ForwardSetState(SCE_ELIXIR_DEFAULT);									\
		last_state = SCE_ELIXIR_DEFAULT;										\
		is_at_symb = true; /* otherwise atoms of the form :'tes#{}t@test'
							  will be incorrectly highlighted */

#define CHECK_ESCAPE_FORMAT_SEQ													\
	if (sc.ch == '\\') {														\
		if (options.escapeSequence) {											\
			is_char_escape = false;												\
			sc.SetState(SCE_ELIXIR_ESCAPESEQ);									\
			escapeSeq.initEscapeState(sc.chNext);								\
		}																		\
		sc.Forward(); /* Skip any character after the backslash */				\
		CHECK_LINE_END															\
		continue;																\
	} else if (sc.ch == '~' && options.formatSequence) {						\
		sc.SetState(SCE_ELIXIR_FORMATSEQ);										\
		formatSeq.initFormatState();											\
		continue;																\
	CHECK_INTERPOLATE_STRING

#define CHECK_CLOSING_TRIPLE													\
	if (sc.Match(GetTripleQuote(closing_char))) {								\
		last_state = sc.state;													\
		sc.Forward(2);															\
		sc.ForwardSetState(SCE_ELIXIR_DEFAULT);									\
	}

#define CHECK_CLOSING_CHAR														\
	} else if (sc.ch == closing_char) {											\
		sc.Forward();															\
		if (sc.state == SCE_ELIXIR_LITERAL										\
			|| sc.state == SCE_ELIXIR_LITERALVAL) {								\
			while (strchr("uismxfU", sc.ch)) /* regex modifiers */				\
				sc.Forward();													\
		}																		\
		last_state = sc.state;													\
		sc.SetState(SCE_ELIXIR_DEFAULT);										\
	}

#define CHECK_CLOSING_STRING													\
	} else {																	\
		sc.SetState(string_state);												\
		if (sc.state == SCE_ELIXIR_TRIPLE ||									\
			sc.state == SCE_ELIXIR_TRIPLEVAL ||									\
			sc.state == SCE_ELIXIR_LITERALTRIPLE ||								\
			sc.state == SCE_ELIXIR_LITERALTRIPLEVAL) {							\
			CHECK_CLOSING_TRIPLE												\
		CHECK_CLOSING_CHAR														\
	}

#define DEFINE_ASSIGN_TO_STRFIELD												\
	while (--back > 1 && IsSpaceEquivStyle(styler.StyleAt(back)));				\
	if (back > 1 && styler.StyleAt(back--) == SCE_ELIXIR_OPERATOR) {			\
		if (styler[back] == '=' && styler[back + 1] == '>') {					\
			while (--back && IsSpaceEquivStyle(styler.StyleAt(back)));			\
			assign_to_strfield = (styler.StyleAt(back) == SCE_ELIXIR_STRING);	\
																				\
		} else if (styler[back] == '<' && styler[back + 1] == '>') {			\
			while (--back && IsSpaceEquivStyle(styler.StyleAt(back)));			\
			assign_to_strfield = IsStringValStyle(styler.StyleAt(back));		\
		}																		\
	}

#define PREPARE_OPER_STATE														\
	if (ident_state != PIPEOPER_STATE && ident_state != ALIAS_AS_STATE			\
									  && ident_state != ALIAS_GRP_STATE)		\
		ident_state = NONE_STATE;												\
	assign_to_strfield = false;

#define IS_PIPEOPER_FUNC														\
	(ident_state == PIPEOPER_STATE && sc.ch != '.')

#define IS_TYPE_FUNC															\
	(maybe_typefunc && sc.ch != ':')


void SCI_METHOD LexerElixir::Lex(Sci_PositionU startPos, Sci_Position length,
								 int initStyle, IDocument *pAccess) {
	Accessor styler(pAccess, NULL);
	
	std::vector<SingleStringExpState> stringStateStack;
	SingleStringExpState *currentStringExp = NULL;
	
	Sci_PositionU endPos = startPos + length;
	
	// esh: before debugging, you need to start viewing logs with the command `journalctl -f`
	//~ printf("!!!Lex: currLine = %li, currChar = '%c', lastChar = '%c', "
				//~ "initStyle = %i, startPos = %li, length = %li\n",
		   //~ styler.GetLine(startPos) + 1, styler[startPos],
		   //~ styler[endPos - 2], initStyle, startPos, length);
	
	StyleContext sc(startPos, length, initStyle, styler);
	
	int radix_digits = 0;
	int exponent_digits = 0;
	number_state_t number_state;
	
	ident_state_t ident_state = NONE_STATE;
	module_type_t module_type = NONE_MODULE;
	
	char ident[100];
	bool is_at_symb = false;			// esh: "at" - is "@" symb (for node)
	
	// esh: added string_state for escape/format sequences highlighting
	int string_state = -1;
	char closing_char = ' ';
	bool canbe_interpolate = false;
	bool assign_to_strfield = false;
	
	if (IsStringStyle(initStyle) || IsNestedStringStyle(initStyle)) {
		Sci_Position back = startPos;
		int backStyle;
		while (--back >= 0) {
			backStyle = styler.StyleAt(back);
			if (back > 0 && (IsStringStyle(backStyle) ||
							 IsNestedStringStyle(backStyle))) {
				continue;
				
			} else if (backStyle == SCE_ELIXIR_STRING_SUBOPER && styler[back] == '}') {
				int nestingCount = 1;
				while (--back) {
					if (styler.StyleAt(back) == SCE_ELIXIR_STRING_SUBOPER) {
						if (styler[back] == '}') {
							nestingCount++;
						} else if (styler[back] == '{') {
							nestingCount--;
							back--; // skip back { in #{}
						}
					}
					if (nestingCount == 0)
						break;
				}
				continue;
				
			} else {
				// esh: define string_state, closing_char, canbe_interpolate
				if (!IsStringStyle(backStyle)) back++;
				string_state = styler.StyleAt(back);
				
				Sci_Position index = back;
				if (styler[index] == '~') {
					canbe_interpolate = strchr(L_LITERAL_PREFIX, styler[++index]);
					index++;
				} else {
					canbe_interpolate = true;
				}
				closing_char = GetClosingChar(styler[index]);
				
				// esh: define assign_to_strfield
				DEFINE_ASSIGN_TO_STRFIELD
				break;
			}
		}
	} else if (IsSpaceEquivStyle(initStyle)) {
		// esh: define assign_to_strfield
		Sci_Position back = startPos;
		DEFINE_ASSIGN_TO_STRFIELD
	}
	
	int last_state = SCE_ELIXIR_DEFAULT;
	// esh: define last_state
	if (startPos > 0) {
		Sci_Position back = startPos;
		while (--back && IsSpaceEquivStyle(styler.StyleAt(back)));
		
		last_state = styler.StyleAt(back);
		if (last_state == SCE_ELIXIR_STD_WORD) {
			while (back > 0 && styler.StyleAt(back - 1) == SCE_ELIXIR_STD_WORD)
				back--;
			if (styler.Match(back, "do") || styler.Match(back, "end"))
				last_state = SCE_ELIXIR_DEFAULT;
		}
	}
	
	bool maybe_typefunc = false;
	// esh: define maybe_typefunc
	if (!IsStdWordOrAttrStyle(initStyle)) {
		Sci_Position back = startPos;
		int backStyle;
		while (--back >= 0) {
			backStyle = styler.StyleAt(back);
			if (!IsStdWordOrAttrStyle(backStyle)) {
				continue;
				
			} else if (backStyle == SCE_ELIXIR_STD_MODULE_ATTR) {
				while (back > 0 && styler[back - 1] != '@')
					back--;
				
				maybe_typefunc = (styler.Match(back, "spec") ||
								  styler.Match(back, "type") ||
								  styler.Match(back, "callback") ||
								  styler.Match(back, "macrocallback"));
			}
			break;
		}
	}
	
	// esh: for escape sequences highlighting for SCE_ELIXIR_CHARACTER
	bool is_char_escape = false;
	// esh: define is_char_escape
	if (initStyle == SCE_ELIXIR_ESCAPESEQ) {
		Sci_Position back = startPos;
		int backStyle;
		while (--back) {
			backStyle = styler.StyleAt(back);
			if (backStyle != SCE_ELIXIR_ESCAPESEQ) {
				if (backStyle == SCE_ELIXIR_CHARACTER) {
					is_char_escape = true;
				} else if (IsStringStyle(backStyle)) {
					is_char_escape = false;
				} else if (styler[++back] == '?') {
					is_char_escape = true;
				} else {
					is_char_escape = false;
				}
				break;
			}
		}
	}
	
	bool is_dot_oper = false; // example: Struct.field, Module.func()
	
	// Set up state stack from last line and remove any subsequent string at eol states
	std::map<Sci_Position, std::vector<SingleStringExpState>>::iterator ssIter;
	ssIter = stringStateAtEol.find(sc.currentLine - 1);
	if (ssIter != stringStateAtEol.end() && !ssIter->second.empty()) {
		stringStateStack = ssIter->second;
		currentStringExp = &stringStateStack.back();
	}
	ssIter = stringStateAtEol.lower_bound(sc.currentLine);
	if (ssIter != stringStateAtEol.end()) {
		stringStateAtEol.erase(ssIter, stringStateAtEol.end());
	}
	
	// Remove any subsequent module aliases at eol
	std::map<Sci_Position, ModuleAliases>::iterator maIter;
	maIter = moduleAliasesAtEol.lower_bound(sc.currentLine);
	if (maIter != moduleAliasesAtEol.end()) {
		moduleAliasesAtEol.erase(maIter, moduleAliasesAtEol.end());
	}
	
	Sci_PositionU lineEndCurr = styler.LineEnd(sc.currentLine);
	
	for (; sc.More(); sc.Forward()) {
		if (sc.state == SCE_ELIXIR_STRING_SUBOPER) {
			SingleStringExpState expState =
					PopFromStateStack(stringStateStack, currentStringExp);
			
			sc.SetState(expState.state);
			closing_char = expState.closingChar;
			canbe_interpolate = true;
		}
		
		CHECK_LINE_END
		
		// Determine if the current state should terminate.
		switch (sc.state) {
			/* COMMENTS ----------------------------------------------------- */
			case SCE_ELIXIR_COMMENT : {
				HighlightTaskMarker(sc, styler, taskMarkers, true,
									SCE_ELIXIR_TASKMARKER);
				if (sc.atLineEnd)
					sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			/* -------------------------------------------------------------- */
			
			/* Numerics ----------------------------------------------------- */
			case SCE_ELIXIR_NUMBER : {
				switch (number_state) {
					
					/* Simple integer */
					case NUMERAL_START : {
						if (IsDigit(sc.ch)) {
							radix_digits *= 10;
							radix_digits += sc.ch - '0'; // Assuming ASCII here!
							continue;
						} else if (sc.ch == '#') {
							if (radix_digits < 2 || radix_digits > 36) {
								sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
							} else {
								number_state = NUMERAL_BASE_VALUE;
								continue;
							}
						} else if (IsDotDigit(sc.ch, sc.chNext)) {
							number_state = NUMERAL_FLOAT;
							continue;
						} else if (IsDecExponent(sc.ch)) {
							exponent_digits = 0;
							number_state = NUMERAL_EXPONENT;
							continue;
						} else if (sc.ch == '_') {
							if (IsDigit(sc.chNext)) {
								continue;
							} else {
								sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
							}
						} else if (IsAlpha(sc.ch)) {
							sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
						}
					} break;
					
					/* Integer in other base than 10 (x#yyy) */
					case NUMERAL_BASE_VALUE : {
						if (isRadix(radix_digits, sc.ch)) {
							continue;
						} else if (IsAlnum(sc.ch)) {
							sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
						}
					} break;
					
					/* Float (x.yyy) */
					case NUMERAL_FLOAT : {
						if (IsDecExponent(sc.ch)) {
							exponent_digits = 0;
							number_state = NUMERAL_EXPONENT;
							continue;
						} else if (IsDigit(sc.ch)) {
							continue;
						} else if (IsAlpha(sc.ch)) {
							sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
						}
					} break;
					
					/* Exponent, either integer or float (xEyy, x.yyEzzz) */
					case NUMERAL_EXPONENT : {
						if (IsSignDigit(sc.ch, sc.chNext)) {
							continue;
						} else if (IsDigit(sc.ch)) {
							exponent_digits++;
							continue;
						} else if (exponent_digits == 0 || IsAlpha(sc.ch)) {
							sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
						}
					} break;
				}
				last_state = sc.state;
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			/* -------------------------------------------------------------- */
			
			/* Atoms -------------------------------------------------------- */
			case SCE_ELIXIR_ATOM : {
				if (sc.ch == '@' && !is_at_symb) {
					sc.ChangeState(SCE_ELIXIR_NODE);
					is_at_symb = true;
					continue;
				} else if (IsAlnumWordChar(sc.ch)) {
					continue;
				} else if (isWordEnd(sc.ch)) {
					sc.Forward();
				}
				sc.GetCurrent(ident, sizeof(ident));
				SKIP_SPACES
				if (sc.ch == '.') {
					CHANGE_STATE_BY_ERLMODULE
				} else if (sc.ch == ',' && ident_state == ALIAS_STATE) {
					CHANGE_STATE_BY_ERLMODULE
					InsertModule(sc, ident);
					ident_state = ALIAS_AS_STATE;
				} else if (stdAtoms.InList(ident)) {
					sc.ChangeState(SCE_ELIXIR_STD_ATOM);
				}
				last_state = sc.state;
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_ATOM_QUOTED : {
				if (sc.ch == '@' && !is_at_symb) {
					sc.ChangeState(SCE_ELIXIR_NODE_QUOTED);
					string_state = sc.state;
					is_at_symb = true;
				} else if (sc.ch == '\\') {
					sc.Forward(); // Skip any character after the backslash
					CHECK_LINE_END
					continue;
				CHECK_INTERPOLATE_STRING
				} else if (sc.ch == closing_char) {
					last_state = sc.state;
					sc.ForwardSetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
			/* -------------------------------------------------------------- */
			
			/* Nodes -------------------------------------------------------- */
			case SCE_ELIXIR_NODE : {
				if (sc.ch == '@') {
					sc.ChangeState(SCE_ELIXIR_ATOM);
					continue;
				} else if (IsAlnumWordChar(sc.ch)) {
					continue;
				} else if (isWordEnd(sc.ch)) {
					sc.Forward();
				}
				last_state = sc.state;
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_NODE_QUOTED : {
				if (sc.ch == '\\') {
					sc.Forward(); // Skip any character after the backslash
					CHECK_LINE_END
					continue;
				CHECK_INTERPOLATE_STRING
				} else if (sc.ch == closing_char) {
					last_state = sc.state;
					sc.ForwardSetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
			/* -------------------------------------------------------------- */
			
			case SCE_ELIXIR_TRIPLE :
			case SCE_ELIXIR_TRIPLEVAL :
			case SCE_ELIXIR_LITERALTRIPLE :
			case SCE_ELIXIR_LITERALTRIPLEVAL : {
				CHECK_ESCAPE_FORMAT_SEQ
				} else {
					CHECK_CLOSING_TRIPLE
				}
			} break;
			
			case SCE_ELIXIR_STRING :
			case SCE_ELIXIR_STRINGVAL :
			case SCE_ELIXIR_CHARSTR :
			case SCE_ELIXIR_CHARSTRVAL :
			case SCE_ELIXIR_LITERAL :
			case SCE_ELIXIR_LITERALVAL : {
				CHECK_ESCAPE_FORMAT_SEQ
				CHECK_CLOSING_CHAR
			} break;
			
			case SCE_ELIXIR_CHARACTER : {
				if (sc.ch == '\\') {
					// esh: we will check the escapeSequence parameter later,
					//		set SCE_ELIXIR_ESCAPESEQ for validation
					is_char_escape = true;
					sc.SetState(SCE_ELIXIR_ESCAPESEQ);
					escapeSeq.initEscapeState(sc.chNext);
					sc.Forward(); // Skip any character after the backslash
					CHECK_LINE_END
					continue;
				} else if (sc.atLineEnd) {
					last_state = SCE_ELIXIR_CHARACTER;
					sc.SetState(SCE_ELIXIR_DEFAULT);
				} else {
					last_state = SCE_ELIXIR_CHARACTER;
					sc.ForwardSetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
			
			case SCE_ELIXIR_ESCAPESEQ : {
				escapeSeq.digitsLeft--;
				if (!escapeSeq.atEscapeEnd(sc.ch)) {
					continue; // esh: continue of escape chars
				}
				if (is_char_escape) {
					if (!sc.atLineStart && IsDigit(sc.ch)) {
						sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
					} else if (!options.escapeSequence) {
						sc.ChangeState(SCE_ELIXIR_CHARACTER);
					}
					last_state = sc.state;
					sc.SetState(SCE_ELIXIR_DEFAULT);
					is_char_escape = false;
				} else {
					if (sc.ch == '\\') {
						escapeSeq.initEscapeState(sc.chNext);
						sc.Forward(); // Skip any character after the backslash
						CHECK_LINE_END
						continue;
					} else if (sc.ch == '~' && options.formatSequence) {
						sc.SetState(SCE_ELIXIR_FORMATSEQ);
						formatSeq.initFormatState();
						continue;
					CHECK_INTERPOLATE_STRING
					CHECK_CLOSING_STRING
				}
			} break;
			
			case SCE_ELIXIR_FORMATSEQ : {
				if (!formatSeq.atFormatEnd(sc.ch)) {
					continue; // esh: continue of format chars
				}
				if (formatSeq.atFormatNone()) {
					sc.ChangeState(string_state);
				}
				if (sc.ch == '\\') {
					if (options.escapeSequence) {
						sc.SetState(SCE_ELIXIR_ESCAPESEQ);
						escapeSeq.initEscapeState(sc.chNext);
					}
					sc.Forward(); // Skip any character after the backslash
					CHECK_LINE_END
					continue;
				} else if (sc.ch == '~') {
					sc.SetState(SCE_ELIXIR_FORMATSEQ);
					formatSeq.initFormatState();
					continue;
				CHECK_INTERPOLATE_STRING
				CHECK_CLOSING_STRING
			} break;
			
			case SCE_ELIXIR_MODULE : {
				if (IsAlnumWordChar(sc.ch)) {
					continue;
				}
				SKIP_SPACES
				MOVE_INDEX_TO_NONSPACE
				if (sc.ch == '.') {
					if (IsUpper(styler[i])) {
						sc.Forward(); // skip '.'
						SKIP_SPACES
						CHECK_LINE_END
						continue;
					}
				}
				sc.GetCurrent(ident, sizeof(ident));
				RemoveAllSpaces(ident);
				
				const char *alias = NULL;
				if (ident_state == ALIAS_STATE) {
					InsertModule(sc, ident);
					if (sc.atLineEnd) {
						alias = InsertAlias(NULL);
						ident_state = NONE_STATE;
					} else if (sc.ch == ',') {
						ident_state = ALIAS_AS_STATE;
					} else if (sc.ch == '.' && styler[i] == '{') {
						// example: alias Sayings.{Greetings, Farewells}
						ident_state = ALIAS_GRP_STATE;
					}
				} else if (ident_state == ALIAS_AS_STATE) {
					alias = InsertAlias(ident);
					ident_state = NONE_STATE;
				} else if (ident_state == ALIAS_GRP_STATE) {
					alias = InsertAlias(ident);
					if (sc.ch == '}' || sc.atLineEnd)
						ident_state = NONE_STATE;
				}
				const char *module;
				char *tmpAlias = NULL;
				if (alias) {
					module = GetModule(alias, sc.currentLine);
				} else {
					const char *search = strchr(ident, '.');
					tmpAlias = (search && search > ident)
									? strndup(ident, search - ident)
									: strdup(ident);
					module = GetModule(tmpAlias, sc.currentLine);
				}
				
				if (stdExcepts.InList(module)) {
					sc.ChangeState(SCE_ELIXIR_STD_EXCEPT);
				} else if (stdModules.InList(module)) {
					sc.ChangeState(SCE_ELIXIR_STD_MODULE);
					module_type = (strcmp(module, "Kernel") == 0) ? KERNEL_MODULE
																  : OTHER_MODULE;
				} else if (stdErlModules.InList(module)) {
					sc.ChangeState(SCE_ELIXIR_STD_ERL_MODULE);
				}
				if (sc.ch == '.' && ident_state == ALIAS_GRP_STATE) {
					sc.SetState(SCE_ELIXIR_OPERATOR);
				} else {
					last_state = sc.state;
					sc.SetState(SCE_ELIXIR_DEFAULT);
				}
				// clean it only after working with the module,
				// since the module may contain tmpAlias
				free(tmpAlias);
			} break;
			
			case SCE_ELIXIR_MODULE_ATTR : {
				if (IsAlnumWordChar(sc.ch)) {
					continue;
				} else if (isWordEnd(sc.ch)) {
					sc.Forward();
				}
				sc.GetCurrent(ident, sizeof(ident));
				RemoveAllSpaces(ident);
				
				if (stdModuleAttrs.InList(ident)) {
					sc.ChangeState(SCE_ELIXIR_STD_MODULE_ATTR);
					
					if (strcmp(ident, "@spec") == 0 ||
						strcmp(ident, "@type") == 0 ||
						strcmp(ident, "@typep") == 0 ||
						strcmp(ident, "@callback") == 0 ||
						strcmp(ident, "@macrocallback") == 0) {
						ident_state = TYPEDEF_STATE;
						maybe_typefunc = true;
					} else {
						ident_state = NONE_STATE;
						maybe_typefunc = false;
					}
				}
				last_state = sc.state;
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_IDENTIFIER : {
				if (IsAlnumWordChar(sc.ch)) {
					continue;
				} else if (isWordEnd(sc.ch)) {
					sc.Forward();
				}
				sc.GetCurrent(ident, sizeof(ident));
				
				if (sc.ch == ':') { // init field of map/struct or Erlang type oper (::)
					if (sc.chNext != ':') {
						if (ident_state == NONE_STATE ||
							(ident_state == ALIAS_AS_STATE
							 && strcmp(ident, "as") == 0)) {
							sc.ChangeState(SCE_ELIXIR_FIELD);
						} else {
							sc.ChangeState(SCE_ELIXIR_UNKNOWN);
						}
						if (!IsSpace(sc.chNext)) {
							sc.SetState(SCE_ELIXIR_OPERATOR);
							sc.ForwardSetState(SCE_ELIXIR_UNKNOWN);
							sc.Forward();
						}
					}
				} else if (ident_state == DEFNAME_STATE ||
						   ident_state == TYPEDEF_STATE) {
					sc.ChangeState(SCE_ELIXIR_DEFNAME);
				} else {
					SKIP_SPACES
					if (is_dot_oper) { // using field/method of module
						if (sc.ch == '(' || IS_PIPEOPER_FUNC || IS_TYPE_FUNC) {
							CHANGE_STATE_BY_MODULE
						} else if (sc.ch == '/') {
							MOVE_INDEX_TO_NONSPACE
							if (IsDigit(styler[i]))
								CHANGE_STATE_BY_MODULE
						}
						is_dot_oper = false;
					} else if (stdWords.InList(ident)) {
						sc.ChangeState(SCE_ELIXIR_STD_WORD);
					} else if ((IsUpper(sc.ch) || sc.Match('_', '_') ||
								(sc.ch == ':' && IsLower(sc.chNext)))
							   && addWords.InList(ident)) {
						sc.ChangeState(SCE_ELIXIR_ADD_WORD);
					} else if (stdAtoms.InList(ident)) {
						sc.ChangeState(SCE_ELIXIR_STD_ATOM);
					} else if (stdMacros.InList(ident)) {
						sc.ChangeState(SCE_ELIXIR_STD_MACRO);
					} else if (usrWords.InList(ident)) {
						sc.ChangeState(SCE_ELIXIR_USR_WORD);
					} else if (IS_TYPE_FUNC) {
						sc.ChangeState(typeFuncs.InList(ident) ? SCE_ELIXIR_TYPE_FUNC
															   : SCE_ELIXIR_FUNCTION);
					} else if (sc.ch == '(' || IS_PIPEOPER_FUNC) {
						CHANGE_STATE_BY_FUNCLIST
					} else if (sc.ch == '/') {
						MOVE_INDEX_TO_NONSPACE
						if (IsDigit(styler[i])) {
							CHANGE_STATE_BY_FUNCLIST
						} else CHECK_LIB_MACROS
					} else CHECK_LIB_MACROS
				}
				if (sc.state == SCE_ELIXIR_STD_WORD
					&& (strcmp(ident, "def") == 0 ||
						strcmp(ident, "defp") == 0 ||
						strcmp(ident, "defguard") == 0 ||
						strcmp(ident, "defguardp") == 0 ||
						strcmp(ident, "defmacro") == 0 ||
						strcmp(ident, "defmacrop") == 0 ||
						strcmp(ident, "defmemo") == 0 ||
						strcmp(ident, "defmemop") == 0))
					ident_state = DEFNAME_STATE;
				else if (sc.state == SCE_ELIXIR_USR_WORD
						 && strncmp(ident, "def", 3) == 0)
					ident_state = DEFNAME_STATE;
				else if (sc.state == SCE_ELIXIR_ADD_WORD
						 && strcmp(ident, "alias") == 0)
					ident_state = ALIAS_STATE;
				else if (ident_state != ALIAS_AS_STATE)
					ident_state = NONE_STATE;
				
				if (IsKeywordStyle(sc.state))
					maybe_typefunc = false;
				
				module_type = NONE_MODULE;
				last_state = (sc.state == SCE_ELIXIR_STD_WORD &&
							  (strcmp(ident, "do") == 0 ||
							   strcmp(ident, "end") == 0)) ? SCE_ELIXIR_DEFAULT
														   : sc.state;
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_OPERATOR :
			case SCE_ELIXIR_EEX_OPER :
			case SCE_ELIXIR_PIN_OPER :
			case SCE_ELIXIR_MAP_OPER :
			case SCE_ELIXIR_CAPTURE_OPER : {
				last_state = sc.state;
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_ATOM_PUNCT : {
				atomPunctSeq.charsLeft--;
				if (!atomPunctSeq.atAtomPunctEnd(sc.ch)) {
					continue; // esh: continue of atom-punct chars
				}
				last_state = sc.state;
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_LINE_CONTINUED : {
				if (sc.atLineStart) {
					sc.SetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
			
			case SCE_ELIXIR_UNKNOWN : {
				if (sc.atLineStart) {
					last_state = sc.state;
					sc.SetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
		}
		
		CHECK_LINE_END
		
		// Determine if a new state should be entered.
		if (sc.state == SCE_ELIXIR_DEFAULT || sc.state == SCE_ELIXIR_UNKNOWN) {
			if (sc.ch == '#') {
				sc.SetState(SCE_ELIXIR_COMMENT);
				
			} else if (sc.ch == '\"') {
				if (sc.Match(R"(""")")) {
					sc.SetState(assign_to_strfield ? SCE_ELIXIR_TRIPLEVAL
												   : SCE_ELIXIR_TRIPLE);
					sc.Forward(2);
				} else if (IsQuotedField(sc.currentPos, endPos, styler)) {
					sc.SetState(SCE_ELIXIR_ATOM_QUOTED);
					is_at_symb = false;
				} else {
					sc.SetState(assign_to_strfield ? SCE_ELIXIR_STRINGVAL
												   : SCE_ELIXIR_STRING);
				}
				closing_char = sc.ch;
				string_state = sc.state;
				canbe_interpolate = true;
				
			} else if (sc.ch == '\'') {
				if (sc.Match(R"(''')")) {
					sc.SetState(assign_to_strfield ? SCE_ELIXIR_TRIPLEVAL
												   : SCE_ELIXIR_TRIPLE);
					sc.Forward(2);
				} else if (IsQuotedField(sc.currentPos, endPos, styler)) {
					sc.SetState(SCE_ELIXIR_ATOM_QUOTED);
					is_at_symb = false;
				} else {
					sc.SetState(assign_to_strfield ? SCE_ELIXIR_CHARSTRVAL
												   : SCE_ELIXIR_CHARSTR);
				}
				closing_char = sc.ch;
				string_state = sc.state;
				canbe_interpolate = true;
				
			} else if (sc.ch == '~') {
				if (strchr(L_LITERAL_PREFIX, sc.chNext)) {
					SET_LITERAL_STATE
					canbe_interpolate = true;
				} else if (strchr(U_LITERAL_PREFIX, sc.chNext)) {
					SET_LITERAL_STATE
					canbe_interpolate = false;
				} else {
					sc.SetState(SCE_ELIXIR_UNKNOWN);
				}
			} else if (sc.ch == '?') {
				sc.SetState(SCE_ELIXIR_CHARACTER);
				
			} else if (sc.Match("<%=") || sc.Match("<%%") || sc.Match("<%#")) {
				PREPARE_OPER_STATE
				sc.SetState(SCE_ELIXIR_EEX_OPER);
				sc.Forward(2);
				
			} else if (sc.Match('<', '%') || sc.Match('%', '>')) {
				PREPARE_OPER_STATE
				sc.SetState(SCE_ELIXIR_EEX_OPER);
				sc.Forward();
				
			} else if (sc.ch == '%') {
				sc.SetState(SCE_ELIXIR_UNKNOWN);
				SKIP_NEXT_SPACES
				
				if (IsAlphaWordChar(sc.chNext) || sc.chNext == '{'
											   || sc.chNext == '^') {
					sc.ChangeState(SCE_ELIXIR_MAP_OPER);
				}
			} else if (sc.ch == '@') {
				sc.SetState(SCE_ELIXIR_UNKNOWN);
				SKIP_NEXT_SPACES
				
				if (IsLower(sc.chNext) || sc.chNext == '_') {
					sc.ChangeState(SCE_ELIXIR_MODULE_ATTR);
					sc.Forward();
				}
			} else if (sc.ch == ':' && IsAlphaWordChar(sc.chNext)) {
				sc.SetState(SCE_ELIXIR_ATOM);
				sc.Forward();
				is_at_symb = false;
				
			} else if (sc.ch == ':' && IsQuote(sc.chNext)) {
				sc.SetState(SCE_ELIXIR_ATOM_QUOTED);
				sc.Forward();
				is_at_symb = false;
				closing_char = sc.ch;
				string_state = sc.state;
				canbe_interpolate = true;
				
			} else if (sc.ch == ':' && atomPunctSeq.atAtomPunctBeg(sc.chNext)) {
				sc.SetState(SCE_ELIXIR_ATOM_PUNCT);
				sc.Forward();
				if (sc.Match('-', '>') || sc.Match('<', '-') || sc.Match('<', '=')
															 || sc.Match('>', '='))
					sc.Forward();
				else if (sc.Match("%{}"))
					sc.Forward(2);
				else
					atomPunctSeq.initAtomPunctState(sc.ch);
				
			} else if (IsDigit(sc.ch)) {
				number_state = NUMERAL_START;
				radix_digits = sc.ch - '0';
				sc.SetState(SCE_ELIXIR_NUMBER);
				
			} else if (IsUpper(sc.ch)) {
				sc.SetState(SCE_ELIXIR_MODULE);
				is_dot_oper = false;
				
			} else if (IsLower(sc.ch) || sc.ch == '_') {
				sc.SetState(SCE_ELIXIR_IDENTIFIER);
				
			} else if (sc.ch == '\\') {
				sc.SetState(SCE_ELIXIR_OPERATOR);
				
				if (sc.chNext == '\\') {
					sc.Forward();
					ident_state = NONE_STATE;
					assign_to_strfield = false;
				} else {
					SKIP_NEXT_SPACES
					sc.ChangeState((sc.currentPos + 1) >= lineEndCurr
											? SCE_ELIXIR_LINE_CONTINUED
											: SCE_ELIXIR_UNKNOWN);
				}
			} else if (IsOperator(sc.ch)) {
				sc.SetState(SCE_ELIXIR_OPERATOR);
				PREPARE_OPER_STATE
				
				is_dot_oper = (sc.ch == '.');
				
				if (sc.ch == '&') {
					sc.chNext == '&' ? sc.Forward()
									 : sc.ChangeState(SCE_ELIXIR_CAPTURE_OPER);
				} else if (sc.ch == '^') {
					sc.ChangeState(SCE_ELIXIR_PIN_OPER);
				} else if (sc.Match('=', '>')) {
					assign_to_strfield = (last_state == SCE_ELIXIR_STRING);
					sc.Forward();
				} else if (sc.Match('<', '>')) {
					assign_to_strfield = IsStringValStyle(last_state);
					sc.Forward();
				} else if (sc.Match('=', '~') || sc.Match(':', ':')) {
					sc.Forward();
				} else if (sc.Match('.', '.')) { // range, example: 1..10
					is_dot_oper = false;
					sc.Forward();
				} else if (sc.Match('|', '>')) {
					ident_state = PIPEOPER_STATE;
					sc.Forward();
				} else if (sc.ch == '{' && currentStringExp != NULL) {
					currentStringExp->nestingCount++;
				} else if (sc.ch == '}' && currentStringExp != NULL) {
					if (currentStringExp->nestingCount == 0)
						sc.ChangeState(SCE_ELIXIR_STRING_SUBOPER);
					else
						currentStringExp->nestingCount--;
				}
			}
			if (!IsOperatorStyle(sc.state) && sc.state != SCE_ELIXIR_DEFAULT) {
				assign_to_strfield = false;
			}
		}
	}
	sc.Complete();
}

static int ClassifyElixirFoldPoint(Accessor &styler, int styleNext,
								   Sci_Position keyword_start) {
	int lev = 0;
	if (styler.Match(keyword_start,"case")
		|| styler.Match(keyword_start,"cond")
		|| styler.Match(keyword_start,"if")
		|| styler.Match(keyword_start,"def")
		|| styler.Match(keyword_start,"receive")
		|| styler.Match(keyword_start,"with")
		|| (styler.Match(keyword_start,"fn") &&
			(styleNext != SCE_ELIXIR_FUNCTION))) {
		++lev;
	} else if (styler.Match(keyword_start,"end")) {
		--lev;
	}
	return lev;
}

void SCI_METHOD LexerElixir::Fold(Sci_PositionU startPos, Sci_Position length,
								  int initStyle, IDocument *pAccess) {
	Accessor styler(pAccess, NULL);
	
	Sci_PositionU endPos = startPos + length;
	Sci_Position currentLine = styler.GetLine(startPos);
	int lev;
	int previousLevel = styler.LevelAt(currentLine) & SC_FOLDLEVELNUMBERMASK;
	int currentLevel = previousLevel;
	int styleNext = styler.StyleAt(startPos);
	int style = initStyle;
	int stylePrev;
	Sci_Position keyword_start = 0;
	char ch;
	char chNext = styler.SafeGetCharAt(startPos);
	
	for (Sci_PositionU i = startPos; i < endPos; i++) {
		ch = chNext;
		chNext = styler.SafeGetCharAt(i + 1);
		// Get styles
		stylePrev = style;
		style = styleNext;
		styleNext = styler.StyleAt(i + 1);
		
		if (!IsFoldingStyle(stylePrev) && IsFoldingStyle(style)) {
			keyword_start = i;
		}
		// Fold on keywords
		if (IsFoldingStyle(stylePrev) && !IsFoldingStyle(style)
			&& style != SCE_ELIXIR_ATOM) {
			currentLevel += ClassifyElixirFoldPoint(styler, styleNext,
													keyword_start);
		}
		// Fold on comments
		if (IsCommentStyle(style)) {
			if (ch == '%' && chNext == '{') {
				currentLevel++;
			} else if (ch == '%' && chNext == '}') {
				currentLevel--;
			}
		}
		// Fold on braces
		if (style == SCE_ELIXIR_OPERATOR) {
			if (ch == '{' || ch == '(' || ch == '[') {
				currentLevel++;
			} else if (ch == '}' || ch == ')' || ch == ']') {
				currentLevel--;
			}
		}
		if (IsEOL(ch, chNext)) {
			lev = previousLevel;
			if (currentLevel > previousLevel)
				lev |= SC_FOLDLEVELHEADERFLAG;
			
			if (lev != styler.LevelAt(currentLine))
				styler.SetLevel(currentLine, lev);
			
			currentLine++;
			previousLevel = currentLevel;
		}
	}
	// Fill in the real level of the next line, keeping
	// the current flags as they will be filled in later
	styler.SetLevel(currentLine, previousLevel | (styler.LevelAt(currentLine) &
												  ~SC_FOLDLEVELNUMBERMASK));
}

LexerModule lmElixir(SCLEX_ELIXIR, LexerElixir::LexerFactoryElixir,
					 "elixir", elixirWordListDesc);
