// Scintilla source code edit control
/** @file LexYAML.cxx
 ** Lexer for YAML.
 **/
// Copyright 2003- by Sean O'Dell <sean@celsoft.com>
// The License.txt file describes the conditions under which this software may be distributed.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>

#include "ILexer.h"
#include "Scintilla.h"
#include "SciLexer.h"

#include "WordList.h"
#include "LexAccessor.h"
#include "Accessor.h"
#include "StyleContext.h"
#include "CharacterSet.h"
#include "LexerModule.h"
#include "LexerCommon.h"

using namespace Scintilla;

static const char * const yamlWordListDesc[] = {
	"Common keywords (eg. true/false/null)",
	"Type keywords (eg. boolean/integer/string)",
	"Task markers",
	0
};

#define YAML_STATE_BITSIZE		10
#define YAML_STATE_MASK			(0x00000C00)
#define YAML_INDENT_MASK		(0x000003FF)
#define YAML_FLOW_SHIFT			12
#define YAML_FLOW_LEVEL_BITS	2
#define YAML_FLOW_LEVELS		10
#define YAML_FLOW_ALL_MASK		(0xFFFFF000)
#define YAML_STATE_KEY_PENDING	(1 << YAML_STATE_BITSIZE)
#define YAML_STATE_TEXT_PENDING	(2 << YAML_STATE_BITSIZE)
#define YAML_STATE_TEXT			(3 << YAML_STATE_BITSIZE)

enum {
	YAML_FLOW_NONE = 0,
	YAML_FLOW_SEQ = 1,
	YAML_FLOW_MAP_KEY = 2,
	YAML_FLOW_MAP_VALUE = 3
};

static int YAMLFlowDepth(int flowBits) {
	int depth = 0;
	for (int level = 0; level < YAML_FLOW_LEVELS; level++) {
		const int shift = YAML_FLOW_SHIFT + level * YAML_FLOW_LEVEL_BITS;
		if (((flowBits >> shift) & 0x3) == YAML_FLOW_NONE)
			break;
		depth++;
	}
	return depth;
}

static int YAMLFlowTop(int flowBits) {
	const int depth = YAMLFlowDepth(flowBits);
	if (depth == 0)
		return YAML_FLOW_NONE;
	const int shift = YAML_FLOW_SHIFT + (depth - 1) * YAML_FLOW_LEVEL_BITS;
	return (flowBits >> shift) & 0x3;
}

static int YAMLPushFlow(int flowBits, int context) {
	const int depth = YAMLFlowDepth(flowBits);
	if (depth >= YAML_FLOW_LEVELS)
		return flowBits;
	const int shift = YAML_FLOW_SHIFT + depth * YAML_FLOW_LEVEL_BITS;
	return (flowBits & ~(0x3 << shift)) | (context << shift);
}

static int YAMLPopFlow(int flowBits) {
	const int depth = YAMLFlowDepth(flowBits);
	if (depth == 0)
		return flowBits;
	const int shift = YAML_FLOW_SHIFT + (depth - 1) * YAML_FLOW_LEVEL_BITS;
	return flowBits & ~(0x3 << shift);
}

static int YAMLSetFlowTop(int flowBits, int context) {
	const int depth = YAMLFlowDepth(flowBits);
	if (depth == 0)
		return flowBits;
	const int shift = YAML_FLOW_SHIFT + (depth - 1) * YAML_FLOW_LEVEL_BITS;
	return (flowBits & ~(0x3 << shift)) | (context << shift);
}

static inline int GetStringStyle(int ch) {
	return (ch == '\"') ? SCE_YAML_DOUBLESTRING : SCE_YAML_SINGLESTRING;
}

static inline char GetClosingChar(int style) {
	return (style == SCE_YAML_DOUBLESTRING) ? '\"' : '\'';
}

static inline bool IsOpenBracket(int ch) {
	return ch == '[' || ch == '{';
}

static inline bool IsCloseBracket(int ch) {
	return ch == ']' || ch == '}';
}

static bool IsYAMLFlowIndicator(int ch) {
	return IsOpenBracket(ch) || IsCloseBracket(ch) || ch == ',';
}

#define IS_YAML_SEPARATOR(ch)		(IsSpace(ch) || ch == ':')
#define IS_YAML_BLOCK_SCALAR(ch)	(ch == '|' || ch == '>')
#define IS_YAML_NUMBER_END(ch)		(IsSpace(ch) || ch == ',' ||	\
									 IsCloseBracket(ch) || ch == '#')

#define CHECK_TYPE_WORD_STRING(quote)					\
	if (typeValue) {									\
		sc.GetCurrent(ident, sizeof(ident));			\
		const char *str = ident;						\
		if (*str == quote) str++;						\
		if (type_words.InList(str))						\
			sc.ChangeState(SCE_YAML_TYPE_WORD_STR);		\
	}													\
	typeValue = false;									\
	sc.ForwardSetState(SCE_YAML_DEFAULT)

static bool IsYAMLStringState(int state) {
	return state == SCE_YAML_DOUBLESTRING ||
		   state == SCE_YAML_SINGLESTRING ||
		   state == SCE_YAML_STRING_CONTINUED ||
		   state == SCE_YAML_BACKSLASH_WRONG;
}

static bool IsYAMLValueStart(const StyleContext &sc) {
	const int prev = sc.chPrev;
	return prev == 0 || IS_YAML_SEPARATOR(prev) ||
		   prev == '-' || IsOpenBracket(prev) ||
		   prev == ',' || prev == '?';
}

static bool IsYAMLActiveFlowIndicator(const StyleContext &sc, int flowDepth) {
	if (!IsYAMLFlowIndicator(sc.ch))
		return false;
	if (flowDepth > 0)
		return true;
	return IsOpenBracket(sc.ch) && IsYAMLValueStart(sc);
}

static int YAMLHexDigitValue(int ch) {
	return IsDigit(ch) ? ch - '0' : ToUpper(ch) - 'A' + 10;
}

static void InitYAMLEscape(EscapeSequence &escapeSeq, int nextChar) {
	escapeSeq.initEscapeState(nextChar);
	if (nextChar == 'x')
		escapeSeq.digitsLeft = 3;
	else if (nextChar == 'u')
		escapeSeq.digitsLeft = 5;
	else if (nextChar == 'U')
		escapeSeq.digitsLeft = 9;
}

// Returns 0 for a valid double-quoted escape sequence; otherwise returns
// the number of characters starting at the backslash to highlight as wrong.
static Sci_Position YAMLBadEscapeLength(StyleContext &sc) {
	switch (sc.GetRelative(1)) {
		case '0': case 'a': case 'b': case 't': case 'n': case 'v':
		case 'f': case 'r': case 'e': case ' ': case '\"': case '/':
		case '\\': case 'N': case '_': case 'L': case 'P':
			return 0;
		case 'x':
			for (Sci_Position i = 2; i < 4; i++) {
				if (!IsDigit(sc.GetRelative(i), 16))
					return i;
			}
			return 0;
		case 'u': {
			for (Sci_Position i = 2; i < 6; i++) {
				if (!IsDigit(sc.GetRelative(i), 16))
					return i;
			}
			int value = 0;
			for (Sci_Position i = 2; i < 6; i++)
				value = (value << 4) | YAMLHexDigitValue(sc.GetRelative(i));
			if (value >= 0xD800 && value <= 0xDFFF)
				return 6;
			return 0;
		}
		case 'U': {
			for (Sci_Position i = 2; i < 10; i++) {
				if (!IsDigit(sc.GetRelative(i), 16))
					return i;
			}
			unsigned long value = 0;
			for (Sci_Position i = 2; i < 10; i++)
				value = (value << 4) | YAMLHexDigitValue(sc.GetRelative(i));
			if (value > 0x10FFFFUL || (value >= 0xD800 && value <= 0xDFFF))
				return 10;
			return 0;
		}
		default:
			return 2;
	}
}

static void StartBlockScalar(StyleContext &sc, Accessor &styler,
							 Sci_Position line, int parentIndent) {
	int contentIndent = -1;
	bool sawIndent = false;
	
	for (Sci_Position pos = 1; pos <= 2; pos++) {
		const int ch = sc.GetRelative(pos);
		if (ch >= '1' && ch <= '9' && !sawIndent) {
			contentIndent = parentIndent + (ch - '0');
			sawIndent = true;
		} else if (!IsSign(ch)) {
			break;
		}
	}
	
	if (contentIndent >= 0) {
		styler.SetLineState(line, YAML_STATE_TEXT | contentIndent);
	} else {
		styler.SetLineState(line, YAML_STATE_TEXT_PENDING | parentIndent);
	}
}

static bool IsTypeKey(const char *ident) {
	while (IsSpaceOrTab(*ident))
		ident++;
	
	size_t len = strlen(ident);
	while (len > 0 && IsSpaceOrTab(ident[len - 1]))
		len--;
	
	if (len >= 2 && IsQuote(ident[0]) && ident[len - 1] == ident[0]) {
		ident++;
		len -= 2;
	}
	
	return (len == 4 && strncmp(ident, "type", 4) == 0) ||
		   (len == 6 && strncmp(ident, "x-type", 6) == 0);
}

static bool IsQuotedKey(StyleContext &sc) {
	const int quote = sc.ch;
	if (!IsQuote(quote))
		return false;
	
	Sci_Position pos = 1;
	while (true) {
		const int ch = sc.GetRelative(pos);
		if (ch == 0 || IsCRLF(ch))
			return false;
		
		if (quote == '\'') {
			if (ch == '\'') {
				if (sc.GetRelative(pos + 1) == '\'') {
					pos += 2;	// Escaped single quote
					continue;
				}
				pos++;
				break;
			}
		} else {
			if (ch == '\\') {
				const int next = sc.GetRelative(pos + 1);
				if (next == 0 || IsCRLF(next))
					return false;
				pos += 2;	// Skip escaped character
				continue;
			}
			if (ch == '\"') {
				pos++;
				break;
			}
		}
		pos++;
	}
	
	while (IsSpaceOrTab(sc.GetRelative(pos)))
		pos++;
	
	return sc.GetRelative(pos) == ':';
}

enum YAMLNumberType {
	YAML_NUMBER_NONE,
	YAML_NUMBER_DECIMAL,
	YAML_NUMBER_HEX,
	YAML_NUMBER_EXP
};

static YAMLNumberType ParseNumber(StyleContext &sc) {
	Sci_Position pos = 0;
	if (IsSign(sc.GetRelative(pos)))
		pos++;
	
	if (sc.GetRelative(pos) == '0' &&
		IsHex(sc.GetRelative(pos + 1))) {
		pos += 2;
		int digits = 0;
		while (IsDigit(sc.GetRelative(pos), 16)) {
			digits++;
			pos++;
		}
		if (digits == 0)
			return YAML_NUMBER_NONE;
		
		while (IsSpaceOrTab(sc.GetRelative(pos)))
			pos++;
		const int ch = sc.GetRelative(pos);
		if (ch == ':' || !(ch == 0 || IS_YAML_NUMBER_END(ch)))
			return YAML_NUMBER_NONE;
		
		return YAML_NUMBER_HEX;
	}
	
	int digits = 0;
	while (IsDigit(sc.GetRelative(pos))) {
		digits++;
		pos++;
	}
	if (sc.GetRelative(pos) == '.') {
		pos++;
		while (IsDigit(sc.GetRelative(pos))) {
			digits++;
			pos++;
		}
	}
	if (digits == 0)
		return YAML_NUMBER_NONE;
	
	if (IsDecExponent(sc.GetRelative(pos))) {
		pos++;
		if (IsSign(sc.GetRelative(pos)))
			pos++;
		int expDigits = 0;
		while (IsDigit(sc.GetRelative(pos))) {
			expDigits++;
			pos++;
		}
		if (expDigits == 0)
			return YAML_NUMBER_NONE;
		
		while (IsSpaceOrTab(sc.GetRelative(pos)))
			pos++;
		const int ch = sc.GetRelative(pos);
		if (ch == ':' || !(ch == 0 || IS_YAML_NUMBER_END(ch)))
			return YAML_NUMBER_NONE;
		
		return YAML_NUMBER_EXP;
	}
	
	while (IsSpaceOrTab(sc.GetRelative(pos)))
		pos++;
	const int ch = sc.GetRelative(pos);
	if (ch == ':' || !(ch == 0 || IS_YAML_NUMBER_END(ch)))
		return YAML_NUMBER_NONE;
	
	return YAML_NUMBER_DECIMAL;
}

static int StartNumber(StyleContext &sc) {
	if (!(IsDigit(sc.ch) || IsSign(sc.ch) || sc.ch == '.'))
		return 0;
	
	if (IsDigit(sc.chPrev) || IsAlphaWordChar(sc.chPrev))
		return 0;
	if (sc.chPrev == '-' &&
		(IsDigit(sc.GetRelative(-2)) || IsAlphaWordChar(sc.GetRelative(-2))))
		return 0;
	
	const YAMLNumberType type = ParseNumber(sc);
	if (type == YAML_NUMBER_NONE)
		return 0;
	
	switch (type) {
		case YAML_NUMBER_HEX:
			return SCE_YAML_HEXNUMBER;
		case YAML_NUMBER_EXP:
			return SCE_YAML_EXPNUMBER;
		default:
			return SCE_YAML_NUMBER;
	}
}

static void ColouriseYAMLDoc(Sci_PositionU startPos, Sci_Position length,
							 int initStyle, WordList *keywordLists[],
							 Accessor &styler) {
	// esh: escapesequence highlighting
	const bool escapeSequence =
					styler.GetPropertyInt("lexer.yaml.escape.sequence", 0) != 0;
	EscapeSequence escapeSeq = EscapeSequence();
	
	WordList &com_words = *keywordLists[0];
	WordList &type_words = *keywordLists[1];
	WordList &task_markers = *keywordLists[2];
	
	StyleContext sc(startPos, length, initStyle, styler);
	Sci_Position lineCurrent = styler.GetLine(startPos);
	
	int indent = 0;
	char ident[100];
	bool inQuotes = false;
	bool blockScalarLine = false;
	bool blockScalarPending = false;
	int blockParentIndent = 0;
	int pendingStringStyle = 0;
	int numberStyle = 0;
	bool typeValue = false;
	int escapeReturnState = SCE_YAML_DOUBLESTRING;
	bool singleQuoteEscape = false;
	Sci_Position wrongEscapeLeft = 0;
	int prevLineState = 0;
	if (lineCurrent > 0)
		prevLineState = styler.GetLineState(lineCurrent - 1);
	int flowBits = prevLineState & YAML_FLOW_ALL_MASK;
	int flowDepth = YAMLFlowDepth(flowBits);
	
	for (; sc.More(); sc.Forward()) {
		if (sc.state == SCE_YAML_BACKSLASH_WRONG) {
			if (wrongEscapeLeft > 0)
				wrongEscapeLeft--;
			if (wrongEscapeLeft == 0)
				sc.SetState(SCE_YAML_DOUBLESTRING);
		}
		
		int prevState = 0;
		if (sc.atLineStart) {
			if (lineCurrent > 0)
				prevState = styler.GetLineState(lineCurrent - 1);
			flowBits = prevState & YAML_FLOW_ALL_MASK;
			flowDepth = YAMLFlowDepth(flowBits);
		}
		
		if (sc.atLineStart && !IsYAMLStringState(sc.state) && flowDepth == 0) {
			int spaceIndent = 0;
			indent = 0;
			inQuotes = false;
			blockScalarLine = false;
			blockScalarPending = false;
			pendingStringStyle = 0;
			singleQuoteEscape = false;
			typeValue = false;
			sc.SetState(SCE_YAML_DEFAULT);
			
			while (sc.More() && sc.ch == ' ') {
				spaceIndent++;
				indent++;
				sc.Forward();
			}
			
			const int prevType = prevState & YAML_STATE_MASK;
			const int prevIndent = prevState & YAML_INDENT_MASK;
			bool isText = false;
			
			if (prevType == YAML_STATE_KEY_PENDING && sc.atLineEnd) {
				// Keep waiting for a possible block scalar on a later line.
				styler.SetLineState(lineCurrent, YAML_STATE_KEY_PENDING | prevIndent);
			} else if (sc.ch == '\t') { // Tabs are not allowed for indentation
				styler.SetLineState(lineCurrent, 0);
				sc.ChangeState(SCE_YAML_ERROR);
			} else {
				if (prevType == YAML_STATE_TEXT) {
					if (sc.atLineEnd || spaceIndent >= prevIndent) {
						isText = true;
						styler.SetLineState(lineCurrent, YAML_STATE_TEXT | prevIndent);
					}
				} else if (prevType == YAML_STATE_TEXT_PENDING) {
					if (sc.atLineEnd) {
						isText = true;
						styler.SetLineState(lineCurrent, YAML_STATE_TEXT_PENDING | prevIndent);
					} else if (spaceIndent > prevIndent) {
						isText = true;
						styler.SetLineState(lineCurrent, YAML_STATE_TEXT | spaceIndent);
					}
				}
				
				if (isText) {
					sc.ChangeState(SCE_YAML_TEXT);
				} else {
					int lastDashIndent = -1;
					
					// Sequence indicators are counted as indentation and
					// highlighted like the ':' operator.
					while (sc.More()) {
						if (sc.ch == '-' && IsSpace(sc.chNext)) {
							lastDashIndent = indent;
							indent++;
							sc.SetState(SCE_YAML_OPERATOR);
							sc.ForwardSetState(SCE_YAML_DEFAULT);
						} else if (sc.ch == ' ') {
							indent++;
							sc.Forward();
						} else {
							break;
						}
					}
					
					if (IS_YAML_BLOCK_SCALAR(sc.ch)) {
						blockParentIndent = spaceIndent;
						if (lastDashIndent >= 0) {
							blockParentIndent = lastDashIndent;
						} else if (prevType == YAML_STATE_KEY_PENDING) {
							blockParentIndent = prevIndent;
						}
						blockScalarPending = true;
					} else if (lastDashIndent >= 0 && (sc.atLineEnd || IsCRLF(sc.ch))) {
						// Sequence item with an empty value: a block scalar
						// may follow on the next line.
						styler.SetLineState(lineCurrent,
											YAML_STATE_KEY_PENDING | lastDashIndent);
					} else {
						styler.SetLineState(lineCurrent, 0);
						
						if (sc.atLineStart && (sc.Match("---") || sc.Match("..."))) {
							sc.SetState(SCE_YAML_DOCUMENT);
						} else if (sc.ch == '#') {
							sc.ChangeState(SCE_YAML_COMMENT);
						} else if (IsQuote(sc.ch)) {
							if (IsQuotedKey(sc)) {
								sc.ChangeState(SCE_YAML_IDENTIFIER);
							} else {
								pendingStringStyle = GetStringStyle(sc.ch);
							}
						} else if ((numberStyle = StartNumber(sc)) != 0) {
							sc.SetState(numberStyle);
						} else if (!sc.atLineEnd) {
							sc.ChangeState(SCE_YAML_IDENTIFIER);
						}
					}
				}
			}
		} else if (sc.atLineStart) {
			styler.SetLineState(lineCurrent, flowBits);
		}
		
		// Determine if the current state should terminate.
		switch (sc.state) {
			case SCE_YAML_IDENTIFIER:
			case SCE_YAML_FLOW_IDENT:
				if (sc.atLineEnd) {
					sc.ChangeState(SCE_YAML_DEFAULT);
				} else if (IsQuote(sc.ch)) {
					inQuotes = !inQuotes;
				} else if (sc.ch == '#' && !inQuotes && IsSpace(sc.chPrev)) {
					sc.ChangeState(SCE_YAML_DEFAULT);
					sc.SetState(SCE_YAML_COMMENT);
				} else if (IsYAMLActiveFlowIndicator(sc, flowDepth)) {
					sc.SetState(SCE_YAML_DEFAULT);
				} else if (sc.ch == ':' && !inQuotes) {
					sc.GetCurrent(ident, sizeof(ident));
					typeValue = IsTypeKey(ident);
					
					sc.SetState(SCE_YAML_OPERATOR);
					
					if (flowDepth > 0 &&
						YAMLFlowTop(flowBits) == YAML_FLOW_MAP_KEY) {
						flowBits = YAMLSetFlowTop(flowBits, YAML_FLOW_MAP_VALUE);
						flowDepth = YAMLFlowDepth(flowBits);
					}
					
					Sci_Position pos = 1;
					while (IsSpaceOrTab(sc.GetRelative(pos))) pos++;
					
					const int next = sc.GetRelative(pos);
					if (next == 0 || IsCRLF(next))
						styler.SetLineState(lineCurrent, YAML_STATE_KEY_PENDING | indent);
					inQuotes = false;
				}
				break;
			
			case SCE_YAML_OPERATOR:
			case SCE_YAML_SPEC_OPER:
			case SCE_YAML_FLOW_OPER:
				sc.SetState(SCE_YAML_DEFAULT);
				break;
			
			case SCE_YAML_COMMENT:
				HighlightTaskMarker(sc, styler, task_markers, true,
									SCE_YAML_TASKMARKER);
				if (sc.atLineEnd)
					sc.SetState(SCE_YAML_DEFAULT);
				break;
			
			case SCE_YAML_COM_WORD:
				if (sc.atLineEnd || IsSpace(sc.ch) ||
					IsYAMLActiveFlowIndicator(sc, flowDepth)) {
					sc.GetCurrent(ident, sizeof(ident));
					
					if (typeValue && type_words.InList(ident)) {
						sc.ChangeState(SCE_YAML_TYPE_WORD);
					} else if (!com_words.InList(ident)) {
						sc.ChangeState(SCE_YAML_DEFAULT);
					}
					sc.SetState(SCE_YAML_DEFAULT);
					typeValue = false;
				}
				break;
			
			case SCE_YAML_NUMBER:
			case SCE_YAML_HEXNUMBER:
			case SCE_YAML_EXPNUMBER: {
				bool cont;
				if (sc.state == SCE_YAML_NUMBER) {
					cont = IsDigit(sc.ch) || sc.ch == '.' ||
						   (IsSign(sc.ch) &&
							!IsDigit(sc.chPrev) && !IsAlphaWordChar(sc.chPrev));
				} else if (sc.state == SCE_YAML_HEXNUMBER) {
					cont = IsDigit(sc.ch, 16) || IsHex(sc.ch) ||
						   (IsSign(sc.ch) &&
							!IsDigit(sc.chPrev) && !IsAlphaWordChar(sc.chPrev));
				} else {
					cont = IsDigit(sc.ch) || sc.ch == '.' || IsDecExponent(sc.ch) ||
						   (IsSign(sc.ch) &&
							(IsDecExponent(sc.chPrev) ||
							 (!IsDigit(sc.chPrev) && !IsAlphaWordChar(sc.chPrev))));
				}
				if (cont) continue;
				sc.SetState(SCE_YAML_DEFAULT);
				break;
			}
			
			case SCE_YAML_REFERENCE:
				if (IsSpace(sc.ch))
					sc.SetState(SCE_YAML_DEFAULT);
				break;
			
			case SCE_YAML_DOUBLESTRING:
				if (sc.ch == '\\') {
					if (IsCRLF(sc.chNext)) {
						sc.SetState(SCE_YAML_STRING_CONTINUED);
						continue;
					}
					const Sci_Position badEscape = YAMLBadEscapeLength(sc);
					if (badEscape > 0) {
						sc.SetState(SCE_YAML_BACKSLASH_WRONG);
						wrongEscapeLeft = badEscape;
						continue;
					}
					if (escapeSequence) {
						escapeReturnState = SCE_YAML_DOUBLESTRING;
						sc.SetState(SCE_YAML_ESCAPESEQ);
						InitYAMLEscape(escapeSeq, sc.chNext);
					}
					sc.Forward(); // Skip any character after the backslash
					continue;
				} else if (sc.ch == '\"') {
					CHECK_TYPE_WORD_STRING('\"');
				}
				break;
			
			case SCE_YAML_SINGLESTRING:
				if (sc.ch == '\'') {
					if (sc.chNext == '\'') {
						if (escapeSequence) {
							escapeReturnState = SCE_YAML_SINGLESTRING;
							singleQuoteEscape = true;
							sc.SetState(SCE_YAML_ESCAPESEQ);
						}
						sc.Forward(); // Skip escaped single quote
						continue;
					}
					CHECK_TYPE_WORD_STRING('\'');
				}
				break;
			
			case SCE_YAML_ESCAPESEQ:
				if (singleQuoteEscape) {
					singleQuoteEscape = false;
					sc.SetState(SCE_YAML_SINGLESTRING);
					
					if (sc.ch == '\'') {
						if (sc.chNext == '\'') {
							escapeReturnState = SCE_YAML_SINGLESTRING;
							singleQuoteEscape = true;
							sc.SetState(SCE_YAML_ESCAPESEQ);
							sc.Forward();
							continue;
						}
						sc.ForwardSetState(SCE_YAML_DEFAULT);
						typeValue = false;
					}
					break;
				}
				
				escapeSeq.digitsLeft--;
				if (!escapeSeq.atEscapeEnd(sc.ch))
					continue;
				if (sc.ch == '\\') {
					InitYAMLEscape(escapeSeq, sc.chNext);
					sc.Forward();
					continue;
				}
				sc.SetState(escapeReturnState);
				if (sc.ch == GetClosingChar(escapeReturnState)) {
					sc.ForwardSetState(SCE_YAML_DEFAULT);
					typeValue = false;
				}
				break;
		}
		
		// Determine if a new state should be entered.
		if (sc.state == SCE_YAML_DEFAULT) {
			if (pendingStringStyle) {
				sc.SetState(pendingStringStyle);
				pendingStringStyle = 0;
			} else if (blockScalarPending) {
				StartBlockScalar(sc, styler, lineCurrent, blockParentIndent);
				sc.SetState(SCE_YAML_SPEC_OPER);
				blockScalarLine = true;
				blockScalarPending = false;
			} else if (sc.ch == '#' && !inQuotes && IS_YAML_SEPARATOR(sc.chPrev)) {
				sc.SetState(SCE_YAML_COMMENT);
			} else if (blockScalarLine && (IsSign(sc.ch) || IsDigit(sc.ch))) {
				sc.SetState(SCE_YAML_SPEC_OPER);
			} else if (!blockScalarLine) {
				if (IsYAMLActiveFlowIndicator(sc, flowDepth)) {
					sc.SetState(SCE_YAML_FLOW_OPER);
					if (sc.ch == '[') {
						flowBits = YAMLPushFlow(flowBits, YAML_FLOW_SEQ);
					} else if (sc.ch == '{') {
						flowBits = YAMLPushFlow(flowBits, YAML_FLOW_MAP_KEY);
					} else if (IsCloseBracket(sc.ch)) {
						const int top = YAMLFlowTop(flowBits);
						if ((sc.ch == ']' && top == YAML_FLOW_SEQ) ||
							(sc.ch == '}' &&
							 (top == YAML_FLOW_MAP_KEY ||
							  top == YAML_FLOW_MAP_VALUE))) {
							flowBits = YAMLPopFlow(flowBits);
						}
					} else {
						const int top = YAMLFlowTop(flowBits);
						if (top == YAML_FLOW_MAP_KEY ||
							top == YAML_FLOW_MAP_VALUE)
							flowBits = YAMLSetFlowTop(flowBits, YAML_FLOW_MAP_KEY);
					}
					flowDepth = YAMLFlowDepth(flowBits);
				} else if (flowDepth == 0 &&
						   IS_YAML_BLOCK_SCALAR(sc.ch) &&
						   IS_YAML_SEPARATOR(sc.chPrev)) {
					StartBlockScalar(sc, styler, lineCurrent, indent);
					sc.SetState(SCE_YAML_SPEC_OPER);
					blockScalarLine = true;
				} else if (sc.ch == '&' || sc.ch == '*') {
					sc.SetState(SCE_YAML_REFERENCE);
				} else if (flowDepth > 0 &&
						   YAMLFlowTop(flowBits) == YAML_FLOW_MAP_KEY &&
						   (IsQuote(sc.ch) || IsAlphaWordChar(sc.ch) ||
							IsDigit(sc.ch) || IsSign(sc.ch) || sc.ch == '.')) {
					if (IsQuote(sc.ch)) {
						if (IsQuotedKey(sc)) {
							sc.SetState(SCE_YAML_FLOW_IDENT);
							inQuotes = true;
						} else {
							sc.SetState(GetStringStyle(sc.ch));
						}
					} else {
						sc.SetState(SCE_YAML_FLOW_IDENT);
					}
				} else if ((numberStyle = StartNumber(sc)) != 0) {
					sc.SetState(numberStyle);
				} else if (sc.ch == '\"' && IsYAMLValueStart(sc)) {
					sc.SetState(SCE_YAML_DOUBLESTRING);
				} else if (sc.ch == '\'' && IsYAMLValueStart(sc)) {
					sc.SetState(SCE_YAML_SINGLESTRING);
				} else if (IsAlphaWordChar(sc.ch)) {
					sc.SetState(SCE_YAML_COM_WORD);
				}
			}
		}
		
		if (sc.atLineEnd) {
			if (sc.state == SCE_YAML_STRING_CONTINUED
				|| sc.state == SCE_YAML_BACKSLASH_WRONG) {
				sc.SetState(SCE_YAML_DOUBLESTRING);
				wrongEscapeLeft = 0;
			} else if (!IsYAMLStringState(sc.state)) {
				sc.SetState(SCE_YAML_DEFAULT);
			}
			int lineState = styler.GetLineState(lineCurrent);
			lineState = (lineState & ~YAML_FLOW_ALL_MASK) | flowBits;
			styler.SetLineState(lineCurrent, lineState);
			lineCurrent++;
		}
	}
	sc.Complete();
}

static bool IsCommentLine(Sci_Position line, Accessor &styler) {
	Sci_Position pos = styler.LineStart(line);
	if (styler[pos] == '#')
		return true;
	return false;
}

static void FoldYAMLDoc(Sci_PositionU startPos, Sci_Position length,
						int /*initStyle - unused*/, WordList *[],
						Accessor &styler) {
	const Sci_Position maxPos = startPos + length;
	const Sci_Position maxLines = styler.GetLine(maxPos - 1);             // Requested last line
	const Sci_Position docLines = styler.GetLine(styler.Length() - 1);  // Available last line
	const bool foldComment = styler.GetPropertyInt("fold.comment.yaml") != 0;
	
	// Backtrack to previous non-blank line so we can determine indent level
	// for any white space lines
	// and so we can fix any preceding fold level (which is why we go back
	// at least one line in all cases)
	int spaceFlags = 0;
	Sci_Position lineCurrent = styler.GetLine(startPos);
	int indentCurrent = styler.IndentAmount(lineCurrent, &spaceFlags, NULL);
	while (lineCurrent > 0) {
		lineCurrent--;
		indentCurrent = styler.IndentAmount(lineCurrent, &spaceFlags, NULL);
		if (!(indentCurrent & SC_FOLDLEVELWHITEFLAG) &&
				(!IsCommentLine(lineCurrent, styler)))
			break;
	}
	int indentCurrentLevel = indentCurrent & SC_FOLDLEVELNUMBERMASK;
	
	// Set up initial loop state
	int prevComment = 0;
	if (lineCurrent >= 1)
		prevComment = foldComment && IsCommentLine(lineCurrent - 1, styler);
	
	// Process all characters to end of requested range
	// or comment that hangs over the end of the range.  Cap processing in all cases
	// to end of document (in case of unclosed comment at end).
	while ((lineCurrent <= docLines) &&
		   ((lineCurrent <= maxLines) || prevComment)) {
		// Gather info
		int lev = indentCurrent;
		Sci_Position lineNext = lineCurrent + 1;
		int indentNext = indentCurrent;
		if (lineNext <= docLines) {
			// Information about next line is only available if not at end of document
			indentNext = styler.IndentAmount(lineNext, &spaceFlags, NULL);
		}
		const int comment = foldComment && IsCommentLine(lineCurrent, styler);
		const int comment_start = (comment && !prevComment &&
								   (lineNext <= docLines) &&
								   IsCommentLine(lineNext, styler) &&
								   (lev > SC_FOLDLEVELBASE));
		const int comment_continue = (comment && prevComment);
		if (!comment)
			indentCurrentLevel = indentCurrent & SC_FOLDLEVELNUMBERMASK;
		if (indentNext & SC_FOLDLEVELWHITEFLAG)
			indentNext = SC_FOLDLEVELWHITEFLAG | indentCurrentLevel;
		
		if (comment_start) {
			// Place fold point at start of a block of comments
			lev |= SC_FOLDLEVELHEADERFLAG;
		} else if (comment_continue) {
			// Add level to rest of lines in the block
			lev = lev + 1;
		}
		
		// Skip past any blank lines for next indent level info; we skip also
		// comments (all comments, not just those starting in column 0)
		// which effectively folds them into surrounding code rather
		// than screwing up folding.
		
		while ((lineNext < docLines) &&
				((indentNext & SC_FOLDLEVELWHITEFLAG) ||
				 (lineNext <= docLines && IsCommentLine(lineNext, styler)))) {
			lineNext++;
			indentNext = styler.IndentAmount(lineNext, &spaceFlags, NULL);
		}
		
		const int levelAfterComments = indentNext & SC_FOLDLEVELNUMBERMASK;
		const int levelBeforeComments = Maximum(indentCurrentLevel,
												levelAfterComments);
		
		// Now set all the indent levels on the lines we skipped
		// Do this from end to start.  Once we encounter one line
		// which is indented more than the line after the end of
		// the comment-block, use the level of the block before
		
		Sci_Position skipLine = lineNext;
		int skipLevel = levelAfterComments;
		
		while (--skipLine > lineCurrent) {
			int skipLineIndent = styler.IndentAmount(skipLine, &spaceFlags, NULL);
			
			if ((skipLineIndent & SC_FOLDLEVELNUMBERMASK) > levelAfterComments)
				skipLevel = levelBeforeComments;
			
			int whiteFlag = skipLineIndent & SC_FOLDLEVELWHITEFLAG;
			
			styler.SetLevel(skipLine, skipLevel | whiteFlag);
		}
		
		// Set fold header on non-comment line
		if (!comment && !(indentCurrent & SC_FOLDLEVELWHITEFLAG)) {
			if ((indentCurrent & SC_FOLDLEVELNUMBERMASK) <
				(indentNext & SC_FOLDLEVELNUMBERMASK))
				lev |= SC_FOLDLEVELHEADERFLAG;
		}
		
		// Keep track of block comment state of previous line
		prevComment = comment_start || comment_continue;
		
		// Set fold level for this line and move to next line
		styler.SetLevel(lineCurrent, lev);
		indentCurrent = indentNext;
		lineCurrent = lineNext;
	}
	
	// NOTE: Cannot set level of last line here because indentCurrent doesn't have
	// header flag set; the loop above is crafted to take care of this case!
	//styler.SetLevel(lineCurrent, indentCurrent);
}

LexerModule lmYAML(SCLEX_YAML, ColouriseYAMLDoc, "yaml",
				   FoldYAMLDoc, yamlWordListDesc);
