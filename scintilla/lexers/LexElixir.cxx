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

#include "ILexer.h"
#include "Scintilla.h"
#include "SciLexer.h"

#include "WordList.h"
#include "LexAccessor.h"
#include "Accessor.h"
#include "StyleContext.h"
#include "CharacterSet.h"
#include "LexerModule.h"

using namespace Scintilla;

struct AtomPunctSequence {
	int charsLeft;
	char validChar;
	AtomPunctSequence() {
		charsLeft = 0;
		validChar = ' ';
	}
	void initAtomPunctState(int nextChar) {
		if (strchr("&-+=.", nextChar)) {
			charsLeft = 3;
			validChar = nextChar;
		} else if (nextChar == '<') {
			charsLeft = 2;
			validChar = '>';
		} else if (nextChar == '{') {
			charsLeft = 2;
			validChar = '}';
		} else if (nextChar == '*') {
			charsLeft = 2;
			validChar = '*';
		} else {
			charsLeft = 1;
		}
	}
	bool atAtomPunctEnd(int currChar) const {
		return (charsLeft <= 0) || (currChar != validChar);
	}
};

static bool is_radix(int radix, int ch) {
	int digit;
	
	if (radix < 2 || radix > 36)
		return false;
	
	if (isdigit(ch)) {
		digit = ch - '0';
	} else if (isalnum(ch)) {
		digit = toupper(ch) - 'A' + 10;
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
	TYPEOPER_STATE,		// ::
	DOTOPER_STATE		// . example: Struct.field, Module.func()
} ident_state_t;

typedef enum {
	NONE_MODULE,
	OTHER_MODULE,
	KERNEL_MODULE
} module_type_t;

static inline bool IsElixirOperator(const int ch) {
	return (isoperator(ch) || ch == '\\');
}

static inline bool IsSpaceEquivStyle(int style) {
	return (style == SCE_ELIXIR_DEFAULT ||
			style == SCE_ELIXIR_COMMENT);
}

static inline bool IsOperatorStyle(int style) {
	return (style == SCE_ELIXIR_OPERATOR ||
			style == SCE_ELIXIR_MAP_OPER ||
			style == SCE_ELIXIR_CAPTURE_OPER);
}

static inline bool IsStdWordOrAttrStyle(int style) {
	return (style == SCE_ELIXIR_STD_WORD ||
			style == SCE_ELIXIR_ADD_WORD ||
			style == SCE_ELIXIR_STD_MODULE_ATTR);
}

static inline bool IsStringValStyle(int style) {
	return (style == SCE_ELIXIR_TRIPLEVAL ||
			style == SCE_ELIXIR_STRINGVAL ||
			style == SCE_ELIXIR_CHARLISTVAL ||
			style == SCE_ELIXIR_LITERALVAL ||
			style == SCE_ELIXIR_LITERALTRIPLEVAL);
}

static inline bool IsStringStyle(int style) {
	return (IsStringValStyle(style) ||
			style == SCE_ELIXIR_TRIPLE ||
			style == SCE_ELIXIR_STRING ||
			style == SCE_ELIXIR_CHARLIST ||
			style == SCE_ELIXIR_LITERAL ||
			style == SCE_ELIXIR_LITERALTRIPLE);
}

static inline bool IsNestedStringStyle(int style) {
	return (style == SCE_ELIXIR_ESCAPESEQ ||
			style == SCE_ELIXIR_FORMATSEQ);
}

static inline bool IsAWordStart(const int ch) {
	return (ch < 0x80) && (ch != ' ') && (isalpha(ch) || ch == '_');
}

static inline bool IsAWordEnd(const int ch) {
	return (ch == '!' || ch == '?');
}

static inline bool IsAWordChar(const int ch) {
	return (ch < 0x80) && (ch != ' ') && (isalnum(ch) || ch == '_');
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

#define L_LITERAL_PREFIX "scrwp"
#define U_LITERAL_PREFIX "SCRWNUDT"

#define MOVE_INDEX_TO_NONSPACE								\
	Sci_PositionU i = sc.currentPos + 1;					\
	while (i < endPos && IsASpaceOrTab(styler[i]))			\
		i++;

#define SKIP_SPACES											\
	while (sc.More() && IsASpaceOrTab(sc.ch))				\
		sc.Forward();

#define CHANGE_STATE_BY_MODULE								\
	module_type == KERNEL_MODULE && stdFuncs.InList(cur)	\
		? sc.ChangeState(SCE_ELIXIR_STD_FUNC)				\
		: sc.ChangeState(SCE_ELIXIR_FUNCTION);

#define CHANGE_STATE_BY_FUNCLIST										\
	if (stdFuncs.InList(cur)) {											\
		sc.ChangeState(SCE_ELIXIR_STD_FUNC);							\
	} else if (!exclLibFuncs.InList(cur) && libMacros.InList(cur)) {	\
		sc.ChangeState(SCE_ELIXIR_LIB_FUNC);							\
	} else {															\
		sc.ChangeState(SCE_ELIXIR_FUNCTION);							\
	}

#define CHECK_LIB_MACROS												\
	if ((!IsElixirOperator(sc.ch) || strchr("{[<^!%~", sc.ch) ||		\
		 (sc.ch == ':' && sc.chNext != ':'))							\
		&& !exclLibMacros.InList(cur) && libMacros.InList(cur)) {		\
		if ((strcmp(cur, "channel") == 0 && sc.ch != '"') ||			\
			(strcmp(cur, "socket") == 0 && sc.ch != '"') ||				\
			(strcmp(cur, "schema") == 0 && sc.ch != '"') ||				\
			(strcmp(cur, "field") == 0 && sc.ch != ':')) {				\
			/* do not change the state */								\
		} else {														\
			sc.ChangeState(SCE_ELIXIR_LIB_MACRO);						\
		}																\
	}

#define SET_LITERAL_STATE													\
	closing_char = GetClosingChar(sc.GetRelative(2));						\
	if (closing_char == ' ') {												\
		sc.SetState(SCE_ELIXIR_UNKNOWN);									\
		sc.Forward();														\
	} else {																\
		sc.SetState(assign_to_strfield ? SCE_ELIXIR_LITERALVAL				\
									   : SCE_ELIXIR_LITERAL);				\
		sc.Forward(2);														\
		if (sc.Match(R"(""")") || sc.Match(R"(''')")) {						\
			sc.ChangeState(assign_to_strfield ? SCE_ELIXIR_LITERALTRIPLEVAL	\
											  : SCE_ELIXIR_LITERALTRIPLE);	\
			sc.Forward(2);													\
		}																	\
		string_state = sc.state;											\
	}

#define CHECK_INTERPOLATE_STRING											\
	} else if (canbe_interpolate && sc.Match("#{")) {						\
		sc.SetState(SCE_ELIXIR_STRING_SUBOPER);								\
		sc.Forward();														\
		sc.ForwardSetState(SCE_ELIXIR_DEFAULT);								\
		nesting_count = 1;													\
		is_at_symb = true; /* otherwise atoms of the form :'tes#{}t@test'
							  will be incorrectly highlighted */

#define CHECK_ESCAPE_FORMAT_SEQ												\
	if (sc.ch == '\\') {													\
		if (escapeSequence) {												\
			is_char_escape = false;											\
			sc.SetState(SCE_ELIXIR_ESCAPESEQ);								\
			escapeSeq.initEscapeState(sc.chNext);							\
		}																	\
		sc.Forward(); /* Skip any character after the backslash */			\
		continue;															\
	} else if (sc.ch == '~' && formatSequence) {							\
		sc.SetState(SCE_ELIXIR_FORMATSEQ);									\
		formatSeq.initFormatState();										\
		continue;															\
	CHECK_INTERPOLATE_STRING

#define CHECK_CLOSING_STRING												\
	} else {																\
		sc.SetState(string_state);											\
		if ((string_state == SCE_ELIXIR_TRIPLE ||							\
			 string_state == SCE_ELIXIR_TRIPLEVAL ||						\
			 string_state == SCE_ELIXIR_LITERALTRIPLE ||					\
			 string_state == SCE_ELIXIR_LITERALTRIPLEVAL)					\
			&& sc.Match(GetTripleQuote(closing_char))) {					\
			sc.Forward(2);													\
			sc.ForwardSetState(SCE_ELIXIR_DEFAULT);							\
		} else if (sc.ch == closing_char) {									\
			sc.ForwardSetState(SCE_ELIXIR_DEFAULT);							\
		}																	\
	}

#define DEFINE_ASSIGN_TO_STRFIELD											\
	while (--back > 1 && IsSpaceEquivStyle(styler.StyleAt(back)))			\
		;																	\
	if (back > 1 && styler.StyleAt(back--) == SCE_ELIXIR_OPERATOR) {		\
		if (styler[back] == '=' && styler[back + 1] == '>') {				\
			while (--back && IsSpaceEquivStyle(styler.StyleAt(back)))		\
				;															\
			assign_to_strfield =											\
				(styler.StyleAt(back) == SCE_ELIXIR_STRING);				\
																			\
		} else if (styler[back] == '<' && styler[back + 1] == '>') {		\
			while (--back && IsSpaceEquivStyle(styler.StyleAt(back)))		\
				;															\
			assign_to_strfield = IsStringValStyle(styler.StyleAt(back));	\
		}																	\
	}


static void ColouriseElixirDoc(Sci_PositionU startPos, Sci_Position length,
							   int initStyle, WordList *keywordlists[],
							   Accessor &styler) {
	// esh: escapesequence highlighting
	const bool escapeSequence = styler.GetPropertyInt("lexer.elixir.escape.sequence", 0) != 0;
	EscapeSequence escapeSeq = EscapeSequence();
	
	// esh: formatsequence highlighting
	const bool formatSequence = styler.GetPropertyInt("lexer.elixir.format.sequence", 0) != 0;
	ErlFormatSequence formatSeq = ErlFormatSequence();
	
	AtomPunctSequence atomPunctSeq = AtomPunctSequence();
	
	Sci_PositionU endPos = startPos + length;
	
	//~ esh: before debugging, you need to start viewing logs with the command `journalctl -f`
	//~ printf("!!!Lex: currLine = %li, currChar = '%c', lastChar = '%c', "
				//~ "initStyle = %i, startPos = %li, length = %li\n",
		   //~ styler.GetLine(startPos) + 1, styler[startPos],
		   //~ styler[endPos - 2], initStyle, startPos, length);
	
	StyleContext sc(startPos, length, initStyle, styler);
	
	WordList &stdWords = *keywordlists[0];
	WordList &addWords = *keywordlists[1];
	WordList &stdAtoms = *keywordlists[2];
	WordList &stdFuncs = *keywordlists[3];
	WordList &stdModules = *keywordlists[4];
	WordList &stdModuleAttrs = *keywordlists[5];
	WordList &stdErlModules = *keywordlists[6];
	WordList &stdExcepts = *keywordlists[7];
	WordList &stdMacros = *keywordlists[8];
	WordList &typeFuncs = *keywordlists[9];
	WordList &libMacros = *keywordlists[10];
	WordList &exclLibMacros = *keywordlists[11];
	WordList &exclLibFuncs = *keywordlists[12];
	
	int radix_digits = 0;
	int exponent_digits = 0;
	number_state_t number_state;
	
	ident_state_t ident_state = NONE_STATE;
	module_type_t module_type = NONE_MODULE;
	
	char cur[100];
	bool is_at_symb = false;			// esh: "at" - is "@" symb (for node)
	
	// esh: added string_state for escape/format sequences highlighting
	int string_state = -1;
	int nesting_count = 0;
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
				
			} else if (backStyle == SCE_ELIXIR_STRING_SUBOPER) {
				while (--back && styler.StyleAt(back) != SCE_ELIXIR_STRING_SUBOPER)
					;
				back--; // skip back { in #{}
				continue;
				
			} else {
				// esh: define string_state, closing_char, canbe_interpolate
				if (!IsStringStyle(backStyle)) back++;
				string_state = styler.StyleAt(back);
				
				int index = back;
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
		while (--back && IsSpaceEquivStyle(styler.StyleAt(back)))
			;
		last_state = styler.StyleAt(back);
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
				
				// check spec/type/callback/macrocallback
				maybe_typefunc = (back + 3 < endPos &&
								  strchr("stcm", styler[back]) &&
								  strchr("pya", styler[back + 1]) &&
								  strchr("eplc", styler[back + 2]) &&
								  strchr("celr", styler[back + 3]));
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
	
	for (; sc.More(); sc.Forward()) {
		if (sc.state == SCE_ELIXIR_STRING_SUBOPER) {
			sc.SetState(string_state);
		}
		
		// Determine if the current state should terminate.
		switch (sc.state) {
			/* COMMENTS ----------------------------------------------------- */
			case SCE_ELIXIR_COMMENT : {
				if (sc.atLineEnd)
					sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			/* -------------------------------------------------------------- */
			
			/* Numerics ----------------------------------------------------- */
			case SCE_ELIXIR_NUMBER : {
				switch (number_state) {
					
					/* Simple integer */
					case NUMERAL_START : {
						if (isdigit(sc.ch)) {
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
						} else if (sc.ch == '.' && isdigit(sc.chNext)) {
							number_state = NUMERAL_FLOAT;
							continue;
						} else if (sc.ch == 'e' || sc.ch == 'E') {
							exponent_digits = 0;
							number_state = NUMERAL_EXPONENT;
							continue;
						} else if (sc.ch == '_') {
							if (isdigit(sc.chNext)) {
								continue;
							} else {
								sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
							}
						} else if (isalpha(sc.ch)) {
							sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
						}
					} break;
					
					/* Integer in other base than 10 (x#yyy) */
					case NUMERAL_BASE_VALUE : {
						if (is_radix(radix_digits, sc.ch)) {
							continue;
						} else if (isalnum(sc.ch)) {
							sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
						}
					} break;
					
					/* Float (x.yyy) */
					case NUMERAL_FLOAT : {
						if (sc.ch == 'e' || sc.ch == 'E') {
							exponent_digits = 0;
							number_state = NUMERAL_EXPONENT;
							continue;
						} else if (isdigit(sc.ch)) {
							continue;
						} else if (isalpha(sc.ch)) {
							sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
						}
					} break;
					
					/* Exponent, either integer or float (xEyy, x.yyEzzz) */
					case NUMERAL_EXPONENT : {
						if ((sc.ch == '-' || sc.ch == '+')
								&& isdigit(sc.chNext)) {
							continue;
						} else if (isdigit(sc.ch)) {
							exponent_digits++;
							continue;
						} else if (exponent_digits == 0 || isalpha(sc.ch)) {
							sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
						}
					} break;
				}
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			/* -------------------------------------------------------------- */
			
			/* Atoms -------------------------------------------------------- */
			case SCE_ELIXIR_ATOM : {
				if (sc.ch == '@' && !is_at_symb) {
					sc.ChangeState(SCE_ELIXIR_NODE);
					is_at_symb = true;
					continue;
				} else if (IsAWordChar(sc.ch)) {
					continue;
				} else if (IsAWordEnd(sc.ch)) {
					sc.Forward();
				}
				sc.GetCurrent(cur, sizeof(cur));
				SKIP_SPACES
				if (sc.ch == '.') {
					if (stdErlModules.InList(cur)) {
						sc.ChangeState(SCE_ELIXIR_STD_ERL_MODULE);
					} else {
						sc.ChangeState(SCE_ELIXIR_ERL_MODULE);
					}
				} else if (stdAtoms.InList(cur)) {
					sc.ChangeState(SCE_ELIXIR_STD_ATOM);
				}
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_ATOM_QUOTED : {
				if (sc.ch == '@' && !is_at_symb) {
					sc.ChangeState(SCE_ELIXIR_NODE_QUOTED);
					string_state = sc.state;
					is_at_symb = true;
				} else if (sc.ch == '\\') {
					sc.Forward(); // Skip any character after the backslash
					continue;
				CHECK_INTERPOLATE_STRING
				} else if (sc.ch == closing_char) {
					sc.ForwardSetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
			/* -------------------------------------------------------------- */
			
			/* Nodes -------------------------------------------------------- */
			case SCE_ELIXIR_NODE : {
				if (sc.ch == '@') {
					sc.ChangeState(SCE_ELIXIR_ATOM);
					continue;
				} else if (IsAWordChar(sc.ch)) {
					continue;
				} else if (IsAWordEnd(sc.ch)) {
					sc.Forward();
				}
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_NODE_QUOTED : {
				if (sc.ch == '\\') {
					sc.Forward(); // Skip any character after the backslash
					continue;
				CHECK_INTERPOLATE_STRING
				} else if (sc.ch == closing_char) {
					sc.ForwardSetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
			/* -------------------------------------------------------------- */
			
			case SCE_ELIXIR_TRIPLE :
			case SCE_ELIXIR_TRIPLEVAL :
			case SCE_ELIXIR_LITERALTRIPLE :
			case SCE_ELIXIR_LITERALTRIPLEVAL : {
				CHECK_ESCAPE_FORMAT_SEQ
				} else if (sc.Match(GetTripleQuote(closing_char))) {
					sc.Forward(2);
					sc.ForwardSetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
			
			case SCE_ELIXIR_STRING :
			case SCE_ELIXIR_STRINGVAL :
			case SCE_ELIXIR_CHARLIST :
			case SCE_ELIXIR_CHARLISTVAL :
			case SCE_ELIXIR_LITERAL :
			case SCE_ELIXIR_LITERALVAL : {
				CHECK_ESCAPE_FORMAT_SEQ
				} else if (sc.ch == closing_char) {
					sc.Forward();
					if (sc.state == SCE_ELIXIR_LITERAL
						|| sc.state == SCE_ELIXIR_LITERALVAL) {
						while (strchr("uismxfU", sc.ch)) // regex modifiers
							sc.Forward();
					}
					sc.SetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
			
			case SCE_ELIXIR_CHARACTER : {
				if (sc.ch == '\\') {
					// esh: we will check the escapeSequence parameter later,
					//		set SCE_ELIXIR_ESCAPESEQ for validation
					is_char_escape = true;
					sc.SetState(SCE_ELIXIR_ESCAPESEQ);
					escapeSeq.initEscapeState(sc.chNext);
					sc.Forward(); // Skip any character after the backslash
					continue;
				} else if (sc.atLineEnd) {
					sc.SetState(SCE_ELIXIR_DEFAULT);
				} else {
					sc.ForwardSetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
			
			case SCE_ELIXIR_ESCAPESEQ : {
				escapeSeq.digitsLeft--;
				if (!escapeSeq.atEscapeEnd(sc.ch)) {
					continue; // esh: continue of escape chars
				}
				if (is_char_escape) {
					if (!sc.atLineStart && isdigit(sc.ch)) {
						sc.ChangeState(SCE_ELIXIR_UNKNOWN); // error
					} else if (!escapeSequence) {
						sc.ChangeState(SCE_ELIXIR_CHARACTER);
					}
					sc.SetState(SCE_ELIXIR_DEFAULT);
					is_char_escape = false;
				} else {
					if (sc.ch == '\\') {
						escapeSeq.initEscapeState(sc.chNext);
						sc.Forward();
						continue;
					} else if (sc.ch == '~' && formatSequence) {
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
					if (escapeSequence) {
						sc.SetState(SCE_ELIXIR_ESCAPESEQ);
						escapeSeq.initEscapeState(sc.chNext);
					}
					sc.Forward(); // Skip any character after the backslash
					continue;
				} else if (sc.ch == '~') {
					sc.SetState(SCE_ELIXIR_FORMATSEQ);
					formatSeq.initFormatState();
					continue;
				CHECK_INTERPOLATE_STRING
				CHECK_CLOSING_STRING
			} break;
			
			case SCE_ELIXIR_MODULE : {
				if (IsAWordChar(sc.ch)) {
					continue;
				}
				SKIP_SPACES
				if (sc.ch == '.') {
					MOVE_INDEX_TO_NONSPACE
					if (isupper(styler[i])) {
						sc.Forward(); // skip '.'
						SKIP_SPACES
						continue;
					}
				}
				sc.GetCurrent(cur, sizeof(cur));
				RemoveAllSpaces(cur);
				
				if (stdModules.InList(cur)) {
					sc.ChangeState(SCE_ELIXIR_STD_MODULE);
					module_type = (strcmp(cur, "Kernel") == 0) ? KERNEL_MODULE
															   : OTHER_MODULE;
				} else if (stdExcepts.InList(cur)) {
					sc.ChangeState(SCE_ELIXIR_STD_EXCEPT);
				}
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_MODULE_ATTR : {
				if (IsAWordChar(sc.ch)) {
					continue;
				} else if (IsAWordEnd(sc.ch)) {
					sc.Forward();
				}
				sc.GetCurrent(cur, sizeof(cur));
				
				if (stdModuleAttrs.InList(cur)) {
					sc.ChangeState(SCE_ELIXIR_STD_MODULE_ATTR);
					
					if (strcmp(cur, "@spec") == 0 ||
						strcmp(cur, "@type") == 0 ||
						strcmp(cur, "@callback") == 0 ||
						strcmp(cur, "@macrocallback") == 0) {
						ident_state = TYPEDEF_STATE;
						maybe_typefunc = true;
					} else {
						ident_state = NONE_STATE;
						maybe_typefunc = false;
					}
				}
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_IDENTIFIER : {
				if (IsAWordChar(sc.ch)) {
					continue;
				} else if (IsAWordEnd(sc.ch)) {
					sc.Forward();
				}
				sc.GetCurrent(cur, sizeof(cur));
				
				if (sc.ch == ':') { // init field of map/struct or Erlang type oper (::)
					if (sc.chNext != ':') {
						sc.ChangeState(ident_state == NONE_STATE ? SCE_ELIXIR_FIELD
																 : SCE_ELIXIR_UNKNOWN);
						if (!IsASpace(sc.chNext)) {
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
					if (ident_state == DOTOPER_STATE) { // using field/method of module
						if (sc.ch == '(') {
							CHANGE_STATE_BY_MODULE
						} else if (sc.ch == '/') {
							MOVE_INDEX_TO_NONSPACE
							if (isdigit(styler[i]))
								CHANGE_STATE_BY_MODULE
						}
					} else if (stdWords.InList(cur)) {
						sc.ChangeState(SCE_ELIXIR_STD_WORD);
					} else if ((isupper(sc.ch) || sc.Match("__")) &&
							   addWords.InList(cur)) {
						sc.ChangeState(SCE_ELIXIR_ADD_WORD);
					} else if (stdAtoms.InList(cur)) {
						sc.ChangeState(SCE_ELIXIR_STD_ATOM);
					} else if (stdMacros.InList(cur)) {
						sc.ChangeState(SCE_ELIXIR_STD_MACRO);
					} else if (ident_state == TYPEOPER_STATE) {
						sc.ChangeState(strcmp(cur, "t") == 0 ? SCE_ELIXIR_FUNCTION
															 : SCE_ELIXIR_TYPE_FUNC);
					} else if (maybe_typefunc && sc.ch != ':' &&
							   typeFuncs.InList(cur)) {
						sc.ChangeState(SCE_ELIXIR_TYPE_FUNC);
					} else if (sc.ch == '(' || ident_state == PIPEOPER_STATE) {
						CHANGE_STATE_BY_FUNCLIST
					} else if (sc.ch == '/') {
						MOVE_INDEX_TO_NONSPACE
						if (isdigit(styler[i])) {
							CHANGE_STATE_BY_FUNCLIST
						} else {
							CHECK_LIB_MACROS
						}
					} else {
						CHECK_LIB_MACROS
					}
				}
				ident_state = (sc.state == SCE_ELIXIR_STD_WORD &&
							   (strcmp(cur, "def") == 0 ||
								strcmp(cur, "defp") == 0 ||
								strcmp(cur, "defguard") == 0 ||
								strcmp(cur, "defguardp") == 0 ||
								strcmp(cur, "defmacro") == 0 ||
								strcmp(cur, "defmacrop") == 0 ||
								strcmp(cur, "defmemo") == 0 ||
								strcmp(cur, "defmemop") == 0)) ? DEFNAME_STATE
															   : NONE_STATE;
				if (sc.state == SCE_ELIXIR_STD_WORD
					|| sc.state == SCE_ELIXIR_ADD_WORD)
					maybe_typefunc = false;
				
				module_type = NONE_MODULE;
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_OPERATOR :
			case SCE_ELIXIR_MAP_OPER :
			case SCE_ELIXIR_CAPTURE_OPER : {
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_ATOM_PUNCT : {
				atomPunctSeq.charsLeft--;
				if (!atomPunctSeq.atAtomPunctEnd(sc.ch)) {
					continue; // esh: continue of atom-punct chars
				}
				sc.SetState(SCE_ELIXIR_DEFAULT);
			} break;
			
			case SCE_ELIXIR_UNKNOWN : {
				if (sc.atLineStart) {
					sc.SetState(SCE_ELIXIR_DEFAULT);
				}
			} break;
		}
		
		// Determine if a new state should be entered.
		if (sc.state == SCE_ELIXIR_DEFAULT || sc.state == SCE_ELIXIR_UNKNOWN) {
			if (sc.ch == '#') {
				sc.SetState(SCE_ELIXIR_COMMENT);
				
			} else if (sc.ch == '\"') {
				if (sc.Match(R"(""")")) {
					sc.SetState(assign_to_strfield ? SCE_ELIXIR_TRIPLEVAL
												   : SCE_ELIXIR_TRIPLE);
					sc.Forward(2);
				} else {
					sc.SetState(assign_to_strfield ? SCE_ELIXIR_STRINGVAL
												   : SCE_ELIXIR_STRING);
				}
				closing_char = '\"';
				string_state = sc.state;
				canbe_interpolate = true;
				
			} else if (sc.ch == '\'') {
				if (sc.Match(R"(''')")) {
					sc.SetState(assign_to_strfield ? SCE_ELIXIR_TRIPLEVAL
												   : SCE_ELIXIR_TRIPLE);
					sc.Forward(2);
				} else {
					sc.SetState(assign_to_strfield ? SCE_ELIXIR_CHARLISTVAL
												   : SCE_ELIXIR_CHARLIST);
				}
				closing_char = '\'';
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
				
			} else if (sc.ch == '%') {
				sc.SetState(SCE_ELIXIR_UNKNOWN);
				if (isupper(sc.chNext) || strchr("{_", sc.chNext)) {
					sc.ChangeState(SCE_ELIXIR_MAP_OPER);
				}
			} else if (sc.ch == '@') {
				sc.SetState(SCE_ELIXIR_UNKNOWN);
				if (islower(sc.chNext) || sc.chNext == '_') {
					sc.ChangeState(SCE_ELIXIR_MODULE_ATTR);
					sc.Forward();
				}
			} else if (sc.ch == ':' && IsAWordStart(sc.chNext)) {
				sc.SetState(SCE_ELIXIR_ATOM);
				sc.Forward();
				is_at_symb = false;
				
			} else if (sc.ch == ':' && (sc.chNext == '\"' || sc.chNext == '\'')) {
				sc.SetState(SCE_ELIXIR_ATOM_QUOTED);
				sc.Forward();
				is_at_symb = false;
				closing_char = sc.ch;
				string_state = sc.state;
				canbe_interpolate = true;
				
			} else if (sc.ch == ':' && strchr("!@%^/<>{}*&-+=.", sc.chNext)) {
				atomPunctSeq.initAtomPunctState(sc.chNext);
				sc.SetState(SCE_ELIXIR_ATOM_PUNCT);
				sc.Forward();
				
			} else if (isdigit(sc.ch)) {
				number_state = NUMERAL_START;
				radix_digits = sc.ch - '0';
				sc.SetState(SCE_ELIXIR_NUMBER);
				
			} else if (isupper(sc.ch)) {
				sc.SetState(SCE_ELIXIR_MODULE);
				
			} else if (islower(sc.ch) || sc.ch == '_') {
				sc.SetState(SCE_ELIXIR_IDENTIFIER);
				
			} else if (IsElixirOperator(sc.ch)) {
				sc.SetState(SCE_ELIXIR_OPERATOR);
				
				ident_state = NONE_STATE;
				assign_to_strfield = false;
				
				if (sc.ch == '&') {
					sc.chNext == '&' ? sc.Forward()
									 : sc.ChangeState(SCE_ELIXIR_CAPTURE_OPER);
				} else if (sc.Match("=>")) {
					assign_to_strfield = (last_state == SCE_ELIXIR_STRING);
					sc.Forward();
				} else if (sc.Match("<>")) {
					assign_to_strfield = IsStringValStyle(last_state);
					sc.Forward();
				} else if (sc.Match("=~")) {
					sc.Forward();
				} else if (sc.Match("|>")) {
					ident_state = PIPEOPER_STATE;
					sc.Forward();
				} else if (sc.Match("::")) {
					ident_state = TYPEOPER_STATE;
					sc.Forward();
				} else if (sc.ch == '.') {
					ident_state = DOTOPER_STATE;
				} else if (sc.ch == '{' && nesting_count > 0) {
					nesting_count++;
				} else if (sc.ch == '}' && nesting_count > 0) {
					nesting_count--;
					if (nesting_count == 0) {
						sc.ChangeState(SCE_ELIXIR_STRING_SUBOPER);
					}
				}
			}
			if (!IsOperatorStyle(sc.state) && sc.state != SCE_ELIXIR_DEFAULT) {
				assign_to_strfield = false;
			}
		}
		if (last_state != sc.state && !IsSpaceEquivStyle(sc.state)) {
			last_state = sc.state;
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

static void FoldElixirDoc(Sci_PositionU startPos, Sci_Position length,
						  int initStyle, WordList** /*keywordlists*/,
						  Accessor &styler) {
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
	bool atEOL;
	
	for (Sci_PositionU i = startPos; i < endPos; i++) {
		ch = chNext;
		chNext = styler.SafeGetCharAt(i + 1);
		// Get styles
		stylePrev = style;
		style = styleNext;
		styleNext = styler.StyleAt(i + 1);
		atEOL = ((ch == '\r') && (chNext != '\n')) || (ch == '\n');
		
		if (stylePrev != SCE_ELIXIR_STD_WORD
			&& style == SCE_ELIXIR_STD_WORD) {
			keyword_start = i;
		}
		// Fold on keywords
		if (stylePrev == SCE_ELIXIR_STD_WORD
			&& style != SCE_ELIXIR_STD_WORD
			&& style != SCE_ELIXIR_ATOM
		) {
			currentLevel += ClassifyElixirFoldPoint(styler,
													styleNext,
													keyword_start);
		}
		// Fold on comments
		if (style == SCE_ELIXIR_COMMENT) {
			
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
		if (atEOL) {
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

static const char * const elixirWordListDesc[] = {
	"Standard keywords",
	"Additional keywords",
	"Standard atoms",
	"Standard functions (BIFs)",
	"Standard modules (BIMs)",
	"Standard module attributes",
	"Erlang modules (Erlang BIMs)",
	"Standard exceptions",
	"Standard macros",
	"Built-in type functions",
	"Lib macros (Bureaucrat/Ecto/ExUnit/Phoenix/Plug/...)",
	0
};

LexerModule lmElixir(
	SCLEX_ELIXIR,
	ColouriseElixirDoc,
	"elixir",
	FoldElixirDoc,
	elixirWordListDesc);
