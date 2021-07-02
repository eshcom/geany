// Scintilla source code edit control
// Encoding: UTF-8
// Copyright 1998-2001 by Neil Hodgson <neilh@scintilla.org>
// The License.txt file describes the conditions under which this software may be distributed.
/** @file LexErlang.cxx
 ** Lexer for Erlang.
 ** Enhanced by Etienne 'Lenain' Girondel (lenaing@gmail.com)
 ** Originally wrote by Peter-Henry Mander,
 ** based on Matlab lexer by José Fonseca.
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

struct ErlFormatSequence {
	enum
	{
		FORMAT_NONE,
		FORMAT_INIT,
		FORMAT_END,
		FORMAT_MOD,					// [t l]
		FORMAT_SPEC,				// [c f e g s w p b x n i W P B X # +]
		FORMAT_NUM_SIGN,			// [-+]
		FORMAT_NUM_BASE_DIGITS,		// [0-9]+
		FORMAT_NUM_PREC_DIGITS,		// [0-9]+
		FORMAT_NUM_BASE_ASTER,		// *
		FORMAT_NUM_PREC_ASTER,		// *
		FORMAT_NUM_PREC_DOT,		// .
		FORMAT_NUM_PAD_DOT,			// .
		FORMAT_NUM_PAD_CHAR			// [<any printable>]
	};
	int formatState;
	CharacterSet setMod;
	CharacterSet setSpec;
	CharacterSet setNumSign;
	CharacterSet setNumDigits;
	ErlFormatSequence() {
		formatState = FORMAT_NONE;
		setMod = CharacterSet(CharacterSet::setNone, "tl");
		setSpec = CharacterSet(CharacterSet::setNone, "cfegswpbxniWPBX#+");
		setNumSign = CharacterSet(CharacterSet::setNone, "-+");
		setNumDigits = CharacterSet(CharacterSet::setDigits);
	}
	void initFormatState() {
		formatState = FORMAT_INIT;
	}
	bool atFormatEnd(int currChar) {
		switch (formatState) {
			case FORMAT_INIT:
				if (setSpec.Contains(currChar)) {
					formatState = FORMAT_SPEC;
				} else if (setMod.Contains(currChar)) {
					formatState = FORMAT_MOD;
				} else if (setNumSign.Contains(currChar)) {
					formatState = FORMAT_NUM_SIGN;
				} else if (setNumDigits.Contains(currChar)) {
					formatState = FORMAT_NUM_BASE_DIGITS;
				} else if (currChar == '*') {
					formatState = FORMAT_NUM_BASE_ASTER;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PREC_DOT;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_SIGN:
				if (setNumDigits.Contains(currChar)) {
					formatState = FORMAT_NUM_BASE_DIGITS;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PREC_DOT;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_BASE_DIGITS:
			case FORMAT_NUM_BASE_ASTER:
				if (setSpec.Contains(currChar)) {
					formatState = FORMAT_SPEC;
				} else if (setMod.Contains(currChar)) {
					formatState = FORMAT_MOD;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PREC_DOT;
				} else if (formatState == FORMAT_NUM_BASE_ASTER ||
						   !setNumDigits.Contains(currChar)) {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_PREC_DOT:
				if (setNumDigits.Contains(currChar)) {
					formatState = FORMAT_NUM_PREC_DIGITS;
				} else if (currChar == '*') {
					formatState = FORMAT_NUM_PREC_ASTER;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_PREC_DIGITS:
			case FORMAT_NUM_PREC_ASTER:
				if (setSpec.Contains(currChar)) {
					formatState = FORMAT_SPEC;
				} else if (setMod.Contains(currChar)) {
					formatState = FORMAT_MOD;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PAD_DOT;
				} else if (formatState == FORMAT_NUM_PREC_ASTER ||
						   !setNumDigits.Contains(currChar)) {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_PAD_DOT:
				formatState = FORMAT_NUM_PAD_CHAR;
				break;
			case FORMAT_NUM_PAD_CHAR:
				if (setSpec.Contains(currChar)) {
					formatState = FORMAT_SPEC;
				} else if (setMod.Contains(currChar)) {
					formatState = FORMAT_MOD;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_MOD:
				if (setSpec.Contains(currChar)) {
					formatState = FORMAT_SPEC;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_SPEC:
				formatState = FORMAT_END;
				break;
		}
		return (formatState == FORMAT_END ||
				formatState == FORMAT_NONE);
	}
	bool atFormatNone() {
		return (formatState == FORMAT_NONE);
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
} number_parse_state_t;

typedef enum {
	NONE_MODULE,
	OTHER_MODULE,
	ERLANG_MODULE
} module_type_t;

static inline bool IsAWordChar(const int ch) {
	return (ch < 0x80) && (ch != ' ') && (isalnum(ch) || ch == '_');
}

static void ColouriseErlangDoc(Sci_PositionU startPos, Sci_Position length,
							   int initStyle, WordList *keywordlists[],
							   Accessor &styler) {
	// esh: escapesequence highlighting
	const bool escapeSequence = styler.GetPropertyInt("lexer.erlang.escape.sequence", 0) != 0;
	EscapeSequence escapeSeq = EscapeSequence();
	
	// esh: formatsequence highlighting
	const bool formatSequence = styler.GetPropertyInt("lexer.erlang.format.sequence", 0) != 0;
	ErlFormatSequence formatSeq = ErlFormatSequence();
	
	//~ esh: logs are written to a file ~/.cache/upstart/unity7.log (ubuntu 16)
	//~ printf("!!!LexErlang: currLine = %li, currChar = '%c', lastChar = '%c', initStyle = %i\n",
		   //~ styler.GetLine(startPos) + 1, styler[startPos],
		   //~ styler[startPos + length - 2], initStyle);
	
	StyleContext sc(startPos, length, initStyle, styler);
	
	WordList &reservedWords = *keywordlists[0];
	WordList &erlangBIFs = *keywordlists[1];
	WordList &erlangPreproc = *keywordlists[2];
	WordList &erlangModulesAtt = *keywordlists[3];
	WordList &erlangDoc = *keywordlists[4];
	WordList &erlangDocMacro = *keywordlists[5];
	WordList &erlangAtomSpec = *keywordlists[6];
	
	int radix_digits = 0;
	int exponent_digits = 0;
	number_parse_state_t number_state;
	
	module_type_t module_type = NONE_MODULE;
	
	char cur[100];
	int last_state;
	
	bool is_at_symb = false;		// esh: "at" - is "@" symb (for node name)
	bool is_var_record_name = false;	// esh: #RecordName{}, #?MODULE{}
	
	styler.StartAt(startPos);
	
	for (; sc.More(); sc.Forward()) {
		// Determine if the current state should terminate.
		switch (sc.state) {
			/* COMMENTS ------------------------------------------------------*/
			case SCE_ERLANG_COMMENT_DOC :
			case SCE_ERLANG_COMMENT_DOC_MACRO : {
				if (isalnum(sc.ch))
					continue;
				// Try to match documentation comment
				sc.GetCurrent(cur, sizeof(cur));
				if (sc.state == SCE_ERLANG_COMMENT_DOC) {
					if (!erlangDoc.InList(cur)) {
						sc.ChangeState(last_state);
					}
				} else {
					if (!erlangDocMacro.InList(cur)) {
						sc.ChangeState(last_state);
					} else {
						while (sc.ch != '}' && !sc.atLineEnd)
							sc.Forward();
						if (sc.ch != '}')
							sc.ChangeState(last_state);
					}
				}
				if (sc.atLineEnd) {
					sc.SetState(SCE_ERLANG_DEFAULT);
					break;
				} else {
					sc.SetState(last_state);
				}
			}
			// V--- Falling through!
			case SCE_ERLANG_COMMENT_FUNCTION :
			case SCE_ERLANG_COMMENT_MODULE : {
				if (sc.atLineEnd) {
					sc.SetState(SCE_ERLANG_DEFAULT);
					
				} else if (sc.ch == '{') {
					sc.Forward();
					while (!sc.atLineEnd && IsASpaceOrTab(sc.ch))
						sc.Forward();
					if (sc.ch == '@' && isalnum(sc.chNext)) {
						last_state = sc.state;
						sc.SetState(SCE_ERLANG_COMMENT_DOC_MACRO);
						sc.Forward();
					}
				} else if (sc.ch == '@' && isalnum(sc.chNext)) {
					last_state = sc.state;
					sc.SetState(SCE_ERLANG_COMMENT_DOC);
					sc.Forward();
				}
			} break;
			
			case SCE_ERLANG_COMMENT : {
				if (sc.atLineEnd)
					sc.SetState(SCE_ERLANG_DEFAULT);
			} break;
			/* -------------------------------------------------------------- */
			
			/* Numerics ------------------------------------------------------*/
			case SCE_ERLANG_NUMBER : {
				switch (number_state) {
					
					/* Simple integer */
					case NUMERAL_START : {
						if (isdigit(sc.ch)) {
							radix_digits *= 10;
							radix_digits += sc.ch - '0'; // Assuming ASCII here!
							continue;
						} else if (sc.ch == '#') {
							if (radix_digits < 2 || radix_digits > 36) {
								sc.ChangeState(SCE_ERLANG_UNKNOWN); // error
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
						} else if (isalpha(sc.ch)) {
							sc.ChangeState(SCE_ERLANG_UNKNOWN); // error
						}
					} break;
					
					/* Integer in other base than 10 (x#yyy) */
					case NUMERAL_BASE_VALUE : {
						if (is_radix(radix_digits, sc.ch)) {
							continue;
						} else if (isalnum(sc.ch)) {
							sc.ChangeState(SCE_ERLANG_UNKNOWN); // error
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
							sc.ChangeState(SCE_ERLANG_UNKNOWN); // error
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
							sc.ChangeState(SCE_ERLANG_UNKNOWN); // error
						}
					} break;
				}
				sc.SetState(SCE_ERLANG_DEFAULT);
			} break;
			/* -------------------------------------------------------------- */
			
			/* Preprocessor --------------------------------------------------*/
			case SCE_ERLANG_PREPROC : {
				if (IsAWordChar(sc.ch)) {
					continue;
				}
				sc.GetCurrent(cur, sizeof(cur));
				if (erlangModulesAtt.InList(cur)) {
					sc.ChangeState(SCE_ERLANG_MODULES_ATT);
				} else if (!erlangPreproc.InList(cur)) {
					sc.ChangeState(SCE_ERLANG_UNKNOWN); // error
				}
				sc.SetState(SCE_ERLANG_DEFAULT);
			} break;
			/* -------------------------------------------------------------- */
			
			/* Atoms ---------------------------------------------------------*/
			case SCE_ERLANG_ATOM : {
				if (sc.ch == '@' && !is_at_symb) {
					is_at_symb = true;
					sc.ChangeState(SCE_ERLANG_NODE_NAME);
					continue;
				} else if (IsAWordChar(sc.ch)) {
					continue;
				}
				sc.GetCurrent(cur, sizeof(cur));
				while (sc.More() && IsASpaceOrTab(sc.ch))
					sc.Forward();
				
				if (sc.ch == ':' && sc.chNext != '=' && sc.chNext != ':') {
					// esh: set module type,
					//		exclude map-key updates, example: #{data:=test}
					//		exclude record field type, example: handler = none :: atom()
					module_type = (strcmp(cur, "erlang") == 0) ? ERLANG_MODULE:
																 OTHER_MODULE;
					sc.Forward();
					sc.ChangeState(SCE_ERLANG_MODULES);
				} else {
					if (reservedWords.InList(cur)) {
						sc.ChangeState(SCE_ERLANG_KEYWORD);
						
					} else if (((module_type == NONE_MODULE && sc.ch == '(')
								|| module_type == ERLANG_MODULE)
							   && erlangBIFs.InList(cur)) {
						sc.ChangeState(SCE_ERLANG_BIFS);
						
					} else if (sc.ch == '(' || sc.ch == '/') {
						sc.ChangeState(SCE_ERLANG_FUNCTION_NAME);
						
					} else if (erlangAtomSpec.InList(cur)) {
						sc.ChangeState(SCE_ERLANG_ATOM_SPEC);
					}
				}
				sc.SetState(SCE_ERLANG_DEFAULT);
			} break;
			
			case SCE_ERLANG_ATOM_QUOTED : {
				if (sc.ch == '@' && !is_at_symb) {
					is_at_symb = true;
					sc.ChangeState(SCE_ERLANG_NODE_NAME_QUOTED);
					
				} else if (sc.ch == '\'' && sc.chPrev != '\\') {
					sc.Forward();
					sc.SetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			/* -------------------------------------------------------------- */
			
			/* Node names ----------------------------------------------------*/
			case SCE_ERLANG_NODE_NAME : {
				if (sc.ch == '@') {
					sc.ChangeState(SCE_ERLANG_ATOM);
					
				} else if (!IsAWordChar(sc.ch)) {
					sc.SetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			
			case SCE_ERLANG_NODE_NAME_QUOTED : {
				if (sc.ch == '@') {
					sc.ChangeState(SCE_ERLANG_ATOM_QUOTED);
					
				} else if (sc.ch == '\'' && sc.chPrev != '\\') {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			/* -------------------------------------------------------------- */
			
			/* Macros/Records-------------------------------------------------*/
			case SCE_ERLANG_RECORD :
				if (is_var_record_name) {
					sc.SetState(SCE_ERLANG_DEFAULT);
					break;
				}
			case SCE_ERLANG_MACRO : {
				if (!IsAWordChar(sc.ch) && sc.ch != '@') {
					sc.SetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			
			case SCE_ERLANG_RECORD_QUOTED :
			case SCE_ERLANG_MACRO_QUOTED  : {
				if (sc.ch == '\'' && sc.chPrev != '\\') {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			/* -------------------------------------------------------------- */
			
			case SCE_ERLANG_VARIABLE : {
				if (!IsAWordChar(sc.ch) && sc.ch != '@') {
					sc.SetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			
			case SCE_ERLANG_STRING : {
				if (sc.ch == '\\') {
					if (escapeSequence) {
						sc.SetState(SCE_ERLANG_ESCAPESEQUENCE);
						escapeSeq.initEscapeState(sc.chNext);
					}
					sc.Forward(); // Skip any character after the backslash
					continue;
					
				} else if (sc.ch == '~' && formatSequence) {
					sc.SetState(SCE_ERLANG_FORMATSEQUENCE);
					formatSeq.initFormatState();
					continue;
					
				} else if (sc.ch == '\"') {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			
			case SCE_ERLANG_ESCAPESEQUENCE : {
				escapeSeq.digitsLeft--;
				if (!escapeSeq.atEscapeEnd(sc.ch)) {
					continue; // esh: continue of escape chars
				}
				if (sc.ch == '\\') {
					escapeSeq.initEscapeState(sc.chNext);
					sc.Forward();
					continue;
					
				} else if (sc.ch == '~' && formatSequence) {
					sc.SetState(SCE_ERLANG_FORMATSEQUENCE);
					formatSeq.initFormatState();
					continue;
					
				} else if (sc.ch == '\"') {
					sc.SetState(SCE_ERLANG_STRING);
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
					
				} else {
					sc.SetState(SCE_ERLANG_STRING);
				}
			} break;
			
			case SCE_ERLANG_FORMATSEQUENCE : {
				if (!formatSeq.atFormatEnd(sc.ch)) {
					continue; // esh: continue of format chars
				}
				if (formatSeq.atFormatNone()) {
					sc.ChangeState(SCE_ERLANG_STRING);
				}
				if (sc.ch == '\\') {
					if (escapeSequence) {
						sc.SetState(SCE_ERLANG_ESCAPESEQUENCE);
						escapeSeq.initEscapeState(sc.chNext);
					}
					sc.Forward(); // Skip any character after the backslash
					continue;
					
				} else if (sc.ch == '~') {
					sc.SetState(SCE_ERLANG_FORMATSEQUENCE);
					formatSeq.initFormatState();
					continue;
					
				} else if (sc.ch == '\"') {
					sc.SetState(SCE_ERLANG_STRING);
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
					
				} else {
					sc.SetState(SCE_ERLANG_STRING);
				}
			} break;
			
			case SCE_ERLANG_CHARACTER : {
				if (sc.chPrev == '\\') {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				} else if (sc.ch != '\\') {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			
			case SCE_ERLANG_OPERATOR : {
				if ((sc.chPrev == '.') &&
					(sc.ch == '*' || sc.ch == '/' || sc.ch == '\\'
					 || sc.ch == '^')) {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				} else {
					sc.SetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			
			case SCE_ERLANG_MAP_OPER : {
				sc.SetState(SCE_ERLANG_DEFAULT);
			} break;
		}
		
		// Determine if a new state should be entered.
		if (sc.state == SCE_ERLANG_DEFAULT ||
			sc.state == SCE_ERLANG_UNKNOWN) {
			if (sc.ch == '%') {
				sc.SetState(SCE_ERLANG_COMMENT);
				if (sc.chNext == '%') {
					if (sc.GetRelative(2) == '%') {
						// Switch to comment level 3 (Module)
						sc.ChangeState(SCE_ERLANG_COMMENT_MODULE);
						sc.Forward();
					} else {
						// Switch to comment level 2 (Function)
						sc.ChangeState(SCE_ERLANG_COMMENT_FUNCTION);
					}
					sc.Forward();
				}
			} else if (sc.ch == '\'') {
				is_at_symb = false;
				sc.SetState(SCE_ERLANG_ATOM_QUOTED);
				
			} else if (sc.ch == '\"') {
				sc.SetState(SCE_ERLANG_STRING);
				
			} else if (sc.ch == '$') {
				sc.SetState(SCE_ERLANG_CHARACTER);
				
			} else if (sc.ch == '-' && islower(sc.chNext)) {
				sc.SetState(SCE_ERLANG_PREPROC);
				sc.Forward();
				
			} else if (sc.ch == '?') {
				sc.SetState(SCE_ERLANG_UNKNOWN);
				while (sc.More() && IsASpaceOrTab(sc.chNext))
					sc.Forward();
				if (sc.chNext == '\'') {
					sc.ChangeState(SCE_ERLANG_MACRO_QUOTED);
					sc.Forward();
				} else if (isalpha(sc.chNext)) {
					sc.ChangeState(SCE_ERLANG_MACRO);
					sc.Forward();
				}
			} else if (sc.ch == '#') {
				sc.SetState(SCE_ERLANG_UNKNOWN);
				while (sc.More() && IsASpaceOrTab(sc.chNext))
					sc.Forward();
				if (sc.chNext == '\'') {
					sc.ChangeState(SCE_ERLANG_RECORD_QUOTED);
					sc.Forward();
				} else if (sc.chNext == '{') {
					sc.ChangeState(SCE_ERLANG_MAP_OPER);
				} else if (islower(sc.chNext)) {
					sc.ChangeState(SCE_ERLANG_RECORD);
					sc.Forward();
					is_var_record_name = false;
				} else if (isupper(sc.chNext) || sc.chNext == '?') {
					//~ examples: #RecordName{}, #?MODULE{}
					sc.ChangeState(SCE_ERLANG_RECORD);
					is_var_record_name = true;
				}
			} else if (isdigit(sc.ch)) {
				number_state = NUMERAL_START;
				radix_digits = sc.ch - '0';
				sc.SetState(SCE_ERLANG_NUMBER);
				
			} else if (isupper(sc.ch) || sc.ch == '_') {
				sc.SetState(SCE_ERLANG_VARIABLE);
				
			} else if (islower(sc.ch)) {
				is_at_symb = false;
				sc.SetState(SCE_ERLANG_ATOM);
				
			} else if (isoperator(static_cast<char>(sc.ch))
						|| sc.ch == '\\') {
				sc.SetState(SCE_ERLANG_OPERATOR);
				module_type = (sc.ch == ':' && sc.chNext != '=' &&
							   sc.chNext != ':') ? OTHER_MODULE:
												   NONE_MODULE;
			}
		}
	}
	sc.Complete();
}

static int ClassifyErlangFoldPoint(Accessor &styler, int styleNext,
								   Sci_Position keyword_start) {
	int lev = 0;
	if (styler.Match(keyword_start,"case")
		|| (
			styler.Match(keyword_start,"fun")
			&& (SCE_ERLANG_FUNCTION_NAME != styleNext)
			)
		|| styler.Match(keyword_start,"if")
		|| styler.Match(keyword_start,"query")
		|| styler.Match(keyword_start,"receive")
	) {
		++lev;
	} else if (styler.Match(keyword_start,"end")) {
		--lev;
	}
	return lev;
}

static void FoldErlangDoc(Sci_PositionU startPos, Sci_Position length,
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
		
		if (stylePrev != SCE_ERLANG_KEYWORD
			&& style == SCE_ERLANG_KEYWORD) {
			keyword_start = i;
		}
		// Fold on keywords
		if (stylePrev == SCE_ERLANG_KEYWORD
			&& style != SCE_ERLANG_KEYWORD
			&& style != SCE_ERLANG_ATOM
		) {
			currentLevel += ClassifyErlangFoldPoint(styler,
													styleNext,
													keyword_start);
		}
		// Fold on comments
		if (style == SCE_ERLANG_COMMENT
			|| style == SCE_ERLANG_COMMENT_MODULE
			|| style == SCE_ERLANG_COMMENT_FUNCTION) {
			
			if (ch == '%' && chNext == '{') {
				currentLevel++;
			} else if (ch == '%' && chNext == '}') {
				currentLevel--;
			}
		}
		// Fold on braces
		if (style == SCE_ERLANG_OPERATOR) {
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

static const char * const erlangWordListDesc[] = {
	"Erlang Reserved words",
	"Erlang BIFs",
	"Erlang Preprocessor",
	"Erlang Module Attributes",
	"Erlang Documentation",
	"Erlang Documentation Macro",
	"Erlang Atom Special",
	0
};

LexerModule lmErlang(
	SCLEX_ERLANG,
	ColouriseErlangDoc,
	"erlang",
	FoldErlangDoc,
	erlangWordListDesc);
