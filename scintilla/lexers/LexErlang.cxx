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
#include "LexerCommon.h"

using namespace Scintilla;

static bool isRadix(int radix, int ch) {
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

static bool isFuncDefinition(Sci_Position pos, Sci_PositionU endPos,
							 Accessor &styler) {
	/* find ") ->" or ") when" */
	bool found_end_bracket = false;
	bool is_str = false;
	char str_quote;
	int brackets = 0;
	
	int limit = pos + 300;
	if (limit > endPos) limit = endPos;
	
	char ch;
	while (pos < limit) {
		ch = styler[pos];
		if (is_str) {
			if (ch == '\\')
				pos++;		// Skip any character after the backslash
			else if (ch == str_quote)
				is_str = false;
		} else if (ch == '$') {
			if (pos + 1 < limit && styler[pos + 1] == '\\')
				pos += 2;	// Skip $\", $\', ...
			else
				pos++;		// Skip $", $', ...
		} else if (ch == '\"' || ch == '\'') {
			str_quote = ch;
			is_str = true;
		} else if (ch == '(') {
			brackets++;
		} else if (ch == ')' && --brackets < 0) {
			found_end_bracket = true;
			break;
		}
		pos++;
	}
	if (found_end_bracket) {
		pos++; // skip ')'
		while (pos < endPos && IsSpace(styler[pos]))
			pos++;
		if (pos + 1 < endPos && styler[pos] == '-' && styler[pos + 1] == '>')
			return true;
		else if (pos + 3 < endPos &&
				 styler[pos] == 'w' && styler[pos + 1] == 'h' &&
				 styler[pos + 2] == 'e' && styler[pos + 3] == 'n')
			return true;
	}
	return false;
}

static Sci_Position findStartBracket(Sci_Position pos, Accessor &styler)
{
	int brackets = 0;
	int limit = pos > 300 ? pos - 300 : 0;
	
	while (pos > limit) {
		if (styler.StyleAt(pos) == SCE_ERLANG_OPERATOR) {
			if (styler[pos] == ')')
				brackets++;
			else if (styler[pos] == '(' && --brackets < 0)
				return pos; /* found start bracket */
		}
		pos--;
	}
	return -1;
}

typedef enum {
	NUMERAL_START,
	NUMERAL_BASE_VALUE,
	NUMERAL_FLOAT,
	NUMERAL_EXPONENT
} number_state_t;

typedef enum {
	NONE_MODULE,
	OTHER_MODULE,
	ERLANG_MODULE
} module_type_t;

static inline bool IsCommentStyle(int style) {
	return (style == SCE_ERLANG_TASKMARKER ||
			style == SCE_ERLANG_COMMENT ||
			style == SCE_ERLANG_COMMENT_MODULE ||
			style == SCE_ERLANG_COMMENT_FUNCTION);
}

static inline bool IsSpaceEquivStyle(int style) {
	return (IsCommentStyle(style) ||
			style == SCE_ERLANG_DEFAULT ||
			style == SCE_ERLANG_COMMENT_TAG ||
			style == SCE_ERLANG_COMMENT_MACRO_TAG);
}

static inline bool IsValidFuncDefStyle(int style) {
	return (IsSpaceEquivStyle(style) ||
			style == SCE_ERLANG_STD_WORD ||
			style == SCE_ERLANG_OPERATOR ||
			style == SCE_ERLANG_ATOM);
}

#define SKIP_NEXT_SPACES							\
	while (sc.More() && IsSpaceOrTab(sc.chNext))	\
		sc.Forward();

static void ColouriseErlangDoc(Sci_PositionU startPos, Sci_Position length,
							   int initStyle, WordList *keywordlists[],
							   Accessor &styler) {
	// esh: escapesequence highlighting
	const bool escapeSequence =
					styler.GetPropertyInt("lexer.erlang.escape.sequence", 0) != 0;
	EscapeSequence escapeSeq = EscapeSequence();
	
	// esh: formatsequence highlighting
	const bool formatSequence =
					styler.GetPropertyInt("lexer.erlang.format.sequence", 0) != 0;
	ErlFormatSequence formatSeq = ErlFormatSequence();
	
	// esh: find func definition for correct highlighting
	//		(SCE_ERLANG_FUNCTION or SCE_ERLANG_STD_FUNC)
	if (IsValidFuncDefStyle(initStyle) && startPos > 0) {
		Sci_Position lineCurrent = styler.GetLine(startPos);
		if (lineCurrent > 0) {
			Sci_PositionU limit = startPos > 300 ? startPos - 300 : 0;
			Sci_PositionU newStartPos = startPos;
			bool end_bracket_found = false;
			while (--newStartPos > limit &&
				   IsValidFuncDefStyle(styler.StyleAt(newStartPos))) {
				if (styler[newStartPos] == ')') {
					end_bracket_found = true;
					break;
				}
			}
			if (end_bracket_found) {
				newStartPos = findStartBracket(--newStartPos, styler);
				if (newStartPos > 0) {
					int style = styler.StyleAt(newStartPos - 1);
					if (style == SCE_ERLANG_FUNCTION || style == SCE_ERLANG_STD_FUNC) {
						Sci_Position newLine = styler.GetLine(newStartPos);
						if (newLine < lineCurrent) {
							newStartPos = styler.LineStart(newLine);
							length += (startPos - newStartPos);
							startPos = newStartPos;
							initStyle = styler.StyleAt(startPos - 1);
						}
					}
				}
			}
		}
	}
	
	Sci_PositionU endPos = startPos + length;
	
	//~ esh: before debugging, you need to start viewing logs with the command `journalctl -f`
	//~ printf("!!!Lex: currLine = %li, currChar = '%c', lastChar = '%c', "
				//~ "initStyle = %i, startPos = %li, length = %li\n",
		   //~ styler.GetLine(startPos) + 1, styler[startPos],
		   //~ styler[endPos - 2], initStyle, startPos, length);
	
	StyleContext sc(startPos, length, initStyle, styler);
	
	WordList &stdWords = *keywordlists[0];
	WordList &stdAtoms = *keywordlists[1];
	WordList &stdFuncs = *keywordlists[2];
	WordList &stdModules = *keywordlists[3];
	WordList &stdModuleAttrs = *keywordlists[4];
	WordList &stdMacros = *keywordlists[5];
	WordList &typeFuncs = *keywordlists[6];
	WordList &preprocList = *keywordlists[7];
	WordList &commentTags = *keywordlists[8];
	WordList &commentMacroTags = *keywordlists[9];
	WordList &taskMarkers = *keywordlists[10];
	
	int radix_digits = 0;
	int exponent_digits = 0;
	number_state_t number_state;
	
	module_type_t module_type = NONE_MODULE;
	
	char cur[100];
	int last_comment_state;
	int last_state = SCE_ERLANG_DEFAULT;
	int last_oper = ' ';
	
	bool is_at_symb = false;			// esh: "at" - is "@" symb (for node)
	bool is_var_record_name = false;	// esh: #RecordName{}, #?MODULE{}
	
	// esh: for escape sequences highlighting for SCE_ERLANG_CHARACTER
	bool is_char_escape = false;
	// esh: define is_char_escape
	if (initStyle == SCE_ERLANG_ESCAPESEQ) {
		Sci_Position back = startPos;
		int backStyle;
		while (--back) {
			backStyle = styler.StyleAt(back);
			if (backStyle != SCE_ERLANG_ESCAPESEQ) {
				if (backStyle == SCE_ERLANG_CHARACTER) {
					is_char_escape = true;
				} else if (backStyle == SCE_ERLANG_STRING) {
					is_char_escape = false;
				} else if (styler[++back] == '$') {
					is_char_escape = true;
				} else {
					is_char_escape = false;
				}
				break;
			}
		}
	} else if (startPos > 0) {
		// esh: define last_state, last_oper
		Sci_Position back = startPos;
		while (--back && IsSpaceEquivStyle(styler.StyleAt(back)))
			;
		last_state = styler.StyleAt(back);
		if (last_state == SCE_ERLANG_OPERATOR) {
			last_oper = styler.SafeGetCharAt(back);
		}
	}
	
	for (; sc.More(); sc.Forward()) {
		// Determine if the current state should terminate.
		switch (sc.state) {
			/* COMMENTS ----------------------------------------------------- */
			case SCE_ERLANG_COMMENT_TAG :
			case SCE_ERLANG_COMMENT_MACRO_TAG : {
				if (isalnum(sc.ch))
					continue;
				// Try to match documentation comment
				sc.GetCurrent(cur, sizeof(cur));
				if (sc.state == SCE_ERLANG_COMMENT_TAG) {
					if (!commentTags.InList(cur))
						sc.ChangeState(last_comment_state);
				} else {
					if (!commentMacroTags.InList(cur)) {
						sc.ChangeState(commentTags.InList(cur) ? SCE_ERLANG_COMMENT_TAG
															   : last_comment_state);
					} else {
						while (sc.ch != '}' && !sc.atLineEnd)
							sc.Forward();
						if (sc.ch != '}')
							sc.ChangeState(last_comment_state);
					}
				}
				if (sc.atLineEnd) {
					sc.SetState(SCE_ERLANG_DEFAULT);
					break;
				} else {
					sc.SetState(last_comment_state);
				}
			}
			// V--- Falling through!
			case SCE_ERLANG_COMMENT_FUNCTION :
			case SCE_ERLANG_COMMENT_MODULE : {
				if (sc.atLineEnd) {
					sc.SetState(SCE_ERLANG_DEFAULT);
					
				} else if (sc.ch == '{') {
					sc.Forward();
					while (!sc.atLineEnd && IsSpaceOrTab(sc.ch))
						sc.Forward();
					
					if (sc.ch == '@' && isalnum(sc.chNext)) {
						last_comment_state = sc.state;
						sc.SetState(SCE_ERLANG_COMMENT_MACRO_TAG);
						sc.Forward();
					}
				} else if (sc.ch == '@' && isalnum(sc.chNext)) {
					last_comment_state = sc.state;
					sc.SetState(SCE_ERLANG_COMMENT_TAG);
					sc.Forward();
				}
			} break;
			
			case SCE_ERLANG_COMMENT : {
				HighlightTaskMarker(sc, styler, taskMarkers, true,
									SCE_ERLANG_TASKMARKER);
				if (sc.atLineEnd)
					sc.SetState(SCE_ERLANG_DEFAULT);
			} break;
			/* -------------------------------------------------------------- */
			
			/* Numerics ----------------------------------------------------- */
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
						if (isRadix(radix_digits, sc.ch)) {
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
			
			/* Preprocessor ------------------------------------------------- */
			case SCE_ERLANG_PREPROC : {
				if (IsAlnumWordChar(sc.ch)) {
					continue;
				}
				sc.GetCurrent(cur, sizeof(cur));
				RemoveAllSpaces(cur);
				
				if (stdModuleAttrs.InList(cur)) {
					sc.ChangeState(SCE_ERLANG_STD_MODULE_ATTR);
				} else if (!preprocList.InList(cur)) {
					sc.ChangeState(SCE_ERLANG_MODULE_ATTR);
				}
				sc.SetState(SCE_ERLANG_DEFAULT);
			} break;
			/* -------------------------------------------------------------- */
			
			/* Atoms -------------------------------------------------------- */
			case SCE_ERLANG_ATOM : {
				if (sc.ch == '@' && !is_at_symb) {
					is_at_symb = true;
					sc.ChangeState(SCE_ERLANG_NODE);
					continue;
				} else if (IsAlnumWordChar(sc.ch)) {
					continue;
				} else if (sc.ch == '-' && last_oper == '/' && islower(sc.chNext)) {
					sc.Forward();
					continue;
				}
				sc.GetCurrent(cur, sizeof(cur));
				while (sc.More() && IsSpaceOrTab(sc.ch))
					sc.Forward();
				
				if (sc.ch == ':' && sc.chNext != '=' && sc.chNext != ':') {
					// esh: set module type,
					//		exclude map-key updates, example: #{data:=test}
					//		exclude record field type, example: handler = none :: atom()
					module_type = (strcmp(cur, "erlang") == 0) ? ERLANG_MODULE
															   : OTHER_MODULE;
					sc.Forward();
					sc.ChangeState(stdModules.InList(cur) ? SCE_ERLANG_STD_MODULE
														  : SCE_ERLANG_MODULE);
				} else {
					if (stdWords.InList(cur)) {
						sc.ChangeState(SCE_ERLANG_STD_WORD);
						
					} else if (module_type == ERLANG_MODULE) {
						sc.ChangeState(stdFuncs.InList(cur) ? SCE_ERLANG_STD_FUNC
															: SCE_ERLANG_UNKNOWN);
					} else if (module_type == OTHER_MODULE && sc.ch == '(') {
						sc.ChangeState(SCE_ERLANG_FUNCTION);
						
					} else if (module_type == NONE_MODULE && sc.ch == '(') {
						sc.ChangeState(stdFuncs.InList(cur) &&
										!isFuncDefinition(sc.currentPos + 1,
														  endPos, styler)
											? SCE_ERLANG_STD_FUNC
											: SCE_ERLANG_FUNCTION);
					} else if (sc.ch == '/') {
						Sci_PositionU i = sc.currentPos + 1;
						while (i < endPos && IsSpaceOrTab(styler[i]))
							i++;
						if (isdigit(styler[i])) {
							if (module_type == ERLANG_MODULE) {
								sc.ChangeState(stdFuncs.InList(cur)
													? SCE_ERLANG_STD_FUNC
													: SCE_ERLANG_UNKNOWN);
							} else if (module_type == NONE_MODULE &&
										stdFuncs.InList(cur)) {
								sc.ChangeState(SCE_ERLANG_STD_FUNC);
							} else {
								sc.ChangeState(SCE_ERLANG_FUNCTION);
							}
						}
					} else if (stdAtoms.InList(cur)) {
						sc.ChangeState(SCE_ERLANG_STD_ATOM);
					}
				}
				sc.SetState(SCE_ERLANG_DEFAULT);
			} break;
			
			case SCE_ERLANG_ATOM_QUOTED : {
				if (sc.ch == '@' && !is_at_symb) {
					is_at_symb = true;
					sc.ChangeState(SCE_ERLANG_NODE_QUOTED);
				} else if (sc.ch == '\\') {
					sc.Forward(); // Skip any character after the backslash
					continue;
				} else if (sc.ch == '\'') {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			/* -------------------------------------------------------------- */
			
			/* Nodes -------------------------------------------------------- */
			case SCE_ERLANG_NODE : {
				if (sc.ch == '@') {
					sc.ChangeState(SCE_ERLANG_ATOM);
				} else if (!IsAlnumWordChar(sc.ch)) {
					sc.SetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			
			case SCE_ERLANG_NODE_QUOTED : {
				if (sc.ch == '@') {
					sc.ChangeState(SCE_ERLANG_ATOM_QUOTED);
				} else if (sc.ch == '\\') {
					sc.Forward(); // Skip any character after the backslash
					continue;
				} else if (sc.ch == '\'') {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			/* -------------------------------------------------------------- */
			
			/* Macros/Records------------------------------------------------ */
			case SCE_ERLANG_RECORD :
				if (is_var_record_name) {
					sc.SetState(SCE_ERLANG_DEFAULT);
					break;
				}
			case SCE_ERLANG_MACRO : {
				if (!IsAlnumWordChar(sc.ch) && sc.ch != '@') {
					sc.GetCurrent(cur, sizeof(cur));
					RemoveAllSpaces(cur);
					
					if (stdMacros.InList(cur)) {
						sc.ChangeState(SCE_ERLANG_STD_MACRO);
					}
					sc.SetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			
			case SCE_ERLANG_RECORD_QUOTED :
			case SCE_ERLANG_MACRO_QUOTED : {
				if (sc.ch == '\\') {
					sc.Forward(); // Skip any character after the backslash
					continue;
				} else if (sc.ch == '\'') {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			/* -------------------------------------------------------------- */
			
			case SCE_ERLANG_VARIABLE : {
				if (!IsAlnumWordChar(sc.ch) && sc.ch != '@')
					sc.SetState(SCE_ERLANG_DEFAULT);
			} break;
			
			case SCE_ERLANG_STRING : {
				if (sc.ch == '\\') {
					if (escapeSequence) {
						is_char_escape = false;
						sc.SetState(SCE_ERLANG_ESCAPESEQ);
						escapeSeq.initEscapeState(sc.chNext);
					}
					sc.Forward(); // Skip any character after the backslash
					continue;
				} else if (sc.ch == '~' && formatSequence) {
					sc.SetState(SCE_ERLANG_FORMATSEQ);
					formatSeq.initFormatState();
					continue;
				} else if (sc.ch == '\"') {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			
			case SCE_ERLANG_CHARACTER : {
				if (sc.ch == '\\') {
					// esh: we will check the escapeSequence parameter later,
					//		set SCE_ERLANG_ESCAPESEQ for validation
					is_char_escape = true;
					sc.SetState(SCE_ERLANG_ESCAPESEQ);
					escapeSeq.initEscapeState(sc.chNext);
					sc.Forward(); // Skip any character after the backslash
					continue;
				} else if (sc.atLineEnd) {
					sc.SetState(SCE_ERLANG_DEFAULT);
				} else {
					sc.ForwardSetState(SCE_ERLANG_DEFAULT);
				}
			} break;
			
			case SCE_ERLANG_ESCAPESEQ : {
				escapeSeq.digitsLeft--;
				if (!escapeSeq.atEscapeEnd(sc.ch)) {
					continue; // esh: continue of escape chars
				}
				if (is_char_escape) {
					if (!sc.atLineStart && isdigit(sc.ch)) {
						sc.ChangeState(SCE_ERLANG_UNKNOWN); // error
					} else if (!escapeSequence) {
						sc.ChangeState(SCE_ERLANG_CHARACTER);
					}
					sc.SetState(SCE_ERLANG_DEFAULT);
					is_char_escape = false;
				} else {
					if (sc.ch == '\\') {
						escapeSeq.initEscapeState(sc.chNext);
						sc.Forward();
						continue;
					} else if (sc.ch == '~' && formatSequence) {
						sc.SetState(SCE_ERLANG_FORMATSEQ);
						formatSeq.initFormatState();
						continue;
					} else {
						sc.SetState(SCE_ERLANG_STRING);
						if (sc.ch == '\"')
							sc.ForwardSetState(SCE_ERLANG_DEFAULT);
					}
				}
			} break;
			
			case SCE_ERLANG_FORMATSEQ : {
				if (!formatSeq.atFormatEnd(sc.ch)) {
					continue; // esh: continue of format chars
				}
				if (formatSeq.atFormatNone()) {
					sc.ChangeState(SCE_ERLANG_STRING);
				}
				if (sc.ch == '\\') {
					if (escapeSequence) {
						sc.SetState(SCE_ERLANG_ESCAPESEQ);
						escapeSeq.initEscapeState(sc.chNext);
					}
					sc.Forward(); // Skip any character after the backslash
					continue;
				} else if (sc.ch == '~') {
					sc.SetState(SCE_ERLANG_FORMATSEQ);
					formatSeq.initFormatState();
					continue;
				} else {
					sc.SetState(SCE_ERLANG_STRING);
					if (sc.ch == '\"')
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
		if (sc.state == SCE_ERLANG_DEFAULT || sc.state == SCE_ERLANG_UNKNOWN) {
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
				
			} else if (sc.ch == '-' &&
					   (IsSpaceEquivStyle(last_state) ||
						last_state == SCE_ERLANG_OPERATOR) &&
					   (last_oper == ' ' || last_oper == '.')) {
				sc.SetState(SCE_ERLANG_UNKNOWN);
				SKIP_NEXT_SPACES
				
				if (islower(sc.chNext)) {
					sc.ChangeState(SCE_ERLANG_PREPROC);
					sc.Forward();
				}
			} else if (sc.ch == '?') {
				sc.SetState(SCE_ERLANG_UNKNOWN);
				SKIP_NEXT_SPACES
				
				if (sc.chNext == '\'') {
					sc.ChangeState(SCE_ERLANG_MACRO_QUOTED);
					sc.Forward();
				} else if (isalpha(sc.chNext)) {
					sc.ChangeState(SCE_ERLANG_MACRO);
					sc.Forward();
				}
			} else if (sc.ch == '#') {
				sc.SetState(SCE_ERLANG_UNKNOWN);
				SKIP_NEXT_SPACES
				
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
				
			} else if (IsOperator(sc.ch) || sc.ch == '\\') {
				last_oper = sc.ch;
				sc.SetState(SCE_ERLANG_OPERATOR);
				module_type = (sc.ch == ':' && sc.chNext != '=' &&
							   sc.chNext != ':') ? OTHER_MODULE
												 : NONE_MODULE;
			}
		}
		if (last_state != sc.state && !IsSpaceEquivStyle(sc.state)) {
			last_state = sc.state;
		}
	}
	sc.Complete();
}

static int ClassifyErlangFoldPoint(Accessor &styler, int styleNext,
								   Sci_Position keyword_start) {
	int lev = 0;
	if (styler.Match(keyword_start,"case")
		|| styler.Match(keyword_start,"if")
		|| styler.Match(keyword_start,"maybe")
		|| styler.Match(keyword_start,"query")
		|| styler.Match(keyword_start,"receive")
		|| (styler.Match(keyword_start,"fun") &&
			(styleNext != SCE_ERLANG_FUNCTION))) {
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
		
		if (stylePrev != SCE_ERLANG_STD_WORD
			&& style == SCE_ERLANG_STD_WORD) {
			keyword_start = i;
		}
		// Fold on keywords
		if (stylePrev == SCE_ERLANG_STD_WORD
			&& style != SCE_ERLANG_STD_WORD
			&& style != SCE_ERLANG_ATOM) {
			currentLevel += ClassifyErlangFoldPoint(styler,
													styleNext,
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
	"Standard keywords",
	"Standard atoms",
	"Standard functions (BIFs)",
	"Standard modules (BIMs)",
	"Standard module attributes",
	"Standard macros",
	"Built-in type functions",
	"Preprocessor instructions",
	"Documentation tags",
	"Documentation macro tags",
	"Task marker and error marker keywords",
	0
};

LexerModule lmErlang(
	SCLEX_ERLANG,
	ColouriseErlangDoc,
	"erlang",
	FoldErlangDoc,
	erlangWordListDesc);
