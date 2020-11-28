// Scintilla source code edit control
// Encoding: UTF-8
/** @file LexCSS.cxx
 ** Lexer for Cascading Style Sheets
 ** Written by Jakub Vrána
 ** Improved by Philippe Lhoste (CSS2)
 ** Improved by Ross McKay (SCSS mode; see http://sass-lang.com/ )
 **/
// Copyright 1998-2002 by Neil Hodgson <neilh@scintilla.org>
// The License.txt file describes the conditions under which this software may be distributed.

// TODO: handle SCSS nested properties like font: { weight: bold; size: 1em; }
// TODO: handle SCSS interpolation: #{}
// TODO: add features for Less if somebody feels like contributing; http://lesscss.org/
// TODO: refactor this monster so that the next poor slob can read it!

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

static inline bool IsDimension(const char* s) {
	//~ % em ex px pt pc in ft mm cm Hz kHz deg rad grad s ms turn
	return (strcmp(s, "%") == 0 || strcmp(s, "em") == 0 ||
			strcmp(s, "ex") == 0 || strcmp(s, "px") == 0 ||
			strcmp(s, "pt") == 0 || strcmp(s, "pc") == 0 ||
			strcmp(s, "in") == 0 || strcmp(s, "ft") == 0 ||
			strcmp(s, "mm") == 0 || strcmp(s, "cm") == 0 ||
			strcmp(s, "hz") == 0 || strcmp(s, "khz") == 0 ||
			strcmp(s, "deg") == 0 || strcmp(s, "rad") == 0 ||
			strcmp(s, "grad") == 0 || strcmp(s, "s") == 0 ||
			strcmp(s, "ms") == 0 || strcmp(s, "turn") == 0 ||
			strcmp(s, "dpi") == 0);
}

static inline bool IsUrl(const char *pref, Accessor &styler,
						 Sci_PositionU pos, Sci_PositionU endPos) {
	return (pos < endPos - 2) &&
		   (strcmp(pref, "http") == 0 || strcmp(pref, "https") == 0 ||
			strcmp(pref, "ftp") == 0 || strcmp(pref, "sftp") == 0) &&
			styler[pos] == ':' && styler[pos + 1] == '/'
							   && styler[pos + 2] == '/';
}

static inline bool IsAWordChar(const unsigned int ch) {
	/* FIXME:
	 * The CSS spec allows "ISO 10646 characters U+00A1 and higher" to be treated as word chars.
	 * Unfortunately, we are only getting string bytes here, and not full unicode characters. We cannot guarantee
	 * that our byte is between U+0080 - U+00A0 (to return false), so we have to allow all characters U+0080 and higher
	 */
	return ch >= 0x80 || isalnum(ch) || ch == '-' || ch == '_';
}

static inline bool IsAWordOrPercent(const int ch) {
	return IsAWordChar(ch) || ch == '%';
}

static inline bool IsAWordOrSpace(const int ch) {
	return IsAWordChar(ch) || IsASpace(ch);
}

static inline bool IsCssOperValue(const int ch) {
	return ch == '(' || ch == ')' || ch == ',' || ch == '/' ||
		   ch == '*' || ch == '+';
}

static inline bool IsCssSelectorOper(const int ch) {
	return ch == '.' || ch == ':' || ch == '&' ||
		   ch == '>' || ch == '+' || ch == '[' || ch == ']';
}

static inline bool IsCssOperator(const int ch) {
	if (!((ch < 0x80) && isalnum(ch)) &&
		(ch == '{' || ch == '}' || ch == ':' || ch == ',' || ch == ';' ||
		 ch == '.' || ch == '#' || ch == '!' || ch == '@' ||
		 /* CSS2 */
		 ch == '*' || ch == '>' || ch == '+' || ch == '=' || ch == '~' ||
		 ch == '|' || ch == '[' || ch == ']' || ch == '(' || ch == ')')) {
		return true;
	}
	return false;
}

//~ esh: CheckSubVar func
static inline bool CheckSubVar(StyleContext &sc, bool *isSubVar,
							   int *beforeSubVarState) {
	if (sc.Match('#', '{')) {
		if (sc.state != SCE_CSS_SUBVAR_OPER)
			*beforeSubVarState = sc.state;
		sc.SetState(SCE_CSS_SUBVAR_OPER);
		sc.Forward();
		*isSubVar = true;
		return true;
	} else if (*isSubVar && sc.ch == '}') {
		sc.SetState(SCE_CSS_SUBVAR_OPER);
		sc.ForwardSetState(*beforeSubVarState);
		*isSubVar = false;
		// check consecutive sub-var
		return CheckSubVar(sc, isSubVar, beforeSubVarState);
	}
	return false;
}

// look behind (from start of document to our start position) to determine current nesting level
static inline int NestingLevelLookBehind(Sci_PositionU startPos,
										 Accessor &styler) {
	int ch;
	int nestingLevel = 0;
	
	for (Sci_PositionU i = 0; i < startPos; i++) {
		// esh: added check comment style
		if (styler.StyleAt(i) == SCE_CSS_COMMENT)
			continue;
		ch = styler.SafeGetCharAt(i);
		if (ch == '{')
			nestingLevel++;
		else if (ch == '}')
			nestingLevel--;
	}
	return nestingLevel;
}

static void ColouriseCssDoc(Sci_PositionU startPos, Sci_Position length,
							int initStyle, WordList *keywordlists[],
							Accessor &styler) {
	WordList &css1Props = *keywordlists[0];
	WordList &pseudoClasses = *keywordlists[1];
	WordList &css2Props = *keywordlists[2];
	WordList &css3Props = *keywordlists[3];
	WordList &pseudoElements = *keywordlists[4];
	WordList &exProps = *keywordlists[5];
	WordList &exPseudoClasses = *keywordlists[6];
	WordList &exPseudoElements = *keywordlists[7];
	WordList &namedColors = *keywordlists[8];
	
	// esh: escapesequence highlighting
	const bool escapeSequence = styler.GetPropertyInt("lexer.css.escape.sequence", 0) != 0;
	EscapeSequence escapeSeq = EscapeSequence();
	int stringState = -1;
	
	// esh: Backtrack to previous line in case need to fix its tab whinging (taken from LexPython.cxx)
	Sci_Position lineCurrent = styler.GetLine(startPos);
	if (startPos > 0) {
		if (lineCurrent > 0) {
			lineCurrent--;
			// Look for backslash-continued lines
			int eolStyle;
			while (lineCurrent > 0) {
				eolStyle = styler.StyleAt(styler.LineStart(lineCurrent) - 1);
				if (eolStyle == SCE_CSS_DOUBLESTRING
						|| eolStyle == SCE_CSS_SINGLESTRING
						|| eolStyle == SCE_CSS_ESCAPESEQUENCE) {
					lineCurrent--;
				} else {
					break;
				}
			}
			Sci_PositionU newStartPos = styler.LineStart(lineCurrent);
			length += (startPos - newStartPos);
			startPos = newStartPos;
		}
		initStyle = startPos == 0 ? SCE_CSS_DEFAULT : styler.StyleAt(startPos - 1);
	}
	
	const Sci_PositionU endPos = startPos + length;
	
	StyleContext sc(startPos, length, initStyle, styler);
	
	int lastState = -1; // before operator
	int lastStateC = -1; // before comment
	int lastStateS = -1; // before single-quoted/double-quoted string
	int beforeVarState = -1; // before variable (SCSS)
	int beforeValState = -1; // before value (SCSS)
	int op = ' '; // last operator
	int opPrev = ' '; // last operator
	bool insideParentheses = false; // true if currently in a CSS url() or similar construct
	
	// esh:
	int hexColorLen = 0;
	bool isSubVar = false;	// example: #{$name}, where $name - var
							//          #{unique-id()}, where unique-id() - func
	int beforeSubVarState = -1;
	// esh:
	bool isMediaFunc = false; // and, not, only
	
	// SCSS/LESS/HSS support single-line comments
	typedef enum _CommentModes { eCommentBlock = 0, eCommentLine = 1} CommentMode;
	CommentMode comment_mode = eCommentBlock;
	
	// must keep track of nesting level in document types that support it (SCSS/LESS/HSS)
	int nestingLevel = NestingLevelLookBehind(startPos, styler);
	
	// "the loop"
	for (; sc.More(); sc.Forward()) {
		if (sc.state == SCE_CSS_COMMENT && ((comment_mode == eCommentBlock && sc.Match('*', '/')) ||
											(comment_mode == eCommentLine && sc.atLineEnd))) {
			// esh: --END OF COMMENT--
			if (lastStateC == -1) {
				// backtrack to get last state:
				// comments are like whitespace, so we must return to the previous state
				Sci_PositionU i = startPos;
				for (; i > 0; i--) {
					if ((lastStateC = styler.StyleAt(i - 1)) != SCE_CSS_COMMENT) {
						if (lastStateC == SCE_CSS_OPERATOR) {
							op = styler.SafeGetCharAt(i - 1);
							opPrev = styler.SafeGetCharAt(i - 2);
							while (--i) {
								lastState = styler.StyleAt(i - 1);
								if (lastState != SCE_CSS_OPERATOR && lastState != SCE_CSS_COMMENT)
									break;
							}
							if (i == 0)
								lastState = SCE_CSS_DEFAULT;
						}
						break;
					}
				}
				if (i == 0)
					lastStateC = SCE_CSS_DEFAULT;
			}
			if (comment_mode == eCommentBlock) {
				sc.Forward();
				if (lastStateC == SCE_CSS_NUMBER || lastStateC == SCE_CSS_DIMENSION ||
					lastStateC == SCE_CSS_NAMED_COLOR || lastStateC == SCE_CSS_HEX_COLOR ||
					lastStateC == SCE_CSS_IMPORTANT || lastStateC == SCE_CSS_ERR_VALUE)
					sc.ForwardSetState(SCE_CSS_VALUE);
				else
					sc.ForwardSetState(lastStateC);
			} else /* eCommentLine */ {
				sc.SetState(lastStateC);
			}
		}
		
		if (sc.state == SCE_CSS_COMMENT)
			continue;
		
		// esh: check sub-var
		if (sc.state != SCE_CSS_VALUE &&
			sc.state != SCE_CSS_HEX_COLOR &&
			sc.state != SCE_CSS_IMPORTANT &&
			CheckSubVar(sc, &isSubVar, &beforeSubVarState)) {
			continue;
		} else if (sc.state == SCE_CSS_SUBVAR_OPER && !isSubVar &&
				   (beforeSubVarState == SCE_CSS_DOUBLESTRING ||
					beforeSubVarState == SCE_CSS_SINGLESTRING)) {
			sc.SetState(beforeSubVarState); // after sub-var inside string
			// restore the state of the previous string (if there was a
			// sub-string inside the sub-var and the stringState was changed)
			stringState = sc.state;
		}
		
		if (sc.state == SCE_CSS_DOUBLESTRING ||
			sc.state == SCE_CSS_SINGLESTRING ||
			sc.state == SCE_CSS_ESCAPESEQUENCE) {
			
			if (sc.state == SCE_CSS_ESCAPESEQUENCE) {
				escapeSeq.digitsLeft--;
				if (!escapeSeq.atEscapeEnd(sc.ch)) {
					continue; // esh: continue of escape chars
				}
			}
			
			if (sc.ch == '\\') {
				if (escapeSequence && sc.state != SCE_CSS_ESCAPESEQUENCE) {
					sc.SetState(SCE_CSS_ESCAPESEQUENCE);
					escapeSeq.resetEscapeState(sc.chNext);
				}
				sc.Forward(); // Skip any character after the backslash
				continue; // esh: continue of string value
				
			} else if (sc.ch != (stringState == SCE_CSS_DOUBLESTRING ? '\"' : '\'')) {
				if (sc.state == SCE_CSS_ESCAPESEQUENCE)
					sc.SetState(stringState);
				continue; // esh: continue of string value
			}
			
			// esh: the block code is commented -  no need to check (check done above)
			//~ Sci_PositionU i = sc.currentPos;
			//~ while (i && styler[i - 1] == '\\')
				//~ i--;
			//~ if ((sc.currentPos - i) % 2 == 1)
				//~ continue; // esh: continue of string value
			
			if (sc.state == SCE_CSS_ESCAPESEQUENCE)
				sc.SetState(stringState);
			// esh: end of string value
			sc.ForwardSetState(lastStateS);
		}
		
		if (sc.state == SCE_CSS_OPERATOR) {
			if (op == ' ') {
				Sci_PositionU i = startPos;
				op = styler.SafeGetCharAt(i - 1);
				opPrev = styler.SafeGetCharAt(i - 2);
				while (--i) {
					lastState = styler.StyleAt(i - 1);
					if (lastState != SCE_CSS_OPERATOR && lastState != SCE_CSS_COMMENT)
						break;
				}
			}
			switch (op) { // esh: lastState before this oper
				case '@':
					sc.SetState(SCE_CSS_DIRECTIVE);
					break;
				case '>':
				case '+':
					if (lastState == SCE_CSS_TAG || lastState == SCE_CSS_CLASS ||
						lastState == SCE_CSS_ID || lastState == SCE_CSS_PSEUDOCLASS ||
						lastState == SCE_CSS_EXTENDED_PSEUDOCLASS ||
						lastState == SCE_CSS_UNKNOWN_PSEUDOCLASS)
						sc.SetState(SCE_CSS_DEFAULT);
					break;
				case '[':
					if (lastState == SCE_CSS_TAG || lastState == SCE_CSS_DEFAULT ||
						lastState == SCE_CSS_CLASS || lastState == SCE_CSS_ID ||
						lastState == SCE_CSS_PSEUDOCLASS ||
						lastState == SCE_CSS_EXTENDED_PSEUDOCLASS ||
						lastState == SCE_CSS_UNKNOWN_PSEUDOCLASS)
						sc.SetState(SCE_CSS_ATTRIBUTE);
					break;
				case ']':
					if (lastState == SCE_CSS_ATTRIBUTE)
						sc.SetState(SCE_CSS_TAG);
					break;
				case '{':
					nestingLevel++;
					switch (lastState) {
						case SCE_CSS_TAG:
						case SCE_CSS_MEDIA:
						case SCE_CSS_DIRECTIVE:
						case SCE_CSS_VALUE:		// esh: this can be value of directive/media
							sc.SetState(SCE_CSS_IDENTIFIER);
							break;
					}
					break;
				case '}':
					if (--nestingLevel < 0)
						nestingLevel = 0;
					// esh: TODO: lastState check can be deleted and call sc.SetState for any state
					switch (lastState) {
						case SCE_CSS_DEFAULT:
						case SCE_CSS_VALUE:
						case SCE_CSS_IDENTIFIER:
						case SCE_CSS_IDENTIFIER2:
						case SCE_CSS_IDENTIFIER3:
							sc.SetState(nestingLevel > 0 ? SCE_CSS_IDENTIFIER :
														   SCE_CSS_DEFAULT);
							break;
					}
					break;
				case '(':
					switch (lastState) {
						case SCE_CSS_TAG:
						case SCE_CSS_CLASS:
						case SCE_CSS_PSEUDOCLASS:
						case SCE_CSS_UNKNOWN_PSEUDOCLASS:
							sc.SetState(SCE_CSS_TAG);
							break;
						case SCE_CSS_EXTENDED_PSEUDOCLASS:
							sc.SetState(SCE_CSS_EXTENDED_PSEUDOCLASS);
							break;
					}
					break;
				case ')':
					if (lastState == SCE_CSS_TAG || lastState == SCE_CSS_DEFAULT ||
						lastState == SCE_CSS_CLASS || lastState == SCE_CSS_ID ||
						lastState == SCE_CSS_PSEUDOCLASS ||
						lastState == SCE_CSS_EXTENDED_PSEUDOCLASS ||
						lastState == SCE_CSS_UNKNOWN_PSEUDOCLASS ||
						lastState == SCE_CSS_PSEUDOELEMENT ||
						lastState == SCE_CSS_EXTENDED_PSEUDOELEMENT)
						sc.SetState(SCE_CSS_TAG);
					break;
				case ':':
					switch (lastState) {
						case SCE_CSS_TAG:
						case SCE_CSS_DEFAULT:
						case SCE_CSS_CLASS:
						case SCE_CSS_ID:
						case SCE_CSS_PSEUDOCLASS:
						case SCE_CSS_EXTENDED_PSEUDOCLASS:
						case SCE_CSS_UNKNOWN_PSEUDOCLASS:
						case SCE_CSS_PSEUDOELEMENT:
						case SCE_CSS_EXTENDED_PSEUDOELEMENT:
							sc.SetState(SCE_CSS_PSEUDOCLASS);
							break;
						case SCE_CSS_IDENTIFIER:
						case SCE_CSS_IDENTIFIER2:
						case SCE_CSS_IDENTIFIER3:
						case SCE_CSS_EXTENDED_IDENTIFIER:
						case SCE_CSS_UNKNOWN_IDENTIFIER:
						case SCE_CSS_VARIABLE:
							sc.SetState(SCE_CSS_VALUE); // esh: (ident: val) or (var: val)
							beforeValState = lastState; // esh: save ident/var-state to beforeValState
							break;
					}
					break;
				case '.':
					if (lastState == SCE_CSS_TAG || lastState == SCE_CSS_DEFAULT ||
						lastState == SCE_CSS_CLASS || lastState == SCE_CSS_ID ||
						lastState == SCE_CSS_PSEUDOCLASS ||
						lastState == SCE_CSS_EXTENDED_PSEUDOCLASS ||
						lastState == SCE_CSS_UNKNOWN_PSEUDOCLASS ||
						lastState == SCE_CSS_IDENTIFIER) // esh: after '@dir {' state is ident
						sc.SetState(SCE_CSS_CLASS);
					break;
				case '#':
					if (lastState == SCE_CSS_TAG || lastState == SCE_CSS_DEFAULT ||
						lastState == SCE_CSS_CLASS || lastState == SCE_CSS_ID ||
						lastState == SCE_CSS_PSEUDOCLASS ||
						lastState == SCE_CSS_EXTENDED_PSEUDOCLASS ||
						lastState == SCE_CSS_UNKNOWN_PSEUDOCLASS)
						sc.SetState(SCE_CSS_ID);
					break;
				case ',':
				case '|':
				case '~':
					if (lastState == SCE_CSS_TAG)
						sc.SetState(SCE_CSS_DEFAULT);
					break;
				case ';':
					switch (lastState) {
						case SCE_CSS_VALUE:
							// data URLs can have semicolons; simplistically check 
							// for wrapping parentheses and move along
							if (insideParentheses) { // esh: oper ';' inside parentheses inside val
								sc.SetState(lastState);
							} else {
								// esh: beforeValState can be var or ident
								if (beforeValState == SCE_CSS_VARIABLE) { // esh: (var: val;)
									sc.SetState(SCE_CSS_DEFAULT);
								} else {								  // esh: (ident: val;)
									sc.SetState(SCE_CSS_IDENTIFIER);
								}
							}
							break;
						case SCE_CSS_VARIABLE:
							// esh: beforeVarState can be SCE_CSS_DEFAULT, SCE_CSS_VALUE
							if (beforeVarState == SCE_CSS_VALUE) { // esh: if (var) inside (val;)
								// data URLs can have semicolons; simplistically check
								// for wrapping parentheses and move along
								if (insideParentheses) { // esh: var and oper ';' inside parentheses inside (val;)
									sc.SetState(SCE_CSS_VALUE);
								} else {
									sc.SetState(SCE_CSS_IDENTIFIER);
								}
							} else {
								sc.SetState(SCE_CSS_DEFAULT);
							}
							break;
						default:
							sc.SetState(nestingLevel > 0 ? SCE_CSS_IDENTIFIER :
														   SCE_CSS_DEFAULT);
					}
					break;
			}
		}
		
		if (sc.ch == '*' && (sc.state == SCE_CSS_DEFAULT ||
							 sc.state == SCE_CSS_IDENTIFIER)) {
			sc.SetState(SCE_CSS_TAG);
			continue;
		}
		
		// check for inside parentheses (whether part of an "operator" or not)
		if (sc.ch == '(')
			insideParentheses = true;
		else if (sc.ch == ')')
			insideParentheses = false;
		
		// variable name
		// esh: @ - for LESS, $ - for SCSS/HSS
		if ((sc.ch == '@' || sc.ch == '$') && IsAWordChar(sc.chNext) &&
			!(sc.state == SCE_CSS_VALUE && IsAWordChar(sc.chPrev))) {
			switch (sc.state) {
				case SCE_CSS_DEFAULT:
				case SCE_CSS_IDENTIFIER:
					if (sc.ch == '@') // give priority to pseudo elements for LESS
						break;
					beforeVarState = sc.state;
					sc.SetState(SCE_CSS_VARIABLE);
					continue;
				case SCE_CSS_VALUE:			// esh: var inside val
				case SCE_CSS_OPER_VALUE:	// esh: var inside val after oper-val
				case SCE_CSS_SUBVAR_OPER:	// esh: var inside sub-var
					beforeVarState = SCE_CSS_VALUE; // esh: oper-val is part of val
					sc.SetState(SCE_CSS_VARIABLE);
					continue;
			}
		}
		if (sc.state == SCE_CSS_VARIABLE) {
			if (IsAWordChar(sc.ch)) {
				// still looking at the variable name
				continue;
			}
			// esh: beforeVarState can be SCE_CSS_DEFAULT, SCE_CSS_IDENTIFIER, SCE_CSS_VALUE
			if (beforeVarState == SCE_CSS_VALUE) {
				// not looking at the variable name any more, and it was part of a value
				sc.SetState(SCE_CSS_VALUE);
			}
		}
		// nested rule parent selector
		if (sc.ch == '&') {
			switch (sc.state) {
				case SCE_CSS_DEFAULT:
				case SCE_CSS_IDENTIFIER:
					sc.SetState(SCE_CSS_TAG);
					continue;
			}
		}
		
		// esh: directive, media
		if (sc.state == SCE_CSS_DIRECTIVE &&
			!IsAWordChar(sc.ch) && IsAWordChar(sc.chPrev)) {
			char word[100];
			sc.GetCurrentLowered(word, sizeof(word));
			char *word2 = word;
			while (*word2 && !IsAWordChar(*word2))
				word2++;
			if (op == '@' && strcmp(word2, "media") == 0) {
				sc.ChangeState(SCE_CSS_MEDIA);
			}
			// determine next state:
			int ch = ' ';
			int chNext;
			bool wordExists = false;
			bool isComment = false;
			for (Sci_PositionU i = sc.currentPos; i < styler.Length(); i++) {
				ch = styler.SafeGetCharAt(i);
				chNext = styler.SafeGetCharAt(i + 1);
				if (!isComment && ch == '/' && chNext == '*') {
					isComment = true;
					i++;
					continue;
				} else if (isComment && ch == '*' && chNext == '/') {
					isComment = false;
					i++;
					continue;
				} else if (isComment) {
					continue;
				}
				if ((IsCssOperator(ch) && ch != ',') || ch == '&')
					break;
				else if (IsAWordChar(ch))
					wordExists = true;
			}
			// set next state for directive/media
			if ((IsCssSelectorOper(ch) && !(ch == '.' && isdigit(chNext)))
				|| (ch == '{' && wordExists))
				sc.SetState(SCE_CSS_DEFAULT);	// fixate directive by default
			else
				sc.SetState(SCE_CSS_VALUE);		// fixate directive by val
		}
		
		// esh: Determine if the current value-state should terminate.
		switch (sc.state) {
			case SCE_CSS_NUMBER:
				if (IsADigit(sc.ch) || (sc.ch == '.' &&
										IsADigit(sc.chNext)))
					continue;
				if (IsAWordOrPercent(sc.ch)) {
					sc.SetState(SCE_CSS_DIMENSION);
					continue;
				}
				// end of number
				break;
				
			case SCE_CSS_DIMENSION: {
				if (IsAWordOrPercent(sc.ch))
					continue;
				
				char dim[10];
				sc.GetCurrentLowered(dim, sizeof(dim));
				if (!IsDimension(dim))
					sc.ChangeState(SCE_CSS_ERR_VALUE);
			}	break;
			
			case SCE_CSS_HEX_COLOR:
				if (IsADigit(sc.ch, 16)) {
					hexColorLen++;
					continue;
				}
				// end of hex-color
				if ((hexColorLen != 3 && hexColorLen != 6)
					|| IsAWordChar(sc.ch))				// bad hex-color
					sc.ChangeState(SCE_CSS_ERR_VALUE);
				break;
				
			case SCE_CSS_IMPORTANT: {
				if (IsAWordChar(sc.ch))
					continue;
				
				char imp[100];
				sc.GetCurrentLowered(imp, sizeof(imp));
				char *imp2 = imp;
				while (*imp2 && !IsAWordChar(*imp2))
					imp2++;
				if (strcmp(imp2, "important") != 0)
					sc.ChangeState(SCE_CSS_ERR_VALUE);
			}	break;
			
			case SCE_CSS_URL_VALUE:
				if (sc.ch == ')')
					sc.SetState(SCE_CSS_OPER_VALUE);
				continue;
		}
		
		// esh: Determine if a new value-state should be entered.
		if (sc.state == SCE_CSS_OPER_VALUE ||	// oper-val: (),/ (see IsCssOperValue func)
			sc.state == SCE_CSS_SUBVAR_OPER ||	// sc.ch inside sub-var
			(sc.state == SCE_CSS_VALUE &&		// sub-val: solid, linear, transparent, ...
			 !IsAWordChar(sc.chPrev))) {
			
			// start new typed-value states
			// (number, hexadec-color, named-color, dimension, ...)
			if ((IsADigit(sc.ch) || (sc.ch == '.' && IsADigit(sc.chNext)) ||
				((sc.ch == '+' || sc.ch == '-') && (sc.chNext == '.' ||
													IsADigit(sc.chNext))))) {
				sc.SetState(SCE_CSS_NUMBER); // fixate sub-val/oper-val by number
				continue;
				
			} else if (sc.ch == '#' && IsADigit(sc.chNext, 16)) {
				sc.SetState(SCE_CSS_HEX_COLOR); // fixate sub-val/oper-val by hexadec-color
				hexColorLen = 0;
				continue;
				
			} else if (IsAWordChar(sc.ch)) {
				// sc.chPrev != word-char (alnum, '_', '-')
				// --START OF WORD--
				if (sc.state != SCE_CSS_VALUE)
					sc.SetState(SCE_CSS_VALUE);
				continue;
			}
		
		// Determine if the value-state (word) should terminate.
		} else if (sc.state == SCE_CSS_VALUE &&
				   !IsAWordChar(sc.ch)) {
			// sc.chPrev == word-char (alnum, '_', '-')
			// --END OF WORD--
			
			// look ahead to see oper '(', ':'
			int ch = ' ';
			int chNext;
			Sci_PositionU i = sc.currentPos;
			bool isComment = false;
			for (; i < styler.Length(); i++) {
				ch = styler.SafeGetCharAt(i);
				chNext = styler.SafeGetCharAt(i + 1);
				if (!isComment && ch == '/' && chNext == '*') {
					isComment = true;
					i++;
					continue;
				} else if (isComment && ch == '*' && chNext == '/') {
					isComment = false;
					i++;
					continue;
				} else if (isComment) {
					continue;
				} else if (!IsASpace(ch)) {
					break;
				}
			}
			char word[100];
			sc.GetCurrentLowered(word, sizeof(word));
			char *word2 = word;
			while (*word2 && !IsAWordChar(*word2))
				word2++;
			
			if (ch == '(') {
				sc.ChangeState(SCE_CSS_FUNCTION);
				isMediaFunc = (strcmp(word2, "and") == 0 ||
							   strcmp(word2, "not") == 0 ||
							   strcmp(word2, "only") == 0);
				
			} else if (ch == ':' && insideParentheses && isMediaFunc) {
				if (css1Props.InList(word2))
					sc.ChangeState(SCE_CSS_IDENTIFIER);
				else if (css2Props.InList(word2))
					sc.ChangeState(SCE_CSS_IDENTIFIER2);
				else if (css3Props.InList(word2))
					sc.ChangeState(SCE_CSS_IDENTIFIER3);
				else if (exProps.InList(word2))
					sc.ChangeState(SCE_CSS_EXTENDED_IDENTIFIER);
				else
					sc.ChangeState(SCE_CSS_UNKNOWN_IDENTIFIER);
				
			// find a word to defined list:
			} else if (namedColors.InList(word2)) {
				sc.ChangeState(SCE_CSS_NAMED_COLOR);
			} else if (insideParentheses &&
					   IsUrl(word2, styler, sc.currentPos, endPos)) {
				sc.ChangeState(SCE_CSS_URL_VALUE);
				continue;
			} else {
				// fixate sub-val by sub-val (by default value state)
				sc.SetState(SCE_CSS_VALUE);
			}
			
		// Determine if the value-state (word) should continuation.
		} else if (sc.state == SCE_CSS_VALUE) {
			// --CONTINUATION OF THE WORD--
			continue;
		}
		
		if ((sc.state == SCE_CSS_VALUE || sc.state == SCE_CSS_NUMBER ||
			 sc.state == SCE_CSS_DIMENSION || sc.state == SCE_CSS_HEX_COLOR ||
			 sc.state == SCE_CSS_NAMED_COLOR || sc.state == SCE_CSS_ERR_VALUE ||
			 sc.state == SCE_CSS_FUNCTION || sc.state == SCE_CSS_OPER_VALUE ||
			 sc.state == SCE_CSS_IMPORTANT || sc.state == SCE_CSS_SUBVAR_OPER)) {
			 // TODO: refactoring: invert conditions
			
			// esh: check sub-var
			if (sc.Match('#', '{')) {
				if (sc.state != SCE_CSS_SUBVAR_OPER)
					beforeSubVarState = sc.state;
				sc.SetState(SCE_CSS_SUBVAR_OPER);
				sc.Forward();
				isSubVar = true;
				continue;
			} else if (isSubVar && sc.ch == '}') {
				sc.SetState(SCE_CSS_SUBVAR_OPER);
				isSubVar = false;
				continue;
			}
			
			if (sc.ch == '!') {
				sc.SetState(SCE_CSS_IMPORTANT); // fixate current state (before sc.currentPos) by important
				while (sc.currentPos < endPos && IsASpace(sc.chNext))
					sc.Forward();
				continue;
				
			} else if (IsCssOperValue(sc.ch) || ((sc.ch == ':' || sc.ch == ';') &&
												 insideParentheses)) {
				if (!sc.Match('/', '*') && !(sc.Match('/', '/') &&
											 !insideParentheses)) {
					sc.SetState(SCE_CSS_OPER_VALUE); // fixate current state (before sc.currentPos) by oper-val
					continue;
				}
			} else if (sc.state != SCE_CSS_VALUE) {
				// fixate current state (before sc.currentPos) by sub-val (by default value state)
				sc.SetState(SCE_CSS_VALUE);
			}
		}
		
		// check for nested rule selector (SCSS/LESS)
		if (sc.state == SCE_CSS_IDENTIFIER && !insideParentheses &&
			(IsAWordChar(sc.ch) || sc.ch == ':' || sc.ch == '.' ||
			 (sc.ch == '#' && sc.chNext != '{'))) {				// skip sub-var
			// look ahead to see whether { comes before next ; and }
			int ch, chNext;
			int subVarLevel = 0;
			bool isComment = false;
			for (Sci_PositionU i = sc.currentPos; i < styler.Length(); i++) {
				ch = styler.SafeGetCharAt(i);
				chNext = styler.SafeGetCharAt(i + 1);
				if (!isComment && ch == '/' && chNext == '*') {
					isComment = true;
					i++;
					continue;
				} else if (isComment && ch == '*' && chNext == '/') {
					isComment = false;
					i++;
					continue;
				} else if (isComment) {
					continue;
				}
				if (ch == ';' || ch == '}') {
					if (ch == '}' && subVarLevel > 0) {			// skip sub-var
						subVarLevel--;
						continue;
					}
					break;
				}
				if (ch == '{') {
					if (styler.SafeGetCharAt(i - 1) == '#') {	// skip sub-var
						subVarLevel++;
					} else if (subVarLevel == 0) {
						sc.SetState(SCE_CSS_DEFAULT);
						break;
					}
				}
			}
		}
		
		if (IsAWordChar(sc.ch)) {
			if (sc.state == SCE_CSS_DEFAULT)
				sc.SetState(SCE_CSS_TAG);
			continue;
		}
		
		if (IsAWordChar(sc.chPrev) &&
			(sc.state == SCE_CSS_IDENTIFIER ||
			 sc.state == SCE_CSS_IDENTIFIER2 ||
			 sc.state == SCE_CSS_IDENTIFIER3 ||
			 sc.state == SCE_CSS_EXTENDED_IDENTIFIER ||
			 sc.state == SCE_CSS_UNKNOWN_IDENTIFIER ||
			 sc.state == SCE_CSS_PSEUDOCLASS ||
			 sc.state == SCE_CSS_PSEUDOELEMENT ||
			 sc.state == SCE_CSS_EXTENDED_PSEUDOCLASS ||
			 sc.state == SCE_CSS_EXTENDED_PSEUDOELEMENT ||
			 sc.state == SCE_CSS_UNKNOWN_PSEUDOCLASS)) {
			char s[100];
			sc.GetCurrentLowered(s, sizeof(s));
			char *s2 = s;
			while (*s2 && !IsAWordChar(*s2))
				s2++;
			switch (sc.state) {
				case SCE_CSS_IDENTIFIER:
				case SCE_CSS_IDENTIFIER2:
				case SCE_CSS_IDENTIFIER3:
				case SCE_CSS_EXTENDED_IDENTIFIER:
				case SCE_CSS_UNKNOWN_IDENTIFIER:
					if (css1Props.InList(s2))
						sc.ChangeState(SCE_CSS_IDENTIFIER);
					else if (css2Props.InList(s2))
						sc.ChangeState(SCE_CSS_IDENTIFIER2);
					else if (css3Props.InList(s2))
						sc.ChangeState(SCE_CSS_IDENTIFIER3);
					else if (exProps.InList(s2))
						sc.ChangeState(SCE_CSS_EXTENDED_IDENTIFIER);
					else
						sc.ChangeState(SCE_CSS_UNKNOWN_IDENTIFIER);
					break;
					
				case SCE_CSS_PSEUDOCLASS:
				case SCE_CSS_PSEUDOELEMENT:
				case SCE_CSS_EXTENDED_PSEUDOCLASS:
				case SCE_CSS_EXTENDED_PSEUDOELEMENT:
				case SCE_CSS_UNKNOWN_PSEUDOCLASS:
					if (op == ':' && opPrev != ':' && pseudoClasses.InList(s2))
						sc.ChangeState(SCE_CSS_PSEUDOCLASS);
					else if (opPrev == ':' && pseudoElements.InList(s2))
						sc.ChangeState(SCE_CSS_PSEUDOELEMENT);
					else if ((op == ':' || (op == '(' &&
							  lastState == SCE_CSS_EXTENDED_PSEUDOCLASS)) &&
							 opPrev != ':' && exPseudoClasses.InList(s2))
						sc.ChangeState(SCE_CSS_EXTENDED_PSEUDOCLASS);
					else if (opPrev == ':' && exPseudoElements.InList(s2))
						sc.ChangeState(SCE_CSS_EXTENDED_PSEUDOELEMENT);
					else
						sc.ChangeState(SCE_CSS_UNKNOWN_PSEUDOCLASS);
					break;
			}
		}
		
		if (sc.ch != '.' && sc.ch != ':' && sc.ch != '#' && (
			sc.state == SCE_CSS_CLASS || sc.state == SCE_CSS_ID ||
			(sc.ch != '(' && sc.ch != ')' && ( /* This line of the condition makes it possible to extend pseudo-classes with parentheses */
				sc.state == SCE_CSS_PSEUDOCLASS ||
				sc.state == SCE_CSS_PSEUDOELEMENT ||
				sc.state == SCE_CSS_EXTENDED_PSEUDOCLASS ||
				sc.state == SCE_CSS_EXTENDED_PSEUDOELEMENT ||
				sc.state == SCE_CSS_UNKNOWN_PSEUDOCLASS
			))
		)) sc.SetState(SCE_CSS_TAG);
		
		if (sc.Match('/', '*')) {
			lastStateC = sc.state;
			comment_mode = eCommentBlock;
			sc.SetState(SCE_CSS_COMMENT);
			sc.Forward();
			
		} else if (sc.Match('/', '/') && !insideParentheses) {
			// note that we've had to treat ([...]// as the start of a URL not a comment, e.g. url(http://example.com), url(//example.com)
			lastStateC = sc.state;
			comment_mode = eCommentLine;
			sc.SetState(SCE_CSS_COMMENT);
			sc.Forward();
			
		} else if ((sc.state == SCE_CSS_VALUE || sc.state == SCE_CSS_ATTRIBUTE)
				   && (sc.ch == '\"' || sc.ch == '\'')) {
			lastStateS = sc.state;
			sc.SetState((sc.ch == '\"' ? SCE_CSS_DOUBLESTRING : SCE_CSS_SINGLESTRING));
			stringState = sc.state;
			
		} else if (IsCssOperator(sc.ch)
				   && (sc.state != SCE_CSS_ATTRIBUTE || sc.ch == ']')
				   && (sc.state != SCE_CSS_VALUE || sc.ch == ';' || sc.ch == '{' || sc.ch == '}')
				   && ((sc.state != SCE_CSS_DIRECTIVE && sc.state != SCE_CSS_MEDIA) ||
					   sc.ch == ';' || sc.ch == '{')) {
			if (sc.state != SCE_CSS_OPERATOR)
				lastState = sc.state;
			sc.SetState(SCE_CSS_OPERATOR);
			op = sc.ch;
			opPrev = sc.chPrev;
		}
	}
	sc.Complete();
}

static void FoldCSSDoc(Sci_PositionU startPos, Sci_Position length,
					   int, WordList *[], Accessor &styler) {
	
	bool foldComment = styler.GetPropertyInt("fold.comment") != 0;
	bool foldCompact = styler.GetPropertyInt("fold.compact", 1) != 0;
	
	Sci_PositionU endPos = startPos + length;
	int visibleChars = 0;
	Sci_Position lineCurrent = styler.GetLine(startPos);
	
	int levelPrev = styler.LevelAt(lineCurrent) & SC_FOLDLEVELNUMBERMASK;
	int levelCurrent = levelPrev;
	char chNext = styler[startPos];
	bool inComment = (styler.StyleAt(startPos - 1) == SCE_CSS_COMMENT);
	
	for (Sci_PositionU i = startPos; i < endPos; i++) {
		char ch = chNext;
		chNext = styler.SafeGetCharAt(i + 1);
		int style = styler.StyleAt(i);
		bool atEOL = (ch == '\r' && chNext != '\n') || (ch == '\n');
		if (foldComment) {
			if (!inComment && (style == SCE_CSS_COMMENT))
				levelCurrent++;
			else if (inComment && (style != SCE_CSS_COMMENT))
				levelCurrent--;
			inComment = (style == SCE_CSS_COMMENT);
		}
		if (style == SCE_CSS_OPERATOR) {
			if (ch == '{') {
				levelCurrent++;
			} else if (ch == '}') {
				levelCurrent--;
			}
		}
		if (atEOL) {
			int lev = levelPrev;
			if (visibleChars == 0 && foldCompact)
				lev |= SC_FOLDLEVELWHITEFLAG;
			if ((levelCurrent > levelPrev) && (visibleChars > 0))
				lev |= SC_FOLDLEVELHEADERFLAG;
			if (lev != styler.LevelAt(lineCurrent)) {
				styler.SetLevel(lineCurrent, lev);
			}
			lineCurrent++;
			levelPrev = levelCurrent;
			visibleChars = 0;
		}
		if (!IsASpace(ch))
			visibleChars++;
	}
	// Fill in the real level of the next line, keeping the current flags as they will be filled in later
	int flagsNext = styler.LevelAt(lineCurrent) & ~SC_FOLDLEVELNUMBERMASK;
	styler.SetLevel(lineCurrent, levelPrev | flagsNext);
}

static const char * const cssWordListDesc[] = {
	"CSS1 Properties",
	"Pseudo-classes",
	"CSS2 Properties",
	"CSS3 Properties",
	"Pseudo-elements",
	"Browser-Specific CSS Properties",
	"Browser-Specific Pseudo-classes",
	"Browser-Specific Pseudo-elements",
	"Named Colors",
	0
};

LexerModule lmCss(SCLEX_CSS, ColouriseCssDoc, "css",
				  FoldCSSDoc, cssWordListDesc);
