// Scintilla source code edit control
/** @file LexProps.cxx
 ** Lexer for properties files.
 **/
// Copyright 1998-2001 by Neil Hodgson <neilh@scintilla.org>
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

using namespace Scintilla;

static inline bool IsAssignChar(const int ch) {
	return (ch == '=') || (ch == ':');
}

// symb '-' can be part of word (example: -gtk-icon, value2-0)
static inline bool IsAWordChar(const unsigned int ch) {
	return isalnum(ch) || ch == '_' || ch == '-';
}

// exclude symb '-' from oper-value
static inline bool IsOperValue(const int ch) {
	return ch == '!' || ch == '#' || ch == '%' || ch == '&' ||
		   ch == '*' || ch == '+' || ch == ',' || ch == '.' ||
		   ch == ':' || ch == ';' || ch == '=' || ch == '?' ||
		   ch == '@' || ch == '^' || ch == '~' || ch == '|' ||
		   ch == '[' || ch == ']' || ch == '{' || ch == '}' ||
		   ch == '(' || ch == ')' || ch == '<' || ch == '>' ||
		   ch == '/' || ch == '\\' || ch == '$' || ch == '"' ||
		   ch == '\'';
}

// symb '-' can be part of word or subtract-oper
static inline bool IsSubtractOper(Accessor &styler, Sci_PositionU pos,
								  Sci_PositionU endPos) {
	if (styler[pos++] == '-') {
		bool isSubtract = true;
		bool isHexhum = false;
		while (pos < endPos) {
			if (pos < (endPos - 2) && pos > 0
				&& styler[pos - 1] == '-'
				&& styler[pos] == '0' && styler[pos + 1] == 'x'
				&& isxdigit(styler[pos + 2])) {
				pos = pos + 3;
				isHexhum = true;
				continue;
			} else if ((styler[pos] == '.') ||
					   (!isHexhum && isdigit(styler[pos])) ||
					   (isHexhum && isxdigit(styler[pos]))) {
				pos++;
				continue;
			} else if (styler[pos] == '-') {
				isHexhum = false;
				pos++;
				continue;
			} else if (isalpha(styler[pos]) || styler[pos] == '_') {
				isSubtract = false;
			}
			break;
		}
		return isSubtract;
	} else {
		return false;
	}
}

static inline bool IsUrl(const char *pref, Accessor &styler,
						 Sci_PositionU pos, Sci_PositionU endPos) {
	return (pos < endPos - 2) &&
		   (strcmp(pref, "http") == 0 || strcmp(pref, "https") == 0 ||
			strcmp(pref, "ftp") == 0 || strcmp(pref, "sftp") == 0) &&
			styler[pos] == ':' && styler[pos + 1] == '/'
							   && styler[pos + 2] == '/';
}

static inline bool IsEmail(Accessor &styler, Sci_PositionU pos,
						   Sci_PositionU endPos) {
	int mailState = 0;
	for (; pos < endPos; pos++) {
		if (IsACRLF(styler[pos])) { // end of line
			while (--pos > 0 && IsASpaceOrTab(styler[pos]));
			if (!IsUpperOrLowerCase(styler[pos]))
				mailState = 0;
			break;
		} else if (mailState == 0) {
			if (styler[pos] == '@' && pos > 0 && pos < (endPos - 1)
				&& isalnum(styler[pos - 1])
				&& isalnum(styler[pos + 1])) {
				mailState = 1;
			} else if (!IsAWordChar(styler[pos]) && styler[pos] != '.') {
				break;
			}
		} else if (mailState == 1) {
			if (IsAWordChar(styler[pos])) {
				mailState = 2;
			} else {
				break;
			}
		} else if (mailState == 2) {
			if (styler[pos] == '.') {
				mailState = 3;
			} else if (!IsAWordChar(styler[pos])) {
				break;
			}
		} else if (mailState == 3) {
			if (IsAWordChar(styler[pos])) {
				mailState = 4;
			} else {
				break;
			}
		} else if (mailState == 4) {
			if (!IsAWordChar(styler[pos]) && styler[pos] != '.') {
				mailState = 0;
				break;
			}
		}
	}
	return mailState == 4;
}

//~ esh: CheckSubVar func
static inline bool CheckSubVar(StyleContext &sc, Accessor &styler,
							   Sci_PositionU endPos, bool *isSubVar,
							   int *beforeSubVarState) {
	if ((sc.ch == '#' || sc.ch == '$' || sc.ch == '%')
		&& sc.chNext == '{') {
		for (Sci_PositionU i = sc.currentPos + 2; i < endPos; i++) {
			if (IsACRLF(styler[i])) { // end of line
				break;
			} else if (styler[i] == '}') {
				if (sc.state == SCE_PROPS_DOUBLESTRING ||
					sc.state == SCE_PROPS_SINGLESTRING)
					*beforeSubVarState = sc.state;
				else
					*beforeSubVarState = SCE_PROPS_VALUE;
				
				sc.SetState(SCE_PROPS_SUBVAR_OPER);
				sc.Forward();
				*isSubVar = true;
				return true;
			}
		}
	} else if (*isSubVar && sc.ch == '}') {
		sc.SetState(SCE_PROPS_SUBVAR_OPER);
		*isSubVar = false;
		return true;
	}
	return false;
}

static inline void CheckSqBrackets(StyleContext &sc, int *levelSqBrackets) {
	if (sc.ch == '[')
		(*levelSqBrackets)++;
	else if (sc.ch == ']')
		(*levelSqBrackets)--;
}

static void ColourisePropsDoc(Sci_PositionU startPos, Sci_Position length,
							  int initStyle, WordList *keywordlists[],
							  Accessor &styler) {
	styler.StartAt(startPos);
	styler.StartSegment(startPos);
	Sci_PositionU endPos = startPos + length;
	
	WordList &commonWords = *keywordlists[0];
	WordList &namedColors = *keywordlists[1];
	
	// property lexer.props.allow.initial.spaces
	//	For properties files, set to 0 to style all lines that start with whitespace in the default style.
	//	This is not suitable for SciTE .properties files which use indentation for flow control but
	//	can be used for RFC2822 text where indentation is used for continuation lines.
	const bool allowInitialSpaces = styler.GetPropertyInt("lexer.props.allow.initial.spaces", 1) != 0;
	
	StyleContext sc(startPos, length, initStyle, styler);
	
	bool goToLineEnd = false;
	bool maybeIpAddr = false;
	int numDigCnt = 0; // for hexnumber
	int numDotCnt = 0; // for hexnumber/number/IP
	int hexColorLen = 0;
	int levelSqBrackets = 0;
	
	bool isSubVar = false;
	int beforeSubVarState = -1;
	
	for (; sc.More(); sc.Forward()) {
		
		if (sc.atLineStart) {
			sc.SetState(SCE_PROPS_DEFAULT);
			
			if (!allowInitialSpaces && IsASpaceOrTab(sc.ch)) {
				// don't allow initial spaces
				goToLineEnd = true;
				continue;
			}
			goToLineEnd = false;
			isSubVar = false;
		}
		if (goToLineEnd)
			continue;
		
		// start new section/key/assignment state
		switch (sc.state) {
			case SCE_PROPS_DEFAULT:
				if (sc.ch == '#' || sc.ch == '!' || sc.ch == ';') {
					sc.ChangeState(SCE_PROPS_COMMENT);
					goToLineEnd = true;
				} else if (sc.ch == '[') {
					sc.ChangeState(SCE_PROPS_SECTION);
				} else if (sc.ch == '@') {
					sc.ChangeState(SCE_PROPS_DEFVAL);
					if (IsAssignChar(sc.chNext))
						sc.ForwardSetState(SCE_PROPS_ASSIGNMENT);
					sc.ForwardSetState(SCE_PROPS_VALUE);
					break;
				} else if (sc.ch == '=') {
					sc.SetState(SCE_PROPS_ASSIGNMENT);
					sc.ForwardSetState(SCE_PROPS_VALUE);
					break;
				} else if (sc.ch == '{' || sc.ch == '}') {
					sc.SetState(SCE_PROPS_OPER_VALUE);
					sc.ForwardSetState(SCE_PROPS_DEFAULT);
				} else if (!IsASpace(sc.ch)) {
					sc.ChangeState(SCE_PROPS_KEY);
				}
				continue;
				
			case SCE_PROPS_SECTION:
				if (sc.ch == ']') {
					sc.ForwardSetState(SCE_PROPS_DEFAULT);
				} else if (sc.atLineEnd) {
					// incomplete section -> changing to SCE_PROPS_DEFAULT
					sc.ChangeState(SCE_PROPS_DEFAULT);
				}
				continue;
				
			case SCE_PROPS_KEY:
				if (IsAssignChar(sc.ch)) {
					sc.SetState(SCE_PROPS_ASSIGNMENT);
					sc.ForwardSetState(SCE_PROPS_VALUE);
					break;
				} else if (sc.atLineEnd) {
					// incomplete key -> changing to SCE_PROPS_DEFAULT
					sc.ChangeState(SCE_PROPS_DEFAULT);
				}
				continue;
		}
		
		// Determine if the current state should terminate.
		switch (sc.state) {
			case SCE_PROPS_VARIABLE:
				if (IsAWordChar(sc.ch))
					continue;
				// end of var
				break;
				
			case SCE_PROPS_HEX_COLOR:
				if (IsADigit(sc.ch, 16)) {
					hexColorLen++;
					continue;
				}
				// end of hex-color
				if ((hexColorLen != 3 && hexColorLen != 6)
					|| IsAWordChar(sc.ch))				// bad hex-color
					sc.ChangeState(SCE_PROPS_VALUE);
				break;
				
			case SCE_PROPS_DOUBLESTRING:
				if (sc.ch == '\"' && sc.chPrev != '\\'
					&& levelSqBrackets == 0) { // exclude regular expression
					// end of string
					sc.ForwardSetState(SCE_PROPS_VALUE);
				} else {
					CheckSqBrackets(sc, &levelSqBrackets);
					continue;
				}
				break;
				
			case SCE_PROPS_SINGLESTRING:
				if (sc.ch == '\'' && sc.chPrev != '\\'
					&& levelSqBrackets == 0) { // exclude regular expression
					// end of string
					sc.ForwardSetState(SCE_PROPS_VALUE);
				} else {
					CheckSqBrackets(sc, &levelSqBrackets);
					continue;
				}
				break;
				
			case SCE_PROPS_NUMBER:
				if (IsADigit(sc.ch) || (sc.ch == '.' &&
										IsADigit(sc.chNext))) {
					if (sc.ch == '.') {
						if (sc.chPrev == '.') {
							sc.ChangeState(SCE_PROPS_VALUE);
							break;
						} else {
							numDotCnt++;
						}
					}
					continue;
				}
				// end of number
				if (sc.ch == '.'
					|| (IsAWordChar(sc.ch) &&
						!IsSubtractOper(styler, sc.currentPos, endPos)))	// bad num/ip
					sc.ChangeState(SCE_PROPS_VALUE);
				else if (numDotCnt == 3 && maybeIpAddr)						// ip-address
					sc.ChangeState(SCE_PROPS_IP_VALUE);
				else if (numDotCnt > 1)										// bad num/ip
					sc.ChangeState(SCE_PROPS_VALUE);
				break;
				
			case SCE_PROPS_HEXNUMBER:
				if ((IsADigit(sc.ch, 16)) || (sc.ch == '.' &&
											  IsADigit(sc.chNext, 16))) {
					if (sc.ch == '.') {
						if (sc.chPrev == '.') {
							sc.ChangeState(SCE_PROPS_VALUE);
							break;
						} else {
							numDotCnt++;
						}
					} else {
						numDigCnt++;
					}
					continue;
				}
				// end of hex-number
				if (sc.ch == '.' || (numDigCnt % 2) != 0
					|| (IsAWordChar(sc.ch) &&
						!IsSubtractOper(styler, sc.currentPos, endPos)))	// bad hexnum
					sc.ChangeState(SCE_PROPS_VALUE);
				else if (numDotCnt == 3)									// ip-address
					sc.ChangeState(SCE_PROPS_IP_VALUE);
				else if (numDotCnt != 0)									// bad hexnum
					sc.ChangeState(SCE_PROPS_VALUE);
				break;
		}
		
		// Determine if a new state should be entered.
		if (sc.state == SCE_PROPS_OPER_VALUE ||
			sc.state == SCE_PROPS_SUBVAR_OPER ||
			!IsAWordChar(sc.chPrev)) {
			// start new typed-value state
			if (sc.chPrev != '.' && sc.Match('0', 'x')
				&& IsADigit(styler.SafeGetCharAt(sc.currentPos + 2), 16)) {
				sc.SetState(SCE_PROPS_HEXNUMBER);
				sc.Forward();
				numDigCnt = 0;
				numDotCnt = 0;
				continue;
				
			} else if (sc.chPrev != '.' &&
					   (IsADigit(sc.ch) || (sc.ch == '.' && IsADigit(sc.chNext)) ||
						((sc.ch == '+' || sc.ch == '-') &&
						 (sc.chNext == '.' ||
						  (IsADigit(sc.chNext) &&
						   !(sc.chNext == '0' &&
							 styler.SafeGetCharAt(sc.currentPos + 2) == 'x')))))) {
				sc.SetState(SCE_PROPS_NUMBER);
				if (sc.ch == '.') {
					numDotCnt = 1;
					maybeIpAddr = false;
				} else if (sc.ch == '+' || sc.ch == '-') {
					numDotCnt = 0;
					maybeIpAddr = true;
				} else {
					numDotCnt = 0;
					maybeIpAddr = true;
				}
				continue;
				
			} else if (sc.ch == '$' && IsUpperOrLowerCase(sc.chNext)) {
				sc.SetState(SCE_PROPS_VARIABLE);
				continue;
				
			} else if (sc.ch == '#' && IsADigit(sc.chNext, 16)) {
				sc.SetState(SCE_PROPS_HEX_COLOR);
				hexColorLen = 0;
				continue;
				
			} else if ((sc.ch == '\"' || sc.ch == '\'') && sc.chPrev != '\\') {
				int levelSqBrcks = 0;
				bool isString = false;
				for (Sci_PositionU i = sc.currentPos + 1; i < endPos; i++) {
					if (IsACRLF(styler[i])) { // end of line
						break;
					} else if (styler[i] == '[') {
						levelSqBrcks++;
					} else if (styler[i] == ']') {
						levelSqBrcks--;
					} else if (styler[i] == sc.ch && styler[i - 1] != '\\'
								&& levelSqBrcks == 0) { // exclude regular expression
						// start of string
						sc.SetState(sc.ch == '\"' ? SCE_PROPS_DOUBLESTRING:
													SCE_PROPS_SINGLESTRING);
						levelSqBrackets = 0;
						isString = true;
						break;
					}
				}
				if (isString) continue;
				
			} else if (IsAWordChar(sc.ch) && /* exclude subtract-oper */
					   !IsSubtractOper(styler, sc.currentPos, endPos)) {
				// sc.chPrev != word-char (alnum, '_', '-')
				// --START OF WORD--
				if (sc.state != SCE_PROPS_VALUE)
					sc.SetState(SCE_PROPS_VALUE);
				continue;
			}
			
		} else if (sc.state == SCE_PROPS_VALUE &&
				   !IsAWordChar(sc.ch)) {
			// sc.chPrev == word-char (alnum, '_', '-')
			// --END OF WORD--
			char word[100];
			sc.GetCurrentLowered(word, sizeof(word));
			char *word2 = word;
			while (*word2 && !IsAWordChar(*word2))
				word2++;
			
			// check url:
			if (IsUrl(word2, styler, sc.currentPos, endPos)) {
				sc.ChangeState(SCE_PROPS_URL_VALUE);
				goToLineEnd = true;
				continue;
			}
			// check email:
			if (IsEmail(styler, sc.currentPos, endPos)) {
				sc.ChangeState(SCE_PROPS_MAIL_VALUE);
				goToLineEnd = true;
				continue;
			}
			// find a word to defined lists:
			if (commonWords.InList(word2)) {
				sc.ChangeState(SCE_PROPS_COMMONWORD);
			} else if (namedColors.InList(word2)) {
				sc.ChangeState(SCE_PROPS_NAMED_COLOR);
			}
		} else if (sc.state == SCE_PROPS_VALUE) {
			// --CONTINUATION OF THE WORD--
			continue;
		}
		
		if (CheckSubVar(sc, styler, endPos, &isSubVar,
						&beforeSubVarState))
			continue;
		
		if (IsOperValue(sc.ch) || sc.ch == '-')
			sc.SetState(SCE_PROPS_OPER_VALUE);
		else if (sc.state != SCE_PROPS_VALUE)
			sc.SetState(SCE_PROPS_VALUE);
	}
	sc.Complete();
}

// adaption by ksc, using the "} else {" trick of 1.53
// 030721
static void FoldPropsDoc(Sci_PositionU startPos, Sci_Position length,
						 int, WordList *[], Accessor &styler) {
	
	const bool foldCompact = styler.GetPropertyInt("fold.compact", 1) != 0;
	
	const Sci_PositionU endPos = startPos + length;
	int visibleChars = 0;
	Sci_Position lineCurrent = styler.GetLine(startPos);
	
	char chNext = styler[startPos];
	int styleNext = styler.StyleAt(startPos);
	bool headerPoint = false;
	int lev;
	
	for (Sci_PositionU i = startPos; i < endPos; i++) {
		const char ch = chNext;
		chNext = styler[i+1];
		
		const int style = styleNext;
		styleNext = styler.StyleAt(i + 1);
		const bool atEOL = (ch == '\r' && chNext != '\n') || (ch == '\n');
		
		if (style == SCE_PROPS_SECTION) {
			headerPoint = true;
		}
		
		if (atEOL) {
			lev = SC_FOLDLEVELBASE;
			
			if (lineCurrent > 0) {
				const int levelPrevious = styler.LevelAt(lineCurrent - 1);
				
				if (levelPrevious & SC_FOLDLEVELHEADERFLAG) {
					lev = SC_FOLDLEVELBASE + 1;
				} else {
					lev = levelPrevious & SC_FOLDLEVELNUMBERMASK;
				}
			}
			
			if (headerPoint) {
				lev = SC_FOLDLEVELBASE;
			}
			if (visibleChars == 0 && foldCompact)
				lev |= SC_FOLDLEVELWHITEFLAG;
			
			if (headerPoint) {
				lev |= SC_FOLDLEVELHEADERFLAG;
			}
			if (lev != styler.LevelAt(lineCurrent)) {
				styler.SetLevel(lineCurrent, lev);
			}
			
			lineCurrent++;
			visibleChars = 0;
			headerPoint = false;
		}
		if (!IsASpace(ch))
			visibleChars++;
	}
	
	if (lineCurrent > 0) {
		const int levelPrevious = styler.LevelAt(lineCurrent - 1);
		if (levelPrevious & SC_FOLDLEVELHEADERFLAG) {
			lev = SC_FOLDLEVELBASE + 1;
		} else {
			lev = levelPrevious & SC_FOLDLEVELNUMBERMASK;
		}
	} else {
		lev = SC_FOLDLEVELBASE;
	}
	int flagsNext = styler.LevelAt(lineCurrent);
	styler.SetLevel(lineCurrent, lev | (flagsNext & ~SC_FOLDLEVELNUMBERMASK));
}

static const char *const propsWordListDesc[] = {
	"Common keywords and identifiers",
	"Named Colors",
	0
};

LexerModule lmProps(SCLEX_PROPERTIES, ColourisePropsDoc, "props",
					FoldPropsDoc, propsWordListDesc);
