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

static inline bool isassignchar(unsigned char ch) {
	return (ch == '=') || (ch == ':');
}

static inline bool IsAWordChar(const unsigned int ch) {
	return isalnum(ch) || ch == '-' || ch == '_';
}

static inline bool IsSpaceOrTab(int ch) {
	return ch == ' ' || ch == '\t';
}

static inline bool IsValidMail(Accessor &styler, Sci_PositionU pos,
							   Sci_PositionU endPos) {
	int mailState = 0;
	for (; pos < endPos; pos++) {
		if (styler[pos] == '\r' || styler[pos] == '\n') { // end of line
			while (--pos > 0 && IsSpaceOrTab(styler[pos]));
			if (!IsUpperOrLowerCase(styler[pos]))
				mailState = 0;
			break;
		} else if (mailState == 0) {
			if (styler[pos] == '@' && pos > 0 && pos < (endPos - 1) &&
				isalnum(styler[pos - 1]) && isalnum(styler[pos + 1])) {
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

inline bool IsOperValue(const int ch) {
	return ch == '!' || ch == '#' || ch == '%' || ch == '&' ||
		   ch == '*' || ch == '+' || ch == ',' || ch == '.' ||
		   ch == ':' || ch == ';' || ch == '=' || ch == '?' ||
		   ch == '@' || ch == '^' || ch == '~' || ch == '|' ||
		   ch == '[' || ch == ']' || ch == '{' || ch == '}' ||
		   ch == '(' || ch == ')' || ch == '<' || ch == '>' ||
		   ch == '/' || ch == '\\' || ch == '$' || ch == '"' ||
		   ch == '\'';
}

static void ColourisePropsDoc(Sci_PositionU startPos, Sci_Position length, int initStyle,
							  WordList *keywordlists[], Accessor &styler) {
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
	bool isSubVar = false;
	int hexColorLen = 0;
	int numDotCnt = 0;
	
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
		
		switch (sc.state) {
			case SCE_PROPS_DEFAULT:
				if (IsASpaceOrTab(sc.ch))
					continue;
				if (sc.ch == '#' || sc.ch == '!' || sc.ch == ';') {
					sc.ChangeState(SCE_PROPS_COMMENT);
					goToLineEnd = true;
					continue;
				} else if (sc.ch == '[') {
					sc.ChangeState(SCE_PROPS_SECTION);
				} else if (sc.ch == '@') {
					sc.ChangeState(SCE_PROPS_DEFVAL);
					if (isassignchar(sc.chNext))
						sc.ForwardSetState(SCE_PROPS_ASSIGNMENT);
					sc.ForwardSetState(SCE_PROPS_VALUE);
				} else if (sc.ch == '=') {
					sc.SetState(SCE_PROPS_ASSIGNMENT);
					continue;
				} else {
					sc.ChangeState(SCE_PROPS_KEY);
				}
				break;
				
			case SCE_PROPS_SECTION:
				if (sc.ch == ']')
					sc.ForwardSetState(SCE_PROPS_DEFAULT);
				break;
				
			case SCE_PROPS_KEY:
				if (isassignchar(sc.ch)) {
					sc.SetState(SCE_PROPS_ASSIGNMENT);
					continue;
				}
				break;
		}
		
		switch (sc.state) {
			case SCE_PROPS_ASSIGNMENT:
			case SCE_PROPS_VALUE:
			case SCE_PROPS_OPER_VALUE:
				if (!IsAWordChar(sc.chPrev)) {
					if (sc.Match('#', '{')) {
						for (Sci_PositionU i = sc.currentPos + 2; i < endPos; i++) {
							if (styler[i] == '\r' || styler[i] == '\n') { // end of line
								break;
							} else if (styler[i] == '}') {
								sc.SetState(SCE_PROPS_SUBVAR_OPER);
								sc.Forward();
								sc.ForwardSetState(SCE_PROPS_VALUE);
								isSubVar = true;
								break;
							}
						}
					}
					if (sc.Match('0', 'x') &&
							IsADigit(styler.SafeGetCharAt(sc.currentPos + 2), 16)) {
						sc.SetState(SCE_PROPS_HEXNUMBER);
						sc.Forward();
						numDotCnt = 0;
						continue;
						
					} else if (sc.chPrev != '.' && sc.chPrev != '+' && sc.chPrev != '-' &&
							   (IsADigit(sc.ch) || (sc.ch == '.' && IsADigit(sc.chNext)) ||
								((sc.ch == '+' || sc.ch == '-') &&
								 (sc.chNext == '.' || IsADigit(sc.chNext))))) {
						sc.SetState(SCE_PROPS_NUMBER);
						if (sc.ch == '.') {
							numDotCnt = 1;
							maybeIpAddr = false;
						} else if (sc.ch == '+' || sc.ch == '-') {
							numDotCnt = 0;
							maybeIpAddr = false;
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
						
					} else if (sc.ch == '\"') {
						for (Sci_PositionU i = sc.currentPos + 1; i < endPos; i++) {
							if (styler[i] == '\r' || styler[i] == '\n') { // end of line
								break;
							} else if (styler[i] == '"') {
								sc.SetState(SCE_PROPS_DOUBLESTRING);
								break;
							}
						}
					} else if (sc.ch == '\'') {
						for (Sci_PositionU i = sc.currentPos + 1; i < endPos; i++) {
							if (styler[i] == '\r' || styler[i] == '\n') { // end of line
								break;
							} else if (styler[i] == '\'') {
								sc.SetState(SCE_PROPS_SINGLESTRING);
								break;
							}
						}
					} else if (IsAWordChar(sc.ch)) {
						sc.SetState(SCE_PROPS_VALUE);
						continue;
					}
				} else if (!IsAWordChar(sc.ch)) { // here also true condition: IsAWordChar(sc.chPrev)
					char s[100];
					sc.GetCurrentLowered(s, sizeof(s));
					
					// check url:
					if ((strcmp(s, "http") == 0 || strcmp(s, "https") == 0 ||
						 strcmp(s, "ftp") == 0 || strcmp(s, "sftp") == 0) &&
							sc.ch == ':' && sc.chNext == '/' &&
							styler.SafeGetCharAt(sc.currentPos + 2) == '/') {
						sc.ChangeState(SCE_PROPS_URL_VALUE);
						goToLineEnd = true;
						continue;
					}
					// check email:
					if (IsValidMail(styler, sc.currentPos, endPos)) {
						sc.ChangeState(SCE_PROPS_MAIL_VALUE);
						goToLineEnd = true;
						continue;
					}
					// find a word to defined lists:
					if (commonWords.InList(s)) {
						sc.ChangeState(SCE_PROPS_COMMONWORD);
					} else if (namedColors.InList(s)) {
						sc.ChangeState(SCE_PROPS_NAMED_COLOR);
					}
					sc.SetState(SCE_PROPS_VALUE);
				}
				break;
			
			case SCE_PROPS_VARIABLE:
				if (IsAWordChar(sc.ch))
					continue;
				sc.SetState(SCE_PROPS_VALUE);
				break;
			case SCE_PROPS_HEX_COLOR:
				if (IsADigit(sc.ch, 16)) {
					hexColorLen++;
					continue;
				}
				if ((hexColorLen != 3 && hexColorLen != 6) || IsAWordChar(sc.ch))
					sc.ChangeState(SCE_PROPS_VALUE);
				else
					sc.SetState(SCE_PROPS_VALUE);
				break;
			case SCE_PROPS_DOUBLESTRING:
				if (sc.ch == '\"')
					sc.ForwardSetState(SCE_PROPS_VALUE);
				break;
			case SCE_PROPS_SINGLESTRING:
				if (sc.ch == '\'')
					sc.ForwardSetState(SCE_PROPS_VALUE);
				break;
			case SCE_PROPS_NUMBER:
				if (IsADigit(sc.ch) || (sc.ch == '.' && IsADigit(sc.chNext))) {
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
				if (IsAWordChar(sc.ch) || sc.ch == '.' || sc.ch == '+' || sc.ch == '-') {
					sc.ChangeState(SCE_PROPS_VALUE);			// bad num
				} else {
					if (numDotCnt == 3 && maybeIpAddr) {		// fixate ip-address
						sc.ChangeState(SCE_PROPS_IP_VALUE);
						sc.SetState(SCE_PROPS_VALUE);
					} else if (numDotCnt < 2) {					// fixate decimal num
						sc.SetState(SCE_PROPS_VALUE);
					} else {									// bad num
						sc.ChangeState(SCE_PROPS_VALUE);
					}
				}
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
					}
					continue;
				}
				if (IsAWordChar(sc.ch) || sc.ch == '.') {		// bad hexnum
					sc.ChangeState(SCE_PROPS_VALUE);
				} else if (numDotCnt == 3) {					// fixate ip-address
					sc.ChangeState(SCE_PROPS_IP_VALUE);
					sc.SetState(SCE_PROPS_VALUE);
				} else if (numDotCnt == 0) {					// fixate hexnum
					sc.SetState(SCE_PROPS_VALUE);
				} else {
					sc.ChangeState(SCE_PROPS_VALUE);
				}
				break;
		}
		if (sc.state == SCE_PROPS_VALUE || sc.state == SCE_PROPS_OPER_VALUE ||
			sc.state == SCE_PROPS_VARIABLE || sc.state == SCE_PROPS_HEX_COLOR ||
			sc.state == SCE_PROPS_NUMBER || sc.state == SCE_PROPS_HEXNUMBER ||
			sc.state == SCE_PROPS_ASSIGNMENT) {
			if (IsOperValue(sc.ch)) {
				if (isSubVar && sc.ch == '}') {
					sc.SetState(SCE_PROPS_SUBVAR_OPER);
					isSubVar = false;
				} else {
					sc.SetState(SCE_PROPS_OPER_VALUE);
				}
			}
		}
		if (sc.atLineEnd) {
			if (sc.state == SCE_PROPS_HEX_COLOR)
				// incomplete typed values are changed to SCE_PROPS_VALUE
				sc.ChangeState(SCE_PROPS_VALUE);
			else if (sc.state == SCE_PROPS_SECTION ||
					 sc.state == SCE_PROPS_KEY)
				// incomplete section/key are changed to SCE_PROPS_DEFAULT
				sc.ChangeState(SCE_PROPS_DEFAULT);
		}
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
		if (!isspacechar(ch))
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
