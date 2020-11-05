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

static inline bool AtEOL(Accessor &styler, Sci_PositionU i) {
	return (styler[i] == '\n') ||
		   ((styler[i] == '\r') && (styler.SafeGetCharAt(i + 1) != '\n'));
}

static inline bool isassignchar(unsigned char ch) {
	return (ch == '=') || (ch == ':');
}

static inline bool IsAWordChar(const unsigned int ch) {
	return ch >= 0x80 || isalnum(ch) || ch == '-' || ch == '_';
}

inline bool IsOperValue(const int ch) {
	return ch == '!' || ch == '#' || ch == '%' || ch == '&' ||
		   ch == '*' || ch == '+' || ch == ',' || ch == '.' ||
		   ch == ':' || ch == ';' || ch == '=' || ch == '?' ||
		   ch == '@' || ch == '^' || ch == '~' || ch == '|' ||
		   ch == '[' || ch == ']' || ch == '{' || ch == '}' ||
		   ch == '(' || ch == ')' || ch == '<' || ch == '>' ||
		   ch == '/' || ch == '\\';
}

static void ColourisePropsDoc(Sci_PositionU startPos, Sci_Position length, int,
							  WordList *keywordlists[], Accessor &styler) {
	styler.StartAt(startPos);
	styler.StartSegment(startPos);
	Sci_PositionU pos = startPos;
	Sci_PositionU endPos = startPos + length;
	
	WordList &commonWords = *keywordlists[0];
	WordList &namedColors = *keywordlists[1];
	
	// property lexer.props.allow.initial.spaces
	//	For properties files, set to 0 to style all lines that start with whitespace in the default style.
	//	This is not suitable for SciTE .properties files which use indentation for flow control but
	//	can be used for RFC2822 text where indentation is used for continuation lines.
	const bool allowInitialSpaces = styler.GetPropertyInt("lexer.props.allow.initial.spaces", 1) != 0;
	
	int state = SCE_PROPS_DEFAULT;
	bool beginLine = true;
	bool goToEndLine = false;
	bool isSubVar = false;
	int ch, chNext, hexColorLen;
	Sci_PositionU wordPos;
	
	while (pos < endPos) {
		
		if (beginLine) {
			beginLine = false;
			if (allowInitialSpaces) {
				// Skip initial spaces
				while ((pos < endPos) && isspacechar(styler[pos]))
					pos++;
				if (pos == endPos) break;
			} else if (isspacechar(styler[pos])) {
				// don't allow initial spaces
				goToEndLine = true;
			}
		}
		
		if (AtEOL(styler, pos)) {
			// End of line - colourise it
			if (state == SCE_PROPS_DOUBLESTRING ||
				state == SCE_PROPS_SINGLESTRING ||
				state == SCE_PROPS_HEX_COLOR)
				state = SCE_PROPS_VALUE;
			styler.ColourTo(pos++, state);
			state = SCE_PROPS_DEFAULT;
			beginLine = true;
			goToEndLine = false;
			continue;
		} else if (goToEndLine) {
			pos++;
			continue;
		}
		
		switch (state) {
			case SCE_PROPS_DEFAULT:
				if (styler[pos] == '#' || styler[pos] == '!' || styler[pos] == ';') {
					state = SCE_PROPS_COMMENT;
					goToEndLine = true;
					pos++;
				} else if (styler[pos] == '[') {
					state = SCE_PROPS_SECTION;
					goToEndLine = true;
					pos++;
				} else if (styler[pos] == '@') {
					styler.ColourTo(pos++, SCE_PROPS_DEFVAL);
					if ((pos < endPos) && isassignchar(styler[pos]))
						styler.ColourTo(pos++, SCE_PROPS_ASSIGNMENT);
					state = SCE_PROPS_VALUE;
				} else {
					// Search for the '=' character
					while ((pos < endPos) && !isassignchar(styler[pos]))
						pos++;
					if ((pos < endPos) && isassignchar(styler[pos])) {
						styler.ColourTo(pos - 1, SCE_PROPS_KEY);
						styler.ColourTo(pos++, SCE_PROPS_ASSIGNMENT);
						state = SCE_PROPS_VALUE;
					}
				}
				break;
				
			case SCE_PROPS_VALUE:
				while ((pos < endPos) && IsASpaceOrTab(styler[pos]))
					pos++;
				if (pos == endPos) break;
				
				ch = styler[pos];
				chNext = styler.SafeGetCharAt(pos + 1);
				
				if (ch == '$' && IsUpperOrLowerCase(chNext)) {
					state = SCE_PROPS_VARIABLE;
					pos++;
				//~ } else if (ch == '#' && chNext == '{') {
					//~ styler.ColourTo(++pos, SCE_PROPS_SUBVAR_OPER);
					//~ isSubVar = true;
					//~ pos++;
				//~ } else if (ch == '#' && IsADigit(chNext, 16)) {
					//~ state = SCE_PROPS_HEX_COLOR;
					//~ hexColorLen = 0;
					//~ pos++;
				//~ } else if (ch == '\"') {
					//~ state = SCE_PROPS_DOUBLESTRING;
					//~ pos++;
				//~ } else if (ch == '\'') {
					//~ state = SCE_PROPS_SINGLESTRING;
					//~ pos++;
				//~ } else if (IsOperValue(ch)) {
					//~ if (isSubVar && ch == '}') {
						//~ isSubVar = false;
						//~ styler.ColourTo(pos++, SCE_PROPS_SUBVAR_OPER);
					//~ } else {
						//~ styler.ColourTo(pos++, SCE_PROPS_OPER_VALUE);
					//~ }
				//~ } else if (IsAWordChar(ch)) {
					//~ state = SCE_PROPS_COMMONWORD;
					//~ wordPos = pos++;
				} else {
					goToEndLine = true;
				}
				break;
				
			case SCE_PROPS_VARIABLE:
				if (IsAWordChar(styler[pos])) {
					pos++;
					continue;
				}
				styler.ColourTo(pos - 1, state);
				state = SCE_PROPS_VALUE;
				break;
			case SCE_PROPS_HEX_COLOR:
				pos++;
				break;
			case SCE_PROPS_DOUBLESTRING:
				pos++;
				break;
			case SCE_PROPS_SINGLESTRING:
				pos++;
				break;
			case SCE_PROPS_SUBVAR_OPER:
				pos++;
				break;
			case SCE_PROPS_NUMBER:
				pos++;
				break;
			case SCE_PROPS_HEXNUMBER:
				pos++;
				break;
			case SCE_PROPS_URL_VALUE:
				pos++;
				break;
			case SCE_PROPS_MAIL_VALUE:
				pos++;
				break;
			case SCE_PROPS_OPER_VALUE:
				pos++;
				break;
			case SCE_PROPS_COMMONWORD:
				if (IsAWordChar(ch)) {
					pos++;
					continue;
				}
				char s[100];
				Sci_PositionU i = 0;
				while (wordPos < pos && i < (sizeof(s) - 1))
					s[i++] = MakeLowerCase(styler[wordPos++]);
				s[i] = '\0';
				
				if (commonWords.InList(s)) {
					styler.ColourTo(pos - 1, SCE_PROPS_COMMONWORD);
				} else if (namedColors.InList(s)) {
					styler.ColourTo(pos - 1, SCE_PROPS_NAMED_COLOR);
				} else {
					state = SCE_PROPS_VALUE;
				}
				break;
		}
	}
	if (endPos > 0) {
		if (state == SCE_PROPS_DOUBLESTRING ||
			state == SCE_PROPS_SINGLESTRING ||
			state == SCE_PROPS_HEX_COLOR)
			state = SCE_PROPS_VALUE;
		styler.ColourTo(endPos - 1, state);
	}
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
