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

static void ColourisePropsDoc(Sci_PositionU startPos, Sci_Position length, int initStyle,
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
	
	StyleContext sc(startPos, length, initStyle, styler);
	
	bool goToLineEnd = false;
	bool isSubVar = false;
	int hexColorLen = 0;
	
	for (; sc.More(); sc.Forward()) {
		
		if (sc.atLineStart) {
			sc.SetState(SCE_PROPS_DEFAULT);
			
			if (!allowInitialSpaces && IsASpaceOrTab(sc.ch)) {
				// don't allow initial spaces
				goToLineEnd = true;
				continue;
			}
		}
		if (sc.atLineEnd) {
			if (sc.state == SCE_PROPS_DOUBLESTRING ||
				sc.state == SCE_PROPS_SINGLESTRING ||
				sc.state == SCE_PROPS_HEX_COLOR)
				// incomplete typed values are changed to SCE_PROPS_VALUE
				sc.ChangeState(SCE_PROPS_VALUE);
			else if (sc.state == SCE_PROPS_KEY)
				// incomplete key are changed to SCE_PROPS_DEFAULT
				sc.ChangeState(SCE_PROPS_DEFAULT);
			
			goToLineEnd = false;
			continue;
		}
		if (goToLineEnd)
			continue;
		if (IsASpaceOrTab(sc.ch))
			continue;
		
		// Determine if the current state should terminate.
		switch (sc.state) {
			case SCE_PROPS_DEFAULT:
				if (sc.ch == '#' || sc.ch == '!' || sc.ch == ';') {
					sc.ChangeState(SCE_PROPS_COMMENT);
					goToLineEnd = true;
				} else if (sc.ch == '[') {
					sc.ChangeState(SCE_PROPS_SECTION);
					goToLineEnd = true;
				} else if (sc.ch == '@') {
					sc.ChangeState(SCE_PROPS_DEFVAL);
					sc.Forward();
					if (isassignchar(sc.ch)) {
						sc.SetState(SCE_PROPS_ASSIGNMENT);
						sc.Forward();
					}
					sc.SetState(SCE_PROPS_VALUE);
				} else {
					sc.ChangeState(SCE_PROPS_KEY);
				}
				continue;
			case SCE_PROPS_KEY:
				if (isassignchar(sc.ch))
					sc.SetState(SCE_PROPS_ASSIGNMENT);
				continue;
			case SCE_PROPS_ASSIGNMENT:
				sc.SetState(SCE_PROPS_VALUE);
				continue;
			case SCE_PROPS_VALUE:
				continue;
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
