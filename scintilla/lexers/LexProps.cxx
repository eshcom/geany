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

static void ColourisePropsLine(
	Accessor &styler,
	Sci_PositionU pos,
	Sci_PositionU endLine,
	bool allowInitialSpaces) {
	
	if (allowInitialSpaces) {
		while ((pos < endLine) && isspacechar(styler[pos]))	// Skip initial spaces
			pos++;
	} else {
		if (isspacechar(styler[pos])) // don't allow initial spaces
			pos = endLine;
	}
	
	if (pos < endLine) {
		if (styler[pos] == '#' || styler[pos] == '!' ||
			styler[pos] == ';') {
			// esh: ColourTo func colourise by style include styler[endLine - 1] char
			styler.ColourTo(endLine - 1, SCE_PROPS_COMMENT);
		} else if (styler[pos] == '[') {
			styler.ColourTo(endLine - 1, SCE_PROPS_SECTION);
		} else if (styler[pos] == '@') {
			styler.ColourTo(pos, SCE_PROPS_DEFVAL);
			if (isassignchar(styler[++pos]))
				styler.ColourTo(pos, SCE_PROPS_ASSIGNMENT);
			styler.ColourTo(endLine - 1, SCE_PROPS_DEFAULT);
		} else {
			// Search for the '=' character
			while ((pos < endLine) && !isassignchar(styler[pos]))
				pos++;
			if ((pos < endLine) && isassignchar(styler[pos])) {
				styler.ColourTo(pos - 1, SCE_PROPS_KEY);
				styler.ColourTo(pos, SCE_PROPS_ASSIGNMENT);
				styler.ColourTo(endLine - 1, SCE_PROPS_DEFAULT);
			} else {
				styler.ColourTo(endLine - 1, SCE_PROPS_DEFAULT);
			}
		}
	} else {
		styler.ColourTo(endLine - 1, SCE_PROPS_DEFAULT);
	}
}

static void ColourisePropsDoc(Sci_PositionU startPos, Sci_Position length, int, WordList *[], Accessor &styler) {
	styler.StartAt(startPos);
	styler.StartSegment(startPos);
	Sci_PositionU startLine = startPos;
	Sci_PositionU endPos = startPos + length;
	
	// property lexer.props.allow.initial.spaces
	//	For properties files, set to 0 to style all lines that start with whitespace in the default style.
	//	This is not suitable for SciTE .properties files which use indentation for flow control but
	//	can be used for RFC2822 text where indentation is used for continuation lines.
	const bool allowInitialSpaces = styler.GetPropertyInt("lexer.props.allow.initial.spaces", 1) != 0;
	
	for (Sci_PositionU i = startPos; i < endPos; i++) {
		if (AtEOL(styler, i)) {
			// End of line - colourise it
			// esh: colourise from startLine before i + 1
			ColourisePropsLine(styler, startLine, i + 1, allowInitialSpaces);
			startLine = i + 1; // esh: i + EOL
		}
	}
	if (startLine < endPos) {	// Last line does not have ending characters
		// esh: colourise from startLine before endPos
		ColourisePropsLine(styler, startLine, endPos, allowInitialSpaces);
	}
}

// adaption by ksc, using the "} else {" trick of 1.53
// 030721
static void FoldPropsDoc(Sci_PositionU startPos, Sci_Position length, int, WordList *[], Accessor &styler) {
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

static const char *const emptyWordListDesc[] = {
	0
};

LexerModule lmProps(SCLEX_PROPERTIES, ColourisePropsDoc, "props", FoldPropsDoc, emptyWordListDesc);
