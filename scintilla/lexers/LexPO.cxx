// Scintilla source code edit control
/** @file LexPO.cxx
 ** Lexer for GetText Translation (PO) files.
 **/
// Copyright 2012 by Colomban Wendling <ban@herbesfolles.org>
// The License.txt file describes the conditions under which this software may be distributed.

// see https://www.gnu.org/software/gettext/manual/gettext.html#PO-Files for the syntax reference
// some details are taken from the GNU msgfmt behavior (like that indent is allows in front of lines)

// TODO:
// * style for previous untranslated string? ("#|" comment)

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

static inline bool IsStringStyle(int style) {
	return (style == SCE_PO_MSGID_TEXT ||
			style == SCE_PO_MSGSTR_TEXT ||
			style == SCE_PO_MSGCTXT_TEXT);
}

static inline bool IsNestedStringStyle(int style) {
	return (style == SCE_PO_ESCAPESEQ ||
			style == SCE_PO_FORMATSEQ);
}

#define CHECK_TEXT_EOL												\
	if (sc.atLineEnd) { /* invalid inside a string */				\
		if (stringState == SCE_PO_MSGCTXT_TEXT)						\
			sc.ChangeState(SCE_PO_MSGCTXT_TEXT_EOL);				\
		else if (stringState == SCE_PO_MSGID_TEXT)					\
			sc.ChangeState(SCE_PO_MSGID_TEXT_EOL);					\
		else if (stringState == SCE_PO_MSGSTR_TEXT)					\
			sc.ChangeState(SCE_PO_MSGSTR_TEXT_EOL);					\
		sc.SetState(SCE_PO_DEFAULT);

#define CHECK_FORMAT_SEQUENCE										\
	} else if (sc.ch == '%') {										\
		if (formatSequence) {										\
			sc.SetState(SCE_PO_FORMATSEQ);							\
			formatSeq.initFormatState();							\
		}															\
		continue;

#define CHECK_ESCAPE_SEQUENCE										\
	} else if (sc.ch == '\\') {										\
		if (escapeSequence) {										\
			sc.SetState(SCE_PO_ESCAPESEQ);							\
			escapeSeq.initEscapeState(sc.chNext);					\
		}															\
		sc.Forward(); /* Skip any character after the backslash */	\
		continue;

#define PROCESS_END_SEQUENCE										\
	} else {														\
		sc.SetState(stringState);									\
		if (sc.ch == '\"')											\
			sc.ForwardSetState(SCE_PO_DEFAULT);						\
	}


static void ColourisePODoc(Sci_PositionU startPos, Sci_Position length,
						   int initStyle, WordList *keywordlists[],
						   Accessor &styler) {
	// esh: escapesequence highlighting
	const bool escapeSequence =
					styler.GetPropertyInt("lexer.po.escape.sequence", 0) != 0;
	EscapeSequence escapeSeq = EscapeSequence();
	
	// esh: formatsequence highlighting
	const bool formatSequence =
					styler.GetPropertyInt("lexer.po.format.sequence", 0) != 0;
	FormatSequence formatSeq = FormatSequence();
	
	WordList &flagWords = *keywordlists[0];
	WordList &taskMarkers = *keywordlists[1];
	
	StyleContext sc(startPos, length, initStyle, styler);
	Sci_Position curLine = styler.GetLine(startPos);
	// the line state holds the last state on or before the line that isn't the default style
	int curLineState = curLine > 0 ? styler.GetLineState(curLine - 1) :
									 SCE_PO_DEFAULT;
	char ident[100];
	
	// esh: added stringState for escape/format sequences highlighting
	int stringState = -1;
	
	// esh: define stringState
	if (IsStringStyle(initStyle)) {
		stringState = initStyle;
	} else if (initStyle == SCE_PO_MSGID_TEXT_EOL) {
		stringState = SCE_PO_MSGID_TEXT;
	} else if (initStyle == SCE_PO_MSGSTR_TEXT_EOL) {
		stringState = SCE_PO_MSGSTR_TEXT;
	} else if (initStyle == SCE_PO_MSGCTXT_TEXT_EOL) {
		stringState = SCE_PO_MSGCTXT_TEXT;
	} else if (IsNestedStringStyle(initStyle)) {
		Sci_Position back = startPos;
		int backStyle;
		while (--back) {
			backStyle = styler.StyleAt(back);
			if (IsNestedStringStyle(backStyle)) {
				continue;
			} else if (IsStringStyle(backStyle)) {
				stringState = backStyle;
			}
			break;
		}
	}
	
	for (; sc.More(); sc.Forward()) {
		// whether we should leave a state
		switch (sc.state) {
			case SCE_PO_COMMENT:
			case SCE_PO_PROGRAMMER_COMMENT:
				HighlightTaskMarker(sc, styler, taskMarkers, true,
									SCE_PO_TASKMARKER);
				if (sc.atLineEnd)
					sc.SetState(SCE_PO_DEFAULT);
				break;
				
			case SCE_PO_REF_LINE:
				if (sc.atLineEnd)
					sc.SetState(SCE_PO_DEFAULT);
				break;
				
			case SCE_PO_FLAG_LINE:
				if (sc.atLineEnd)
					sc.SetState(SCE_PO_DEFAULT);
				else if (IsLower(sc.ch)) {
					sc.SetState(SCE_PO_FLAG_WORD);
				}
				break;
				
			case SCE_PO_FLAG_WORD:
				if (IsLower(sc.ch) || sc.ch == '-')
					continue;
				sc.GetCurrent(ident, sizeof(ident));
				
				if (strcmp(ident, "fuzzy") == 0) {
					sc.ChangeState(SCE_PO_FUZZY_WORD);
					sc.SetState(SCE_PO_FLAG_LINE);
				} else if (flagWords.InList(ident)) {
					sc.SetState(SCE_PO_FLAG_LINE);
				} else {
					sc.ChangeState(SCE_PO_FLAG_LINE);
				}
				if (sc.atLineEnd)
					sc.SetState(SCE_PO_DEFAULT);
				break;
				
			case SCE_PO_MSGID:
			case SCE_PO_MSGSTR:
			case SCE_PO_MSGCTXT:
				if (IsSpace(sc.ch))
					sc.SetState(SCE_PO_DEFAULT);
				break;
				
			case SCE_PO_ERROR:
				if (sc.atLineEnd)
					sc.SetState(SCE_PO_DEFAULT);
				break;
				
			case SCE_PO_MSGID_TEXT:
			case SCE_PO_MSGSTR_TEXT:
			case SCE_PO_MSGCTXT_TEXT:
				CHECK_TEXT_EOL
				CHECK_ESCAPE_SEQUENCE
				CHECK_FORMAT_SEQUENCE
				} else if (sc.ch == '\"') {
					sc.ForwardSetState(SCE_PO_DEFAULT);
				}
				break;
				
			case SCE_PO_ESCAPESEQ:
				escapeSeq.digitsLeft--;
				CHECK_TEXT_EOL
				} else if (!escapeSeq.atEscapeEnd(sc.ch)) {
					continue; // esh: continue of escape chars
				} else if (sc.ch == '\\') {
					escapeSeq.initEscapeState(sc.chNext);
					sc.Forward();
					continue;
				CHECK_FORMAT_SEQUENCE
				PROCESS_END_SEQUENCE
				break;
				
			case SCE_PO_FORMATSEQ:
				CHECK_TEXT_EOL
				} else if (!formatSeq.atFormatEnd(sc.ch)) {
					continue; // esh: continue of format chars
				} else if (formatSeq.atFormatNone()) {
					sc.ChangeState(stringState);
				}
				if (sc.ch == '%') {
					sc.SetState(SCE_PO_FORMATSEQ);
					formatSeq.initFormatState();
					continue;
				CHECK_ESCAPE_SEQUENCE
				PROCESS_END_SEQUENCE
				break;
		}
		
		// whether we should enter a new state
		if (sc.state == SCE_PO_DEFAULT) {
			// forward to the first non-white character on the line
			bool atLineStart = sc.atLineStart;
			if (atLineStart) {
				// reset line state if it is set to comment state so empty lines don't get
				// comment line state, and the folding code folds comments separately,
				// and anyway the styling don't use line state for comments
				if (curLineState == SCE_PO_COMMENT)
					curLineState = SCE_PO_DEFAULT;
				
				while (sc.More() && !sc.atLineEnd && IsSpace(sc.ch))
					sc.Forward();
			}
			
			if (atLineStart && sc.ch == '#') {
				if (sc.chNext == '.') {
					sc.SetState(SCE_PO_PROGRAMMER_COMMENT);
					sc.Forward();
				} else if (sc.chNext == ':') {
					sc.SetState(SCE_PO_REF_LINE);
					sc.Forward();
				} else if (sc.chNext == ',') {
					sc.SetState(SCE_PO_FLAG_LINE);
					sc.Forward();
				} else {
					sc.SetState(SCE_PO_COMMENT);
				}
			} else if (atLineStart && sc.Match("msgid")) { // includes msgid_plural
				sc.SetState(SCE_PO_MSGID);
			} else if (atLineStart && sc.Match("msgstr")) { // includes [] suffixes
				sc.SetState(SCE_PO_MSGSTR);
			} else if (atLineStart && sc.Match("msgctxt")) {
				sc.SetState(SCE_PO_MSGCTXT);
			} else if (sc.ch == '"') {
				if (curLineState == SCE_PO_MSGCTXT ||
						 curLineState == SCE_PO_MSGCTXT_TEXT)
					sc.SetState(SCE_PO_MSGCTXT_TEXT);
				else if (curLineState == SCE_PO_MSGID ||
						 curLineState == SCE_PO_MSGID_TEXT)
					sc.SetState(SCE_PO_MSGID_TEXT);
				else if (curLineState == SCE_PO_MSGSTR ||
						 curLineState == SCE_PO_MSGSTR_TEXT)
					sc.SetState(SCE_PO_MSGSTR_TEXT);
				else
					sc.SetState(SCE_PO_ERROR);
				
				if (sc.state != SCE_PO_ERROR)
					stringState = sc.state;
			} else if (!IsSpace(sc.ch))
				sc.SetState(SCE_PO_ERROR);
			
			if (sc.state != SCE_PO_DEFAULT)
				curLineState = sc.state;
		}
		
		if (sc.atLineEnd) {
			// Update the line state, so it can be seen by next line
			curLine = styler.GetLine(sc.currentPos);
			styler.SetLineState(curLine, curLineState);
		}
	}
	sc.Complete();
}

static int FindNextNonEmptyLineState(Sci_PositionU startPos, Accessor &styler) {
	Sci_PositionU length = styler.Length();
	for (Sci_PositionU i = startPos; i < length; i++) {
		if (!IsSpace(styler[i])) {
			return styler.GetLineState(styler.GetLine(i));
		}
	}
	return 0;
}

static void FoldPODoc(Sci_PositionU startPos, Sci_Position length,
					  int, WordList *[], Accessor &styler) {
	if (!styler.GetPropertyInt("fold")) return;
	
	bool foldCompact = styler.GetPropertyInt("fold.compact") != 0;
	bool foldComment = styler.GetPropertyInt("fold.comment") != 0;
	
	Sci_PositionU endPos = startPos + length;
	Sci_Position curLine = styler.GetLine(startPos);
	int lineState = styler.GetLineState(curLine);
	int nextLineState;
	int level = styler.LevelAt(curLine) & SC_FOLDLEVELNUMBERMASK;
	int nextLevel;
	int visible = 0;
	int chNext = styler[startPos];
	
	for (Sci_PositionU i = startPos; i < endPos; i++) {
		int ch = chNext;
		chNext = styler.SafeGetCharAt(i+1);
		
		if (!IsSpace(ch)) {
			visible++;
		} else if (IsEOL(ch, chNext) || (i + 1) >= endPos) {
			int lvl = level;
			Sci_Position nextLine = curLine + 1;
			
			nextLineState = styler.GetLineState(nextLine);
			if ((lineState != SCE_PO_COMMENT || foldComment) &&
					nextLineState == lineState &&
					FindNextNonEmptyLineState(i, styler) == lineState)
				nextLevel = SC_FOLDLEVELBASE + 1;
			else
				nextLevel = SC_FOLDLEVELBASE;
			
			if (nextLevel > level)
				lvl |= SC_FOLDLEVELHEADERFLAG;
			if (visible == 0 && foldCompact)
				lvl |= SC_FOLDLEVELWHITEFLAG;
			
			styler.SetLevel(curLine, lvl);
			
			lineState = nextLineState;
			curLine = nextLine;
			level = nextLevel;
			visible = 0;
		}
	}
}

static const char *const poWordListDesc[] = {
	0
};

LexerModule lmPO(SCLEX_PO, ColourisePODoc, "po", FoldPODoc, poWordListDesc);
