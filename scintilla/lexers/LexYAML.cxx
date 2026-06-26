// Scintilla source code edit control
/** @file LexYAML.cxx
 ** Lexer for YAML.
 **/
// Copyright 2003- by Sean O'Dell <sean@celsoft.com>
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

static const char *const yamlWordListDesc[] = {
	"Common keywords (eg. true/false/null)",
	"Type keywords (eg. boolean/integer/string)",
	0
};

static inline bool AtEOL(Accessor &styler, Sci_PositionU i) {
	return IsEOL(styler[i], styler.SafeGetCharAt(i + 1));
}

static inline char GetClosingChar(int style) {
	return (style == SCE_YAML_DOUBLESTRING) ? "\"" : "\'"
}

static bool KeywordAtChar(char* lineBuffer, char* startComment,
						  const WordList &keywords) {
	if (lineBuffer == NULL || startComment <= lineBuffer)
		return false;
	char* endValue = startComment - 1;
	while (endValue >= lineBuffer && *endValue == ' ')
		endValue--;
	Sci_PositionU len = static_cast<Sci_PositionU>(endValue - lineBuffer) + 1;
	char s[100];
	if (len > (sizeof(s) / sizeof(s[0]) - 1))
		return false;
	strncpy(s, lineBuffer, len);
	s[len] = '\0';
	return (keywords.InList(s));
}

#define YAML_STATE_BITSIZE		16
#define YAML_STATE_MASK			(0xFFFF0000)
#define YAML_STATE_DOCUMENT		(1 << YAML_STATE_BITSIZE)
#define YAML_STATE_VALUE		(2 << YAML_STATE_BITSIZE)
#define YAML_STATE_COMMENT		(3 << YAML_STATE_BITSIZE)
#define YAML_STATE_TEXT_PARENT	(4 << YAML_STATE_BITSIZE)
#define YAML_STATE_TEXT			(5 << YAML_STATE_BITSIZE)

static void ColouriseYAMLLine(
	char *lineBuffer,
	Sci_PositionU currentLine,
	Sci_PositionU lengthLine,
	Sci_PositionU startLine,
	Sci_PositionU endPos,
	WordList *keywordLists[],
	Accessor &styler) {
	
	WordList &com_words = *keywordLists[0];
	WordList &type_words = *keywordLists[1];
	
	Sci_PositionU i = 0;
	bool bInQuotes = false;
	unsigned int indentAmount = SpaceCount(lineBuffer);
	
	if (currentLine > 0) {
		int parentLineState = styler.GetLineState(currentLine - 1);
		
		if ((parentLineState&YAML_STATE_MASK) == YAML_STATE_TEXT ||
			(parentLineState&YAML_STATE_MASK) == YAML_STATE_TEXT_PARENT) {
			unsigned int parentIndentAmount = parentLineState&(~YAML_STATE_MASK);
			if (indentAmount > parentIndentAmount) {
				styler.SetLineState(currentLine, YAML_STATE_TEXT | parentIndentAmount);
				styler.ColourTo(endPos, SCE_YAML_TEXT);
				return;
			}
		}
	}
	styler.SetLineState(currentLine, 0);
	if (strncmp(lineBuffer, "---", 3) == 0 || strncmp(lineBuffer, "...", 3) == 0) {	// Document marker
		styler.SetLineState(currentLine, YAML_STATE_DOCUMENT);
		styler.ColourTo(endPos, SCE_YAML_DOCUMENT);
		return;
	}
	// Skip initial spaces
	while ((i < lengthLine) && lineBuffer[i] == ' ') { // YAML always uses space, never TABS or anything else
		i++;
	}
	if (lineBuffer[i] == '\t') { // if we skipped all spaces, and we are NOT inside a text block, this is wrong
		styler.ColourTo(endPos, SCE_YAML_ERROR);
		return;
	}
	if (lineBuffer[i] == '#') {	// Comment
		styler.SetLineState(currentLine, YAML_STATE_COMMENT);
		styler.ColourTo(endPos, SCE_YAML_COMMENT);
		return;
	}
	while (i < lengthLine) {
		if (IsQuote(lineBuffer[i])) {
			bInQuotes = !bInQuotes;
		} else if (lineBuffer[i] == '#' && IsSpace(lineBuffer[i - 1]) && !bInQuotes) {
			styler.ColourTo(startLine + i - 1, SCE_YAML_DEFAULT);
			styler.ColourTo(endPos, SCE_YAML_COMMENT);
			return;
		} else if (lineBuffer[i] == ':' && !bInQuotes) {
			styler.ColourTo(startLine + i - 1, SCE_YAML_IDENTIFIER);
			styler.ColourTo(startLine + i, SCE_YAML_OPERATOR);
			// Non-folding scalar
			i++;
			while ((i < lengthLine) && IsSpace(lineBuffer[i]))
				i++;
			Sci_PositionU endValue = lengthLine - 1;
			while ((endValue >= i) && IsSpace(lineBuffer[endValue]))
				endValue--;
			lineBuffer[endValue + 1] = '\0';
			if (lineBuffer[i] == '|' || lineBuffer[i] == '>') {
				i++;
				if (IsSign(lineBuffer[i]))
					i++;
				while ((i < lengthLine) && IsSpace(lineBuffer[i]))
					i++;
				if (lineBuffer[i] == '\0') {
					styler.SetLineState(currentLine, YAML_STATE_TEXT_PARENT | indentAmount);
					styler.ColourTo(endPos, SCE_YAML_DEFAULT);
					return;
				} else if (lineBuffer[i] == '#') {
					styler.SetLineState(currentLine, YAML_STATE_TEXT_PARENT | indentAmount);
					styler.ColourTo(startLine + i - 1, SCE_YAML_DEFAULT);
					styler.ColourTo(endPos, SCE_YAML_COMMENT);
					return;
				} else {
					styler.ColourTo(endPos, SCE_YAML_ERROR);
					return;
				}
			} else if (lineBuffer[i] == '#') {
				styler.ColourTo(startLine + i - 1, SCE_YAML_DEFAULT);
				styler.ColourTo(endPos, SCE_YAML_COMMENT);
				return;
			}
			Sci_PositionU startComment = i;
			bInQuotes = false;
			while (startComment < lengthLine) { // Comment must be space padded
				if (IsQuote(lineBuffer[startComment]))
					bInQuotes = !bInQuotes;
				if (lineBuffer[startComment] == '#' &&
						IsSpace(lineBuffer[startComment - 1]) && !bInQuotes)
					break;
				startComment++;
			}
			styler.SetLineState(currentLine, YAML_STATE_VALUE);
			if (lineBuffer[i] == '&' || lineBuffer[i] == '*') {
				styler.ColourTo(startLine + startComment - 1, SCE_YAML_REFERENCE);
				if (startComment < lengthLine)
					styler.ColourTo(endPos, SCE_YAML_COMMENT);
				return;
			}
			if (KeywordAtChar(&lineBuffer[i], &lineBuffer[startComment], com_words)) { // Convertible value (true/false, etc.)
				styler.ColourTo(startLine + startComment - 1, SCE_YAML_COM_WORD);
				if (startComment < lengthLine)
					styler.ColourTo(endPos, SCE_YAML_COMMENT);
				return;
			} else if (KeywordAtChar(&lineBuffer[i], &lineBuffer[startComment], type_words)) {
				styler.ColourTo(startLine + startComment - 1, SCE_YAML_TYPE_WORD);
				if (startComment < lengthLine)
					styler.ColourTo(endPos, SCE_YAML_COMMENT);
				return;
			}
			Sci_PositionU i2 = i;
			while ((i < startComment) && lineBuffer[i]) {
				if (!IsDigit(lineBuffer[i])
					&& lineBuffer[i] != '-' && lineBuffer[i] != '.'
					&& lineBuffer[i] != ',' && lineBuffer[i] != ' ') {
					styler.ColourTo(startLine + startComment - 1, SCE_YAML_DEFAULT);
					if (startComment < lengthLine)
						styler.ColourTo(endPos, SCE_YAML_COMMENT);
					return;
				}
				i++;
			}
			if (i > i2) {
				styler.ColourTo(startLine + startComment - 1, SCE_YAML_NUMBER);
				if (startComment < lengthLine)
					styler.ColourTo(endPos, SCE_YAML_COMMENT);
				return;
			}
			break; // shouldn't get here, but just in case, the rest of the line is coloured the default
		}
		i++;
	}
	styler.ColourTo(endPos, SCE_YAML_DEFAULT);
}

static void ColouriseYAMLDoc2(Sci_PositionU startPos, Sci_Position length,
							 int, WordList *keywordLists[], Accessor &styler) {
	// esh: escapesequence highlighting
	const bool escapeSequence =
					styler.GetPropertyInt("lexer.yaml.escape.sequence", 0) != 0;
	EscapeSequence escapeSeq = EscapeSequence();
	
	Sci_PositionU endPos = startPos + length;
	StyleContext sc(startPos, length, initStyle, styler);
	
	WordList &com_words = *keywordLists[0];
	WordList &type_words = *keywordLists[1];
	
	Sci_PositionU lineCurrent = styler.GetLine(startPos);
	
	bool bInQuotes = false;
	unsigned int indentAmount = 0;
	
	for (; sc.More(); sc.Forward()) {
		if (sc.atLineStart) {
			indentAmount = 0;
			
			while (sc.More() && !sc.atLineEnd) {
				if (sc.ch == ' ') {
					// YAML always uses space, never TABS or anything else
					indentAmount++;
					sc.Forward();
					continue;
				} else if (sc.ch == '\t') {
					// if we skipped all spaces, and we are NOT inside a text block, this is wrong
					sc.SetState(SCE_YAML_ERROR);
				}
				break;
			}
			
			if (lineCurrent > 0) {
				int parentLineState = styler.GetLineState(lineCurrent - 1);
				
				if ((parentLineState&YAML_STATE_MASK) == YAML_STATE_TEXT ||
					(parentLineState&YAML_STATE_MASK) == YAML_STATE_TEXT_PARENT) {
					unsigned int parentIndentAmount = parentLineState&(~YAML_STATE_MASK);
					if (indentAmount > parentIndentAmount) {
						styler.SetLineState(lineCurrent, YAML_STATE_TEXT | parentIndentAmount);
						sc.SetState(SCE_YAML_TEXT);
						
						if (sc.atLineEnd) lineCurrent++;
						continue;
					}
				}
			}
			styler.SetLineState(lineCurrent, 0);
		}
		
		if (sc.atLineEnd) {
			lineCurrent++;
			if (sc.state != SCE_YAML_DEFAULT)
				sc.SetState(SCE_YAML_DEFAULT);
		}
		
		// Determine if the current state should terminate.
		switch (sc.state) {
			case SCE_YAML_DOCUMENT:
				if (!IsSpaceOrTab(sc.ch)) {
					sc.ChangeState(SCE_YAML_ERROR);
				}
				break;
				
			case SCE_YAML_DOUBLESTRING:
			case SCE_YAML_SINGLESTRING:
				if (sc.ch == '\\') {
					sc.Forward(); // Skip any character after the backslash
					
					if (sc.atLineEnd) {
						sc.ChangeState(SCE_YAML_ERROR);
						lineCurrent++;
					}
					continue;
				} else if (sc.ch == GetClosingChar(sc.state)) {
					sc.ForwardSetState(SCE_YAML_DEFAULT);
				}
				break;
		}
		
		// Determine if a new state should be entered.
		if (sc.state == SCE_YAML_DEFAULT) {
			if (sc.atLineStart && (sc.Match("---") || sc.Match("..."))) { // Document marker
				styler.SetLineState(lineCurrent, YAML_STATE_DOCUMENT);
				sc.SetState(SCE_YAML_DOCUMENT);
				sc.Forward(2);
			} else if (sc.ch == '#' && IsSpace(sc.chPrev)) {
				styler.SetLineState(lineCurrent, YAML_STATE_COMMENT);
				sc.SetState(SCE_YAML_COMMENT);
			} else if (sc.ch == '\"') {
				sc.SetState(SCE_YAML_DOUBLESTRING);
			} else if (sc.ch == '\'') {
				sc.SetState(SCE_YAML_SINGLESTRING);
			}
		}
	}
	sc.Complete();
}

static void ColouriseYAMLDoc(Sci_PositionU startPos, Sci_Position length,
							 int, WordList *keywordLists[], Accessor &styler) {
	char lineBuffer[4096] = "";
	styler.StartAt(startPos);
	styler.StartSegment(startPos);
	Sci_PositionU linePos = 0;
	Sci_PositionU startLine = startPos;
	Sci_PositionU endPos = startPos + length;
	Sci_PositionU maxPos = styler.Length();
	Sci_PositionU lineCurrent = styler.GetLine(startPos);
	
	for (Sci_PositionU i = startPos; i < maxPos && i < endPos; i++) {
		lineBuffer[linePos++] = styler[i];
		if (AtEOL(styler, i) || (linePos >= sizeof(lineBuffer) - 1)) {
			// End of line (or of line buffer) met, colourise it
			lineBuffer[linePos] = '\0';
			ColouriseYAMLLine(lineBuffer, lineCurrent, linePos, startLine,
							  i, keywordLists, styler);
			linePos = 0;
			startLine = i + 1;
			lineCurrent++;
		}
	}
	if (linePos > 0) {	// Last line does not have ending characters
		ColouriseYAMLLine(lineBuffer, lineCurrent, linePos, startLine,
						  startPos + length - 1, keywordLists, styler);
	}
}

static bool IsCommentLine(Sci_Position line, Accessor &styler) {
	Sci_Position pos = styler.LineStart(line);
	if (styler[pos] == '#')
		return true;
	return false;
}

static void FoldYAMLDoc(Sci_PositionU startPos, Sci_Position length,
						int /*initStyle - unused*/, WordList *[],
						Accessor &styler) {
	const Sci_Position maxPos = startPos + length;
	const Sci_Position maxLines = styler.GetLine(maxPos - 1);             // Requested last line
	const Sci_Position docLines = styler.GetLine(styler.Length() - 1);  // Available last line
	const bool foldComment = styler.GetPropertyInt("fold.comment.yaml") != 0;
	
	// Backtrack to previous non-blank line so we can determine indent level
	// for any white space lines
	// and so we can fix any preceding fold level (which is why we go back
	// at least one line in all cases)
	int spaceFlags = 0;
	Sci_Position lineCurrent = styler.GetLine(startPos);
	int indentCurrent = styler.IndentAmount(lineCurrent, &spaceFlags, NULL);
	while (lineCurrent > 0) {
		lineCurrent--;
		indentCurrent = styler.IndentAmount(lineCurrent, &spaceFlags, NULL);
		if (!(indentCurrent & SC_FOLDLEVELWHITEFLAG) &&
				(!IsCommentLine(lineCurrent, styler)))
			break;
	}
	int indentCurrentLevel = indentCurrent & SC_FOLDLEVELNUMBERMASK;
	
	// Set up initial loop state
	int prevComment = 0;
	if (lineCurrent >= 1)
		prevComment = foldComment && IsCommentLine(lineCurrent - 1, styler);
	
	// Process all characters to end of requested range
	// or comment that hangs over the end of the range.  Cap processing in all cases
	// to end of document (in case of unclosed comment at end).
	while ((lineCurrent <= docLines) &&
		   ((lineCurrent <= maxLines) || prevComment)) {
		// Gather info
		int lev = indentCurrent;
		Sci_Position lineNext = lineCurrent + 1;
		int indentNext = indentCurrent;
		if (lineNext <= docLines) {
			// Information about next line is only available if not at end of document
			indentNext = styler.IndentAmount(lineNext, &spaceFlags, NULL);
		}
		const int comment = foldComment && IsCommentLine(lineCurrent, styler);
		const int comment_start = (comment && !prevComment &&
								   (lineNext <= docLines) &&
								   IsCommentLine(lineNext, styler) &&
								   (lev > SC_FOLDLEVELBASE));
		const int comment_continue = (comment && prevComment);
		if (!comment)
			indentCurrentLevel = indentCurrent & SC_FOLDLEVELNUMBERMASK;
		if (indentNext & SC_FOLDLEVELWHITEFLAG)
			indentNext = SC_FOLDLEVELWHITEFLAG | indentCurrentLevel;
		
		if (comment_start) {
			// Place fold point at start of a block of comments
			lev |= SC_FOLDLEVELHEADERFLAG;
		} else if (comment_continue) {
			// Add level to rest of lines in the block
			lev = lev + 1;
		}
		
		// Skip past any blank lines for next indent level info; we skip also
		// comments (all comments, not just those starting in column 0)
		// which effectively folds them into surrounding code rather
		// than screwing up folding.
		
		while ((lineNext < docLines) &&
				((indentNext & SC_FOLDLEVELWHITEFLAG) ||
				 (lineNext <= docLines && IsCommentLine(lineNext, styler)))) {
			
			lineNext++;
			indentNext = styler.IndentAmount(lineNext, &spaceFlags, NULL);
		}
		
		const int levelAfterComments = indentNext & SC_FOLDLEVELNUMBERMASK;
		const int levelBeforeComments = Maximum(indentCurrentLevel,
												levelAfterComments);
		
		// Now set all the indent levels on the lines we skipped
		// Do this from end to start.  Once we encounter one line
		// which is indented more than the line after the end of
		// the comment-block, use the level of the block before
		
		Sci_Position skipLine = lineNext;
		int skipLevel = levelAfterComments;
		
		while (--skipLine > lineCurrent) {
			int skipLineIndent = styler.IndentAmount(skipLine, &spaceFlags, NULL);
			
			if ((skipLineIndent & SC_FOLDLEVELNUMBERMASK) > levelAfterComments)
				skipLevel = levelBeforeComments;
			
			int whiteFlag = skipLineIndent & SC_FOLDLEVELWHITEFLAG;
			
			styler.SetLevel(skipLine, skipLevel | whiteFlag);
		}
		
		// Set fold header on non-comment line
		if (!comment && !(indentCurrent & SC_FOLDLEVELWHITEFLAG)) {
			if ((indentCurrent & SC_FOLDLEVELNUMBERMASK) <
				(indentNext & SC_FOLDLEVELNUMBERMASK))
				lev |= SC_FOLDLEVELHEADERFLAG;
		}
		
		// Keep track of block comment state of previous line
		prevComment = comment_start || comment_continue;
		
		// Set fold level for this line and move to next line
		styler.SetLevel(lineCurrent, lev);
		indentCurrent = indentNext;
		lineCurrent = lineNext;
	}
	
	// NOTE: Cannot set level of last line here because indentCurrent doesn't have
	// header flag set; the loop above is crafted to take care of this case!
	//styler.SetLevel(lineCurrent, indentCurrent);
}

LexerModule lmYAML(SCLEX_YAML, ColouriseYAMLDoc, "yaml",
				   FoldYAMLDoc, yamlWordListDesc);
