#ifndef LEXERCOMMON_H
#define LEXERCOMMON_H

namespace Scintilla {

template <typename SC, typename LA, typename WL>
void HighlightTaskMarker(SC &sc, LA &styler, const WL &markers,
						 bool caseSensitive, int markerState) {
	if ((IsSpace(sc.chPrev) || IsOperator(sc.chPrev) || sc.chPrev == '#')
		&& markers.Length()) {
		const int lengthMarker = 50;
		char marker[lengthMarker + 1] = "";
		const Sci_Position currPos = sc.currentPos;
		int i = 0;
		while (i < lengthMarker) {
			const char ch = styler.SafeGetCharAt(currPos + i);
			if (IsSpace(ch) || IsOperator(ch) || ch == '#') {
				break;
			}
			marker[i++] = caseSensitive ? ch : MakeLowerCase(ch);
		}
		if (i > 0) {
			marker[i] = '\0';
			if (markers.InList(marker)) {
				int saveState = sc.state;
				sc.SetState(markerState);
				sc.Forward(i);
				sc.SetState(saveState);
			}
		}
	}
}

}

#endif
