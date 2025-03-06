// Scintilla source code edit control
/** @file CharacterSet.h
 ** Encapsulates a set of characters. Used to test if a character is within a set.
 **/
// Copyright 2007 by Neil Hodgson <neilh@scintilla.org>
// The License.txt file describes the conditions under which this software may be distributed.

#ifndef CHARACTERSET_H
#define CHARACTERSET_H

namespace Scintilla {

class CharacterSet {
	int size;
	bool valueAfter;
	bool *bset;
public:
	enum setBase {
		setNone=0,
		setLower=1,
		setUpper=2,
		setDigits=4,
		setAlpha=setLower|setUpper,
		setAlphaNum=setAlpha|setDigits
	};
	CharacterSet(setBase base=setNone, const char *initialSet="",
				 int size_=0x80, bool valueAfter_=false) {
		size = size_;
		valueAfter = valueAfter_;
		bset = new bool[size];
		for (int i=0; i < size; i++) {
			bset[i] = false;
		}
		AddString(initialSet);
		if (base & setLower)
			AddString("abcdefghijklmnopqrstuvwxyz");
		if (base & setUpper)
			AddString("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		if (base & setDigits)
			AddString("0123456789");
	}
	CharacterSet(const CharacterSet &other) {
		size = other.size;
		valueAfter = other.valueAfter;
		bset = new bool[size];
		for (int i=0; i < size; i++) {
			bset[i] = other.bset[i];
		}
	}
	CharacterSet &operator=(CharacterSet &&other) {
		if (this != &other) {
			delete []bset;
			size = other.size;
			valueAfter = other.valueAfter;
			bset = other.bset;
			other.size = 0;
			other.bset = nullptr;
		}
		return *this;
	}
	~CharacterSet() {
		delete []bset;
		bset = nullptr;
		size = 0;
	}
	CharacterSet &operator=(const CharacterSet &other) {
		if (this != &other) {
			bool *bsetNew = new bool[other.size];
			for (int i=0; i < other.size; i++) {
				bsetNew[i] = other.bset[i];
			}
			delete []bset;
			size = other.size;
			valueAfter = other.valueAfter;
			bset = bsetNew;
		}
		return *this;
	}
	void Add(int val) {
		assert(val >= 0);
		assert(val < size);
		bset[val] = true;
	}
	void AddString(const char *setToAdd) {
		for (const char *cp=setToAdd; *cp; cp++) {
			const unsigned char uch = *cp;
			assert(uch < size);
			bset[uch] = true;
		}
	}
	bool Contains(int val) const {
		assert(val >= 0);
		if (val < 0) return false;
		return (val < size) ? bset[val] : valueAfter;
	}
	bool Contains(char ch) const {
		// Overload char as char may be signed
		const unsigned char uch = ch;
		return Contains(uch);
	}
};

// Functions for classifying characters

inline bool IsCRLF(int ch) {
	return (ch == '\r') || (ch == '\n');
}

inline bool IsSpaceOrTab(int ch) {
	return (ch == ' ') || (ch == '\t');
}

inline bool IsSpace(int ch) {
	return (ch == ' ') || ((ch >= 0x09) && (ch <= 0x0d));
}

inline bool IsBlank(int ch) {
	return (ch == ' ') || (ch == 0x09) || (ch == 0x0b);
}

inline bool IsDigit(int ch) {
	return (ch >= '0') && (ch <= '9');
}

inline bool IsDigit(int ch, int base) {
	if (base <= 10) {
		return (ch >= '0') && (ch < '0' + base);
	} else {
		return IsDigit(ch) ||
			   ((ch >= 'A') && (ch < 'A' + base - 10)) ||
			   ((ch >= 'a') && (ch < 'a' + base - 10));
	}
}

inline bool IsASCII(int ch) {
	return (ch >= 0) && (ch < 0x80);
}

inline bool IsLowerCase(int ch) {
	return (ch >= 'a') && (ch <= 'z');
}

inline bool IsUpperCase(int ch) {
	return (ch >= 'A') && (ch <= 'Z');
}

inline bool IsAlpha(int ch) {
	return IsLowerCase(ch) || IsUpperCase(ch);
}

inline bool IsAlnum(int ch) {
	return IsAlpha(ch) || IsDigit(ch);
}

inline bool IsAlphaWordChar(int ch) {
	return IsAlpha(ch) || ch == '_';
}

inline bool IsAlnumWordChar(int ch) {
	return IsAlnum(ch) || ch == '_';
}

inline bool IsWordChar(int ch) {
	return IsAlnumWordChar(ch) || ch == '.';
}

inline bool IsOperator(int ch) {
	if (IsAlnum(ch))
		return false;
	if (ch == '(' || ch == ')' || ch == '{' || ch == '}' ||
		ch == '[' || ch == ']' || ch == '<' || ch == '>' ||
		ch == '?' || ch == '!' || ch == '+' || ch == '=' ||
		ch == '*' || ch == '^' || ch == '&' || ch == '%' ||
		ch == '/' || ch == '|' || ch == '~' || ch == '-' ||
		ch == ':' || ch == ';' || ch == ',' || ch == '.')
		return true;
	return false;
}

// Simple case functions for ASCII supersets.

template <typename T>
inline T MakeUpperCase(T ch) {
	if (ch < 'a' || ch > 'z')
		return ch;
	else
		return ch - 'a' + 'A';
}

template <typename T>
inline T MakeLowerCase(T ch) {
	if (ch < 'A' || ch > 'Z')
		return ch;
	else
		return ch - 'A' + 'a';
}

int CompareCaseInsensitive(const char *a, const char *b);
int CompareNCaseInsensitive(const char *a, const char *b, size_t len);
void RemoveAllSpaces(char *s);

// esh: moved from LexCPP.cxx
struct EscapeSequence {
	int digitsLeft;
	CharacterSet setHexDigits;
	CharacterSet setOctDigits;
	CharacterSet setNoneNumeric;
	CharacterSet *escapeSetValid;
	EscapeSequence() {
		digitsLeft = 0;
		escapeSetValid = nullptr;
		setHexDigits = CharacterSet(CharacterSet::setDigits, "ABCDEFabcdef");
		setOctDigits = CharacterSet(CharacterSet::setNone, "01234567");
	}
	void initEscapeState(int nextChar) {
		digitsLeft = 0;
		escapeSetValid = &setNoneNumeric;
		if (nextChar == 'U') {
			digitsLeft = 9;
			escapeSetValid = &setHexDigits;
		} else if (nextChar == 'u') {
			digitsLeft = 5;
			escapeSetValid = &setHexDigits;
		} else if (nextChar == 'x') {
			digitsLeft = 5;
			escapeSetValid = &setHexDigits;
		} else if (setOctDigits.Contains(nextChar)) {
			digitsLeft = 3;
			escapeSetValid = &setOctDigits;
		}
	}
	bool atEscapeEnd(int currChar) const {
		return (digitsLeft <= 0) || !escapeSetValid->Contains(currChar);
	}
};

struct FormatSequence {
	enum
	{
		FORMAT_NONE,
		FORMAT_INIT,
		FORMAT_END,
		FORMAT_LEN_PREF,		// [h hh l ll j z t L]
		FORMAT_FULL_SPEC,		// [d i u o x X f F e E g G a A c s p n]
		FORMAT_WIDTH_SPEC,		// [d i o u x X f F e E g G]
		FORMAT_FLAG_WIDTH,		// [- + 0 # \s]+
		FORMAT_BASE_WIDTH,		// [1-9]+
		FORMAT_BASE_ASTER,		// *
		FORMAT_PREC_WIDTH,		// [0-9]+
		FORMAT_PREC_ASTER,		// *
		FORMAT_PREC_DOT			// .
	};
	int formatState, lastState;
	int lastLenChar;
	CharacterSet setLenPref;
	CharacterSet setFullSpec;
	CharacterSet setWidthSpec;
	CharacterSet setFlagWidth;
	CharacterSet setBaseWidth;
	CharacterSet setPrecWidth;
	FormatSequence() {
		//~ https://ru.wikipedia.org/wiki/Printf
		//~ https://www.cplusplus.com/reference/cstdio/printf/
		//~ https://www.dummies.com/programming/cpp/using-printf-for-output/
		//~ https://www.tutorialspoint.com/c_standard_library/c_function_printf.htm
		
		//~ A format specifier follows this prototype:
		//~ %[flags][width][.precision][length]specifier
		
		formatState  = FORMAT_NONE;
		setLenPref   = CharacterSet(CharacterSet::setNone, "hljztL");
		setFullSpec  = CharacterSet(CharacterSet::setNone, "diuoxXfFeEgGaAscpn");
		setWidthSpec = CharacterSet(CharacterSet::setNone, "diuoxXfFeEgGaAs");
		setFlagWidth = CharacterSet(CharacterSet::setNone, "-+0# ");
		setBaseWidth = CharacterSet(CharacterSet::setNone, "123456789");
		setPrecWidth = CharacterSet(CharacterSet::setDigits);
	}
	void initFormatState() {
		formatState = FORMAT_INIT;
		lastLenChar = ' ';
	}
	bool atFormatEnd(int currChar) {
		switch (formatState) {
			case FORMAT_INIT:
				if (setLenPref.Contains(currChar)) {
					lastState = formatState;
					lastLenChar = currChar;
					formatState = FORMAT_LEN_PREF;
				} else if (setFullSpec.Contains(currChar)
							|| currChar == '%') {
					formatState = FORMAT_FULL_SPEC;
				} else if (setFlagWidth.Contains(currChar)) {
					formatState = FORMAT_FLAG_WIDTH;
				} else if (setBaseWidth.Contains(currChar)) {
					formatState = FORMAT_BASE_WIDTH;
				} else if (currChar == '*') {
					formatState = FORMAT_BASE_ASTER;
				} else if (currChar == '.') {
					formatState = FORMAT_PREC_DOT;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_FLAG_WIDTH:
				if (setLenPref.Contains(currChar)) {
					lastState = formatState;
					lastLenChar = currChar;
					formatState = FORMAT_LEN_PREF;
				} else if (setWidthSpec.Contains(currChar)) {
					formatState = FORMAT_WIDTH_SPEC;
				} else if (setBaseWidth.Contains(currChar)) {
					formatState = FORMAT_BASE_WIDTH;
				} else if (currChar == '*') {
					formatState = FORMAT_BASE_ASTER;
				} else if (currChar == '.') {
					formatState = FORMAT_PREC_DOT;
				} else if (!setFlagWidth.Contains(currChar)) {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_BASE_WIDTH:
			case FORMAT_BASE_ASTER:
				if (setLenPref.Contains(currChar)) {
					lastState = formatState;
					lastLenChar = currChar;
					formatState = FORMAT_LEN_PREF;
				} else if (setWidthSpec.Contains(currChar)) {
					formatState = FORMAT_WIDTH_SPEC;
				} else if (currChar == '.') {
					formatState = FORMAT_PREC_DOT;
				} else if (formatState == FORMAT_BASE_ASTER ||
							(!setBaseWidth.Contains(currChar)
								&& currChar != '0')) {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_PREC_DOT:
				if (setLenPref.Contains(currChar)) {
					lastState = formatState;
					lastLenChar = currChar;
					formatState = FORMAT_LEN_PREF;
				} else if (setWidthSpec.Contains(currChar)) {
					formatState = FORMAT_WIDTH_SPEC;
				} else if (setPrecWidth.Contains(currChar)) {
					formatState = FORMAT_PREC_WIDTH;
				} else if (currChar == '*') {
					formatState = FORMAT_PREC_ASTER;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_PREC_WIDTH:
			case FORMAT_PREC_ASTER:
				if (setLenPref.Contains(currChar)) {
					lastState = formatState;
					lastLenChar = currChar;
					formatState = FORMAT_LEN_PREF;
				} else if (setWidthSpec.Contains(currChar)) {
					formatState = FORMAT_WIDTH_SPEC;
				} else if (formatState == FORMAT_PREC_ASTER ||
							!setPrecWidth.Contains(currChar)) {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_LEN_PREF:
				if (lastLenChar == currChar &&
					(currChar == 'h' || currChar == 'l')) { // hh ll
					lastLenChar = ' ';
				} else if (lastState == FORMAT_INIT &&
							setFullSpec.Contains(currChar)) {
					formatState = FORMAT_FULL_SPEC;
				} else if (lastState != FORMAT_INIT &&
							setWidthSpec.Contains(currChar)) {
					formatState = FORMAT_WIDTH_SPEC;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_FULL_SPEC:
			case FORMAT_WIDTH_SPEC:
				formatState = FORMAT_END;
				break;
		}
		return (formatState == FORMAT_END ||
				formatState == FORMAT_NONE);
	}
	bool atFormatNone() {
		return (formatState == FORMAT_NONE);
	}
};

struct ErlFormatSequence {
	enum
	{
		FORMAT_NONE,
		FORMAT_INIT,
		FORMAT_END,
		FORMAT_MOD,					// [t l]
		FORMAT_SPEC,				// [c f e g s w p b x n i W P B X # +]
		FORMAT_NUM_SIGN,			// [-+]
		FORMAT_NUM_BASE_DIGITS,		// [0-9]+
		FORMAT_NUM_PREC_DIGITS,		// [0-9]+
		FORMAT_NUM_BASE_ASTER,		// *
		FORMAT_NUM_PREC_ASTER,		// *
		FORMAT_NUM_PREC_DOT,		// .
		FORMAT_NUM_PAD_DOT,			// .
		FORMAT_NUM_PAD_CHAR			// [<any printable>]
	};
	int formatState;
	CharacterSet setMod;
	CharacterSet setSpec;
	CharacterSet setNumSign;
	CharacterSet setNumDigits;
	ErlFormatSequence() {
		formatState = FORMAT_NONE;
		setMod = CharacterSet(CharacterSet::setNone, "tl");
		setSpec = CharacterSet(CharacterSet::setNone, "cfegswpbxniWPBX#+");
		setNumSign = CharacterSet(CharacterSet::setNone, "-+");
		setNumDigits = CharacterSet(CharacterSet::setDigits);
	}
	void initFormatState() {
		formatState = FORMAT_INIT;
	}
	bool atFormatEnd(int currChar) {
		switch (formatState) {
			case FORMAT_INIT:
				if (setSpec.Contains(currChar)) {
					formatState = FORMAT_SPEC;
				} else if (setMod.Contains(currChar)) {
					formatState = FORMAT_MOD;
				} else if (setNumSign.Contains(currChar)) {
					formatState = FORMAT_NUM_SIGN;
				} else if (setNumDigits.Contains(currChar)) {
					formatState = FORMAT_NUM_BASE_DIGITS;
				} else if (currChar == '*') {
					formatState = FORMAT_NUM_BASE_ASTER;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PREC_DOT;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_SIGN:
				if (setNumDigits.Contains(currChar)) {
					formatState = FORMAT_NUM_BASE_DIGITS;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PREC_DOT;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_BASE_DIGITS:
			case FORMAT_NUM_BASE_ASTER:
				if (setSpec.Contains(currChar)) {
					formatState = FORMAT_SPEC;
				} else if (setMod.Contains(currChar)) {
					formatState = FORMAT_MOD;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PREC_DOT;
				} else if (formatState == FORMAT_NUM_BASE_ASTER ||
						   !setNumDigits.Contains(currChar)) {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_PREC_DOT:
				if (setNumDigits.Contains(currChar)) {
					formatState = FORMAT_NUM_PREC_DIGITS;
				} else if (currChar == '*') {
					formatState = FORMAT_NUM_PREC_ASTER;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_PREC_DIGITS:
			case FORMAT_NUM_PREC_ASTER:
				if (setSpec.Contains(currChar)) {
					formatState = FORMAT_SPEC;
				} else if (setMod.Contains(currChar)) {
					formatState = FORMAT_MOD;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PAD_DOT;
				} else if (formatState == FORMAT_NUM_PREC_ASTER ||
						   !setNumDigits.Contains(currChar)) {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_PAD_DOT:
				formatState = FORMAT_NUM_PAD_CHAR;
				break;
			case FORMAT_NUM_PAD_CHAR:
				if (setSpec.Contains(currChar)) {
					formatState = FORMAT_SPEC;
				} else if (setMod.Contains(currChar)) {
					formatState = FORMAT_MOD;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_MOD:
				if (setSpec.Contains(currChar)) {
					formatState = FORMAT_SPEC;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_SPEC:
				formatState = FORMAT_END;
				break;
		}
		return (formatState == FORMAT_END ||
				formatState == FORMAT_NONE);
	}
	bool atFormatNone() {
		return (formatState == FORMAT_NONE);
	}
};

}

#endif
