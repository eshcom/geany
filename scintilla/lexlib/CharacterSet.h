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

inline bool IsACRLF(int ch) {
	return (ch == '\r') || (ch == '\n');
}

inline bool IsASpace(int ch) {
	return (ch == ' ') || ((ch >= 0x09) && (ch <= 0x0d));
}

inline bool IsASpaceOrTab(int ch) {
	return (ch == ' ') || (ch == '\t');
}

inline bool IsADigit(int ch) {
	return (ch >= '0') && (ch <= '9');
}

inline bool IsADigit(int ch, int base) {
	if (base <= 10) {
		return (ch >= '0') && (ch < '0' + base);
	} else {
		return ((ch >= '0') && (ch <= '9')) ||
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

inline bool IsUpperOrLowerCase(int ch) {
	return IsUpperCase(ch) || IsLowerCase(ch);
}

inline bool IsAlphaNumeric(int ch) {
	return
		((ch >= '0') && (ch <= '9')) ||
		((ch >= 'a') && (ch <= 'z')) ||
		((ch >= 'A') && (ch <= 'Z'));
}


inline bool iswordchar(int ch) {
	return IsAlphaNumeric(ch) || ch == '.' || ch == '_';
}

inline bool iswordstart(int ch) {
	return IsAlphaNumeric(ch) || ch == '_';
}

inline bool isoperator(int ch) {
	if (IsAlphaNumeric(ch))
		return false;
	if (ch == '%' || ch == '^' || ch == '&' || ch == '*' ||
			ch == '(' || ch == ')' || ch == '-' || ch == '+' ||
			ch == '=' || ch == '|' || ch == '{' || ch == '}' ||
			ch == '[' || ch == ']' || ch == ':' || ch == ';' ||
			ch == '<' || ch == '>' || ch == ',' || ch == '/' ||
			ch == '?' || ch == '!' || ch == '.' || ch == '~')
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
		FORMAT_FULL_SPEC,			// [d i o u x e f g c s %]
		FORMAT_NUM_BASE_SPEC,		// [d i o u x e f g]
		FORMAT_NUM_PREC_SPEC,		// [d i o u x]
		FORMAT_NUM_FLAG,			// [-+0\s]+
		FORMAT_NUM_BASE_DIGITS,		// [1-9]+
		FORMAT_NUM_PREC_DIGITS,		// [0-9]+|*
		FORMAT_NUM_PREC_DOT,		// .
		FORMAT_NUM_PREC_ASTER		// *
	};
	int formatState;
	CharacterSet setFullSpec;
	CharacterSet setNumBaseSpec;
	CharacterSet setNumPrecSpec;
	CharacterSet setNumFlag;
	CharacterSet setNumBaseDigits;
	CharacterSet setNumPrecDigits;
	FormatSequence() {
		formatState = FORMAT_NONE;
		setFullSpec = CharacterSet(CharacterSet::setNone, "diouxefgcs%");
		setNumBaseSpec = CharacterSet(CharacterSet::setNone, "diouxefg");
		setNumPrecSpec = CharacterSet(CharacterSet::setNone, "dioux");
		setNumFlag = CharacterSet(CharacterSet::setNone, "-+0 ");
		setNumBaseDigits = CharacterSet(CharacterSet::setNone, "123456789");
		setNumPrecDigits = CharacterSet(CharacterSet::setDigits);
	}
	void initFormatState() {
		formatState = FORMAT_INIT;
	}
	bool atFormatEnd(int currChar) {
		switch (formatState) {
			case FORMAT_INIT:
				if (setFullSpec.Contains(currChar)) {
					formatState = FORMAT_FULL_SPEC;
				} else if (setNumFlag.Contains(currChar)) {
					formatState = FORMAT_NUM_FLAG;
				} else if (setNumBaseDigits.Contains(currChar)) {
					formatState = FORMAT_NUM_BASE_DIGITS;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PREC_DOT;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_FLAG:
				if (setNumBaseDigits.Contains(currChar)) {
					formatState = FORMAT_NUM_BASE_DIGITS;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PREC_DOT;
				} else if (!setNumFlag.Contains(currChar)) {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_BASE_DIGITS:
				if (setNumBaseSpec.Contains(currChar)) {
					formatState = FORMAT_NUM_BASE_SPEC;
				} else if (currChar == '.') {
					formatState = FORMAT_NUM_PREC_DOT;
				} else if (!setNumBaseDigits.Contains(currChar)) {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_PREC_DOT:
				if (setNumPrecDigits.Contains(currChar)) {
					formatState = FORMAT_NUM_PREC_DIGITS;
				} else if (currChar == '*') {
					formatState = FORMAT_NUM_PREC_ASTER;
				}
				break;
			case FORMAT_NUM_PREC_DIGITS:
				if (setNumPrecSpec.Contains(currChar)) {
					formatState = FORMAT_NUM_PREC_SPEC;
				} else if (!setNumPrecDigits.Contains(currChar)) {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_NUM_PREC_ASTER:
				if (setNumPrecSpec.Contains(currChar)) {
					formatState = FORMAT_NUM_PREC_SPEC;
				} else {
					formatState = FORMAT_NONE;
				}
				break;
			case FORMAT_FULL_SPEC:
			case FORMAT_NUM_BASE_SPEC:
			case FORMAT_NUM_PREC_SPEC:
				formatState = FORMAT_NONE;
				break;
		}
		return (formatState == FORMAT_NONE);
	}
};

}

#endif
