/*
*   Copyright (c) 2025, esh <esh.eburg@gmail.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Elixir language
*   files.
*/
/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_ATTRIBUTE, K_FUNCTION, K_TYPE, K_MODULE, K_MACRO, K_PROTO, K_IMPL
} elixirKind;

static kindDefinition ElixirKinds[] = {
	{true, 'a', "attribute",      "module attributes"},
	{true, 'f', "function",       "functions"},
	{true, 't', "type",           "type definitions"},
	{true, 'm', "module",         "modules"},
	{true, 'M', "macro",          "macros"},
	{true, 'p', "protocol",       "protocols"},
	{true, 'i', "implementation", "protocol implementations"},
};

static NestingLevels *nesting = NULL;

struct nlUserData {
	int indent;
};
#define EX_NL_INDENTATION(nl) \
	((struct nlUserData *)nestingLevelGetUserData(nl))->indent

#define SCOPE_SEPARATOR '.'

#define L_LITERAL_PREFIX "scrwp"
#define U_LITERAL_PREFIX "SCRWNUDT"

static ptrArray *stringStack = NULL;

typedef struct sStringInfo {
	char closingChar;
	bool isTriple;
	bool canbeInterpolate;
	int nestingCount;
} stringInfo;

static stringInfo *stringInfoNew(char closingChar, bool isTriple,
								 bool canbeInterpolate, int nestingCount)
{
	stringInfo *const strInfo = xMalloc(1, stringInfo);
	strInfo->closingChar = closingChar;
	strInfo->isTriple = isTriple;
	strInfo->canbeInterpolate = canbeInterpolate;
	strInfo->nestingCount = nestingCount;
	return strInfo;
}

/*
*   FUNCTION DEFINITIONS
*/
/* tagEntryInfo and vString should be preinitialized/preallocated but not
 * necessary. If successful you will find class name in vString
 */

typedef struct {
	vString *name;
	int kindIndex;
} Scope;

static inline bool isStructKind(elixirKind kind)
{
	return kind == K_MODULE || kind == K_PROTO || kind == K_IMPL;
}

static Scope getCurrentScope(void)
{
	vString *const scopeName = vStringNew();
	int scopeKindIndex = -1;
	
	NestingLevel *nl = nestingLevelsGetCurrent(nesting);
	tagEntryInfo *tag = getEntryOfNestingLevel(nl);
	if (tag)
	{
		if (tag->extensionFields.scopeName && *tag->extensionFields.scopeName)
			vStringCatS(scopeName, tag->extensionFields.scopeName);
		
		if (isStructKind(tag->kindIndex))
		{
			if (vStringLength(scopeName) > 0)
				vStringPut(scopeName, SCOPE_SEPARATOR);
			vStringCatS(scopeName, tag->name);
		}
		scopeKindIndex = tag->kindIndex;
	}
	return (Scope){scopeName, scopeKindIndex};
}

#define freeScope(scope) vStringDelete(scope.name);


static inline bool matchTripleQuote(const unsigned char *cp, char quoteChar)
{
	if (quoteChar == '\"')
		return strncmp(cp, R"(""")", 3) == 0;
	if (quoteChar == '\'')
		return strncmp(cp, R"(''')", 3) == 0;
	
	return false;
}

static inline char getClosingChar(char openingChar)
{
	if (openingChar == '\"')
		return '\"';
	else if (openingChar == '\'')
		return '\'';
	else if (openingChar == '<')
		return '>';
	else if (openingChar == '{')
		return '}';
	else if (openingChar == '[')
		return ']';
	else if (openingChar == '(')
		return ')';
	else if (openingChar == '|')
		return '|';
	else if (openingChar == '/')
		return '/';
	else
		return ' ';
}

static int makeTag(const char *name, elixirKind kind, bool private,
				   const Scope scope, const char *displayName,
				   const char *displayScopeName)
{
	int r = CORK_NIL;
	
	if (ElixirKinds[kind].enabled)
	{
		tagEntryInfo tag;
		initTagEntry(&tag, name, kind);
		
		tag.displayName = displayName;
		
		if (vStringLength(scope.name) > 0)
		{
			tag.extensionFields.scopeKindIndex = scope.kindIndex;
			tag.extensionFields.scopeName = vStringValue(scope.name);
			tag.extensionFields.displayScopeName = displayScopeName;
		}
		tag.isFileScope = private;
		
		r = makeTagEntry(&tag);
		//~ printf("!!!tag: scope = %s, name = %s\n",
			   //~ tag.extensionFields.scopeName, tag.name);
	}
	return r;
}

static bool isIdentifierChar(int c)
{
	return (bool)(isalnum(c) || c == '_' || c == '@'
							 || c == '!' || c == '?'
							 || c == SCOPE_SEPARATOR);
}

static void checkMultilineString(const unsigned char *cp)
{
	while (*cp)
	{
		stringInfo *strInfo = NULL;
		if (ptrArrayCount(stringStack) > 0)
			strInfo = ptrArrayLast(stringStack);
		
		if (strInfo && strInfo->nestingCount == 0) // inside string
		{
			if (*cp == '\\')
			{
				cp++; // skip any character after the backslash
				if (!*cp) break;
			}
			else if (*cp == strInfo->closingChar)
			{
				if (strInfo->isTriple)
				{
					if (matchTripleQuote(cp, *cp))
					{
						cp += 2; // skip 2 quote-symbols
						ptrArrayRemoveLast(stringStack);
					}
				}
				else
					ptrArrayRemoveLast(stringStack);
			}
			else if (*cp == '#' && cp[1] == '{' && strInfo->canbeInterpolate)
			{
				strInfo->nestingCount++;
				cp++; // skip #
			}
		}
		else
		{
			if (*cp == '~' && (strchr(L_LITERAL_PREFIX U_LITERAL_PREFIX, cp[1]))
				&& cp[2])
			{
				char closingChar = getClosingChar(cp[2]);
				if (closingChar != ' ')
				{
					bool canbeInterpolate = strchr(L_LITERAL_PREFIX, cp[1]);
					cp += 2; // skip ~LITERAL_PREFIX
					
					bool isTriple = matchTripleQuote(cp, *cp);
					ptrArrayAdd(stringStack, stringInfoNew(closingChar, isTriple,
														   canbeInterpolate, 0));
					if (isTriple) cp += 2; // skip 2 quote-symbols
				}
			}
			else if (*cp == '\"' || *cp == '\'')
			{
				bool isTriple = matchTripleQuote(cp, *cp);
				ptrArrayAdd(stringStack, stringInfoNew(*cp, isTriple, true, 0));
				if (isTriple) cp += 2; // skip 2 quote-symbols
			}
			else if (strInfo)
			{
				if (*cp == '{')
					strInfo->nestingCount++;
				else if (*cp == '}')
					strInfo->nestingCount--;
			}
		}
		cp++;
	}
}

static const unsigned char *skipSpace(const unsigned char *cp)
{
	while (isspace(*cp))
		cp++;
	return cp;
}

static const unsigned char *parseIdentifier(const unsigned char *cp,
											vString *const identifier)
{
	vStringClear(identifier);
	cp = skipSpace(cp);
	
	while (isIdentifierChar(*cp))
		vStringPut(identifier, *cp++);
	
	return cp;
}

static const unsigned char *parseStructTag(const unsigned char *cp, elixirKind kind,
										   bool private, int indent)
{
	vString *const identifier = vStringNew();
	cp = parseIdentifier(cp, identifier);
	
	if (vStringLength(identifier) > 0)
	{
		const char *tagName, *tagFullName = vStringValue(identifier);
		char *tagScopeName = NULL;
		
		if (isStructKind(kind))
		{
			tagName = strrchr(tagFullName, SCOPE_SEPARATOR);
			
			if (tagName && tagName[1])
			{
				if (tagName > tagFullName)
					tagScopeName = strndup(tagFullName, tagName - tagFullName);
				tagName++; // skip dot
			}
			else
				tagName = tagFullName;
		}
		else // kind == K_MACRO
			tagName = tagFullName;
		
		Scope currScope = getCurrentScope();
		Scope scope = {vStringNewCopy(currScope.name), currScope.kindIndex};
		
		if (kind == K_MODULE && tagScopeName && *tagScopeName)
		{
			vStringPut(scope.name, SCOPE_SEPARATOR);
			vStringCatS(scope.name, tagScopeName);
		}
		free(tagScopeName);
		
		int r;
		if (kind == K_MODULE)
			r = makeTag(tagName, kind, private, scope,
						tagFullName, vStringValue(currScope.name));
		else
			r = makeTag(tagName, kind, private, scope, NULL, NULL);
		
		freeScope(currScope);
		freeScope(scope);
		
		NestingLevel *nl = nestingLevelsPush(nesting, r);
		EX_NL_INDENTATION(nl) = indent;
	}
	vStringDelete(identifier);
	return cp;
}

static const unsigned char *parseMemberTag(const unsigned char *cp,
										   elixirKind kind, bool private)
{
	vString *const identifier = vStringNew();
	cp = parseIdentifier(cp, identifier);
	
	if (vStringLength(identifier) > 0)
	{
		Scope scope = getCurrentScope();
		makeTag(vStringValue(identifier), kind, private, scope, NULL, NULL);
		freeScope(scope);
	}
	vStringDelete(identifier);
	return cp;
}

static const unsigned char *parseKeyword(const unsigned char *cp, int indent)
{
	vString *const keyword = vStringNew();
	cp = parseIdentifier(cp, keyword);
	cp = skipSpace(cp);
	
	const char *const kwval = vStringValue(keyword);
	
	if (strcmp(kwval, "defmodule") == 0)
		cp = parseStructTag(cp, K_MODULE, false, indent);
	else if (strcmp(kwval, "defprotocol") == 0)
		cp = parseStructTag(cp, K_PROTO, false, indent);
	else if (strcmp(kwval, "defmacro") == 0)
		cp = parseStructTag(cp, K_MACRO, true, indent);
	else if (strcmp(kwval, "defimpl") == 0)
		cp = parseStructTag(cp, K_IMPL, false, indent);
	else if (strcmp(kwval, "defp") == 0)
		cp = parseMemberTag(cp, K_FUNCTION, true);
	else if (strcmp(kwval, "def") == 0)
		cp = parseMemberTag(cp, K_FUNCTION, false);
	else if (strcmp(kwval, "defmemop") == 0)
		cp = parseMemberTag(cp, K_FUNCTION, true);
	else if (strcmp(kwval, "defmemo") == 0)
		cp = parseMemberTag(cp, K_FUNCTION, false);
	else if (strcmp(kwval, "@type") == 0)
		cp = parseMemberTag(cp, K_TYPE, false);
	else if (strcmp(kwval, "@spec") == 0 ||
			 strcmp(kwval, "@impl") == 0 ||
			 strcmp(kwval, "@doc") == 0 ||
			 strcmp(kwval, "@moduledoc") == 0 ||
			 strcmp(kwval, "@behaviour") == 0 ||
			 strcmp(kwval, "@deprecated") == 0 ||
			 strcmp(kwval, "@derive") == 0 ||
			 strcmp(kwval, "@callback") == 0 ||
			 strcmp(kwval, "@macrocallback") == 0 ||
			 strcmp(kwval, "@optional_callbacks") == 0)
		/* skip */;
	else if (*kwval == '@' && kwval[1] && *cp &&
			 (isalnum(*cp) || strchr("\"{[(%:~_", *cp)))
	{
		Scope scope = getCurrentScope();
		makeTag(kwval, K_ATTRIBUTE, true, scope, NULL, NULL);
		freeScope(scope);
	}
	else if (strcmp(kwval, "end") == 0)
	{
		NestingLevel *nl = nestingLevelsGetCurrent(nesting);
		
		if (nl && EX_NL_INDENTATION(nl) == indent)
			nestingLevelsPop(nesting);
	}
	vStringDelete(keyword);
	return cp;
}

static void findElixirTags(void)
{
	stringStack = ptrArrayNew(NULL);
	nesting = nestingLevelsNew(sizeof(struct nlUserData));
	
	const unsigned char *line;
	
	while ((line = readLineFromInputFile()) != NULL)
	{
		const unsigned char *cp = skipSpace(line);
		
		if (ptrArrayCount(stringStack) > 0)
		{
			checkMultilineString(cp);
			continue;
		}
		else if (islower(*cp) || *cp == '@')
			cp = parseKeyword(cp, cp - line);
		
		checkMultilineString(cp);
	}
	ptrArrayDelete(stringStack);
	nestingLevelsFree(nesting);
}

extern parserDefinition *ElixirParser(void)
{
	static const char *const extensions[] = { "ex", "exs", NULL };
	parserDefinition *def = parserNew("Elixir");
	def->kindTable = ElixirKinds;
	def->kindCount = ARRAY_SIZE(ElixirKinds);
	def->extensions = extensions;
	def->parser = findElixirTags;
	def->useCork = true;
	return def;
}
