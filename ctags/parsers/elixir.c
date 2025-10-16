/*
*   Copyright (c) 2025, Egor Shinkarev <esheburg@gmail.com>
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
	K_ATTRIBUTE, K_FUNCTION, K_TYPE, K_MODULE, K_MACRO, K_PROTO, K_IMPL, K_ALIAS
} elixirKind;

static kindDefinition ElixirKinds[] = {
	{true, 'a', "attribute",      "module attributes"},
	{true, 'f', "function",       "functions"},
	{true, 't', "type",           "type definitions"},
	{true, 'm', "module",         "modules"},
	{true, 'M', "macro",          "macros"},
	{true, 'p', "protocol",       "protocols"},
	{true, 'i', "implementation", "protocol implementations"},
	{true, 'l', "alias",          "alias definitions"},
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

static Scope getCurrentScope(void)
{
	vString *const vScopeName = vStringNew();
	int scopeKindIndex = K_MODULE;
	
	NestingLevel *nl = nestingLevelsGetCurrent(nesting);
	tagEntryInfo *tag = getEntryOfNestingLevel(nl);
	if (tag)
	{
		const char *scope, *name;
		if (tag->displayName && *tag->displayName)
		{
			scope = tag->extensionFields.displayScopeName;
			name = tag->displayName;
		}
		else
		{
			scope = tag->extensionFields.scopeName;
			name = tag->name;
		}
		if (scope && *scope) vStringCatS(vScopeName, scope);
		
		if (tag->kindIndex == K_MODULE ||
			tag->kindIndex == K_PROTO ||
			tag->kindIndex == K_IMPL)
		{
			if (vStringLength(vScopeName) > 0)
				vStringPut(vScopeName, SCOPE_SEPARATOR);
			vStringCatS(vScopeName, name);
		}
		scopeKindIndex = tag->kindIndex;
	}
	return (Scope){vScopeName, scopeKindIndex};
}

#define freeScope(scope) vStringDelete(scope.name);


static inline bool matchTripleQuote(const unsigned char *cp, char quoteChar)
{
	if (quoteChar == '\"')
		return strncmp((const char *)cp, R"(""")", 3) == 0;
	if (quoteChar == '\'')
		return strncmp((const char *)cp, R"(''')", 3) == 0;
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
		
		tag.isFileScope = private;
		tag.displayName = displayName;
		tag.extensionFields.displayScopeName = displayScopeName;
		
		if (vStringLength(scope.name) > 0)
		{
			tag.extensionFields.scopeName = vStringValue(scope.name);
			tag.extensionFields.scopeKindIndex = scope.kindIndex;
		}
		
		r = makeTagEntry(&tag);
		//~ printf("!!!kind: %d, scope: %s, name: %s, dispscope: %s, dispname: %s\n",
			   //~ kind, tag.extensionFields.scopeName, tag.name,
			   //~ displayScopeName, displayName);
	}
	return r;
}

static bool isIdentifierChar(int c)
{
	return (bool)(isalnum(c) || c == '_' || c == '@'
							 || c == '!' || c == '?'
							 || c == ':' || c == SCOPE_SEPARATOR);
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
			if (*cp == '#') break; // skip comment to the end
			
			if (*cp == '~' && strchr(L_LITERAL_PREFIX U_LITERAL_PREFIX, cp[1])
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
	while (isspace(*cp)) cp++;
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
		const char *tagFullName = vStringValue(identifier);
		Scope currScope = getCurrentScope();
		int r;
		
		if (kind == K_MODULE || kind == K_PROTO)
		{
			Scope scope = {vStringNewCopy(currScope.name), currScope.kindIndex};
			const char *search = strrchr(tagFullName, SCOPE_SEPARATOR);
			
			if (search && search[1])
			{	// if SCOPE_SEPARATOR is found and there is something else after it
				if (search > tagFullName)	// there is something else before it
				{
					if (vStringLength(scope.name) > 0)
						vStringPut(scope.name, SCOPE_SEPARATOR);
					vStringNCatS(scope.name, tagFullName, search - tagFullName);
				}
				search++; // skip SCOPE_SEPARATOR
			}
			else
				search = tagFullName;
			
			r = makeTag(search, kind, private, scope,
						tagFullName, vStringValue(currScope.name));
			
			freeScope(scope);
		}
		else if (kind == K_IMPL)
		{
			const char *search = strchr(tagFullName, SCOPE_SEPARATOR);
			char *alias;
			
			if (search && search[1] && search > tagFullName)
			{
				alias = strndup(tagFullName, search - tagFullName);
				search++; // skip SCOPE_SEPARATOR
			}
			else
			{
				alias = strdup(tagFullName);
				search = tagFullName;
			}
			
			vString *const tagName = vStringNew();
			
			for (size_t i = 0; i < countEntryInCorkQueue(); i++)
			{
				const tagEntryInfo *tag = getEntryInCorkQueue(i);
				if (tag && tag->kindIndex == K_ALIAS &&
					strcmp(tag->name, alias) == 0)
				{
					const char *scopeName;
					if (tag->extensionFields.displayScopeName &&
						*tag->extensionFields.displayScopeName)
						scopeName = tag->extensionFields.displayScopeName;
					else
						scopeName = tag->extensionFields.scopeName;
					
					if (scopeName && *scopeName)
						vStringCatS(tagName, scopeName);
					break;
				}
			}
			if (vStringLength(tagName) == 0)
				vStringCatS(tagName, tagFullName);
			else if (search > tagFullName)
			{
				vStringPut(tagName, SCOPE_SEPARATOR);
				vStringCatS(tagName, search);
			}
			free(alias);
			
			Scope scope = {vStringNew(), currScope.kindIndex};
			tagFullName = vStringValue(tagName);
			search = strrchr(tagFullName, SCOPE_SEPARATOR);
			
			if (search && search[1])
			{	// if SCOPE_SEPARATOR is found and there is something else after it
				if (search > tagFullName)	// there is something else before it
					vStringNCatS(scope.name, tagFullName, search - tagFullName);
				search++; // skip SCOPE_SEPARATOR
			}
			else
				search = tagFullName;
			
			r = makeTag(search, kind, private, scope,
						tagFullName, vStringValue(currScope.name));
			freeScope(scope);
			vStringDelete(tagName);
		}
		else
			r = makeTag(tagFullName, kind, private, currScope, NULL, NULL);
		
		freeScope(currScope);
		
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

static const unsigned char *parseAliasTag(const unsigned char *cp)
{
	vString *const identifier = vStringNew();
	cp = parseIdentifier(cp, identifier);
	cp = skipSpace(cp);
	
	int len = vStringLength(identifier);
	if (len > 0)
	{
		const char *const ident = vStringValue(identifier);
		
		if (ident[len - 1] == SCOPE_SEPARATOR && *cp == '{')
		{
			while (*(++cp))
			{
				cp = skipSpace(cp);
				if (isupper(*cp))
				{
					vString *const alias = vStringNew();
					cp = parseIdentifier(cp, alias);
					
					if (vStringLength(alias) > 0)
					{
						vString *const module = vStringNewNInit(ident, len - 1);
						if (vStringLength(module) > 0)
							vStringPut(module, SCOPE_SEPARATOR);
						vStringCat(module, alias);
						
						const char *aliasName = vStringValue(alias);
						const char *search = strrchr(aliasName, SCOPE_SEPARATOR);
						
						if (search && search[1] && search > aliasName)
							search++; // skip SCOPE_SEPARATOR
						else
							search = aliasName;
						
						makeTag(search, K_ALIAS, false,
								(Scope){module, K_MODULE}, NULL, NULL);
						vStringDelete(module);
					}
					vStringDelete(alias);
					if (!*cp) break;
				}
				else if (*cp == ',')
					continue;
				else if (*cp == '}')
				{
					cp++;
					break;
				}
			}
		}
		else if (*cp == ',')
		{
			cp = skipSpace(++cp);
			while (islower(*cp)) cp++; // skip "as" keyword
			
			if (*cp == ':')
			{
				cp = skipSpace(++cp);
				if (isupper(*cp))
				{
					vString *const alias = vStringNew();
					cp = parseIdentifier(cp, alias);
					
					if (vStringLength(alias) > 0)
					{
						vString *const module = vStringNewInit(ident);
						makeTag(vStringValue(alias), K_ALIAS, false,
								(Scope){module, K_MODULE}, NULL, NULL);
						vStringDelete(module);
					}
					vStringDelete(alias);
				}
			}
		}
		else
		{
			const char *search = strrchr(ident, SCOPE_SEPARATOR);
			
			if (search && search[1] && search > ident)
			{	// if SCOPE_SEPARATOR is found and there is something else after it
				// and there is something else before it
				search++; // skip SCOPE_SEPARATOR
				vString *const module = vStringNewInit(ident);
				
				makeTag(search, K_ALIAS, false,
						(Scope){module, K_MODULE}, NULL, NULL);
				vStringDelete(module);
			}
		}
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
	else if (strcmp(kwval, "@typep") == 0)
		cp = parseMemberTag(cp, K_TYPE, true);
	else if (strcmp(kwval, "@type") == 0 || strcmp(kwval, "@opaque") == 0)
		cp = parseMemberTag(cp, K_TYPE, false);
	else if (strcmp(kwval, "@after_compile") == 0 ||
			 strcmp(kwval, "@before_compile") == 0 ||
			 strcmp(kwval, "@behaviour") == 0 ||
			 strcmp(kwval, "@callback") == 0 ||
			 strcmp(kwval, "@compile") == 0 ||
			 strcmp(kwval, "@deprecated") == 0 ||
			 strcmp(kwval, "@derive") == 0 ||
			 strcmp(kwval, "@dialyzer") == 0 ||
			 strcmp(kwval, "@doc") == 0 ||
			 strcmp(kwval, "@external_resource") == 0 ||
			 strcmp(kwval, "@file") == 0 ||
			 strcmp(kwval, "@impl") == 0 ||
			 strcmp(kwval, "@macrocallback") == 0 ||
			 strcmp(kwval, "@moduledoc") == 0 ||
			 strcmp(kwval, "@on_definition") == 0 ||
			 strcmp(kwval, "@on_load") == 0 ||
			 strcmp(kwval, "@optional_callbacks") == 0 ||
			 strcmp(kwval, "@spec") == 0 ||
			 strcmp(kwval, "@typedoc") == 0 ||
			 strcmp(kwval, "@vsn") == 0)
		/* skip */;
	else if (strcmp(kwval, "alias") == 0)
		cp = parseAliasTag(cp);
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
