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
	K_MODULE,
	K_PROTO,
	K_IMPL,
	K_ATTRIBUTE,
	K_FUNCTION,
	K_DELEGATE,
	K_PROTOFUNC,
	K_MACRO,
	K_TYPE,
	K_STRUCT,
	K_OTHER,
	K_SPECIAL
} elixirKind;

static kindDefinition ElixirKinds[] = {
	{true, 'm', "module",					"modules"},
	{true, 'p', "protocol",					"protocols"},
	{true, 'i', "implementation",			"protocol implementations"},
	{true, 'a', "attribute",				"module attributes"},
	{true, 'f', "function",					"functions"},
	{true, 'd', "delegate",					"delegates"},
	{true, 'F', "protocol function",		"protocol functions"},
	{true, 'M', "macro",					"macros"},
	{true, 't', "type",						"type definitions"},
	{true, 'S', "structure / exception",	"structures / exceptions"},
	{true, 'o', "other",					"other definitions (test, scope, etc.)"},
	{true, 's', "special",					"special definitions (alias, use, etc.)"},
};

static NestingLevels *nesting = NULL;

struct nlUserData {
	int indent;
};
#define EX_NL_INDENTATION(nl)			\
	((struct nlUserData *)nestingLevelGetUserData(nl))->indent

#define EMPTY(ptr)						\
	(!(ptr) || !*(ptr))

#define FREE_SCOPE(scope)				\
{										\
	free(scope.displayName);			\
	free(scope.name);					\
}

#define SET_SCOPE_NAME(scope, newName)	\
{										\
	free(scope.name);					\
	scope.name = newName;				\
}

#define FREE_ALIAS_MODULE				\
{										\
	vStringDelete(vAliasModule);		\
	vAliasModule = NULL;				\
}

#define SCOPE_SEPARATOR '.'

#define L_LITERAL_PREFIX "scrwp"
#define U_LITERAL_PREFIX "SCRWNUDT"

static ptrArray *stringStack = NULL;
static vString *vAliasModule = NULL;

typedef struct {
	char *name;
	char *displayName;
	int kind;
} Scope;

typedef struct {
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

static char *makeScopeName(const char *scope, const char *name, int kindIndex,
						   bool display)
{
	vString *const vScopeName = vStringNew();
	
	if (!EMPTY(scope)) vStringCatS(vScopeName, scope);
	
	if (kindIndex == K_MODULE || kindIndex == K_PROTO ||
		kindIndex == K_IMPL || kindIndex == K_OTHER ||
		(kindIndex == K_MACRO && (display || strcmp(name, "__using__") != 0)))
	{
		if (vStringLength(vScopeName) > 0)
			vStringPut(vScopeName, SCOPE_SEPARATOR);
		vStringCatS(vScopeName, name);
	}
	char *scopeName = NULL;
	if (vStringLength(vScopeName) > 0)
	{
		scopeName = strdup(vStringValue(vScopeName));
		vStringDelete(vScopeName);
	}
	return scopeName;
}

static Scope getCurrentScope(void)
{
	char *name = NULL, *displayName = NULL;
	int kind = K_MODULE;
	
	NestingLevel *nl = nestingLevelsGetCurrent(nesting);
	tagEntryInfo *tag = getEntryOfNestingLevel(nl);
	
	if (tag)
	{
		name = makeScopeName(tag->extensionFields.scopeName,
							 tag->name, tag->kindIndex, false);
		displayName = makeScopeName(tag->extensionFields.displayScopeName,
									tag->displayName, tag->kindIndex, true);
		kind = tag->kindIndex;
	}
	return (Scope){name, displayName, kind};
}


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
				   Scope scope, const char *inheritance, const char *displayName)
{
	int r = CORK_NIL;
	
	if (ElixirKinds[kind].enabled)
	{
		tagEntryInfo tag;
		initTagEntry(&tag, name, kind);
		
		tag.isFileScope = private;
		tag.displayName = displayName;
		tag.extensionFields.scopeName = scope.name;
		tag.extensionFields.scopeKindIndex = scope.kind;
		tag.extensionFields.displayScopeName = scope.displayName;
		tag.extensionFields.inheritance = inheritance;
		
		r = makeTagEntry(&tag);
		//~ printf("!!!kind: %d, scope: %s, name: %s, dispscope: %s, dispname: %s\n",
			   //~ kind, tag.extensionFields.scopeName, tag.name,
			   //~ tag.extensionFields.displayScopeName, tag.displayName);
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

static vString *makeTagFullname(const char *ident)
{
	vString *const vFullName = vStringNew();
	const char *search = strchr(ident, SCOPE_SEPARATOR);
	char *prefix;
	
	if (search && search[1] && search > ident)
	{
		prefix = strndup(ident, search - ident);
		search++; // skip SCOPE_SEPARATOR
	}
	else
	{
		prefix = strdup(ident);
		search = ident;
	}
	
	for (size_t i = 0; i < countEntryInCorkQueue(); i++)
	{
		const tagEntryInfo *tag = getEntryInCorkQueue(i);
		if (tag && tag->kindIndex == K_SPECIAL &&
			strcmp(tag->name, prefix) == 0)
		{
			const char *module = tag->extensionFields.inheritance;
			if (!EMPTY(module))
				vStringCatS(vFullName, module);
			break;
		}
	}
	if (vStringLength(vFullName) == 0)
		vStringCatS(vFullName, ident);
	else if (search > ident)
	{
		vStringPut(vFullName, SCOPE_SEPARATOR);
		vStringCatS(vFullName, search);
	}
	free(prefix);
	return vFullName;
}

static const unsigned char *parseStructTag(const unsigned char *cp, elixirKind kind,
										   bool private, int indent)
{
	vString *const identifier = vStringNew();
	cp = parseIdentifier(cp, identifier);
	
	if (vStringLength(identifier) > 0)
	{
		const char *const ident = vStringValue(identifier);
		Scope currScope = getCurrentScope();
		int r;
		
		if (kind == K_MODULE || kind == K_PROTO)
		{
			vString *const vScope = currScope.name ? vStringNewInit(currScope.name)
												   : vStringNew();
			const char *search = strrchr(ident, SCOPE_SEPARATOR);
			
			if (search && search[1])
			{	// if SCOPE_SEPARATOR is found and there is something else after it
				if (search > ident)	// there is something else before it
				{
					if (vStringLength(vScope) > 0)
						vStringPut(vScope, SCOPE_SEPARATOR);
					vStringNCatS(vScope, ident, search - ident);
				}
				search++; // skip SCOPE_SEPARATOR
			}
			else
				search = ident;
			
			SET_SCOPE_NAME(currScope, strdup(vStringValue(vScope)));
			r = makeTag(search, kind, private, currScope, NULL, ident);
			vStringDelete(vScope);
		}
		else if (kind == K_IMPL)
		{
			vString *const vFullName = makeTagFullname(ident);
			const char *const fullName = vStringValue(vFullName);
			const char *search;
			char *scope = NULL;
			
			if (strcmp(fullName, "unquote") == 0)
			{
				search = fullName;
				scope = strdup(currScope.name);
			}
			else
			{
				search = strrchr(fullName, SCOPE_SEPARATOR);
				
				if (search && search[1])
				{	// if SCOPE_SEPARATOR is found and there is something else after it
					if (search > fullName)	// there is something else before it
						scope = strndup(fullName, search - fullName);
					search++; // skip SCOPE_SEPARATOR
				}
				else
					search = fullName;
			}
			
			SET_SCOPE_NAME(currScope, scope);
			r = makeTag(search, kind, private, currScope, NULL, fullName);
			vStringDelete(vFullName); // search can refer to vFullName, so delete here
		}
		else // K_MACRO
			r = makeTag(ident, kind, private, currScope, NULL, ident);
		
		FREE_SCOPE(currScope);
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
		char *module = NULL;
		
		if (kind == K_DELEGATE)
		{
			const unsigned char *search = (const unsigned char *)
											strstr((const char *)cp, "to:");
			if (search)
			{
				cp = skipSpace(search + 3/* skip "to:" */);
				vString *const vIdent = vStringNew();
				cp = parseIdentifier(cp, vIdent);
				
				if (vStringLength(vIdent) > 0)
				{
					vString *const vFullName = makeTagFullname(vStringValue(vIdent));
					module = strdup(vStringValue(vFullName));
					vStringDelete(vFullName);
				}
				vStringDelete(vIdent);
			}
		}
		Scope currScope = getCurrentScope();
		if (kind == K_FUNCTION && (currScope.kind == K_PROTO ||
								   currScope.kind == K_IMPL))
			kind = K_PROTOFUNC;
		
		const char *ident = vStringValue(identifier);
		makeTag(ident, kind, private, currScope, module, ident);
		FREE_SCOPE(currScope);
		free(module);
	}
	vStringDelete(identifier);
	return cp;
}

static const unsigned char *parseSimpleTag(const unsigned char *cp,
										   elixirKind kind, bool private,
										   const char *name)
{
	Scope currScope = getCurrentScope();
	
	if (kind == K_ATTRIBUTE)
		makeTag(name, kind, private, currScope, NULL, name);
	else if (currScope.name) // K_STRUCT
		makeTag(currScope.name, kind, private, currScope, NULL, currScope.displayName);
	
	FREE_SCOPE(currScope);
	return cp;
}

static unsigned int contextual_fake_count = 0;

static const unsigned char *parseOtherTag(const unsigned char *cp,
										  const char *keyword,
										  bool is_code_block, int indent)
{
	const char prefix = *cp;
	
	vString *const vDispName = vStringNewInit(keyword);
	vStringPut(vDispName, ' ');
	vStringPut(vDispName, *cp++);
	
	if (prefix == '"')
	{
		while (*cp)
		{
			if (*cp == '\\')
			{
				if (*(cp + 1))
				{
					vStringPut(vDispName, *cp++);
					vStringPut(vDispName, *cp++);
				}
				else
				{
					vStringPut(vDispName, '"');
					break;
				}
			}
			else if (*cp == '"')
			{
				vStringPut(vDispName, *cp++);
				break;
			}
			else
				vStringPut(vDispName, *cp++);
		}
	}
	else if (prefix == ':')
	{
		while (isalnum(*cp) || *cp == '_')
			vStringPut(vDispName, *cp++);
	}
	else
	{
		while (isalnum(*cp) || *cp == '_' || *cp == '.')
			vStringPut(vDispName, *cp++);
	}
	
	const char *dispName = vStringValue(vDispName);
	
	char name[64];
	sprintf(name, "%s_%d", keyword, contextual_fake_count++);
	
	Scope currScope = getCurrentScope();
	int r = makeTag(name, K_OTHER, false, currScope, NULL, dispName);
	
	vStringDelete(vDispName);
	FREE_SCOPE(currScope);
	
	if (is_code_block)
	{
		NestingLevel *nl = nestingLevelsPush(nesting, r);
		EX_NL_INDENTATION(nl) = indent;
	}
	return cp;
}

#define DEFINE_ALIAS_MODULE(exclude_count)										\
	if (strncmp(ident, "__MODULE__", 10) == 0)									\
	{																			\
		vAliasModule = vStringNewInit(currScope.name);							\
		const char *suffix = strchr(ident, SCOPE_SEPARATOR);					\
		if (suffix && suffix[1])												\
			vStringNCatS(vAliasModule, suffix, strlen(suffix) - exclude_count);	\
	}																			\
	else																		\
		vAliasModule = vStringNewNInit(ident, len - exclude_count);

static const unsigned char *parseGrpAliasTag(const unsigned char *cp)
{
	for (; *cp; cp++)
	{
		cp = skipSpace(cp);
		if (isupper(*cp))
		{
			vString *const vAlias = vStringNew();
			cp = parseIdentifier(cp, vAlias);
			cp = skipSpace(cp);
			
			if (vStringLength(vAlias) > 0)
			{
				vString *const vFullModule = vStringNewCopy(vAliasModule);
				if (vStringLength(vFullModule) > 0)
					vStringPut(vFullModule, SCOPE_SEPARATOR);
				vStringCat(vFullModule, vAlias);
				
				const char *alias = vStringValue(vAlias);
				const char *search = strrchr(alias, SCOPE_SEPARATOR);
				
				// if SCOPE_SEPARATOR is found and there is something else after it
				// and there is something else before it
				if (search && search[1] && search > alias)
					search++; // skip SCOPE_SEPARATOR
				else
					search = alias;
				
				Scope currScope = getCurrentScope();
				makeTag(search, K_SPECIAL, true, currScope,
						vStringValue(vFullModule), search);
				vStringDelete(vFullModule);
				FREE_SCOPE(currScope);
			}
			vStringDelete(vAlias);
		}
		
		if (!*cp)
			break;
		else if (*cp != ',')
		{
			FREE_ALIAS_MODULE;
			if (*cp == '}') cp++;
			break;
		}
	}
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
		Scope currScope = getCurrentScope();
		const char *const ident = vStringValue(identifier);
		
		if (ident[len - 1] == SCOPE_SEPARATOR && *cp == '{')
		{	// alias Sayings.{Greetings, Farewells, Parent.Child}
			DEFINE_ALIAS_MODULE(1);
			cp = parseGrpAliasTag(++cp);
		}
		else if (*cp == ',')
		{	// alias Df.Repo.Users.Storage, as: UsersStorage
			cp = skipSpace(++cp);
			while (islower(*cp)) cp++; // skip "as" keyword
			
			if (*cp == ':')
			{
				cp = skipSpace(++cp);
				if (isupper(*cp))
				{
					vString *const vAlias = vStringNew();
					cp = parseIdentifier(cp, vAlias);
					
					if (vStringLength(vAlias) > 0)
					{
						DEFINE_ALIAS_MODULE(0);
						const char *alias = vStringValue(vAlias);
						makeTag(alias, K_SPECIAL, true, currScope,
								vStringValue(vAliasModule), alias);
						FREE_ALIAS_MODULE;
					}
					vStringDelete(vAlias);
				}
			}
		}
		else
		{	// alias Df.Repo.User
			const char *search = strrchr(ident, SCOPE_SEPARATOR);
			
			// if SCOPE_SEPARATOR is found and there is something else after it
			// and there is something else before it
			if (search && search[1] && search > ident)
			{
				DEFINE_ALIAS_MODULE(0);
				search++; // skip SCOPE_SEPARATOR
				makeTag(search, K_SPECIAL, true, currScope,
						vStringValue(vAliasModule), search);
				FREE_ALIAS_MODULE;
			}
		}
		FREE_SCOPE(currScope);
	}
	vStringDelete(identifier);
	return cp;
}

static const unsigned char *parseSpecialTag(const unsigned char *cp, const char *name)
{
	vString *const identifier = vStringNew();
	cp = parseIdentifier(cp, identifier);
	
	if (vStringLength(identifier) > 0)
	{
		Scope currScope = getCurrentScope();
		vString *const vFullName = makeTagFullname(vStringValue(identifier));
		makeTag(name, K_SPECIAL, true, currScope, vStringValue(vFullName), name);
		vStringDelete(vFullName);
		FREE_SCOPE(currScope);
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
	else if (strcmp(kwval, "defstruct") == 0)
		cp = parseSimpleTag(cp, K_STRUCT, false, NULL);
	else if (strcmp(kwval, "defexception") == 0)
		cp = parseSimpleTag(cp, K_STRUCT, false, NULL);
	else if (strcmp(kwval, "defoverridable") == 0)
		/* skip */;
	else if (strcmp(kwval, "defdelegate") == 0)
		cp = parseMemberTag(cp, K_DELEGATE, false);
	else if (strcmp(kwval, "defp") == 0)
		cp = parseMemberTag(cp, K_FUNCTION, true);
	else if (strcmp(kwval, "def") == 0)
		cp = parseMemberTag(cp, K_FUNCTION, false);
	else if (strcmp(kwval, "defmemop") == 0)
		cp = parseMemberTag(cp, K_FUNCTION, true);
	else if (strcmp(kwval, "defmemo") == 0)
		cp = parseMemberTag(cp, K_FUNCTION, false);
	else if (strncmp(kwval, "def", 3) == 0) // def... - user-defined keyword
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
	else if ((strcmp(kwval, "describe") == 0 || strcmp(kwval, "test") == 0 ||
			  strcmp(kwval, "pipeline") == 0 || strcmp(kwval, "scope") == 0)
			 && (*cp == '"' || *cp == ':' || isupper(*cp)))
		cp = parseOtherTag(cp, kwval, true, indent);
	else if ((strcmp(kwval, "get") == 0 || strcmp(kwval, "post") == 0 ||
			  strcmp(kwval, "put") == 0 || strcmp(kwval, "delete") == 0 ||
			  strcmp(kwval, "forward") == 0 || strcmp(kwval, "resources") == 0 ||
			  strcmp(kwval, "pipe_through") == 0 || strcmp(kwval, "plug") == 0 ||
			  strcmp(kwval, "live_dashboard") == 0)
			 && (*cp == '"' || *cp == ':' || isupper(*cp)))
		cp = parseOtherTag(cp, kwval, false, indent);
	else if (strcmp(kwval, "alias") == 0)
		cp = parseAliasTag(cp);
	else if (strcmp(kwval, "use") == 0)
		cp = parseSpecialTag(cp, "<use>");
	else if (strcmp(kwval, "import") == 0)
		cp = parseSpecialTag(cp, "<import>");
	else if (*kwval == '@' && kwval[1] && *cp &&
			 (isalnum(*cp) || strchr("\"{[(%:~_", *cp)))
		cp = parseSimpleTag(cp, K_ATTRIBUTE, true, kwval);
	else if (strcmp(kwval, "end") == 0)
	{
		NestingLevel *nl = nestingLevelsGetCurrent(nesting);
		
		if (nl && EX_NL_INDENTATION(nl) == indent)
		{
			tagEntryInfo *tag = getEntryOfNestingLevel(nl);
			tag->extensionFields.endLine = getInputLineNumber();
			nestingLevelsPop(nesting);
		}
	}
	vStringDelete(keyword);
	return cp;
}

static void findElixirTags(void)
{
	stringStack = ptrArrayNew(NULL);
	nesting = nestingLevelsNew(sizeof(struct nlUserData));
	
	if (vAliasModule) FREE_ALIAS_MODULE;
	
	const unsigned char *line;
	
	while ((line = readLineFromInputFile()) != NULL)
	{
		const unsigned char *cp = skipSpace(line);
		
		if (ptrArrayCount(stringStack) > 0)
		{
			checkMultilineString(cp);
			continue;
		}
		else if (vAliasModule)
		{
			cp = parseGrpAliasTag(cp);
			if (vAliasModule) continue;
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
