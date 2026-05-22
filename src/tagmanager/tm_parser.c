/*
 *      tm_parser.c - this file is part of Geany, a fast and lightweight IDE
 *
 *      Copyright 2016 The Geany contributors
 *
 *      This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2 of the License, or
 *      (at your option) any later version.
 *
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *
 *      You should have received a copy of the GNU General Public License along
 *      with this program; if not, write to the Free Software Foundation, Inc.,
 *      51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <string.h>
#include <ctype.h>

#include "tm_parser.h"
#include "ctags-api.h"
#include "../utils.h"


typedef struct
{
	const gchar kind;
	TMTagType type;
} TMParserMapEntry;

/* Allows remapping a subparser tag type to another type if there's a clash with
 * the master parser tag type. Only subparser tag types explicitly listed within
 * TMSubparserMapEntry maps are added to tag manager - tags with types not listed
 * are discarded to prevent uncontrolled merging of tags from master parser and
 * subparsers. */
typedef struct
{
	TMTagType orig_type;
	TMTagType new_type;
} TMSubparserMapEntry;


static GHashTable *subparser_map = NULL;


static TMParserMapEntry map_C[] = {
	{'c', tm_tag_class_t},		// classes
	{'d', tm_tag_macro_t},		// macro definitions
	{'e', tm_tag_enumerator_t},	// enumerators (values inside an enumeration)
	{'f', tm_tag_function_t},	// function definitions
	{'g', tm_tag_enum_t},		// enumeration names
	{'m', tm_tag_member_t},		// class, struct, and union members
	{'n', tm_tag_namespace_t},	// namespaces
	{'p', tm_tag_prototype_t},	// function prototypes
	{'s', tm_tag_struct_t},		// structure names
	{'t', tm_tag_typedef_t},	// typedefs
	{'u', tm_tag_union_t},		// union names
	{'v', tm_tag_variable_t},	// variable definitions
	{'x', tm_tag_externvar_t},	// external variable declarations
};

/* C++, same as C */
#define map_CPP map_C

static TMParserMapEntry map_JAVA[] = {
	{'c', tm_tag_class_t},		// classes
	{'f', tm_tag_field_t},		// fields
	{'i', tm_tag_interface_t},	// interfaces
	{'m', tm_tag_method_t},		// methods
	{'p', tm_tag_package_t},	// packages
	{'e', tm_tag_enumerator_t},	// enum constants
	{'g', tm_tag_enum_t},		// enum types
};

static TMParserMapEntry map_MAKEFILE[] = {
	{'m', tm_tag_macro_t},		// macros
	{'t', tm_tag_function_t},	// targets
};

static TMParserMapEntry map_PASCAL[] = {
	{'f', tm_tag_function_t},	// functions
	{'p', tm_tag_function_t},	// procedures
};

static TMParserMapEntry map_PERL[] = {
	{'c', tm_tag_enum_t},		// constants
	{'f', tm_tag_other_t},		// formats
	{'l', tm_tag_macro_t},		// labels
	{'p', tm_tag_package_t},	// packages
	{'s', tm_tag_function_t},	// subroutines
	{'d', tm_tag_prototype_t},	// subroutine declarations
};

static TMParserMapEntry map_PHP[] = {
	{'c', tm_tag_class_t},		// classes
	{'d', tm_tag_macro_t},		// constant definitions
	{'f', tm_tag_function_t},	// functions
	{'i', tm_tag_interface_t},	// interfaces
	{'l', tm_tag_undef_t},		// local variables
	{'n', tm_tag_namespace_t},	// namespaces
	{'t', tm_tag_struct_t},		// traits
	{'v', tm_tag_variable_t},	// variables
};

static TMParserMapEntry map_PYTHON[] = {
	{'c', tm_tag_class_t},		// classes
	{'m', tm_tag_method_t},		// class members/methods
	{'f', tm_tag_function_t},	// functions
	{'v', tm_tag_variable_t},	// variables
	/* defined as externvar to get those excluded as forward type in symbols.c:goto_tag()
	 * so we can jump to the real implementation (if known) instead of to the import statement */
	{'x', tm_tag_externvar_t},	// name referring a class/variable/function/module defined in other module
};

/* different parser than tex.c from universal-ctags */
static TMParserMapEntry map_LATEX[] = {
	{'f', tm_tag_function_t},	// command definitions
	{'c', tm_tag_class_t},		// environment definitions
	{'m', tm_tag_member_t},		// labels, sections and bibliography
	{'d', tm_tag_macro_t},		// subsections
	{'v', tm_tag_variable_t},	// subsubsections
	{'n', tm_tag_namespace_t},	// chapters
	{'s', tm_tag_struct_t},		// labels and bibliography
};

static TMParserMapEntry map_ASM[] = {
	{'d', tm_tag_macro_t},		// defines
	{'l', tm_tag_namespace_t},	// labels
	{'m', tm_tag_function_t},	// macros
	{'t', tm_tag_struct_t},		// types (structs and records)
};

/* not in universal-ctags */
static TMParserMapEntry map_CONF[] = {
	{'n', tm_tag_namespace_t},	// sections
	{'m', tm_tag_macro_t},		// keys
};

static TMParserMapEntry map_SQL[] = {
	{'c', tm_tag_undef_t},		// cursors
	{'d', tm_tag_prototype_t},	// prototypes
	{'f', tm_tag_function_t},	// functions
	{'F', tm_tag_field_t},		// record fields
	{'l', tm_tag_undef_t},		// local variables
	{'L', tm_tag_undef_t},		// block label
	{'P', tm_tag_package_t},	// packages
	{'p', tm_tag_namespace_t},	// procedures
	{'r', tm_tag_undef_t},		// records
	{'s', tm_tag_undef_t},		// subtypes
	{'t', tm_tag_class_t},		// tables
	{'T', tm_tag_macro_t},		// triggers
	{'v', tm_tag_variable_t},	// variables
	{'i', tm_tag_struct_t},		// indexes
	{'e', tm_tag_undef_t},		// events
	{'U', tm_tag_undef_t},		// publications
	{'R', tm_tag_undef_t},		// services
	{'D', tm_tag_undef_t},		// domains
	{'V', tm_tag_member_t},		// views
	{'n', tm_tag_undef_t},		// synonyms
	{'x', tm_tag_undef_t},		// MobiLink Table Scripts
	{'y', tm_tag_undef_t},		// MobiLink Conn Scripts
	{'z', tm_tag_undef_t},		// MobiLink Properties
};

/* not in universal-ctags */
static TMParserMapEntry map_DOCBOOK[] = {
	{'f', tm_tag_function_t},	// chapters
	{'c', tm_tag_class_t},		// sections
	{'m', tm_tag_member_t},		// sect1
	{'d', tm_tag_macro_t},		// sect2
	{'v', tm_tag_variable_t},	// sect3
	{'s', tm_tag_struct_t},		// appendix
};

static TMParserMapEntry map_ELIXIR[] = {
	{'m', tm_tag_namespace_t},	// modules
	{'p', tm_tag_interface_t},	// protocols
	{'i', tm_tag_struct_t},		// protocol implementations
	{'a', tm_tag_variable_t},	// module attributes
	{'f', tm_tag_function_t},	// functions
	{'d', tm_tag_function_t},	// delegates
	{'M', tm_tag_macro_t},		// macros
	{'t', tm_tag_typedef_t},	// type definitions
	{'s', tm_tag_other_t},		// special definitions (alias, use, etc.)
};

static TMParserMapEntry map_ERLANG[] = {
	{'d', tm_tag_macro_t},		// macro definitions
	{'f', tm_tag_function_t},	// functions
	{'m', tm_tag_undef_t},		// modules
	{'r', tm_tag_struct_t},		// record definitions
	{'t', tm_tag_typedef_t},	// type definitions
};

static TMParserMapEntry map_CSS[] = {
	{'c', tm_tag_class_t},		// classes
	{'s', tm_tag_struct_t},		// selectors
	{'i', tm_tag_variable_t},	// identities
};

static TMParserMapEntry map_RUBY[] = {
	{'c', tm_tag_class_t},		// classes
	{'f', tm_tag_method_t},		// methods
	{'m', tm_tag_namespace_t},	// modules
	{'F', tm_tag_member_t},		// singleton methods
};

static TMParserMapEntry map_TCL[] = {
	{'c', tm_tag_class_t},		// classes
	{'m', tm_tag_member_t},		// methods
	{'p', tm_tag_function_t},	// procedures
	{'n', tm_tag_namespace_t},	// modules
};

static TMParserMapEntry map_SH[] = {
	{'f', tm_tag_function_t},	// functions
};

static TMParserMapEntry map_D[] = {
	{'c', tm_tag_class_t},		// classes
	{'e', tm_tag_enumerator_t},	// enumerators (values inside an enumeration)
	{'f', tm_tag_function_t},	// function definitions
	{'g', tm_tag_enum_t},		// enumeration names
	{'i', tm_tag_interface_t},	// interfaces
	{'m', tm_tag_member_t},		// class, struct, and union members
	{'n', tm_tag_namespace_t},	// namespaces
	{'p', tm_tag_prototype_t},	// function prototypes
	{'s', tm_tag_struct_t},		// structure names
	{'t', tm_tag_typedef_t},	// typedefs
	{'u', tm_tag_union_t},		// union names
	{'v', tm_tag_variable_t},	// variable definitions
	{'x', tm_tag_externvar_t},	// external variable declarations
};

static TMParserMapEntry map_DIFF[] = {
	{'f', tm_tag_function_t},	// functions
};

/* different parser than in universal-ctags */
static TMParserMapEntry map_VHDL[] = {
	{'c', tm_tag_variable_t},	// constants
	{'t', tm_tag_typedef_t},	// types
	{'v', tm_tag_variable_t},	// variables
	{'a', tm_tag_undef_t},		// attributes
	{'s', tm_tag_variable_t},	// signals
	{'f', tm_tag_function_t},	// functions
	{'p', tm_tag_function_t},	// procedure
	{'k', tm_tag_member_t},		// components
	{'l', tm_tag_namespace_t},	// packages
	{'m', tm_tag_member_t},		// process
	{'n', tm_tag_class_t},		// entity
	{'o', tm_tag_struct_t},		// architecture
	{'u', tm_tag_undef_t},		// ports
	{'b', tm_tag_member_t},		// blocks
	{'A', tm_tag_typedef_t},	// alias
};

static TMParserMapEntry map_LUA[] = {
	{'f', tm_tag_function_t},	// functions
};

static TMParserMapEntry map_JAVASCRIPT[] = {
	{'f', tm_tag_function_t},	// functions
	{'c', tm_tag_class_t},		// classes
	{'m', tm_tag_method_t},		// methods
	{'p', tm_tag_member_t},		// properties
	{'C', tm_tag_macro_t},		// constants
	{'v', tm_tag_variable_t},	// global variables
	{'g', tm_tag_function_t},	// generators
};

/* not in universal-ctags */
static TMParserMapEntry map_HASKELL[] = {
	{'t', tm_tag_typedef_t},	// types
	{'c', tm_tag_macro_t},		// type constructors
	{'f', tm_tag_function_t},	// functions
	{'m', tm_tag_namespace_t},	// modules
};

static TMParserMapEntry map_CSHARP[] = {
	{'c', tm_tag_class_t},		// classes
	{'d', tm_tag_macro_t},		// macro definitions
	{'e', tm_tag_enumerator_t},	// enumerators (values inside an enumeration)
	{'E', tm_tag_undef_t},		// events
	{'f', tm_tag_field_t},		// fields
	{'g', tm_tag_enum_t},		// enumeration names
	{'i', tm_tag_interface_t},	// interfaces
	{'l', tm_tag_undef_t},		// local variables
	{'m', tm_tag_method_t},		// methods
	{'n', tm_tag_namespace_t},	// namespaces
	{'p', tm_tag_undef_t},		// properties
	{'s', tm_tag_struct_t},		// structure names
	{'t', tm_tag_typedef_t},	// typedefs
};

static TMParserMapEntry map_FREEBASIC[] = {
	{'c', tm_tag_macro_t},		// constants
	{'f', tm_tag_function_t},	// functions
	{'l', tm_tag_namespace_t},	// labels
	{'t', tm_tag_struct_t},		// types
	{'v', tm_tag_variable_t},	// variables
	{'g', tm_tag_externvar_t},	// enumerations
};

/* not in universal-ctags */
static TMParserMapEntry map_HAXE[] = {
	{'m', tm_tag_method_t},		// methods
	{'c', tm_tag_class_t},		// classes
	{'e', tm_tag_enum_t},		// enumerations
	{'v', tm_tag_variable_t},	// variables
	{'i', tm_tag_interface_t},	// interfaces
	{'t', tm_tag_typedef_t},	// typedefs
};

static TMParserMapEntry map_REST[] = {
	{'c', tm_tag_namespace_t},	// chapters
	{'s', tm_tag_member_t},		// sections
	{'S', tm_tag_macro_t},		// subsections
	{'t', tm_tag_variable_t},	// subsubsections
	{'T', tm_tag_undef_t},		// targets
};

static TMParserMapEntry map_HTML[] = {
	{'a', tm_tag_member_t},		// named anchors
	{'h', tm_tag_namespace_t},	// H1 headings
	{'i', tm_tag_class_t},		// H2 headings
	{'j', tm_tag_variable_t},	// H3 headings
};

static TMSubparserMapEntry subparser_HTML_javascript_map[] = {
	{tm_tag_function_t, tm_tag_function_t},
};

static TMParserMapEntry map_F77[] = {
	{'b', tm_tag_undef_t},		// block data
	{'c', tm_tag_macro_t},		// common blocks
	{'e', tm_tag_undef_t},		// entry points
	{'f', tm_tag_function_t},	// functions
	{'i', tm_tag_interface_t},	// interface contents, generic names, and operators
	{'k', tm_tag_member_t},		// type and structure components
	{'l', tm_tag_undef_t},		// labels
	{'L', tm_tag_undef_t},		// local, common block, and namelist variables
	{'m', tm_tag_namespace_t},	// modules
	{'n', tm_tag_undef_t},		// namelists
	{'p', tm_tag_struct_t},		// programs
	{'s', tm_tag_method_t},		// subroutines
	{'t', tm_tag_class_t},		// derived types and structures
	{'v', tm_tag_variable_t},	// program (global) and module variables
	{'E', tm_tag_enum_t},		// enumerations
	{'N', tm_tag_enumerator_t},	// enumeration values
};

#define map_FORTRAN map_F77

#define map_FERITE map_C

/* different parser than in universal-ctags */
static TMParserMapEntry map_MATLAB[] = {
	{'f', tm_tag_function_t},	// Functions
	{'s', tm_tag_struct_t},		// Structures
};

#define map_GLSL map_C

/* not in universal-ctags */
static TMParserMapEntry map_VALA[] = {
	{'c', tm_tag_class_t},		// classes
	{'d', tm_tag_macro_t},		// macro definitions
	{'e', tm_tag_enumerator_t},	// enumerators (values inside an enumeration)
	{'f', tm_tag_field_t},		// fields
	{'g', tm_tag_enum_t},		// enumeration names
	{'i', tm_tag_interface_t},	// interfaces
	{'l', tm_tag_undef_t},		// local variables
	{'m', tm_tag_method_t},		// methods
	{'n', tm_tag_namespace_t},	// namespaces
	{'p', tm_tag_undef_t},		// properties
	{'S', tm_tag_undef_t},		// signals
	{'s', tm_tag_struct_t},		// structure names
};

/* not in universal-ctags */
static TMParserMapEntry map_ACTIONSCRIPT[] = { // see ctags/parsers/flex.c
	{'f', tm_tag_function_t},	// functions
	{'c', tm_tag_class_t},		// classes
	{'i', tm_tag_interface_t},	// interfaces
	{'P', tm_tag_package_t},	// packages
	{'m', tm_tag_method_t},		// methods
	{'p', tm_tag_member_t},		// properties
	{'v', tm_tag_variable_t},	// global variables
	{'l', tm_tag_variable_t},	// local variables
	{'C', tm_tag_macro_t},		// constants
	{'I', tm_tag_externvar_t},	// imports
	{'x', tm_tag_other_t},		// mxtags
};

/* not in universal-ctags */
static TMParserMapEntry map_NSIS[] = {
	{'n', tm_tag_namespace_t},	// sections
	{'f', tm_tag_function_t},	// functions
	{'v', tm_tag_variable_t},	// variables
};

/* not in universal-ctags */
static TMParserMapEntry map_MARKDOWN[] = {
	{'v', tm_tag_variable_t},	// sections
};

/* not in universal-ctags */
static TMParserMapEntry map_TXT2TAGS[] = {
	{'m', tm_tag_member_t},		// sections
};

/* not in universal-ctags */
static TMParserMapEntry map_ABC[] = {
	{'m', tm_tag_member_t},		// sections
	{'s', tm_tag_struct_t},		// header1
};

static TMParserMapEntry map_VERILOG[] = {
	{'c', tm_tag_variable_t},	// constants (define, parameter, specparam)
	{'e', tm_tag_typedef_t},	// events
	{'f', tm_tag_function_t},	// functions
	{'m', tm_tag_class_t},		// modules
	{'n', tm_tag_variable_t},	// net data types
	{'p', tm_tag_variable_t},	// ports
	{'r', tm_tag_variable_t},	// register data types
	{'t', tm_tag_function_t},	// tasks
};

static TMParserMapEntry map_R[] = {
	{'f', tm_tag_function_t},	// functions
	{'l', tm_tag_other_t},		// libraries
	{'s', tm_tag_other_t},		// sources
};

static TMParserMapEntry map_COBOL[] = {
	{'d', tm_tag_variable_t},	// data items
	{'D', tm_tag_interface_t},	// divisions
	{'f', tm_tag_function_t},	// file descriptions (FD, SD, RD)
	{'g', tm_tag_struct_t},		// group items
	{'p', tm_tag_macro_t},		// paragraphs
	{'P', tm_tag_class_t},		// program ids
	{'s', tm_tag_namespace_t},	// sections
	{'S', tm_tag_externvar_t},	// source code file
};

static TMParserMapEntry map_OBJC[] = {
	{'i', tm_tag_interface_t},	// class interface
	{'I', tm_tag_undef_t},		// class implementation
	{'P', tm_tag_undef_t},		// Protocol
	{'m', tm_tag_method_t},		// Object's method
	{'c', tm_tag_class_t},		// Class's method
	{'v', tm_tag_variable_t},	// Global variable
	{'F', tm_tag_field_t},		// Object field
	{'f', tm_tag_function_t},	// A function
	{'p', tm_tag_undef_t},		// A property
	{'t', tm_tag_typedef_t},	// A type alias
	{'s', tm_tag_struct_t},		// A type structure
	{'e', tm_tag_enum_t},		// An enumeration
	{'M', tm_tag_macro_t},		// A preprocessor macro
};

static TMParserMapEntry map_ASCIIDOC[] = {
	{'c', tm_tag_namespace_t},	// chapters
	{'s', tm_tag_member_t},		// sections
	{'S', tm_tag_macro_t},		// level 2 sections
	{'t', tm_tag_variable_t},	// level 3 sections
	{'T', tm_tag_struct_t},		// level 4 sections
	{'u', tm_tag_undef_t},		// level 5 sections
	{'a', tm_tag_undef_t},		// anchors
};

/* not in universal-ctags */
static TMParserMapEntry map_ABAQUS[] = {
	{'c', tm_tag_class_t},		// Parts
	{'m', tm_tag_member_t},		// Assembly
	{'n', tm_tag_interface_t},	// Steps
};

static TMParserMapEntry map_RUST[] = {
	{'n', tm_tag_namespace_t},	// module
	{'s', tm_tag_struct_t},		// structural type
	{'i', tm_tag_interface_t},	// trait interface
	{'c', tm_tag_class_t},		// implementation
	{'f', tm_tag_function_t},	// Function
	{'g', tm_tag_enum_t},		// Enum
	{'t', tm_tag_typedef_t},	// Type Alias
	{'v', tm_tag_variable_t},	// Global variable
	{'M', tm_tag_macro_t},		// Macro Definition
	{'m', tm_tag_field_t},		// A struct field
	{'e', tm_tag_enumerator_t},	// An enum variant
	{'F', tm_tag_method_t},		// A method
};

static TMParserMapEntry map_GO[] = {
	{'p', tm_tag_namespace_t},	// packages
	{'f', tm_tag_function_t},	// functions
	{'c', tm_tag_macro_t},		// constants
	{'t', tm_tag_typedef_t},	// types
	{'v', tm_tag_variable_t},	// variables
	{'s', tm_tag_struct_t},		// structs
	{'i', tm_tag_interface_t},	// interfaces
	{'m', tm_tag_member_t},		// struct members
};

static TMParserMapEntry map_JSON[] = {
	{'o', tm_tag_member_t},		// objects
	{'a', tm_tag_member_t},		// arrays
	{'n', tm_tag_member_t},		// numbers
	{'s', tm_tag_member_t},		// strings
	{'b', tm_tag_member_t},		// booleans
	{'z', tm_tag_member_t},		// nulls
};

/* Zephir, same as PHP */
#define map_ZEPHIR map_PHP

/* not in universal-ctags */
static TMParserMapEntry map_POWERSHELL[] = {
	{'f', tm_tag_function_t},	// functions
	{'v', tm_tag_variable_t},	// variables
};


typedef struct
{
	TMParserMapEntry *entries;
	guint size;
} TMParserMap;

#define MAP_ENTRY(lang) \
	[TM_PARSER_##lang] = {map_##lang, G_N_ELEMENTS(map_##lang)}

/* keep in sync with TM_PARSER_* definitions in the header */
static TMParserMap parser_map[] = {
	MAP_ENTRY(C),
	MAP_ENTRY(CPP),
	MAP_ENTRY(JAVA),
	MAP_ENTRY(MAKEFILE),
	MAP_ENTRY(PASCAL),
	MAP_ENTRY(PERL),
	MAP_ENTRY(PHP),
	MAP_ENTRY(PYTHON),
	MAP_ENTRY(LATEX),
	MAP_ENTRY(ASM),
	MAP_ENTRY(CONF),
	MAP_ENTRY(SQL),
	MAP_ENTRY(DOCBOOK),
	MAP_ENTRY(ELIXIR),
	MAP_ENTRY(ERLANG),
	MAP_ENTRY(CSS),
	MAP_ENTRY(RUBY),
	MAP_ENTRY(TCL),
	MAP_ENTRY(SH),
	MAP_ENTRY(D),
	MAP_ENTRY(FORTRAN),
	MAP_ENTRY(FERITE),
	MAP_ENTRY(DIFF),
	MAP_ENTRY(VHDL),
	MAP_ENTRY(LUA),
	MAP_ENTRY(JAVASCRIPT),
	MAP_ENTRY(HASKELL),
	MAP_ENTRY(CSHARP),
	MAP_ENTRY(FREEBASIC),
	MAP_ENTRY(HAXE),
	MAP_ENTRY(REST),
	MAP_ENTRY(HTML),
	MAP_ENTRY(F77),
	MAP_ENTRY(GLSL),
	MAP_ENTRY(MATLAB),
	MAP_ENTRY(VALA),
	MAP_ENTRY(ACTIONSCRIPT),
	MAP_ENTRY(NSIS),
	MAP_ENTRY(MARKDOWN),
	MAP_ENTRY(TXT2TAGS),
	MAP_ENTRY(ABC),
	MAP_ENTRY(VERILOG),
	MAP_ENTRY(R),
	MAP_ENTRY(COBOL),
	MAP_ENTRY(OBJC),
	MAP_ENTRY(ASCIIDOC),
	MAP_ENTRY(ABAQUS),
	MAP_ENTRY(RUST),
	MAP_ENTRY(GO),
	MAP_ENTRY(JSON),
	MAP_ENTRY(ZEPHIR),
	MAP_ENTRY(POWERSHELL),
};
/* make sure the parser map is consistent and complete */
G_STATIC_ASSERT(G_N_ELEMENTS(parser_map) == TM_PARSER_COUNT);


TMTagType tm_parser_get_tag_type(gchar kind, TMParserType lang)
{
	TMParserMap *map = &parser_map[lang];
	
	for (guint i = 0; i < map->size; i++)
	{
		TMParserMapEntry *entry = &map->entries[i];
		
		if (entry->kind == kind)
			return entry->type;
	}
	return tm_tag_undef_t;
}


gchar tm_parser_get_tag_kind(TMTagType type, TMParserType lang)
{
	TMParserMap *map = &parser_map[lang];
	
	for (guint i = 0; i < map->size; i++)
	{
		TMParserMapEntry *entry = &map->entries[i];
		
		if (entry->type == type)
			return entry->kind;
	}
	return '\0';
}


static void add_subparser(TMParserType lang, TMParserType sublang,
						  TMSubparserMapEntry *map, guint map_size)
{
	GHashTable *lang_map = g_hash_table_lookup(subparser_map,
											   GINT_TO_POINTER(lang));
	if (!lang_map)
	{
		lang_map = g_hash_table_new(g_direct_hash, g_direct_equal);
		g_hash_table_insert(subparser_map, GINT_TO_POINTER(lang), lang_map);
	}
	
	GPtrArray *mapping = g_ptr_array_new();
	for (guint i = 0; i < map_size; i++)
		g_ptr_array_add(mapping, &map[i]);
	
	g_hash_table_insert(lang_map, GINT_TO_POINTER(sublang), mapping);
}


#define SUBPARSER_MAP_ENTRY(lang, sublang, map)				\
	add_subparser(TM_PARSER_##lang, TM_PARSER_##sublang,	\
				  map, G_N_ELEMENTS(map))

static void init_subparser_map(void)
{
	SUBPARSER_MAP_ENTRY(HTML, JAVASCRIPT, subparser_HTML_javascript_map);
}


TMTagType tm_parser_get_subparser_type(TMParserType lang, TMParserType sublang,
									   TMTagType type)
{
	if (!subparser_map)
	{
		subparser_map = g_hash_table_new(g_direct_hash, g_direct_equal);
		init_subparser_map();
	}
	
	GHashTable *lang_map = g_hash_table_lookup(subparser_map,
											   GINT_TO_POINTER(lang));
	if (!lang_map)
		return tm_tag_undef_t;
	
	GPtrArray *mapping = g_hash_table_lookup(lang_map, GINT_TO_POINTER(sublang));
	if (!mapping)
		return tm_tag_undef_t;
	
	for (guint i = 0; i < mapping->len; i++)
	{
		TMSubparserMapEntry *entry = mapping->pdata[i];
		if (entry->orig_type == type)
			return entry->new_type;
	}
	return tm_tag_undef_t;
}


void tm_parser_verify_type_mappings(void)
{
	if (TM_PARSER_COUNT > ctagsGetLangCount())
		g_error("More parsers defined in Geany than in ctags");
	
	for (TMParserType lang = 0; lang < TM_PARSER_COUNT; lang++)
	{
		const gchar *kinds = ctagsGetLangKinds(lang);
		TMParserMap *map = &parser_map[lang];
		
		if (!map->entries || map->size < 1)
			g_error("No tag types in TM for %s, is the language listed in parser_map?",
					ctagsGetLangName(lang));
		
		/* TODO: check also regex parser mappings. At the moment there's no way
		 * to access regex parser definitions in ctags */
		if (ctagsIsUsingRegexParser(lang))
			continue;
		
		if (map->size != strlen(kinds))
			g_error("Different number of tag types in TM (%d) and ctags (%d) for %s",
					map->size, (int)strlen(kinds), ctagsGetLangName(lang));
		
		gchar presence_map[256];
		memset(presence_map, 0, sizeof(presence_map));
		
		for (guint i = 0; i < map->size; i++)
		{
			gboolean ctags_found = FALSE;
			gboolean tm_found = FALSE;
			
			for (guint j = 0; j < map->size; j++)
			{
				/* check that for every type in TM there's a type in ctags */
				if (map->entries[i].kind == kinds[j])
					ctags_found = TRUE;
				/* check that for every type in ctags there's a type in TM */
				if (map->entries[j].kind == kinds[i])
					tm_found = TRUE;
				if (ctags_found && tm_found)
					break;
			}
			if (!ctags_found)
				g_error("Tag type '%c' found in TM but not in ctags for %s",
						map->entries[i].kind, ctagsGetLangName(lang));
			if (!tm_found)
				g_error("Tag type '%c' found in ctags but not in TM for %s",
						kinds[i], ctagsGetLangName(lang));
			
			presence_map[(unsigned char)map->entries[i].kind]++;
		}
		
		for (guint i = 0; i < sizeof(presence_map); i++)
		{
			if (presence_map[i] > 1)
				g_error("Duplicate tag type '%c' found for %s",
						(gchar)i, ctagsGetLangName(lang));
		}
	}
}


const gchar *tm_parser_context_separator(TMParserType lang)
{
	switch (lang)
	{
		case TM_PARSER_ERLANG:
			return ":";
		
		case TM_PARSER_C:	/* for C++ .h headers or C structs */
		case TM_PARSER_CPP:
		case TM_PARSER_GLSL:	/* for structs */
		/*case GEANY_FILETYPES_RUBY:*/ /* not sure what to use atm*/
		case TM_PARSER_PHP:
		case TM_PARSER_POWERSHELL:
		case TM_PARSER_RUST:
		case TM_PARSER_ZEPHIR:
			return "::";
		
		/* avoid confusion with other possible separators in group/section name */
		case TM_PARSER_CONF:
		case TM_PARSER_REST:
			return ":::";
		
		/* no context separator */
		case TM_PARSER_ASCIIDOC:
		case TM_PARSER_TXT2TAGS:
			return "\x03";
		
		default:
			return ".";
	}
}


gboolean tm_parser_has_full_context(TMParserType lang)
{
	switch (lang)
	{
		/* These parsers include full hierarchy in the tag scope,
		 * separated by tm_parser_context_separator() */
		case TM_PARSER_ACTIONSCRIPT:
		case TM_PARSER_C:
		case TM_PARSER_CPP:
		case TM_PARSER_CSHARP:
		case TM_PARSER_COBOL:
		case TM_PARSER_D:
		case TM_PARSER_ELIXIR:
		case TM_PARSER_FERITE:
		case TM_PARSER_GLSL:
		case TM_PARSER_JAVA:
		case TM_PARSER_JAVASCRIPT:
		case TM_PARSER_JSON:
		case TM_PARSER_PHP:
		case TM_PARSER_POWERSHELL:
		case TM_PARSER_PYTHON:
		case TM_PARSER_RUBY:
		case TM_PARSER_RUST:
		case TM_PARSER_SQL:
		case TM_PARSER_TXT2TAGS:
		case TM_PARSER_VALA:
		case TM_PARSER_ZEPHIR:
			return TRUE;
		
		/* These make use of the scope, but don't include nested hierarchy
		 * (either as a parser limitation or a language semantic) */
		case TM_PARSER_ASCIIDOC:
		case TM_PARSER_CONF:
		case TM_PARSER_ERLANG:
		case TM_PARSER_F77:
		case TM_PARSER_FORTRAN:
		case TM_PARSER_GO:
		case TM_PARSER_OBJC:
		case TM_PARSER_REST:
		/* Other parsers don't use scope at all (or should be somewhere above) */
		default:
			return FALSE;
	}
}


gboolean tm_parser_langs_compatible(TMParserType lang, TMParserType other)
{
	if (lang == TM_PARSER_NONE || other == TM_PARSER_NONE)
		return FALSE;
	if (lang == other)
		return TRUE;
	/* Accept CPP tags for C lang and vice versa */
	else if (lang == TM_PARSER_C && other == TM_PARSER_CPP)
		return TRUE;
	else if (lang == TM_PARSER_CPP && other == TM_PARSER_C)
		return TRUE;
	
	return FALSE;
}

gboolean tm_parser_has_quoted_identifiers(TMParserType lang)
{
	switch (lang)
	{
		case TM_PARSER_ERLANG:
			return TRUE;
		/* Other parsers have no quoted identifier */
		default:
			return FALSE;
	}
}

void tm_parser_define_scope(gchar *scope, gsize scopelen, guint scope_parts_cnt,
							TMParserType lang, const gchar *prefix,
							const gchar *suffix, gboolean brackets)
{
	switch (lang)
	{
		case TM_PARSER_ERLANG:
			if (*scope != '\0' && (brackets || g_strcmp0(prefix, "?") == 0 ||
								   !(*scope == '\'' || islower(*scope))))
				// any scope, examples:
				// case (module()):test1() of	scope "case" is a standard keyword
				// ?module:test1()				scope "module" is a macro containing a module
				// Module:test1()				scope "Module" is a variable containing a module
				g_strlcpy(scope, "*", scopelen);
			else if (*scope == '\0' && g_strcmp0(suffix, ":") == 0)
				// scope is not specified, but the separator is there
				g_strlcpy(scope, "*", scopelen);
			break;
		
		case TM_PARSER_ELIXIR:
			if (islower(*scope) || (g_str_has_prefix(scope, "__") &&
									!g_str_has_prefix(scope, "__MODULE__")))
				// any scope, examples:
				// module.handle_special(token.path, mode, context)
				//		scope "module" is a variable containing a module
				// __MACRO__.get_validation_module_for_file(path)
				//		scope "__MACRO__" is a macro containing a module
				g_strlcpy(scope, "*", scopelen);
			else if (*scope != '\0' && brackets)
				// any scope, examples:
				// __MODULE__.module().handle_message(message, context)
				//		scope "module" is a function that returns a module
				g_strlcpy(scope, "*", scopelen);
			else if (*scope == '\0' && g_strcmp0(suffix, ".") == 0)
				// scope is not specified, but the separator is there
				g_strlcpy(scope, "*", scopelen);
			break;
		
		case TM_PARSER_PYTHON:
			if (*scope == '\0')
				g_strlcpy(scope, "*", scopelen);
			else if (g_strcmp0(scope, "self") == 0 ||
					 g_strcmp0(scope, "cls") == 0)
				g_strlcpy(scope, scope_parts_cnt > 1 ? "*" : "", scopelen);
			break;
		
		case TM_PARSER_C:
		case TM_PARSER_CPP:
			if (*scope == '\0' && (g_strcmp0(suffix, "->") == 0 ||
								   g_strcmp0(suffix, "::") == 0 ||
								   g_strcmp0(suffix, ".") == 0))
				// scope is not specified, but the separator is there
				g_strlcpy(scope, "*", scopelen);
			break;
	}
}

gboolean tm_parser_strict_scope(TMParserType lang)
{
	// esh: add langs to exclude here as needed
	switch (lang)
	{
		case TM_PARSER_NONE:
		case TM_PARSER_PYTHON:
		case TM_PARSER_GO:		// as long as ctags does not contain scope (package name)
			return FALSE;
		default:
			return TRUE;
	}
}

gboolean tm_parser_filter_by_file(TMParserType lang, const gchar *scope)
{
	switch (lang)
	{
		case TM_PARSER_ELIXIR:
			return !(scope && *scope); // filter by file only if scope is not specified
		default:
			return (g_strcmp0(scope, "*") != 0);
	}
}

TMTagType tm_parser_get_filter_type(TMParserType lang, TMTagType type)
{
	switch (lang)
	{
		case TM_PARSER_ERLANG:
			// conditions are formed in accordance
			// with the tm_parser_define_type func
			if (//type == tm_tag_macro_t || // exclude macros
				type == tm_tag_struct_t ||
				type == (tm_tag_function_t | tm_tag_typedef_t))
				return type;
			return tm_tag_undef_t;
		default:
			return tm_tag_max_t;
	}
}

void tm_parser_define_type(TMTagType *type, TMParserType lang,
						   const gchar *prefix, const gchar *suffix)
{
	// this func will be expanded in conjunction with the definitions of
	// ONE_CHAR_PREFIX_CHARS/MULTI_CHAR_PREFIX_CHARS,
	// ONE_CHAR_SUFFIX_CHARS/MULTI_CHAR_SUFFIX_CHARS
	
	switch (lang)
	{
		case TM_PARSER_ERLANG:
			if (g_strcmp0(prefix, "?") == 0)
				*type = tm_tag_macro_t;
			else if (g_strcmp0(prefix, "#") == 0) // record
				*type = tm_tag_struct_t;
			else if (g_strcmp0(suffix, "(") == 0)
				*type = tm_tag_function_t | tm_tag_typedef_t;
			else if (g_strcmp0(suffix, ":") == 0)
				*type = tm_tag_max_t & ~(tm_tag_macro_t    | tm_tag_struct_t |
										 tm_tag_function_t | tm_tag_typedef_t);
			else
				*type = tm_tag_max_t & ~(tm_tag_macro_t | tm_tag_typedef_t);
			break;
		
		case TM_PARSER_C:
		case TM_PARSER_CPP:
			if (g_strcmp0(prefix, "->") == 0 || g_strcmp0(prefix, ".") == 0)
			{
				if (g_strcmp0(suffix, "(") == 0)
					//~ examples: pdoc->ExtendWordSelect(), styler.Match()
					*type = tm_tag_function_t;
				else
					//~ examples: pdoc->dbcsCodePage, wordbound.end
					*type = tm_tag_member_t;
			}
			else if (g_strcmp0(suffix, "(") == 0)
				*type = tm_tag_function_t | tm_tag_macro_with_arg_t;
			else if (g_strcmp0(suffix, "::") == 0)
				//~ examples: CharacterSet::setNone, CharacterSet::setDigits
				*type = tm_tag_class_t;
			else if (g_strcmp0(suffix, ":") == 0)
				//~ examples:
				//~ case TM_PARSER_C:
				//~ startPos == 0 ? SCE_ERLANG_DEFAULT : styler.StyleAt(startPos - 1);
				*type = tm_tag_max_t & ~(tm_tag_class_t | tm_tag_function_t |
										 tm_tag_macro_with_arg_t);
			break;
		
		case TM_PARSER_PYTHON:
			if (g_strcmp0(suffix, "=") == 0 || g_strcmp0(suffix, ".") == 0)
				*type = tm_tag_max_t & ~(tm_tag_method_t | tm_tag_function_t);
			else if (g_strcmp0(suffix, "(") == 0)
			{
				if (EMPTY(prefix))
					*type = tm_tag_function_t | tm_tag_class_t;
				else if (g_strcmp0(prefix, ".") == 0)
					*type = tm_tag_function_t | tm_tag_class_t | tm_tag_method_t;
				else
					*type = tm_tag_max_t & ~tm_tag_externvar_t;
			}
			else if (EMPTY(prefix))
				*type = tm_tag_max_t & ~(tm_tag_method_t | tm_tag_member_t);
			break;
		
		case TM_PARSER_GO:
			if (g_strcmp0(suffix, "(") == 0)
				*type = tm_tag_function_t;
			else if (g_strcmp0(suffix, "{") == 0)
				*type = tm_tag_struct_t;
			break;
		
		case TM_PARSER_ELIXIR:
			if (g_strcmp0(suffix, "(") == 0)
				*type = tm_tag_function_t | tm_tag_macro_t | tm_tag_typedef_t;
			else
				*type = tm_tag_max_t;
			
			*type &= ~tm_tag_other_t; // exclude aliases
			break;
		
		default:
			if (g_strcmp0(suffix, "(") == 0)
				*type = tm_tag_function_t  | tm_tag_method_t |
						tm_tag_prototype_t | tm_tag_macro_with_arg_t;
			break;
	}
}
