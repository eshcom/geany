/*
 *      projectprivate.h - this file is part of Geany, a fast and lightweight IDE
 *
 *      Copyright 2008 The Geany contributors
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


#ifndef GEANY_PROJECTPRIVATE_H
#define GEANY_PROJECTPRIVATE_H 1

#include "project.h"

#include <glib.h>

G_BEGIN_DECLS

typedef struct GeanyProjectPrivate
{
	// tags prefs
	gboolean	load_tags_file_on_open;
	gboolean	load_typenames_from_tags_file;
	
	// file prefs
	gboolean	final_new_line;
	gboolean	strip_trailing_spaces;
	gboolean	replace_tabs;
	gboolean	ensure_convert_new_lines;
	
	// editor prefs
	struct GeanyIndentPrefs *indentation;
	gboolean	line_wrapping;
	gint		line_break_column;
	gboolean	auto_continue_multiline;
	gint		long_line_behaviour; /* 0 - disabled, 1 - follow global settings,
										2 - enabled (custom) */
	gint		long_line_column; /* Long line marker position. */
	
	GPtrArray *build_filetypes_list; /* Project has custom filetype builds for these. */
}
GeanyProjectPrivate;

G_END_DECLS

#endif /* GEANY_PROJECT_H */
