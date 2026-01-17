/*
 *      glibcompat.h - this file is part of Geany, a fast and lightweight IDE
 *
 *      Copyright 2012 The Geany contributors
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

/* Compatibility macros to support older GLIB+ versions */

#ifndef GLIB_COMPAT_H
#define GLIB_COMPAT_H 1

#include <glib.h>

G_BEGIN_DECLS

#if !GLIB_CHECK_VERSION(2, 70, 0)
#	define g_pattern_spec_match_string	g_pattern_match_string
#endif

G_END_DECLS

#endif /* GLIB_COMPAT_H */
