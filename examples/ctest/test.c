#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>

#include <sys/stat.h>
#include <sys/types.h>

#include <gio/gio.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <glib/gprintf.h>


#define EMPTY(ptr) \
	(!(ptr) || !*(ptr))

#define SETPTR(ptr, result)			\
	do {							\
		gpointer setptr_tmp = ptr;	\
		ptr = result;				\
		g_free(setptr_tmp);			\
	} while (0)

#define foreach_str(char_ptr, string) \
	for (char_ptr = string; *char_ptr; char_ptr++)

#define foreach_strv(str_ptr, strv) \
	if (strv) foreach_str(str_ptr, strv)


gboolean utils_str_equal(const gchar *a, const gchar *b)
{
	/* (taken from libexo from os-cillation) */
	if (a == NULL && b == NULL) return TRUE;
	else if (a == NULL || b == NULL) return FALSE;
	
	return strcmp(a, b) == 0;
}


static GPtrArray *data = NULL;

gboolean load_data()
{
	const gchar *file = "openjdk8-8u66-b01.java.tags";
	gchar buf[BUFSIZ];
	FILE *fp;
	
	if (!(fp = g_fopen(file, "r")))
		return FALSE;
	
	data = g_ptr_array_new();
	
	while (fgets((gchar *)buf, BUFSIZ, fp) && *buf != '\0')
	{
		gchar *item = g_strdup(buf);
		g_ptr_array_add(data, item);
		//~ printf("%s\n", item);
	}
	fclose(fp);
	return TRUE;
}

void free_data()
{
	if (data)
	{
		for (guint i = 0; i < data->len; ++i)
			g_free(data->pdata[i]);
		
		g_ptr_array_free(data, TRUE);
	}
}

typedef struct
{
	guint count;
	gint64 timestamp;
} FoundStats;

FoundStats by_g_strcmp0(const gchar *search)
{
	FoundStats foundStats = {0, 0};
	
	gint64 start = g_get_real_time();
	for (guint i = 0; i < data->len; ++i)
	{
		//~ if (g_str_has_prefix(data->pdata[i], "lastys"))
			//~ printf("Found: %s\n", (gchar *)data->pdata[i]);
		if (g_strcmp0(data->pdata[i], search) == 0)
			foundStats.count++;
	}
	foundStats.timestamp = (g_get_real_time() - start) / 1; // 1 | 1000 | G_USEC_PER_SEC
	
	return foundStats;
}
FoundStats by_g_strequal(const gchar *search)
{
	FoundStats foundStats = {0, 0};
	
	gint64 start = g_get_real_time();
	for (guint i = 0; i < data->len; ++i)
	{
		if (g_str_equal(data->pdata[i], search))
			foundStats.count++;
	}
	foundStats.timestamp = (g_get_real_time() - start) / 1;
	
	return foundStats;
}
FoundStats by_utils_strequal(const gchar *search)
{
	FoundStats foundStats = {0, 0};
	
	gint64 start = g_get_real_time();
	for (guint i = 0; i < data->len; ++i)
	{
		if (utils_str_equal(data->pdata[i], search))
			foundStats.count++;
	}
	foundStats.timestamp = (g_get_real_time() - start) / 1;
	
	return foundStats;
}

gint utils_strpos(const gchar *haystack, const gchar *needle)
{
	if (!*needle) return -1;
	
	const gchar *sub = strstr(haystack, needle);
	if (!sub) return -1;
	
	return sub - haystack;
}

gint utils_string_find(GString *haystack, gint start, gint end,
					   const gchar *needle)
{
	g_return_val_if_fail(haystack != NULL, -1);
	if (haystack->len == 0)
		return -1;
	
	g_return_val_if_fail(start >= 0, -1);
	if (start >= (gint)haystack->len)
		return -1;
	
	g_return_val_if_fail(!EMPTY(needle), -1);
	
	if (end < 0)
		end = haystack->len;
	
	gint pos = utils_strpos(haystack->str + start, needle);
	if (pos == -1)
		return -1;
	
	pos += start;
	if (pos >= end)
		return -1;
	return pos;
}

gint utils_string_replace(GString *str, gint pos, gint len,
						  const gchar *replace)
{
	g_string_erase(str, pos, len);
	if (replace)
	{
		g_string_insert(str, pos, replace);
		pos += strlen(replace);
	}
	return pos;
}

guint utils_string_replace_all(GString *haystack, const gchar *needle,
							   const gchar *replace)
{
	guint count = 0;
	gint pos = 0;
	gsize needle_length = strlen(needle);
	
	while (1)
	{
		pos = utils_string_find(haystack, pos, -1, needle);
		
		if (pos == -1) break;
		
		pos = utils_string_replace(haystack, pos, needle_length, replace);
		count++;
	}
	return count;
}

void utils_string_reduce_spaces1(GString *haystack)
{
	if (haystack->len == 0) return;
	
	gchar *new_str = g_alloca(haystack->len + 1);
	gchar *dst = new_str;
	gboolean is_space_last = FALSE;
	
	const gchar *src;
	foreach_str(src, haystack->str)
	{
		if (*src == ' ')
		{
			if (is_space_last) continue;
			else is_space_last = TRUE;
		}
		else is_space_last = FALSE;
		
		*dst++ = *src;
	}
	*dst = '\0';
	g_string_assign(haystack, new_str);
}

void utils_string_reduce_spaces2(GString *haystack)
{
	if (haystack->len == 0) return;
	
	gboolean is_space_last = FALSE;
	gssize pos = 0;
	
	while (pos < haystack->len)
	{
		if (haystack->str[pos] == ' ')
		{
			if (is_space_last)
			{
				g_string_erase(haystack, pos, 1);
				continue;
			}
			else is_space_last = TRUE;
		}
		else is_space_last = FALSE;
		
		pos++;
	}
}

void utils_string_reduce_spaces3(GString *haystack)
{
	if (haystack->len == 0) return;
	
	gboolean is_space_last = FALSE;
	gssize pos = 0, erase_pos = 0, erase_cnt = 0;
	
	while (pos < haystack->len)
	{
		if (haystack->str[pos] == ' ')
		{
			if (is_space_last)
				erase_cnt++;
			else
			{
				is_space_last = TRUE;
				erase_pos = pos + 1;
				erase_cnt = 0;
			}
		}
		else
		{
			is_space_last = FALSE;
			
			if (erase_cnt > 0)
			{
				g_string_erase(haystack, erase_pos, erase_cnt);
				pos = erase_pos;
				erase_cnt = 0;
			}
		}
		pos++;
	}
	if (erase_cnt > 0)
		g_string_erase(haystack, erase_pos, erase_cnt);
}

static gchar *gen_multi_options1(const gchar *multi_options, const gchar *option)
{
	GString *multi = g_string_new(multi_options);
	utils_string_reduce_spaces2(multi);
	g_string_prepend_c(multi, ' ');
	utils_string_replace_all(multi, " ", option);
	
	return g_string_free(multi, FALSE);
}

static gchar *gen_multi_options2(const gchar *multi_options, const gchar *option)
{
	GString *multi = g_string_new(NULL);
	gchar **item, **items = g_strsplit(multi_options, " ", -1);
	
	foreach_strv(item, items)
	{
		if (**item)
		{
			g_string_append(multi, option);
			g_string_append(multi, *item);
		}
	}
	return g_string_free(multi, FALSE);
}

// -------------------------------------------------------------------
void run_test_case01()
{
	if (!load_data())
	{
		printf("There is no data to process.\n");
		return;
	}
	
	const gchar *search = "# format=tagmanager\n";
	
	FoundStats foundStats = by_g_strcmp0(search);
	printf("g_strcmp0:       Total=%d, Equal=%d, Timestamp=%ld\n",
		   data->len, foundStats.count, foundStats.timestamp);
	
	foundStats = by_g_strequal(search);
	printf("g_str_equal:     Total=%d, Equal=%d, Timestamp=%ld\n",
		   data->len, foundStats.count, foundStats.timestamp);
	
	foundStats = by_utils_strequal(search);
	printf("utils_str_equal: Total=%d, Equal=%d, Timestamp=%ld\n",
		   data->len, foundStats.count, foundStats.timestamp);
	
	free_data();
}

void run_test_case02()
{
	gchar *none1;
	gchar *none2;
	gchar *null1 = NULL;
	gchar *null2 = NULL;
	gchar *empty1 = "";
	gchar *empty2 = "";
	gchar *space1 = " ";
	gchar *space2 = " ";
	
	printf("g_strcmp0:       "
		   "none_none=%d, none_null=%d, none_empty=%d, none_space=%d, "
		   "null_null=%d, null_empty=%d, null_space=%d, "
		   "empty_empty=%d, empty_space=%d, space_space=%d"
		   "\n",
		   g_strcmp0(none1, none2) == 0, g_strcmp0(none1, null1) == 0,
				g_strcmp0(none1, empty1) == 0, g_strcmp0(none1, space1) == 0,
		   g_strcmp0(null1, null2) == 0, g_strcmp0(null1, empty1) == 0,
				g_strcmp0(null1, space1) == 0,
		   g_strcmp0(empty1, empty2) == 0, g_strcmp0(empty1, space1) == 0,
		   g_strcmp0(space1, space2) == 0);
	
	printf("utils_str_equal: "
		   "none_none=%d, none_null=%d, none_empty=%d, none_space=%d, "
		   "null_null=%d, null_empty=%d, null_space=%d, "
		   "empty_empty=%d, empty_space=%d, space_space=%d"
		   "\n",
		   utils_str_equal(none1, none2), utils_str_equal(none1, null1),
				utils_str_equal(none1, empty1), utils_str_equal(none1, space1),
		   utils_str_equal(null1, null2), utils_str_equal(null1, empty1),
				utils_str_equal(null1, space1),
		   utils_str_equal(empty1, empty2), utils_str_equal(empty1, space1),
		   utils_str_equal(space1, space2));
}

void run_test_case03()
{
	gchar *curr = g_get_current_dir();
	gchar *item1 = g_path_get_basename("/usr/test/file.erl");
	gchar *item2 = g_path_get_dirname("/usr/test/file.erl");
	gchar *item3 = g_path_get_dirname("/usr/test/dir1");
	gchar *item4 = g_path_get_dirname("/usr/test/dir2/");
	gchar *item5 = g_path_get_dirname("/usr/test/dir2\\");
	printf("curr = %s\nitem1 = %s\nitem2 = %s\nitem3 = %s\nitem4 = %s\nitem5 = %s\n",
		   curr, item1, item2, item3, item4, item5);
	//~ Result:
	//~ item1 = file.erl
	//~ item2 = /usr/test
	//~ item3 = /usr/test
	//~ item4 = /usr/test/dir2
	//~ item5 = /usr/test
	g_free(curr);
	g_free(item1);
	g_free(item2);
	g_free(item3);
	g_free(item4);
	g_free(item5);
	
	gchar *build1 = g_build_filename("/usr/test/dir", "item", NULL);
	gchar *build2 = g_build_filename("/usr/test/dir", "item/", NULL);
	gchar *build3 = g_build_filename("/usr/test/dir", "/item", NULL);
	gchar *build4 = g_build_filename("/usr/test/dir", "/item/", NULL);
	gchar *build5 = g_build_filename("/usr/test/dir/", "item", NULL);
	gchar *build6 = g_build_filename("/usr/test/dir/", "item/", NULL);
	gchar *build7 = g_build_filename("/usr/test/dir/", "/item", NULL);
	gchar *build8 = g_build_filename("/usr/test/dir/", "/item/", NULL);
	gchar *build9 = g_build_filename("/usr/test/dir/", "./item", NULL);
	gchar *build10 = g_build_filename("/usr/test/dir/", "./item/", NULL);
	printf("build1 = %s\nbuild2 = %s\nbuild3 = %s\nbuild4 = %s\n"
		   "build5 = %s\nbuild6 = %s\nbuild7 = %s\nbuild8 = %s\n"
		   "build9 = %s\nbuild10 = %s\n",
		   build1, build2, build3, build4, build5, build6,
		   build7, build8, build9, build10);
	//~ Result:
	//~ build1 = /usr/test/dir/item
	//~ build2 = /usr/test/dir/item/
	//~ build3 = /usr/test/dir/item
	//~ build4 = /usr/test/dir/item/
	//~ build5 = /usr/test/dir/item
	//~ build6 = /usr/test/dir/item/
	//~ build7 = /usr/test/dir/item
	//~ build8 = /usr/test/dir/item/
	//~ build9 = /usr/test/dir/./item
	//~ build10 = /usr/test/dir/./item/
	g_free(build1);
	g_free(build2);
	g_free(build3);
	g_free(build4);
	g_free(build5);
	g_free(build6);
	g_free(build7);
	g_free(build8);
	g_free(build9);
	g_free(build10);
}


void run_test_case04()
{
	gchar word[100];
	
	printf("word = %s, len1 = %d\n", word, (int)strlen(word));
	*word = '\0';
	printf("word = %s, len2 = %d\n", word, (int)strlen(word));
	*word = 'T';
	printf("word = %s, len3 = %d\n", word, (int)strlen(word));
}

enum
{
	MATCH_NOT,
	MATCH_FULL,
	MATCH_PREF_1,
	MATCH_PREF_2
};

gint utils_match_dirs(const gchar *dir1, const gchar *dir2)
{
	if (EMPTY(dir1) || EMPTY(dir2))
		return MATCH_NOT;
	
	dir1++;
	dir2++;
	
	while (TRUE)
	{
		if (*dir1 == '\0')
		{
			if (*dir2 == '\0')
				return MATCH_FULL;
			else if (*(--dir1) == G_DIR_SEPARATOR)
				return MATCH_PREF_1;
			else if (*dir2 == G_DIR_SEPARATOR)
			{
				if (*(++dir2) == '\0')
					return MATCH_FULL;
				else
					return MATCH_PREF_1;
			}
			return MATCH_NOT;
		}
		if (*dir2 == '\0')
		{
			if (*(--dir2) == G_DIR_SEPARATOR)
				return MATCH_PREF_2;
			else if (*dir1 == G_DIR_SEPARATOR)
			{
				if (*(++dir1) == '\0')
					return MATCH_FULL;
				else
					return MATCH_PREF_2;
			}
			return MATCH_NOT;
		}
		if (*dir1 != *dir2)
			return MATCH_NOT;
		
		dir1++;
		dir2++;
	}
}


#define STR_SIZE 100

void run_test_case05()
{
	gchar dir1[STR_SIZE], dir2[STR_SIZE];
	
	g_strlcpy(dir1, "", STR_SIZE);
	g_strlcpy(dir2, "", STR_SIZE);
	printf("dir1 = %s, dir2 = %s, match1 = %d, match2 = %d\n", dir1, dir2,
		   utils_match_dirs(dir1, dir2), utils_match_dirs(dir2, dir1));
	
	g_strlcpy(dir1, "/usr/bin", STR_SIZE);
	g_strlcpy(dir2, "/usr/bin/", STR_SIZE);
	printf("dir1 = %s, dir2 = %s, match1 = %d, match2 = %d\n", dir1, dir2,
		   utils_match_dirs(dir1, dir2), utils_match_dirs(dir2, dir1));
	
	g_strlcpy(dir1, "/usr/bin", STR_SIZE);
	g_strlcpy(dir2, "/usr/bin/dir", STR_SIZE);
	printf("dir1 = %s, dir2 = %s, match1 = %d, match2 = %d\n", dir1, dir2,
		   utils_match_dirs(dir1, dir2), utils_match_dirs(dir2, dir1));
	
	g_strlcpy(dir1, "/usr/bin/", STR_SIZE);
	g_strlcpy(dir2, "/usr/bin/dir", STR_SIZE);
	printf("dir1 = %s, dir2 = %s, match1 = %d, match2 = %d\n", dir1, dir2,
		   utils_match_dirs(dir1, dir2), utils_match_dirs(dir2, dir1));
	
	g_strlcpy(dir1, "/usr/bin1", STR_SIZE);
	g_strlcpy(dir2, "/usr/bin2", STR_SIZE);
	printf("dir1 = %s, dir2 = %s, match1 = %d, match2 = %d\n", dir1, dir2,
		   utils_match_dirs(dir1, dir2), utils_match_dirs(dir2, dir1));
	
	g_strlcpy(dir1, "/usr/bin1", STR_SIZE);
	g_strlcpy(dir2, "/usr/bin2/", STR_SIZE);
	printf("dir1 = %s, dir2 = %s, match1 = %d, match2 = %d\n", dir1, dir2,
		   utils_match_dirs(dir1, dir2), utils_match_dirs(dir2, dir1));
}

void run_test_case06()
{
	gchar *path = g_strdup("/usr/local");
	
	gint level = 3;
	while (level > 0 && g_strcmp0(path, G_DIR_SEPARATOR_S) != 0)
	{
		SETPTR(path, g_path_get_dirname(path));
		printf("path = %s\n", path);
		level--;
	}
	//~ Result:
	//~ path = /usr
	//~ path = /
	g_free(path);
}

void run_test_case07()
{
	gchar **item, **items = g_strsplit("  .o .lo  .dll   .pyc ", " ", 0);
	foreach_strv(item, items)
	{
		if (**item)
			printf("item = %s, c = '%c'\n", *item, **item);
	}
	g_strfreev(items);
	
	items = g_strsplit("", " ", 0);
	printf("len = %d, is null items = %d, is null first = %d, is null first = %d\n",
		   g_strv_length(items), items == NULL, *items == NULL, items[0] == NULL);
	g_strfreev(items);
	//~ Result:
	//~ item = .o, c = '.'
	//~ item = .lo, c = '.'
	//~ item = .dll, c = '.'
	//~ item = .pyc, c = '.'
	//~ len = 0, is null items = 0, is null first = 1
	
	printf("has pref1 = %d, has pref2 = %d, has pref3 = %d\n",
		   g_str_has_prefix("", ""), g_str_has_prefix(" ", ""),
		   g_str_has_prefix("test123", ""));
	printf("has suff1 = %d, has suff2 = %d, has suff3 = %d\n",
		   g_str_has_suffix("", ""), g_str_has_suffix(" ", ""),
		   g_str_has_suffix("test123", ""));
	//~ Result:
	//~ has pref1 = 1, has pref2 = 1, has pref3 = 1
	//~ has suff1 = 1, has suff2 = 1, has suff3 = 1
	
	items = g_strsplit("", ";", 0);
	printf("item1: %s, item2: %s\n", items[0], items[1]);
	g_strfreev(items);
	items = g_strsplit("!", ";", 0);
	printf("item1: %s, item2: %s\n", items[0], items[1]);
	g_strfreev(items);
	items = g_strsplit("!;", ";", 0);
	printf("item1: '%s', item2: '%s'\n", items[0], items[1]);
	g_strfreev(items);
	//~ Result:
	//~ item1: (null), item2: (null)
	//~ item1: !, item2: (null)
	//~ item1: '!', item2: ''
}

#define GET_STR_CHAR(obj, index) \
	GET_CHAR(obj, index)

void run_test_case08()
{
	gchar *str1 = "123";
	gchar *str2 = "456";
	gchar *str3 = "789";
	
	inline gchar nested(void)
	{
		return str3[0];
	}
	
	#define GET_CHAR(sci, index) \
		sci[index]
	printf("c1: %c\n", GET_STR_CHAR(str1, 0));
	#undef GET_CHAR
	
	#define GET_CHAR(chunk, index) \
		chunk[index]
	printf("c2: %c\n", GET_STR_CHAR(str2, 0));
	#undef GET_CHAR
	
	printf("c3: %c\n", nested());
}

static gboolean utils_filename_has_prefix1(const gchar *str, const gchar *prefix)
{
	gchar *head = g_strndup(str, strlen(prefix));
	gboolean ret = strcmp(head, prefix) == 0;
	g_free(head);
	return ret;
}

static gboolean utils_filename_has_prefix2(const gchar *str, const gchar *prefix)
{
	return strncmp(str, prefix, strlen(prefix)) == 0;
}

void run_test_case09()
{
	gchar *str1 = "1234";
	gchar *str2 = "123";
	
	printf("compare1: %i\n", utils_filename_has_prefix1(str1, str2));
	printf("compare2: %i\n", utils_filename_has_prefix2(str1, str2));
}

void run_test_case10()
{
	gchar *str1 = NULL;
	gchar *str2 = "*";
	
	printf("compare1: %i\n", g_strcmp0(str1, "*"));
	printf("compare2: %i\n", g_strcmp0(str2, "*"));
}

void run_test_case11()
{
	printf("0 & 31: %i\n", 0 & 31);		// 0
	printf("3 & 31: %i\n", 3 & 31);		// 3
	printf("4 & 31: %i\n", 4 & 31);		// 4
	printf("6 & 31: %i\n", 6 & 31);		// 6
	printf("7 & 31: %i\n", 7 & 31);		// 7
	printf("16 & 31: %i\n", 16 & 31);	// 16
	printf("17 & 31: %i\n", 17 & 31);	// 17
	printf("18 & 31: %i\n", 18 & 31);	// 18
	printf("19 & 31: %i\n", 19 & 31);	// 19
	printf("30 & 31: %i\n", 30 & 31);	// 30
	printf("31 & 31: %i\n", 31 & 31);	// 31
	printf("32 & 31: %i\n", 32 & 31);	// 0
	printf("40 & 31: %i\n", 40 & 31);	// 8
	printf("41 & 31: %i\n", 41 & 31);	// 9
}

void run_test_case12()
{
	gchar *str = "ZzЯя";
	gunichar ch;
	while ((ch = g_utf8_get_char(str)) != '\0')
	{
		printf("%x - %x\n", ch, g_unichar_tolower(ch));
		str = g_utf8_next_char(str);
	}
}

void run_test_case13()
{
	gint pos, ret = -1;
	printf("'%d' - '%d'\n", pos, ret);
}

void run_test_case14()
{
	gchar *str1 = "asdf1234";
	gchar *str2 = "asdf1234";
	printf("g_strcmp0: %d\n", g_strcmp0(str1, str2) == 0);
	printf("utils_str_equal: %d\n", utils_str_equal(str1, str2));
	
	str1 = NULL;
	str2 = "asdf1234";
	printf("g_strcmp0: %d\n", g_strcmp0(str1, str2) == 0);
	printf("utils_str_equal: %d\n", utils_str_equal(str1, str2));
	
	str1 = "asdf1234";
	str2 = NULL;
	printf("g_strcmp0: %d\n", g_strcmp0(str1, str2) == 0);
	printf("utils_str_equal: %d\n", utils_str_equal(str1, str2));
	
	str1 = NULL;
	str2 = NULL;
	printf("g_strcmp0: %d\n", g_strcmp0(str1, str2) == 0);
	printf("utils_str_equal: %d\n", utils_str_equal(str1, str2));
}

void run_test_case15()
{
	gchar *str1 = "asdf1234ssssssssssssssffffffffffffffffffffffffeeeeeeeeeeeeee";
	gchar *str2 = "asdf1234ssssssssssssssffffffffffffffffffffffffeeeeeeeeeeeeef";
	gint64 start;
	gint cnt = 1000000;
	
	start = g_get_real_time();
	for (gint i = 0; i < cnt; i++)
		g_strcmp0(str1, str2);
	printf("g_strcmp0: %ld\n", g_get_real_time() - start);
	
	start = g_get_real_time();
	for (gint i = 0; i < cnt; i++)
		utils_str_equal(str1, str2);
	printf("utils_str_equal: %ld\n", g_get_real_time() - start);
}

gboolean scopes_and_types_equal(const gchar *pscope, gint ptype,
								const gchar *sscope, gint stype)
{
	if (EMPTY(pscope) || EMPTY(sscope))
		return ptype == stype && g_strcmp0(pscope, sscope) == 0;
	
	gchar **pfields = g_strsplit(pscope, "__anon", 2);
	gchar **sfields = g_strsplit(sscope, "anon_", 2);
	
	if (ptype == 64 && stype == 1024
		&& g_strv_length(pfields) == 2	// exists "__anon"
		&& g_strv_length(sfields) == 2)	// exists "anon_"
		stype = 64;
	
	gboolean scopes_equal = g_strcmp0(pfields[0], sfields[0]) == 0;
	
	g_strfreev(pfields);
	g_strfreev(sfields);
	
	return ptype == stype && scopes_equal;
}

void run_test_case16()
{
	gboolean equal;
	
	equal = scopes_and_types_equal("__anon66b155090303", 4, "anon_enum_2", 4);
	printf("equal11: %d\n", equal);
	
	equal = scopes_and_types_equal("__anon5eaccdba0208", 64, "anon_struct_1", 64);
	printf("equal12: %d\n", equal);
	
	equal = scopes_and_types_equal("__anondaae05980308", 64, "anon_struct_2", 1024);
	printf("equal13: %d\n", equal);
	
	equal = scopes_and_types_equal("__anon99646b230111", 16, "anon_namespace_0", 16);
	printf("equal14: %d\n", equal);
	
	equal = scopes_and_types_equal("Scintilla::__anonf5f3056f0211", 16, "Scintilla::anon_namespace_1", 16);
	printf("equal15: %d\n", equal);
	
	equal = scopes_and_types_equal("__anondaae05980308", 64, "", 1024);
	printf("equal16: %d\n", equal);		// not equal
	
	equal = scopes_and_types_equal("", 64, "anon_struct_2", 1024);
	printf("equal17: %d\n", equal);		// not equal
	
	equal = scopes_and_types_equal("", 1, "", 1);
	printf("equal21: %d\n", equal);
	
	equal = scopes_and_types_equal(NULL, 1, NULL, 1);
	printf("equal22: %d\n", equal);
	
	equal = scopes_and_types_equal("", 1, NULL, 1);
	printf("equal23: %d\n", equal);		// not equal
	
	equal = scopes_and_types_equal(NULL, 1, "", 1);
	printf("equal24: %d\n", equal);		// not equal
}

void run_test_case17()
{
	const gchar *str = "*.h *.c *.cxx *.iface ru.po *.pot *.glade filetypes.* "
					   "*.gtkrc *.conf *.xml *.css *.sh Makefile.am";
	//~ const gchar *str = "    *.h   *.c *.cxx    *.iface ru.po   *.pot *.glade  "
					   //~ "filetypes.*  *.gtkrc *.conf *.xml *.css *.sh   Makefile.am  ";
	GString *gstr;
	gint64 start;
	gint cnt = 10000;
	
	start = g_get_real_time();
	for (gint i = 0; i < cnt; i++)
	{
		gstr = g_string_new(str);
		do {} while (utils_string_replace_all(gstr, "  ", " "));
		//~ printf("1: '%s'\n", gstr->str);
		g_string_free(gstr, TRUE);
	}
	printf("utils_string_replace_all: %ld\n", g_get_real_time() - start);
	
	start = g_get_real_time();
	for (gint i = 0; i < cnt; i++)
	{
		gstr = g_string_new(str);
		utils_string_reduce_spaces1(gstr);
		//~ printf("2: '%s'\n", gstr->str);
		g_string_free(gstr, TRUE);
	}
	printf("utils_string_reduce_spaces1: %ld\n", g_get_real_time() - start);
	
	start = g_get_real_time();
	for (gint i = 0; i < cnt; i++)
	{
		gstr = g_string_new(str);
		utils_string_reduce_spaces2(gstr);
		//~ printf("3: '%s'\n", gstr->str);
		g_string_free(gstr, TRUE);
	}
	printf("utils_string_reduce_spaces2: %ld\n", g_get_real_time() - start);
	
	start = g_get_real_time();
	for (gint i = 0; i < cnt; i++)
	{
		gstr = g_string_new(str);
		utils_string_reduce_spaces3(gstr);
		//~ printf("4: '%s'\n", gstr->str);
		g_string_free(gstr, TRUE);
	}
	printf("utils_string_reduce_spaces3: %ld\n", g_get_real_time() - start);
}

void run_test_case18()
{
	const gchar *option = " --name=";
	const gchar *multi = "*.h *.c *.cxx *.iface ru.po *.pot *.glade filetypes.* "
						 "*.gtkrc *.conf *.xml *.css *.sh Makefile.am";
	//~ const gchar *multi = "*.h   *.c *.cxx    *.iface ru.po   *.pot *.glade  "
						 //~ "filetypes.*  *.gtkrc *.conf *.xml *.css *.sh   Makefile.am";
	gint64 start;
	gint cnt = 10000;
	
	start = g_get_real_time();
	for (gint i = 0; i < cnt; i++)
	{
		gchar *tmp = gen_multi_options1(multi, option);
		//~ printf("1: '%s'\n", tmp);
		g_free(tmp);
	}
	printf("gen_multi_options1: %ld\n", g_get_real_time() - start);
	
	start = g_get_real_time();
	for (gint i = 0; i < cnt; i++)
	{
		gchar *tmp = gen_multi_options2(multi, option);
		//~ printf("2: '%s'\n", tmp);
		g_free(tmp);
	}
	printf("gen_multi_options2: %ld\n", g_get_real_time() - start);
	
}

#define FLAG1 1

// The "defined" preproc-keyword and the parentheses must be
// on the same line, but there may be spaces or tabs between them.
#if defined 	 (FLAG1)
	#define FLAG2 11
#else
	#define FLAG2 22
#endif

int main(void)
{
	//~ printf("flag: %d\n", FLAG2);
	
	run_test_case18();
	printf("OK\n");
	return 0;
}
