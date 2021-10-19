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
	printf("build1 = %s\nbuild2 = %s\nbuild3 = %s\nbuild4 = %s\n"
		   "build5 = %s\nbuild6 = %s\nbuild7 = %s\nbuild8 = %s\n",
		   build1, build2, build3, build4, build5, build6, build7, build8);
	//~ Result:
	//~ build1 = /usr/test/dir/item
	//~ build2 = /usr/test/dir/item/
	//~ build3 = /usr/test/dir/item
	//~ build4 = /usr/test/dir/item/
	//~ build5 = /usr/test/dir/item
	//~ build6 = /usr/test/dir/item/
	//~ build7 = /usr/test/dir/item
	//~ build8 = /usr/test/dir/item/
	g_free(build1);
	g_free(build2);
	g_free(build3);
	g_free(build4);
	g_free(build5);
	g_free(build6);
	g_free(build7);
	g_free(build8);
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
}

int main(void)
{
	//~ run_test_case01();
	//~ run_test_case02();
	//~ run_test_case03();
	//~ run_test_case04();
	//~ run_test_case05();
	//~ run_test_case06();
	run_test_case07();
	
	return 0;
}
