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


int main(void)
{
	run_test_case01();
	run_test_case02();
	
	return 0;
}
