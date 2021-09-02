const gchar *message1 = "text";
const gchar *message2 = "text";
ui_set_statusbar(TRUE, "message1 = %s", message1); // esh: log
ui_set_statusbar(TRUE, "message1 = %s, message2 = %s", message1, message2); // esh: log

ui_set_statusbar(TRUE, "No more message items."); // esh: log

----------------------------------------------------------------------------------------

	ui_set_statusbar(TRUE, "tmtag: "
		"name = %s, type = %d, scope = %s, var_type = %s, file = %s, "
		"arglist = %s, lang = %d, impl = %s, inheritance = %s",
		tmtag->name, tmtag->type, tmtag->scope, tmtag->var_type, 
		tmtag->file->short_name, tmtag->arglist, tmtag->lang,
		tmtag->impl, tmtag->inheritance); // esh: log

----------------------------------------------------------------------------------------

configuration_reload_default_session (geany.conf)
load_settings -> configuration_load (geany.conf)
load_startup_files -> (main_load_project_from_command_line, load_session_project_file) -> project_load_file -> load_config (*.geany)
project_load_file_with_session -> -> project_load_file -> load_config (*.geany)
	-> configuration_load_session_files
	-> configuration_save_default_session


(configuration_save, configuration_save_default_session, write_config) -> configuration_save_session_files
project_new -> configuration_clear_default_session
	-> remove_session_files


vte_start -> (vte_terminal_spawn_sync, vte_terminal_fork_command)
vte_send_cmd -> vte_terminal_feed_child

----------------------------------------------------------------------------------------

keybindings/goto_tag -> symbols/symbols_goto_tag -> symbols/goto_tag

----------------------------------------------------------------------------------------

--string example1:
	wildcard = g_strndup(cmd, (gsize) (match - cmd + 1));
	cmd = g_strndup(wildcard + 9, strlen(wildcard) - 10);

--string example2:
	return g_strndup(&text[start], end - start);

--string example3:
	len = (gsize)(cur - begin);
	return len ? g_strndup(begin, len) : NULL;

----------------------------------------------------------------------------------------

--if example1:
if (g_strcmp0(documents[j]->real_path, tag->file->file_name) == 0)

--if example2:
gchar *dir = g_path_get_dirname(doc->real_path);
if (g_str_has_prefix(tag->file->file_name, dir))

----------------------------------------------------------------------------------------

--C:
tmtag: scope = (null), name = find_tags, type = 16, var_type = void, file = prjorg-sidebar.c, arglist = (const gchar *name, gboolean declaration, gboolean case_sensitive, MatchType match_type, gchar *utf8_path)
tmtag: scope = (null), name = find_tags, type = 16, var_type = void, file = geanyctags.c, arglist = (const gchar *name, gboolean declaration, gboolean case_sensitive, MatchType match_type)
tmtag: scope = (null), name = MatchType, type = 4096, var_type = anon_enum_1, file = prjorg-sidebar.c, arglist = (null)
tmtag: scope = (null), name = MatchType, type = 4096, var_type = anon_enum_2, file = geanyctags.c, arglist = (null)

--Erlang:
tmtag: scope = adptransfer_log, name = warn, type = 16, var_type = (null), file = adptransfer_log.erl, arglist = (null)

--Python:
tmtag: scope = encode_revisions, name = append, type = 16, var_type = (null), file = helper.py, arglist = (start, last, list)
tmtag: scope = VCSAction, name = append, type = 128, var_type = (null), file = action.py, arglist = (self, func, *args, **kwargs)
tmtag: scope = VCSAction, name = __init__, type = 128, var_type = (null), file = action.py, arglist = (self, client, register_gtk_quit=False, notification=True,
tmtag: scope = GitAction, name = __init__, type = 128, var_type = (null), file = action.py, arglist = (self, client, register_gtk_quit=False, notification=True,
tmtag: scope = InterfaceView, name = __init__, type = 128, var_type = (null), file = __init__.py, arglist = (self, *args, **kwargs)
tmtag: scope = GtkContextMenu, name = get_widget, type = 128, var_type = (null), file = contextmenu.py, arglist = (self)
tmtag: scope = GtkBuilderWidgetWrapper, name = get_widget, type = 128, var_type = (null), file = __init__.py, arglist = (self, id = None)
tmtag: scope = (null), name = GtkContextMenuCaller, type = 32768, var_type = (null), file = browser.py, arglist = (null)
tmtag: scope = (null), name = vcs, type = 32768, var_type = (null), file = log.py, arglist = (null)
tmtag: scope = (null), name = vcs, type = 32768, var_type = (null), file = action.py, arglist = (null)

--filter by file:
include tags: 32768 | 4096 | 16
no check empty(cursor scope)

----------------------------------------------------------------------------------------

	gint offset;
	gint line_no;

	g_return_if_fail(doc != NULL);

	get_line_and_offset_from_text(result, &line_no, &offset);
	if (! editor_goto_line(doc->editor, line_no, offset))
		utils_beep();
----

	GeanyDocument *new_doc, *old_doc;

	g_return_if_fail(tag);

	old_doc = document_get_current();
	new_doc = document_open_file(tag->file->file_name, FALSE, NULL, NULL);

	if (new_doc)
		navqueue_goto_line(old_doc, new_doc, tag->line);

----------------------------------------------------------------------------------------

palette:
['#2E2E34343636', '#CCCC00000000', '#4E4E9A9A0606', '#C4C4A0A00000', '#34346565A4A4', '#757550507B7B', '#060698209A9A', '#D3D3D7D7CFCF', '#555557575353', '#EFEF29292929', '#8A8AE2E23434', '#FCFCE9E94F4F', '#72729F9FCFCF', '#ADAD7F7FA8A8', '#3434E2E2E2E2', '#EEEEEEEEECEC']
['rgb(0,0,0)', 'rgb(204,0,0)', 'rgb(78,154,6)', 'rgb(196,160,0)', 'rgb(52,101,164)', 'rgb(117,80,123)', 'rgb(6,152,154)', 'rgb(211,215,207)', 'rgb(85,87,83)', 'rgb(239,41,41)', 'rgb(138,226,52)', 'rgb(252,233,79)', 'rgb(114,159,207)', 'rgb(173,127,168)', 'rgb(52,226,226)', 'rgb(238,238,236)']

gsize n_colors = 16;
--------------------

--example set vte_terminal from terminal-screen.c:

  colors = terminal_g_settings_get_rgba_palette (priv->profile, "palette", &n_colors);
  vte_terminal_set_colors (VTE_TERMINAL (screen), &fg, &bg,
                           colors, n_colors);
  vte_terminal_set_color_bold (VTE_TERMINAL (screen), boldp);

----------------------------------------------------------------------------------------

#run debug geany:
gdb geany
#enter:
>r
#enter:
>bt

Thread 1 "geany" received signal SIGSEGV, Segmentation fault.
0x00007ffff7b0b51f in makeTagEntry (tag=tag@entry=0x7fffffffd200) at main/entry.c:1255
1255		if (tag->name [0] == '\0' && (!tag->placeholder))
(gdb) bt
#0  0x00007ffff7b0b51f in makeTagEntry (tag=tag@entry=0x7fffffffd200) at main/entry.c:1255
#1  0x00007ffff7b27038 in makePascalTag (tag=0x7fffffffd200) at parsers/pascal.c:61
#2  findPascalTags () at parsers/pascal.c:272
#3  0x00007ffff7b23376 in createTagsForFile (language=<optimized out>, passCount=1)
    at main/parse.c:2158
#4  0x00007ffff7b263ce in createTagsWithFallback1 (userData=0xd66d90, 
    passCallback=0x7ffff7afe200 <ctags_pass_start>, language=4) at main/parse.c:2233
#5  createTagsWithFallback (buffer=<optimized out>, bufferSize=<optimized out>, 
    fileName=<optimized out>, language=4, tagCallback=<optimized out>, 
    passCallback=0x7ffff7afe200 <ctags_pass_start>, userData=0xd66d90) at main/parse.c:2320
#6  0x00007ffff7b09642 in ctagsParse (buffer=<optimized out>, bufferSize=bufferSize@entry=47, 
    fileName=fileName@entry=0x10e7b30 "/home/tatiana/upload/geany_crash/fhackform2.pas", 
    language=<optimized out>, tagCallback=tagCallback@entry=0x7ffff7afe3c0 <ctags_new_tag>, 
    passCallback=passCallback@entry=0x7ffff7afe200 <ctags_pass_start>, userData=<optimized out>)
    at main/ctags-api.c:74

#exit:
>q

----------------------------------------------------------------------------------------
//флаг переноса строк и граница проверки переноса определяется двумя параметрами:
editor_prefs.line_wrapping
editor_prefs.line_break_column
//также есть еще такой параметр:
editor_prefs.line_breaking

Проблема с переносом строк наблюдается в файле editor.c

Обнаружил, что в функции keyfile.c/open_session_file периодически попадается doc->editor->line_breaking = 1
Значение приходит отсюда: line_breaking = atoi(tmp[8])

Затем, при вводе текста срабатывает функция editor.c/check_line_breaking, в которой editor->line_breaking = 1

keyfile.c/configuration_open_files -> keyfile.c/open_session_file(gchar **tmp, guint len)

Параметр tmp приходит из функции configuration_open_files и инициализируется так:
gchar **tmp = g_ptr_array_index(session_files, i);
Т.е. значение для line_breaking приходит из массива session_files

Наполнение массива session_files выполняется в функции keyfile.c/configuration_load_session_files:
g_ptr_array_add(session_files, tmp_array);

Переменная tmp_array нициализируется так:
g_snprintf(entry, sizeof(entry), "FILE_NAME_%d", i);
tmp_array = g_key_file_get_string_list(config, "files", entry, NULL, &error);

Делаем вывод, что значение для line_breaking приходит из файла сессии geany.

Смотрим файл geany.geany:
FILE_NAME_2=17008;C;0;EUTF-8;1;1;0;%2Fhome%2Fesh%2Fprojects%2Fgithub%2Fgeany%2Fsrc%2Feditor.c;1;4
FILE_NAME_3=45397;C;0;EUTF-8;1;1;0;%2Fhome%2Fesh%2Fprojects%2Fgithub%2Fgeany%2Fsrc%2Fsymbols.c;0;4
Здесь привел пример двух файлов, видим, что для editor.c установлено значение 1

Если в geany нажмем "Документ", то увидим, что стоит галочка "Перенос строк".
Т.е. оказывается перенос строк можно устанавливать индивидуально для каждого файла.

----------------------------------------------------------------------------------------

