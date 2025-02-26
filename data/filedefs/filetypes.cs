# For complete documentation of this file, please see Geany's main documentation
[styling=C]

[keywords]
# Standard keywords: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/
std_words=abstract as base bool break byte case catch char checked class const continue decimal default delegate do double else enum event explicit extern finally fixed float for foreach goto if implicit in int interface internal is lock long namespace new object operator out override params private protected public readonly ref return sbyte sealed short sizeof stackalloc static string struct switch this throw try typeof uint ulong unchecked unsafe ushort using virtual void volatile while
# Additional keywords
add_words=add alias ascending async await descending dynamic from get global group into join let orderby partial remove select set value var where yield
# Common keywords
com_words=null true false
# Some doxygen keywords (incomplete)
doc_words=attention author brief bug class code date def enum example exception file fn namespace note param remarks return see since struct throw todo typedef var version warning union
# Additional classes
add_classes=

[lexer_properties=C]

[settings]
lexer_filetype=C

# default extension used when saving files
extension=cs

# MIME type
mime_type=text/x-csharp

# the following characters are these which a "word" can contains, see documentation
#wordchars=_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789

# single comments, like # in this file
comment_single=//
# multiline comments
comment_open=/*
comment_close=*/

# set to false if a comment character/string should start at column 0 of a line, true uses any
# indentation of the line, e.g. setting to true causes the following on pressing CTRL+d
#	command_example();
# setting to false would generate this
#	command_example();
# This setting works only for single line comments
comment_use_indent=true

# context action command (please see Geany's main documentation for details)
context_action_cmd=

[indentation]
#width=4
# 0 is spaces, 1 is tabs, 2 is tab & spaces
#type=1

[build_settings]
# %f will be replaced by the complete filename
# %e will be replaced by the filename without extension
# (use only one of it at one time)
# be careful following settings are untested
compiler=mcs /t:winexe "%f" /r:System,System.Drawing
run_cmd=mono "%e.exe"
