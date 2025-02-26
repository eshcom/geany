# For complete documentation of this file, please see Geany's main documentation
[styling=C]

[keywords=C]
# Standard keywords
std_words=alignas alignof and and_eq asm auto bitand bitor bool break case catch char char16_t char32_t class compl const const_cast constexpr continue decltype default delete do double dynamic_cast else enum explicit export extern final float for friend goto if inline int long mutable namespace new noexcept not not_eq operator or or_eq override private protected public register reinterpret_cast return short signed sizeof static static_assert static_cast struct switch template this thread_local throw try typedef typeid typename union unsigned using virtual void volatile wchar_t while xor xor_eq
# Common keywords (C-com_words + nullptr)
com_words=TRUE FALSE NULL NAN EOF WEOF true false noreturn stderr stdin stdout nullptr

[lexer_properties=C]

[settings]
lexer_filetype=C

# default extension used when saving files
extension=cpp

# MIME type
mime_type=text/x-c++src

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

[build-menu]
# %f will be replaced by the complete filename
# %e will be replaced by the filename without extension
# (use only one of it at one time)
FT_00_LB=_Compile
FT_00_LB[ru]=_Скомпилировать
FT_00_CM=g++ -Wall -c "%f"
FT_00_WD=
FT_01_LB=_Build
FT_01_LB[ru]=_Собрать
FT_01_CM=g++ -Wall -o "%e" "%f"
FT_01_WD=
FT_02_LB=_Lint
FT_02_LB[ru]=_Проверить код
FT_02_CM=cppcheck --language=c++ --enable=warning,style --template=gcc "%f"
FT_02_WD=
EX_00_LB=_Execute
EX_00_LB[ru]=_Выполнить
EX_00_CM="./%e"
EX_00_WD=
