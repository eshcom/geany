#!/bin/sh
#
# Author:  Colomban Wendling <colomban@geany.org>
# License: GPL v2 or later
#
# Updates the `std_words` and `std_idents` entries in data/filetypes.python.
# Requires both Python 2 and 3.

set -e

file=data/filedefs/filetypes.python

[ -f "$file" ]

py_2_and_3() {
  python2 "$@" && python3 "$@"
}

# sort_filter [exclude...]
sort_filter() {
  python -c '\
from sys import stdin; \
items=set(stdin.read().strip().split("\n")); \
exclude=['"$(for a in "$@"; do printf "'%s', " "$a"; done)"']; \
print(" ".join(sorted([i for i in items if i not in exclude])))
'
}

keywords=$(py_2_and_3 -c 'from keyword import kwlist; print("\n".join(kwlist))')
builtins=$(py_2_and_3 -c 'print("\n".join(dir(__builtins__)))')

std_words=$(echo "$keywords" | sort_filter)
# builtins, but excluding keywords that are already listed in std_words=
std_idents=$(echo "$builtins" | sort_filter $std_words)

sed -e "s/^std_words=.*$/std_words=$std_words/" \
    -e "s/^std_idents=.*$/std_idents=$std_idents/" \
    -i "$file"
