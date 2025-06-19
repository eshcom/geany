f"""asdfsdf
sdf %s{1!r}%s\
"""

f"""asdfsdf
sdf \n{1:%A}\n\
"""

f"""asdfsdf
sdf \{1}\
"""

f"""asdfsdf
\{1}
"""

f"asdfsdf\
sdf \{1}\
"


f"""asdfsdf{{{1}}}"""

#~ https://peps.python.org/pep-0498/

f'''asdfsdf
sdf \
'''

f"""asdfsdf{{{1}}}"""

#---------------------

f'''asdfsdf
sdf {1}\
'''

f"""asdfsdf{{{1}}}"""

#~ https://peps.python.org/pep-0498/

#---------------------

t = f"""{a:\
}"""

t = f'''{a:b\
}'''

t = f'''{{a:b\
}}'''

t = f'''{{{a:b\
}}}'''

t = f'''{{{{a:b\
}}}}'''

t = f'''{{a:b\ }}'''


t = f"{a:\
}"

t = f'{a:b\
}'

t = f'{a:b\	asdf
}'

# backslash should be highlighted as SCE_P_STRINGEOL
t = f'{a:b\	
}'

# backslash should be highlighted as SCE_P_STRINGEOL
t = f'{a:b\		
}'

t = f'{{a:b\
}}'

t = f'{{{a:b\
}}}'

t = f'{{{{a:b\
}}}}'

t = f'{{a:b\ }}'

#---------------------

# valid syntax
f'{f"{1}"}'
f"{f'{1}'}"
f'{f"""{1}"""}'
f"{f'''{1}'''}"
f'''{f'{1}'}'''
f"""{f"{1}"}"""
f"""{f'''{f"{f'{1}'}"}'''}"""

f"{1}\
"

f"\
{1}"

f"\
{1}\
"

f"{f'{1}'}\
"

f"\
{f'{1}'}\
"

# invalid syntax
f'{'
f'}'
f'{'1'}'
f'{2'1'}'
f'{f'{1}'}'
f"{f"{1}"}"
f'{f'''{1}'''}'
f"{f"""{1}"""}"
f"{f'{f"{1}"}'}"
f"{f'{f"""{f'''{1}'''}"""}'}"
f"""{f'''{f"{f'{f"{1}"}'}"}'''}"""

f"{f'\
{1}'}"

f"{f'{1}\
'}"

f"{f'{1\
}'}"

f"2{2 f'2{1}2\
'2}2"

f'2{2 f'2{1}2\
'2}2'

f'''2{2 f'2{1}2\
'2}2'''

f'''2{2 f"""2{1}2\
"""2}2'''

f"""{f'{'1'}'}"""

#---------------------

t = f'''{a:b\
s}'''

f"""{a:\
s}"""

f"""{a:\n
s}"""

t = f'''{a:b%
s}'''

f"""{a:%
s}"""

f"""{a:%s
s}"""

# -------------------

f"""asdfsdf{{{1}}}"""

f"""{a:\
}"""

f"""{a:%
}"""
