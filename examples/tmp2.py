# this is the compiled file
TEST5 = "part1 \
s		 part2 \
		 part3"

TEST5 = "part1 \
		 part2 \
s		 part3"

TEST5 = """part1 \
s		   part2 \
		   part3"""

TEST5 = """part1 \
		   part2 \
s		   part3"""

TEST5 = 'fsdf%ssdf		\
			sdf\nsdf%d	\
			sdf\nsdf	\
\n		'

TEST5 = 'fsdfsdf \		\
			%sdfsdf \   \
			sdf\sdf \	\
%s		'

TEST5 = '''fsdf%ssdf
			sdf\nsdf%d
			sdf\nsdf
\n		'''

TEST5 = '''fsdfsdf \
			%sdfsdf \    
			sdf\sdf \	\
%s		'''

TEST5 = f"asdfsdf		\
		%s	{4+45}		\
			hgjghfhfdg	\
		"

TEST5 = f"asdfsdf		\
			{4+45}		\
		%s	hgjghfhfdg	\
		"

TEST5 = f"asdfsdf		\
			{4+45}		\
			hgjghfhfdg	\
		"

TEST5 = f"asdfsdf		\
			{4+45}		\
			hgjghfhfdg	\
%s		"

TEST5 = f"""asdfsdf
%s			{4+45}
			hgjghfhfdg
		"""

TEST5 = f"""asdfsdf
	%s		{4+45}
			hgjghfhfdg
%s		"""

TEST5 = f"""asdfsdf
			{4+45}
			hgjghfhfdg
		"""


TEST5 = f"""{f'''{f"{f'{1}'}"}'''}"""

test = f"{TEST5:}"
test = f"{TEST5:ssdfsdf}"
test = f"{TEST5:ssdfsdf }"
test = f"{TEST5: ssdfsdf}"
test = f"{TEST5!a}"
test = f"{TEST5!r}"
test = f"{TEST5!s}"

test = f"""{TEST5
!s:
%A}"""

test = f"""{TEST5
!s:
\n}"""

test = f"""{TEST5
:
%A}"""

test = f"""{TEST5
:
\n}"""


test = f"{TEST5:\
%A\
}"

test = f"{TEST5:\
\n\
}"

test = f'{TEST5:\
%A\
}'

test = f'{TEST5:\
\n\
}'

test = f'{TEST5:\
%A\
%A}'

test = f'{TEST5:\
\n\
\n}'


test = f"""{TEST5:\
%A\
}"""

test = f"""{TEST5:\
\n\
}"""

test = f'''{TEST5:\
%A\
}'''

test = f'''{TEST5:\
\n\
}'''

test = f'''{TEST5:\
%A\
%A}'''

test = f'''{TEST5:\
\n\
\n}'''


f'{v:\}'
f'{v:\n\}'
f'{v:%A\}'
f"""{v:\}"""
f"""{v:\n\}"""
f"""{v:%A\}"""

#~ https://peps.python.org/pep-0498/
#~ f'<text> { <expression> <optional !s, !r, or !a> <optional : format specifier> } <text>'

f'My name is {name}, my age next year is {age+1}, my anniversary is {anniversary:%A, %B %d, %Y}.'
f'He said his name is {name!r}.'
f'input={value:#06x}'
f'{date} was on a {date:%A}'
f'{date} was on a {date:\n}'
fr'x={4*10}\n'
f"abc {a['x']} def"
f'''abc {a['x']} def'''
f'a={d["a"]}'
f'abc{expr1:spec1}{expr2!r:spec2}def{expr3}ghi'
f'result: {value:{width}.{precision}}'
f'result: {value:{width}{precision}}'
f'result: {value:sdf{width}234{precision}sdf}'
f'number of items: {len(items)}'
f'mapping is { {a:b for (a, b) in ((1, 2), (3, 4))} }'
fr'{header}:\s+'
f'{a!r}'
f'{(lambda x: x*2)(3)}'
f'{extra},waiters:{len(self._waiters)}'
f" [line {lineno:2d}]"
f'{(lambda x: x*2)(3)}'
f'{test!=None}'
