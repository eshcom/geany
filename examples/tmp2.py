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
