TEST1 = "\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x"
TEST2 = '\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x'
TEST3 = "\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0"
TEST4 = '\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0'

TEST5 = "part1 \n\
		 part2 \n\
		 part3";

TEST5 = "part1 \n
		 part2 \n\
		 part3";

TEST5 = "part1 \n\
		 part2 \n
		 part3";

TEST5 = "part1 \
		 part2 \
		 part3";

TEST5 = "part1 \	
		 part2 \
		 part3";

TEST5 = "part1 \		
		 part2 \
		 part3";

TEST5 = "part1 \
		 part2 \	
		 part3";

TEST5 = "part1 \
		 part2 \		
		 part3";

TEST5 = "part1 \	
		 part2 \	
		 part3";

int ch;
if (TRUE) {
	ch = "t\";
	ch = 't\';
	
	ch = "t\"";
	ch = 't\'';
	
	ch = "t\n";
	ch = 't\n';
	
	ch = "t\r";
	ch = 't\r';
	
	ch = "t\n\";
	ch = 't\n\';
	
	ch = "t\n\"
	ch = 't\n\'
	
	ch = "t\n\
	---
	ch = 't\n\
	---
	
	ch = "t\
	---
	ch = 't\
	---
	
	ch = "t\n\r";
	ch = 't\n\r';
	
	ch = "t\nr";
	ch = 't\nr';
	
	ch = "\377";
	ch = '\377';
	
	ch = "test";
	ch = 'test';
	
	ch = "test"
		 "test";
	ch = 'test'
		 'test';
	
	ch = "test""test";
	ch = 'test''test';
	
	ch = "test"t"test";
	ch = 'test't'test';
	
	ch = "test;
	ch = 'test;
	
	ch = test";";
	ch = test';';
	
	ch = test";
	ch = test';
	
	ch = "5.33";
	ch = '5.33';
	
	ch = "5.33';
	ch = '5.33";
	
	ch = "5.33;
	ch = '5.33;
	
	ch = "5.33	
	ch = '5.33	
	
	ch = "5.33
	ch = '5.33
	
	ch = "5.33     \
t			\ntest";
	ch = '5.33     \
t			\ntest';
	
	ch = "5.33    n\
t			\ntest";
	ch = '5.33    n\
t			\ntest';
	
	ch = "5.33   \n\
t			\ntest";
	ch = '5.33   \n\
t			\ntest';
	
	ch = "5.33   \\\
t			\ntest";
	ch = '5.33   \\\
t			\ntest';
	
	ch = "5.33  \nt\
t			\ntest";
	ch = '5.33  \nt\
t			\ntest';
	
	ch = "5.33   \n\
t			\ntest";
	ch = '5.33   \n\
t			\ntest';
	
	ch = "5.33   \n
t			\ntest";
	ch = '5.33   \n
t			\ntest';
	
	ch = "5.33     \	
t			\ntest";
	ch = '5.33     \	
t			\ntest';
	
	ch = "5.33     \		
t			\ntest";
	ch = '5.33     \		
t			\ntest';
	
	ch = "5.33     \
t			\ntest \
t			\ntest";
	ch = '5.33     \
t			\ntest \
t			\ntest';
	
	ch = "5.33 
t			\ntest \
t			\ntest";
	ch = '5.33 
t			\ntest \
t			\ntest';
	
	ch = "5.33     \
t			\ntest 
t			\ntest";
	ch = '5.33     \
t			\ntest 
t			\ntest';
	
	ch = "5.33     \
t			test 
t			test";
	ch = '5.33     \
t			test 
t			test';
	
	ch = "5.33     \
t			test\n 
t			test\n";
	ch = '5.33     \
t			test\n 
t			test\n';
	
	ch = "5.33   \"
t			\ntest";
	ch = '5.33   \'
t			\ntest';
	
	ch = "5.33   \"
t			"\ntest";
	ch = '5.33   \'
t			'\ntest';
	
	ch = "5.33 
t			\ntest";
	ch = '5.33 
t			\ntest';
}
