#define TEST1 "\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x"
#define TEST2 '\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x'
#define TEST3 "\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0"
#define TEST4 '\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0'

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
			\ntest";
	ch = '5.33     \
			\ntest';
	
	ch = "5.33     \
t			\ntest";
	ch = '5.33     \
t			\ntest';
	
	ch = "5.33    n\
			\ntest";
	ch = '5.33    n\
			\ntest';
	
	ch = "5.33   \n\
			\ntest";
	ch = '5.33   \n\
			\ntest';
	
	ch = "5.33   \\\
			\ntest";
	ch = '5.33   \\\
			\ntest';
	
	ch = "5.33  \nt\
			\ntest";
	ch = '5.33  \nt\
			\ntest';
	
	ch = "5.33   \n\
t			\ntest";
	ch = '5.33   \n\
t			\ntest';
	
	ch = "5.33     \	
			\ntest";
	ch = '5.33     \	
			\ntest';
	
	ch = "5.33     \		
			\ntest";
	ch = '5.33     \		
			\ntest';
	
	ch = "5.33     \
			\ntest \
			\ntest";
	ch = '5.33     \
			\ntest \
			\ntest';
	
	ch = "5.33 
			\ntest \
			\ntest";
	ch = '5.33 
			\ntest \
			\ntest';
	
	ch = "5.33     \
			\ntest 
			\ntest";
	ch = '5.33     \
			\ntest 
			\ntest';
	
	ch = "5.33     \
			test\n 
			test\n";
	ch = '5.33     \
			test\n 
			test\n';
	
	ch = "5.33   \"
			\ntest";
	ch = '5.33   \'
			\ntest';
	
	ch = "5.33   \"
			"\ntest";
	ch = '5.33   \'
			'\ntest';
	
	ch = "5.33 
			\ntest";
	ch = '5.33 
			\ntest';
}
