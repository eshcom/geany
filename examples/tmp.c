#define TEST1 "\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x"
#define TEST2 "\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0"
#define TEST3 '\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x'
#define TEST4 '\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0'

int ch;
if (TRUE) {
	ch = "t\";
	ch = "t\n";
	ch = "t\";
	ch = "t\n\";
	ch = "test";
	ch = "test;
	ch = test";";
	ch = test";
	ch = "5.33";
	ch = "5.33';
	ch = "5.33;
	ch = "t\r";
	ch = "t\nr";
	ch = "t\r";
	ch = "t\n\r";
	ch = "test";
	ch = "test""test";
	ch = "test"t"test";
}
if (TRUE) {
	ch = 't\';
	ch = 't\n';
	ch = 't\';
	ch = 't\n\';
	ch = 'test';
	ch = 'test;
	ch = test';';
	ch = test';
	ch = '5.33';
	ch = '5.33";
	ch = '5.33;
	ch = 't\r';
	ch = 't\nr';
	ch = 't\r';
	ch = 't\n\r';
	ch = 'test''test';
	ch = 'test't'test';
}
