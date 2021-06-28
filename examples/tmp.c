int main()
{
	const double RENT = 3852.99;
	printf("*%8f*\n", RENT);
	printf("*%e*\n", RENT);
	printf("*%4.2f*\n", RENT);
	printf("*%3.1f*\n", RENT);
	printf("*%10.3f*\n", RENT);
	printf("*%10.3E*\n", RENT);
	printf("*%+4.2f*\n", RENT);
	
	printf("[%2s]\n", BLURB);
	printf("[%24s]\n", BLURB);
	printf("[%24.5s]\n", BLURB);
	printf("[%-24.5s]\n", BLURB);
	
	printf("%x %X %#x\n", 31, 31, 31);
	printf("**%d**% d**% d**\n", 42, 42, -42);
	printf("**%5d**%5.3d**%05d**%05.3d**\n", 6, 6, 6, 6);
	printf("%.5s = %0*.*f", "value", 10, 5, M_PI);
	printf("%0*x", 8, 15);
	printf("%0*lli", width, x);
	printf("%0*"PRIi64, width, x);
	printf("%zuT%X\n", s);
	
	printf("*%d*\n", PAGES);
	printf("*%2d*\n", PAGES);
	printf("*%10d*\n", PAGES);
	printf("*%-10d*\n", PAGES);
	
	//~ https://alvinalexander.com/programming/printf-format-cheat-sheet/
	printf("'%5d'", 10);					// '   10'
	printf("'%-5d'", 10);					// '10   '
	printf("'%05d'", 10);					// '00010'
	printf("'%+5d'", 10);					// '  +10'
	printf("'%-+5d'", 10);					// '+10  '
	printf("'%.1f'", 10.3456);				// '10.3'
	printf("'%8.2f'", 10.3456);				// '   10.35'
	printf("'%08.2f'", 10.3456);			// '00010.35'
	printf("'%-8.2f'", 10.3456);			// '10.35   '
	printf("'%s'", "Hello");				// 'Hello'
	printf("'%10s'", "Hello");				// '     Hello'
	printf("'%-10s'", "Hello");				// 'Hello     '
	
	//~ https://www.cplusplus.com/reference/cstdio/printf/
	printf ("Characters: %c %c \n", 'a', 65);
	printf ("Decimals: %d %ld\n", 1977, 650000L);
	printf ("Preceding with blanks: %10d \n", 1977);
	printf ("Preceding with zeros: %010d \n", 1977);
	printf ("Some different radices: %d %x %o %#x %#o \n", 100, 100, 100, 100, 100);
	printf ("floats: %4.2f %+.0e %E \n", 3.1416, 3.1416, 3.1416);
	printf ("Width trick: %*d \n", 5, 10);
	printf ("%s \n", "A string");
	
	return 0;
}

printf("tes%t %sT%%\n%%T%d\n%5dT%.7d\n%5.7d\n%5.*d\n%-+05d\n%%s% %s");
printf("tes%% %llT%lllT%liT%ldT%luT%lfT%lFT%lliT%lldT%llu");
printf("tes%s %hhT%hhhT%hiT%huT%hhxT%hhXT%hhoT%hhiT%hhu");
printf("tes%c %gT%GT%eT%ET%LfT%LFT%LgT%LGT%LeT%LET%pT");

TEST1 = "\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x";
TEST2 = '\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x';
TEST3 = "\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0";
TEST4 = '\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0';

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

TEST5 = "part1 
		 part2 
		 part3";

TEST5 = "part1 
		 part2";


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
