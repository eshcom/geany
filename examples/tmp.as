t2 = new TextField();

ArgumentError error = NaN;
Boolean
Class
Null

t2.x = 20;
t2.y = 64;
t2.width = 150;
t2.height = 20;
t2.text = "Это приложение";

tf = new TextFormat( "Arial", 14, 0xff0000, false, true );

t2.setTextFormat( tf );
addChild( t2 );

t3 = new TextField();

t3.x = 20;
t3.y = 80;
t3.width = 180;
t3.height = 20;
t3.text = "разработано при помощи";

tf = new TextFormat( "Arial", 14, 0xff0000 );

t3.setTextFormat( tf );
addChild( t3 );

t4 = new TextField();

t4.x = 20;
t4.y = 110;
t4.width = 150;
t4.height = 50;
t4.text = "Flash";

tf = new TextFormat( "Calibri", 32, 0xd0d0d0, true, true );

t4.setTextFormat( tf );
addChild( t4 );

t5 = new TextField();

t5.x = 90;
t5.y = 110;
t5.width = 150;
t5.height = 50;

t5.text = "Develop";

tf = new TextFormat( "Calibri", 32, 0xf09e14, true, true );

t5.setTextFormat( tf );
addChild( t5 );
