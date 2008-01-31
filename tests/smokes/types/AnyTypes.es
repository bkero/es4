/*
 * name:
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: predefined any types" );  // print header

var s:AnyString;
s="string";

var b:AnyBoolean;
b=false;

var n:AnyNumber;
n=100.1;

new TestCase("any types","AnyString type","string",s);
new TestCase("any types","AnyBoolean type",false,b);
new TestCase("any types","AnyNumber type",100.1,n);

test();  // after testcases are defined, run them