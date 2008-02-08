/*
 * name:
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: union types" );  // print header

var n:(int,boolean);
n=5;
new TestCase("union types","union type (int,boolean) set to int",5,n);
n=false;
new TestCase("union types","union type (int,boolean) set to false",false,n);
var msg="no error thrown";
try {
  n="string";
} catch (e) {
  msg=e.toString();
}
new TestCase("union types","union type (int,boolean) set to string throws type error"
            ,"TypeError: incompatible types w/o conversion: val=\"string\" type=[ns public '__ES4__']::string  wanted=([ns public '__ES4__']::int , [ns public '__ES4__']::boolean )"
            ,msg);


test();  // after testcases are defined, run them