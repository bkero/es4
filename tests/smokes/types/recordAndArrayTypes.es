/*
 * name:
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: record and array types" );  // print header

var a1:[int]=[1,2,3];
new TestCase("record and array types","using [type] syntax","1,2,3",a1.toString());

test();  // after testcases are defined, run them