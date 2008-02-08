/*
 * null,*,undefined types:
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: name" );  // print header

var a:null;
var b:undefined;
var c:*;
c=10;

new TestCase("nullStartUndefinedTypes","null is a valid var type",null,a);
new TestCase("nullStartUndefinedTypes","undefined is a valid var type",undefined,b);
new TestCase("nullStartUndefinedTypes","* is a valid var type, can hold int",10,c);

c="astring";
new TestCase("nullStartUndefinedTypes","* is a valid var type, can hold string","astring",c);

test();  // after testcases are defined, run them