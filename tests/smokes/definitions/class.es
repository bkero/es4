/*
 * class:
 *    description
 *    A class describes an object by presenting those properties (fields) of the object that are always present (the
 *    fixed properties or fixtures), including variables, constants, and methods
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: name" );  // print header

class c {
   var val=1
   const x=3.14
   function f(n) { return n*(n+1)/2; }
}
var ci=new c

new TestCase("class instance","class is instance of c",true,ci is c);
new TestCase("class","var property",1,ci.val);
new TestCase("class","const property",3.14,ci.x);
new TestCase("class","function",55,ci.f(10));

test();  // after testcases are defined, run them