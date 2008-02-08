/*
 * name:
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
class c2 extends c {
   var val3=3
}
var ci=new c2

new TestCase("class inheritance","class inheritance is instanceof inherting class",true,ci instanceof c);
new TestCase("class inheritance","class inheritance is correct class",true,ci is c2);
new TestCase("class inheritance","class inheritance new var",3,ci.val3);
new TestCase("class inheritance","class inheritance inherited var",1,ci.val);
new TestCase("class inheritance","class inheritance inherited function",15,ci.f(5));
test();  // after testcases are defined, run them