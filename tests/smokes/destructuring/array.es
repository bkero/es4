/*
 * destructuring array patterns:
 *    The left-hand-side of the the = operator may be a record of array pattern, signifying
 *    that the pattern on the left should be used to extract properties from the value on
 *    the right.
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: destructuring array patterns" );  // print header

var [a,b]:[int,int]=[1,2];
function f():[int,int]{ return [3,4]; }
var [c,d]:[int,int]=f();
print("a="+a);
print("b="+b);
print("c="+c);
print("d="+d);

new TestCase("destructing array patterns","var array pattern,1",1,a);
new TestCase("destructing array patterns","var array pattern,2",2,b);
new TestCase("destructing array patterns","function array pattern,1",3,c);
new TestCase("destructing array patterns","function array pattern,2",4,d);

test();  // after testcases are defined, run them