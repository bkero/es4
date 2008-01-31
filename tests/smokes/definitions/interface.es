/*
 * interface:
 *    An interface gives an additional type to an object.  An interfce describes
 *    a collection of methods properties; a class can declare that it implements
 *    an interface and then back that up by providing definitions for the method
 *    in the interface.
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: interfaces" );  // print header

interface I { function f() }
interface J { function g() }

class C implements I {
  function f() {
    return "C.f()";
  }
}
class D implements I,J {
  function f() {
   return "D.f()";
  }
  function g() {
   return "D.g()";
  }
}
class E extends C implements I,J {
  function g() {
    return "E.g()";
  }
}

var c1=new C();
var c2=new D();
var c3=new E();

new TestCase("interface","class implements one interface","C.f()",c1.f());
new TestCase("interface","class implements two interfaces,call first","D.f()",c2.f());
new TestCase("interface","class implements two interfaces,call second","D.g()",c2.g());
new TestCase("interface","class calls inherited method from interface","C.f()",c3.f());
new TestCase("interface","class calls defined method from interface","E.g()",c3.g());

test();  // after testcases are defined, run them