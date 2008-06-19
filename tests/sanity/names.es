// test fixture lookup

x = 10
dynamic class A {
function m () {
  print (x)
  intrinsic::assert (x==10)
}

function n () {
  var o = { x: 20 }
  with (o) {
    intrinsic::assert (x==20)
  }
}
}
dynamic class B extends A {
  var x = -1
}
var a = new A
A.x = -1
a.x = -2
a.m ()
print ("lexical lookup with instance expandos: PASSED!")
var b = new B
b.m ()
print ("lexical lookup with instance fixtures: PASSED!")

// test 'with' lookup

b.n ()
print ("lexical lookup with 'with': PASSED!")

// test scope base namespace selection

namespace N1
namespace N2
N1 var x = 10
N2 var x = 20
{
use namespace N2
{
use namespace N1
print (x)
intrinsic::assert (x==10)
print ("namespace shadowing: PASSED!")
}
}

// test class based namespace selection

/* FIXME

class C {
N1 var x = 10
}
class D extends C {
N2 var x = 20
}
{
use namespace N1
use namespace N2
var x = (new D).x
print (x)
//inrinsic::assert (x==10)
print ("class lookup: PASSED!")
}
*/

// test name reservation

   // requires two compilation units
