// test fixture lookup

x = 10
dynamic class A {
function m () {
  intrinsic::assert (x==10)
}

function n () {
  var o = { x: 20 }
  with (o) {
    intrinsic::assert (x==20)
  }
}
}
class B extends A {
  var x = -1
}
var a = new A
a.x = -1
a.m ()
print ("out of scope instance expandos: PASSED!")
var b = new B
b.m ()
print ("out of scope instance fixtures: PASSED!")
b.n ()
print ("with scope: PASSED!")

// test with lookup

// test class disambiguation
/*
namespace N1
namespace N2
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