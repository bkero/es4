use namespace intrinsic
namespace foo
{
  use default namespace foo
  public class Object {}
  class A {
    prototype var x = 10
    prototype function f() { print("A.prototype.f","\n") }
    function m() {} 
  }
  A.prototype.foo::f()
  new A().foo::f()
}