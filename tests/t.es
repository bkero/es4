use namespace intrinsic
class Object {}
class A {
  prototype var x = 10
  prototype function f() { print("A.prototype.f","\n") }
  function m() {}
}
A.prototype.f()
new A().f()