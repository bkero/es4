use namespace intrinsic
class Object {}
class A { 
  function f() { print("f","\n") }
}
var a = new A()
a.f()

function g() { print("g","\n") }
g()
g=10
print(g,"\n")

const function h() { print("h","\n") }
h()
h=20
print(h,"\n")

