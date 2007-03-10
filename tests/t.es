class Object {}
class A {
   use namespace intrinsic
   function f() { print("A.f","\n") }
}
new A().f()