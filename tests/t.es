class Object {}
class A {
    use namespace intrinsic
    function f() { print("A.f","\n") }
    let l = 20
	static var x = l
	print("A.x ",A.x,"=",l,"\n")
}
new A().f()