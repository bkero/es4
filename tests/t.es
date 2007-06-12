use namespace intrinsic
var a = 1
var b = true
a ||= b
print(a)
/*

interface I {
function m()
}
interface J extends I {
function n()
}
interface K extends I,J {
}
class A implements I,J,K {
function m() { print("A.m") }
function n() { print("A.n") }
}
class B extends A implements I,J,K {}

let i : I = new B
let j : J = new B
let k : K = new B
i.m()
j.n()
k.m()
k.n()

*/