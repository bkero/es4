use namespace intrinsic
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

let i : I = new A
let j : J = new A
let k : K = new A
i.m()
j.m()
k.m()