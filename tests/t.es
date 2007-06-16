
package {
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
public function m() { print("A.m") }
public function n() { print("A.n") }
}
class B extends A implements I,J,K {}

let i : I = new B
let j : J = new B
let k : K = new B
i.m()
j.n()
k.m()
k.n()
}