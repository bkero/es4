class A {
function A(x) { m(this,x) }
function m(this:A,x) { print(this) }
}
new A(10)