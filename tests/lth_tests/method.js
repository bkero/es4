class C {
    var x = 10;
    function f() { return x; }
}
class D extends C {
    var y = 20;
    function g() { return y; }
}
var o = new D;
o.f() + o.g();
