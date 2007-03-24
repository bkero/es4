var x = 10;

class C {
    function g() { return x; }
}

dynamic class D extends C {
    function f() { return x; }
}

var d = new D;
d.x = 37;
d.f() + d.g();  // 37+10 = 47

