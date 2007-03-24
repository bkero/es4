class C {
    var x = 10;
    function f() { return x; }
}
class D extends C {
    var y = 20;
    override function f() { return y; }
}
(new D).f(); // 20
