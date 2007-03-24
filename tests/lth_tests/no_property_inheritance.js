var y = 10;
class C {
    function f() { return y; }  // outer y, not the one in the subclass
}
class D extends C {
    var y = 20;
}
(new D).f();
