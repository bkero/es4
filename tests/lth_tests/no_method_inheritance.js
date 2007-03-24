function g() { return 10; }
class C {
    function f() { return g(); }  // outer g, not the one in the subclass
}
class D extends C {
    function g() { return 20; }
}
(new D).f();
