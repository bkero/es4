class C {
    function f() { return this.x; }
    var x = 10;
}
(new C).f();
