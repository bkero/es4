class C {
    function f() { return 37; }
}

class D extends C {
    override function f() { return super.f(); }
}

(new D).f();  // 37, not a hang...

