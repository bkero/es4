interface I {
    function f(n : Number) : Number;
}

interface J {
    function g(n : Boolean) : String;
}

class C implements I, J {
    var x : Number = 10;
    function f(n : Number) : Number { return x+n; }
    function g(n : Boolean) : String { return "foo"; }
}

(new C).f(8);
