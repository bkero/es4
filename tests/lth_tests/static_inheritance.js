var mine_mine_mine = 10;
class C {
    static var mine_mine_mine = 20;
}
class D extends C {
    static var foo = mine_mine_mine; // 20
    function f() { return mine_mine_mine; }  // 20
}
(new D).f() + D.foo;
