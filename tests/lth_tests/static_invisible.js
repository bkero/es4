/* D is dynamic so that this will typecheck in bang. */
var mine_mine_mine = 10;
class C {
    static var mine_mine_mine = 20;
}
dynamic class D extends C {
}
D.mine_mine_mine; // undefined
