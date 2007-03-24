class C {
  var x = 0;
  function C() { x += 37; }
}
class D extends C {
  // compiler must insert code to call super if it's not called explicitly,
  // that means also before the return statement.
  function D(flag) {
    if (flag) return;
  }
}
(new D(true)).x == 37 && (new D(false)).x == 37;
