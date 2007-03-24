class C {
  var x = 0;
  function C() { x += 37; }
}
class D extends C {
  // compiler must insert code to call super if it's not called explicitly
  function D(flag) {
    if (flag) super();
  }
}
(new D(true)).x == 37 && (new D(false)).x == 37;
