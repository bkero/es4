class C {
  var x;
  function C(n) { x = n; }
}
class D extends C {
  function D(n) { super(n); }
}
(new D(37)).x;
