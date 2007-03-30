class C {
    var x = 20;
}
class D extends C {
}
/* prototype properties in the derived class should override 
   properties from the base class (and by implication also
   properties from the derived class).  Spec section 9.3.2,
   which says that if the prototype has a value x then that
   takes precedence over any x in the instance.  (This is
   vile and counterintuitive.)  */
D.prototype.x = 37;
(new D).x;  // 37
