/* January spec, 9.3.2 */
class A {
    prototype var x = 10;
}

var a1 = new A;
var a2 = new A;
A.prototype.x = 20;
a2.x = 30;
// The expected result is 50, because the last assignment creates a weird 
// shadowing property between a2 and a2.__prototype__.   This does not
// seem reasonable.
a1.x + a2.x;
