/*
package p.q {
public class A { var x = 10 }
}
*/

/*
let f = function (x:int) { return x + x }
f(10)
*/

/*
[x,y] = [10,20]
intrinsic::print(x)
*/

package p {
public class A {
public static var x = 10
}
}
import p.A
intrinsic::print(p.A.x)
