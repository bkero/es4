/*
package p.q {
public class A { var x = 10 }
}
*/

/*
let f = function (x:int) { return x + x }
f(10)
*/

package {
namespace foo
class A { 
use namespace foo
function A (x) : x=x { }
foo var x
}
var a = new A(10)
intrinsic::print(a.foo::x)
}


/*
[x,y] = [10,20]
intrinsic::print(x)
*/

/*
(type function(int):int)
*/