package util {
use namespace intrinsic
public type T = int
}
package {
// import utils.T;  <-- should be an error
import util.T;
function f() : T
   10;
intrinsic::print(f())
}
