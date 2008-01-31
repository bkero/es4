/*
 * package:
 *    A package is a compile-time value comprised of a package name...
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

package org.ecmascript.experiment1 {
  function f(v:int):int {
    return v;
  }
}
package org.ecmascript.experiment2 {
   var n:int=10;
}
/*
package {
  function f(v:int): int {
   return v*2;
  }
}
*/
package org.ecmascript.experiment1 {
     public var n=10;
     public var x=15;
}
package org.ecmascript.experiment2 {
     public var n=20;
     public var x=25;
}
import org.ecmascript.experiment1.*
var n=30;

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: package" );  // print header

new TestCase("import package","after import not fully qualified",10,n);
new TestCase("import package","after import not fully qualified with local var",15,x);
new TestCase("import package","after import fully qualified",10,n);
new TestCase("import package","after import fully qualified with local var",15,x);

test();  // after testcases are defined, run them