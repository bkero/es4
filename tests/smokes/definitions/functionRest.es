/*
 * name:
 *   ...ES4 introduces the parameter syntax "...rest"
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: function rest parameters" );  // print header

function f(...args:Array) {
  var out="";
  for (let i=0;i<args.length;i++) {
    out+=args[i];
    if (i!=args.length-1) out+=",";
  }
  return out;
}
function g(...args) {
  var out="";
  for (let i=0;i<args.length;i++) {
    out+=args[i];
    if (i!=args.length-1) out+=",";
  }
  return out;
}

new TestCase("function rest parameters","rest parameter ... can be typed as Array","1,2,3",f(1,2,3));
new TestCase("function rest parameters","rest parameter ... can be untyped","4,5,6",f(4,5,6));

test();  // after testcases are defined, run them