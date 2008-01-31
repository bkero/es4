/*
 * getters and setters:
 *    Properties can be implemented not as storage locations that hold values but as
 *    virtual properties: as pairs of functions, a getter that produces a property
 *    value and a setter that receives a new property value.
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: getters and setters" );  // print header

class c {
  var _x;
  function get x():int {
    return _x;
  }
  function set x(x1) {
    _x=x1;
  }
}
var c1:c=new c();
c1.x=10;
var c2:c=new c();

new TestCase("getters and setters","use a getter after setter",10,c1.x);
new TestCase("getters and setters","use a getter before setter",undefined,c2.x);

test();  // after testcases are defined, run them