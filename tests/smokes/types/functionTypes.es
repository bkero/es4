/*
 * name:
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: function types" );  // print header

function func1(i1:int,i2:int):void {}
new TestCase("function types","simple typed function","[function Function]",""+func1);

test();  // after testcases are defined, run them