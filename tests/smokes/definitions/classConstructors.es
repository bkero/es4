/*
 * name:
 *    A class describes an object by presenting those properties (fields) of the object that are always present (the
 *    fixed properties or fixtures), including variables, constants, and methods
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: name" );  // print header

var actual="expected"; // set the expected value

new TestCase("name","test description","expected",actual);

test();  // after testcases are defined, run them