/*
 * parameterized types:
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: name" );  // print header

class Pair.<T> {
  var first:T,second:T;
}
var p;
p=new Pair.<int>();
p.first=1;
p.second=2;
new TestCase("parameterized types","parameterized types, int",1,p.first);

q=new Pair.<string>;
q.first="first";
q.second="second";
new TestCase("parameterized types","parameterized types, string","first",q.first);

test();  // after testcases are defined, run them