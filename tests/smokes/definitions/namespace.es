/*
 * namespace:
 *    Namespaces are compile-time values created by a namespace directive.
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: name" );  // print header

namespace ns1
namespace ns2=ns1
namespace ns3="http://www.ecmascript.org/ns1"
namespace ns4="http://www.ecmascript.org/ns1"

ns1 var x="value1";
ns3 var x="value2";

print(ns1::x);
print(ns2::x);
print(ns3::x);
print(ns4::x);

new TestCase("namespace","define anonymous namespace","value1",ns1::x);
new TestCase("namespace","define namespace set to anonymous namespace","value1",ns2::x);
new TestCase("namespace","define namespace with program defined content","value2",ns3::x);
new TestCase("namespace","define namespace set to program defined content","value2",ns4::x);

test();  // after testcases are defined, run them