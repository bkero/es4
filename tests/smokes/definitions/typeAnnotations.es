/*
 * type annotations:
 *    Variables can optionally be annotated with type contraints, and an
 *    implementation can perform static type checking and early binding.
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest();
writeHeaderToLog( "smoke tests: type annotations" );
// var s:String; upcase String not working yet
var s:string;
s="Hello";
var ui:uint;
var i:int;
//function annotatedfunction(b:Boolean):Number {
function annotatedfunction(b):Number {
  if (b)
    return 3.14;
  else
    return 0;
}
ui=100;
i=-500;
new TestCase("type annotation","String compare","Hello",s);
new TestCase("type annotation","uint compare",100,ui);
new TestCase("type annotation","int compare",-500,i);
new TestCase("type annotation","function",3.14,annotatedfunction(true));
ui=-100;
new TestCase("type annotation","set negative (-100) to unsigned annotated var",4294967196,ui);
test();