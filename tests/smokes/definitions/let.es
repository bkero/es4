/*
 * name:
 *    description
 *    let, let const directives introduce bindings into the innermost block object.
 *    let function binds the function in the bock object instead of in the variable object.
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: let" );  // print header

var result1,result2,result3=0,result4;
let a:uint=10, b=1;
{
  let a:int=20;
  result1=a+b;
}
result2=a+b;

for (let i=0;i<10;i++)
 result3+=i+1;
try {
print(i);
} catch (e) {
 result4=e.toString();
}
new TestCase("let inside scope","let in new scope changes value of variable",21,result1);
new TestCase("let outside scope","let outside new scope retains original value of variable",11,result2);
new TestCase("let in loop","let inside loop defines new variable",55,result3);
new TestCase("let outside scope undefined variable","defining a variable inside a scope does not exist outside the scope","ReferenceError: unresolved lexical reference {multiname: [ns public '']::i [ns internal '']::i [ns public '__ES4__']::i }",result4);
test();  // after testcases are defined, run them