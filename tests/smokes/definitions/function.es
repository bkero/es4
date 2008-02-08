/*
 * function:
 *    Function defintions create variable bindings holding function values in the innermost
 *    variable object.  Functions have a parameter list, which is a list of names that will
 *    be bound in the function's variable object and whose initial values will be the arguments
 *    passed to the function in the function call.
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

startTest(); // initialize tests
writeHeaderToLog( "smoke tests: name" );  // print header

function f1() {
  return "f1";
}
function f2(a:string):string {
  return a;
}

new TestCase("function","simple function with no return","f1",f1());
new TestCase("function","function with typed parameter and return simple","echo",f2("echo"));

test();  // after testcases are defined, run them