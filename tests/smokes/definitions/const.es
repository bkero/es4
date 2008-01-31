/*
 * const:
 *    const directive introduces constant bindings in the innermost variable object
 *    from ECMAScript 4th Edition Language Overview, October 2007. 
 *    http://www.ecmascript.org
 */

const cint:int=500;
const cbool=false;
const cunset;
class myclass {
  const mycstring="my string";
}
myclassinst=new myclass;
cunset="set after init";
startTest(); // initialize tests
writeHeaderToLog( "smoke tests: const" );  // print header

new TestCase("const","type annotated const",500,cint);
new TestCase("const","untyped const",false,cbool);
new TestCase("const","const in class","my string",myclassinst.mycstring);
new TestCase("const","const set after initialization","set after init",cunset);

// should throw exception?
cint=400;
new TestCase("const","const cannot be changed",500,cint);

test();  // after testcases are defined, run them