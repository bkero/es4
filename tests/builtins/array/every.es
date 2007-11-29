/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    every.es
   Description:  every(object,checker,thisObj=)
     calls checker on every array element of object in increasing numerical index order, stopping
     as soon as any call returns false.
     checker is called with three arguments: the property value, the property index
     and the object itself.  The thisobj is used as the this object in the call.
     returns true if all the calls to checker returned true values, otherwise it returns false.
   Author:       dschaffe@adobe.com 5-Nov-2007
   *
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( " Array.every()");

function checker1(value,index,obj):Boolean {
  msg+="checker1("+value+","+index+",["+obj+"])";
  if (value=='e')
    return false;
  return true;
}
function checker3(value,index,obj):Boolean {
  msg+=this.message;
  return true;
}

var msg="";
var testarray=Array();
new TestCase(   SECTION,
		"every empty array",
		true,
		testarray.every(checker1));
var msg="";
var testarray=Array('a','b','c','d');
new TestCase(   SECTION,
		"every small array returns true",
		true,
		testarray.every(checker1));
new TestCase(   SECTION,
		"every small array check function",
		"checker1(a,0,[a,b,c,d])checker1(b,1,[a,b,c,d])checker1(c,2,[a,b,c,d])checker1(d,3,[a,b,c,d])",
		msg);

var msg="";
var testarray=Array('a','b','c','d');
new TestCase(   SECTION,
		"static every small array returns true",
		true,
		Array.every(testarray,checker1));
new TestCase(   SECTION,
		"static every small array check function",
		"checker1(a,0,[a,b,c,d])checker1(b,1,[a,b,c,d])checker1(c,2,[a,b,c,d])checker1(d,3,[a,b,c,d])",
		msg);

var msg="";
var testarray=['a','b',,,'e','f','g','h'];
new TestCase(   SECTION,
		"every small array with stop returns false",
		false,
		testarray.every(checker1));
new TestCase(   SECTION,
		"every small array with stop check function",
		"checker1(a,0,[a,b,,,e,f,g,h])checker1(b,1,[a,b,,,e,f,g,h])checker1(undefined,2,[a,b,,,e,f,g,h])checker1(undefined,3,[a,b,,,e,f,g,h])checker1(e,4,[a,b,,,e,f,g,h])",
		msg);

var msg="";
var thisobj=new Object();
thisobj.message="custom object";
var testarray=['a'];
testarray.every(checker3,thisobj);
new TestCase(   SECTION,
		"every small array with a specified this object",
		"custom object",
		msg);

var checker2="a string";
var testarray=Array('a','b','c','d');
var errormsg=""
try {
  Array.every(testarray,checker2);
} catch (e) {
  errormsg=e.toString()
}
new TestCase(   SECTION,
		"every small array checker is not a function",
		"TypeError: Function object required to 'every'",
		errormsg);

test();