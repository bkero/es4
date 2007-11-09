/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    map.es
   Description:  map(object,mapper,thisobj)
     calls mapper on each array element of object in increasing numerical index order, collecting
     the return values from mapper in a new Array object.
     mapper is called with three arguments: the property value, the property index, and object itself.
     The thisobj is used as the this object in the call.
     returns a new Array object where the array element at index i is the value returned from the call
     to mapper on object[i].
   Author:       dschaffe@adobe.com 5-Nov-2007
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( " Array.map()");

function mapper1(value,index,obj) {
   return "("+value+":"+index+")";
}
var mapper2="a string";

var testarray=Array();
var errormsg="";
try {
  var result=testarray.map();
} catch (e) {
  errormsg=e.toString();
}
new TestCase(   SECTION,
		"map mapper is undefined",
		"TypeError: Function object required to 'map'",
		errormsg);

var testarray=[];
var errormsg="";
try {
  var result=testarray.map(mapper2);
} catch (e) {
  errormsg=e.toString();
}
new TestCase(   SECTION,
		"map mapper is undefined",
		"TypeError: Function object required to 'map'",
		errormsg);

var testarray=[];
new TestCase(   SECTION,
		"map empty array",
		"",
		testarray.map(mapper1).toString());

var testarray=['a','b','c'];
new TestCase(   SECTION,
		"map small array",
		"(a:0),(b:1),(c:2)",
		testarray.map(mapper1).toString());

var testarray=['a','b','c'];
new TestCase(   SECTION,
		"static map small array",
		"(a:0),(b:1),(c:2)",
		Array.map(testarray,mapper1).toString());
test();