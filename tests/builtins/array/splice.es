/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    splice.es
   Description:  splice(object,start,deletCount,...items)
     splice replaces the deleteCount array elements of object starting at array index start with values
     from the items, the methods returns a new Array object containing the array elements that were removed
     from objects, in order.
   Author:       dschaffe@adobe.com 5-Nov-2007
   *
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( " Array.splice()");

var testarray=Array();
new TestCase(   SECTION,
		"splice empty array with nothing",
		"",
		testarray.splice().toString());

var message=""
try {
Array.splice()
} catch (e) {
message=e.toString()
}
new TestCase(   SECTION,
		"static splice empty array with undefined produces error",
		"ReferenceError: object reference on undefined value",
		message);

var testarray=Array();
testarray.splice(0,0);
new TestCase(   SECTION,
		"splice empty array with nothing set start",
		"",
		testarray.toString());

var testarray=Array("one","two");
var splice=testarray.splice(2,0,"three","four")
new TestCase(   SECTION,
		"splice small array no delete",
		"one,two,three,four",
		testarray.toString());

var testarray=Array("one","delete1","delete2","two","three");
var splice=testarray.splice(1,2)
new TestCase(   SECTION,
		"splice small array delete 2 items, no add",
		"one,two,three",
		testarray.toString());

var testarray=Array("one","delete1","delete2","four","five","six");
var splice=testarray.splice(1,2,"two","three")
new TestCase(   SECTION,
		"splice small array delete 2 items, add 2 items",
		"one,two,three,four,five,six",
		testarray.toString());

var testarray=Array("one","two");
var splice=Array.splice(testarray,2,0,"three","four")
new TestCase(   SECTION,
		"static splice small array no delete",
		"one,two,three,four",
		testarray.toString());

var testarray=Array("one","delete1","delete2","four","five","six");
var splice=testarray.splice(-5,2,"two","three")
new TestCase(   SECTION,
		"splice small array start is negative",
		"one,two,three,four,five,six",
		testarray.toString());

var testarray=Array("one","delete1","delete2","four","five","six");
var splice=testarray.splice(1,-2,"two","three")
new TestCase(   SECTION,
		"splice small array deletecount is negative",
		"one,two,three,delete1,delete2,four,five,six",
		testarray.toString());
test();