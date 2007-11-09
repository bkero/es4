/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    indexof.es
   Description:  indexOf(object,value,from=...)
     compares value with every array element of object in increasing numerical index order, starting at the 
     index from, stopping when an array lement is equial to value by the === operator, From is rounded toward zero 
     before use.  If from is negative, it is treated as object.length+from, returns array index from first value or -1 
     if no such element is found.
   Author:       dschaffe@adobe.com 5-Nov-2007
   *
   */

var SECTION = ""
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( SECTION + " Array.indexOf()");

var testarray=Array()
new TestCase(   SECTION,
		"indexOf empty array",
		-1,
		testarray.indexOf(1));

var testarray=Array()
new TestCase(   SECTION,
		"static indexOf empty array",
		-1,
		Array.indexOf(testarray,1));

var testarray=Array(1,2,3)
new TestCase(   SECTION,
		"indexOf object not found",
		-1,
		testarray.indexOf(4));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"indexOf first match found",
		0,
		testarray.indexOf(1));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"indexOf from first match found",
		3,
		testarray.indexOf(1,1));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"indexOf from zero",
		0,
		testarray.indexOf(1,0));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"static indexOf from zero",
		0,
		Array.indexOf(testarray,1,0));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"indexOf from negative not found",
		-1,
		testarray.indexOf(2,-1));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"indexOf from negative",
		4,
		testarray.indexOf(2,-3));

var testarray=Array(1,2,3,1,2,3,1)
new TestCase(   SECTION,
		"indexOf from is a decimal",
		3,
		testarray.indexOf(1,3.99));

var testarray=Array(1,2,3,1,2,3)
var msg=""
try {
Array.indexOf(null,1)
} catch (e) {
  msg=e.toString()
}
new TestCase(   SECTION,
		"indexOf null object",
		"ReferenceError: object reference on null value",
		msg);

test();

