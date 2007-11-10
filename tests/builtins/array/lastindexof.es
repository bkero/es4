/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    lastindexof.es
   Description:  lastindexOf(object,value,from=...)
     compares value with every array element of object in decreasing numerical index order, starting at the 
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
		"lastIndexOf empty array",
		-1,
		testarray.lastIndexOf(1));

var testarray=Array()
new TestCase(   SECTION,
		"static lastIndexOf empty array",
		-1,
		Array.lastIndexOf(testarray,1));

var testarray=Array(1,2,3)
new TestCase(   SECTION,
		"lastIndexOf object not found",
		-1,
		testarray.lastIndexOf(4));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"lastIndexOf first match found",
		3,
		testarray.lastIndexOf(1));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"lastIndexOf from first match found",
		0,
		testarray.lastIndexOf(1,2));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"lastIndexOf from zero",
		0,
		testarray.lastIndexOf(1,0));

var testarray=Array(0,1,2,3,1,2,3)
new TestCase(   SECTION,
		"static lastIndexOf from zero not found",
		-1,
		Array.lastIndexOf(testarray,1,0));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"lastIndexOf from negative found",
		4,
		testarray.lastIndexOf(2,-1));

var testarray=Array(1,2,3,1,2,3)
new TestCase(   SECTION,
		"lastIndexOf from negative",
		1,
		testarray.lastIndexOf(2,-3));

var testarray=Array(1,2,3,1,2,3,1)
new TestCase(   SECTION,
		"lastIndexOf from is a decimal",
		3,
		testarray.lastIndexOf(1,3.99));

var testarray=Array(1,2,3,1,2,3)
var msg=""
try {
Array.lastIndexOf(null,1)
} catch (e) {
  msg=e.toString()
}
new TestCase(   SECTION,
		"lastIndexOf null object",
		"ReferenceError: object reference on null value",
		msg);

test();

