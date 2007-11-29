/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    unshift.es
   Description:  unshift(object,...items)
     inserts the values in items as new array elements at the start of object, such
     that their order within the array elements of object is the same as the order in which
     they appear in items. Existing array elements in object are shifted upward in the index range
     and the length property of object is updated.
   Author:       dschaffe@adobe.com 5-Nov-2007
   *
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( " Array.unshift()");

var testarray=Array();
testarray.unshift()
new TestCase(   SECTION,
		"unshift empty array no items",
		"",
		testarray.toString());

var testarray=Array();
testarray.unshift("one","two")
new TestCase(   SECTION,
		"unshift empty array add 2 items",
		"one,two",
		testarray.toString());
var testarray=Array("three","four");
testarray.unshift("one","two")
new TestCase(   SECTION,
		"unshift small array add 2 items",
		"one,two,three,four",
		testarray.toString());

/*
var testarray=Array("three","four");
Array.unshift(testarray,"one","two")
new TestCase(   SECTION,
		"static unshift small array add 2 items",
		"one,two,three,four",
		testarray.toString());
*/
test();