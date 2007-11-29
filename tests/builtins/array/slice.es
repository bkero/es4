/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    slice.es
   Description:  slice(object,start=...,end=...)
     extracts the subrange of array elements from object between start (inclusive)
     and end (exclusive) into a new Array.
     if start is negative, it is treated as object.length+start. If end is negative, it is treated as
     object.length+end.  In either case the values of start and end are bounded between 0 and
     object.length
   Author:       dschaffe@adobe.com 5-Nov-2007
   *
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( " Array.slice()");

var testarray=Array();
new TestCase(   SECTION,
		"slice no args on empty array",
		"",
		testarray.slice().toString());

var testarray=Array();
new TestCase(   SECTION,
		"slice args out of bounds on empty array",
		"",
		testarray.slice(5,6).toString());

var testarray=Array("one","two","three");
new TestCase(   SECTION,
		"slice args out of bounds on small array",
		"",
		testarray.slice(5,6).toString());

var testarray=Array("one","two","three");
new TestCase(   SECTION,
		"slice invalid args end less than start on small array",
		"",
		testarray.slice(2,1).toString());

var testarray=Array("one","two","three","four","five");
new TestCase(   SECTION,
		"slice on small array",
		"two,three",
		testarray.slice(1,3).toString());

var testarray=Array("one","two","three","four","five");
new TestCase(   SECTION,
		"slice start negative on small array",
		"two,three",
		testarray.slice(-4,3).toString());

var testarray=Array("one","two","three","four","five");
new TestCase(   SECTION,
		"slice end negative on small array",
		"two,three",
		testarray.slice(1,-2).toString());

var testarray=Array("one","two","three","four","five");
new TestCase(   SECTION,
		"static slice on small array",
		"two,three",
		Array.slice(testarray,1,3).toString());

test();