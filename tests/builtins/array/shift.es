/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    shift.es
   Description:  shift(object)
     removes the element called 0 in object, moves the element at index i+1 to index i,
     and decrements the length property of object by 1.
   Author:       dschaffe@adobe.com 5-Nov-2007
   *
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( " Array.shift()");

var testarray=Array();
var obj=testarray.shift()
new TestCase(   SECTION,
		"shift on empty array returns undefined",
		undefined,
		obj);
new TestCase(   SECTION,
		"shift on empty array original array is empty",
		"",
		testarray.toString());

var testarray=Array("one");
var obj=testarray.shift()
new TestCase(   SECTION,
		"shift on single item array returns first item",
		"one",
		obj);
new TestCase(   SECTION,
		"shift on single item array original array becomes empty",
		"",
		testarray.toString());

var testarray=Array("one","two","three","four");
var obj=testarray.shift()
new TestCase(   SECTION,
		"shift on small array returns first item",
		"one",
		obj);
new TestCase(   SECTION,
		"shift on small array original array shifts one",
		"two,three,four",
		testarray.toString());

var testarray=Array("one","two","three","four");
var obj=Array.shift(testarray)
new TestCase(   SECTION,
		"static shift on small array returns first item",
		"one",
		obj);
new TestCase(   SECTION,
		"static shift on small array original array shifts one",
		"two,three,four",
		testarray.toString());

test();