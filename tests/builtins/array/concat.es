/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    concat.es
   Description:  The static concat method collects the array elements from object followed by the array
    elements from the additional items, in order, into a new Array object.  All the items must be objects.
    returns a new array object
   Author:       dschaffe@adobe.com 5-Nov-2007
   *
   */

var VERSION = "ECMA_1";
startTest();

writeHeaderToLog( SECTION + " Array.concat()");

var testarray=Array()
testarray.concat()
new TestCase(   SECTION,
		"concat empty array no arguments testarray.concat()",
		"",
		testarray.toString());

/*
var testarray=Array.concat()
new TestCase(   SECTION,
		"static concat no arguments Array.concat()",
		"",
		testarray.toString());
*/

var origarray=Array(1,2,3)
var testarray=origarray.concat()
print(testarray.toString())
new TestCase(   SECTION,
		"concat no arguments Array.concat()",
		"1,2,3",
		testarray.toString());

var origarray=Array(1,2,3)
var addarray=Array()
var testarray=origarray.concat(addarray)
new TestCase(   SECTION,
		"concat empty array",
		"1,2,3",
		testarray.toString());
var origarray=Array(1,2,3)
var addarray=Array(4,5,6)
var testarray=origarray.concat(addarray)
new TestCase(   SECTION,
		"concat simple array",
		"1,2,3,4,5,6",
		testarray.toString());

new TestCase(   SECTION,
		"concat simple array original array unchanged",
		"1,2,3",
		origarray.toString());

var origarray=Array(1,2,3)
var testarray=origarray.concat(4,5,6)
new TestCase(   SECTION,
		"concat simple ints",
		"1,2,3,4,5,6",
		testarray.toString());

var origarray=Array(1,2,3)
var testarray=origarray.concat(true,"strings",3.14,-50)
new TestCase(   SECTION,
		"concat simple objects",
		"1,2,3,true,strings,3.14,-50",
		testarray.toString());

test();

