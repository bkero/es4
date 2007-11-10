/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    pop.es
   Description:  pop(object)
     pop method extracts the last array element from object and removes it by decreasing the value of the length
     of property object by 1.
   Author:       dschaffe@adobe.com 5-Nov-2007
   *
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( " Array.pop()");

var testarray=Array();
new TestCase(   SECTION,
		"pop empty array",
		undefined,
		testarray.pop());
new TestCase(   SECTION,
		"pop empty array, original array change",
		"",
		testarray.toString());

var testarray=Array("one");
new TestCase(   SECTION,
                "pop single length array element",
                "one",
                testarray.pop());
new TestCase(   SECTION,
                "pop single length array, original array change",
                "",
                testarray.toString());


var testarray=Array("one","two","three");
new TestCase(   SECTION,
                "pop simple array element",
                "three",
                testarray.pop());
new TestCase(   SECTION,
                "pop simple array element, original array change",
                "one,two",
                testarray.toString());

var testarray=Array("one");
new TestCase(   SECTION,
                "static pop single length array element",
                "one",
                Array.pop(testarray));
new TestCase(   SECTION,
                "static pop single length array, original array change",
                "",
                testarray.toString());


var testarray=Array("one","two","three");
new TestCase(   SECTION,
                "static pop simple array element",
                "three",
                Array.pop(testarray));
new TestCase(   SECTION,
                "static pop simple array element, original array change",
                "one,two",
                testarray.toString());

test();