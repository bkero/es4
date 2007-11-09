/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    push.es
   Description:  push(object,...items)
     pop method extracts the last array element from object and removes it by decreasing the value of the length
     of property object by 1.
   Author:       dschaffe@adobe.com 5-Nov-2007
   *
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( " Array.push()");

var testarray=Array();
testarray.push();
new TestCase(   SECTION,
		"push empty array",
		"",
		testarray.toString());

var testarray=Array();
testarray.push("one")
new TestCase(   SECTION,
                "push single item to item array",
                "one",
                testarray.toString());

var testarray=Array("one","two","three");
testarray.push()
new TestCase(   SECTION,
                "push nothing to small array",
                "one,two,three",
                testarray.toString());

var testarray=Array("one","two","three");
testarray.push("four","five","six")
new TestCase(   SECTION,
                "push several elements to small array",
                "one,two,three,four,five,six",
                testarray.toString());
var testarray=Array("one","two","three");
Array.push(testarray)
new TestCase(   SECTION,
                "static push nothing to small array",
                "one,two,three",
                testarray.toString());

var testarray=Array("one","two","three");
Array.push(testarray,"four","five","six")
new TestCase(   SECTION,
                "static push several elements to small array",
                "one,two,three,four,five,six",
                testarray.toString());
test();