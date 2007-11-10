/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    foreach.es
   Description:  foreach(object,eacher,thisobj)
     calls checker on every array element of object in increasing numerical index order,
     collecting all the array elements for which checker returns a value.
     checker is called with three arguments, the property value, the property index, the object itself.
     the thisobj is used as the this object in the call.
     returns a new Array object containing the elements that were collected in the order they were
     collected.
   Author:       dschaffe@adobe.com 5-Nov-2007
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( " Array.forEach()");

function eacher1(value,index,obj) {
  result+="("+value+":"+index+")";
}
var eacher2="a string";

var testarray=Array();
var errormsg="";
try {
  var result=testarray.forEach();
} catch (e) {
  errormsg=e.toString();
}
new TestCase(   SECTION,
		"forEach eacher is undefined",
		"TypeError: Function object required to 'forEach'",
		errormsg);

var testarray=[];
var errormsg="";
try {
  var result=testarray.forEach(eacher2);
} catch (e) {
  errormsg=e.toString();
}
new TestCase(   SECTION,
		"forEach eacher is undefined",
		"TypeError: Function object required to 'forEach'",
		errormsg);

var testarray=[];
var result="";
testarray.forEach(eacher1);
new TestCase(   SECTION,
		"forEach empty array",
		"",
		result.toString());

var testarray=['a','b','c'];
var result="";
testarray.forEach(eacher1);
new TestCase(   SECTION,
		"forEach simple array",
		"(a:0)(b:1)(c:2)",
		result.toString());

var testarray=['a','b','c'];
var result="";
Array.forEach(testarray,eacher1);
new TestCase(   SECTION,
		"static forEach simple array",
		"(a:0)(b:1)(c:2)",
		result.toString());
test();