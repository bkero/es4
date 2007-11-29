/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    filter.es
   Description:  filter(object,checker,thisobj)
     calls checker on every array element of object in increasing numerical index order,
     collecting all the array elements for which checker returns a true value.
     checker is called with three arguments: the property value, the property index, and object
     itself. The thisobj is used as the this object in the call.
     returns a new Array object containing the elements that were collected in the order
     they were collected.
   Author:       dschaffe@adobe.com 5-Nov-2007
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog( " Array.filter()");

function checker1(value,index,obj) {
  if (value%2==0)
    return true;
  return false;
}
var checker2="a string";

var testarray=Array();
var errormsg="";
try {
  var result=testarray.filter();
} catch (e) {
  errormsg=e.toString();
}
new TestCase(   SECTION,
		"filter checker is undefined",
		"TypeError: Function object required to 'filter'",
		errormsg);

var testarray=[];
var errormsg="";
try {
  var result=testarray.filter(checker2);
} catch (e) {
  errormsg=e.toString();
}
new TestCase(   SECTION,
		"filter checker is undefined",
		"TypeError: Function object required to 'filter'",
		errormsg);

var testarray=[];
var result=testarray.filter(checker1);
new TestCase(   SECTION,
		"filter empty array",
		"",
		result.toString());

var testarray=[1,3,5,7,9];
var result=testarray.filter(checker1);
new TestCase(   SECTION,
		"filter array all false",
		"",
		result.toString());

var testarray=[1,2,3,4,5,6,7,8,9];
var result=testarray.filter(checker1);
new TestCase(   SECTION,
		"filter small array",
		"2,4,6,8",
		result.toString());

var testarray=[1,2,3,4,5,6,7,8,9];
var result=Array.filter(testarray,checker1);
new TestCase(   SECTION,
		"static filter small array",
		"2,4,6,8",
		result.toString());
test();