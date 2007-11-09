/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/**
   File Name:    some.es
   Description:  some(object,checker,thisobj=)
     calls checker on every array element in object in increasing numerical index order,
     stopping as soon as checker returns a true value.
     checker is called with three arguments: the property value, the property index, and the object
     itself.  The thisobj is used as the this object in the call.
     returns true when checker returns a true value, otherwise returns false if all the calls to checker
     return false values.
   Author:       dschaffe@adobe.com 5-Nov-2007
   *
   */
var SECTION="";
var VERSION = "ECMA_1";

startTest();

writeHeaderToLog(" Array.some()");
var msg;
function checker1(value,index,obj) {
  msg+="(value="+value+",index="+index+",object=["+obj+"])";
  if (value=='t')
    return true;
  return false;
}

var testarray=Array();
var errormsg="";
try {
  result=testarray.some();
} catch (e) {
  errormsg=e.toString();
}
new TestCase(   SECTION,
		"some no checker",
		"TypeError: Function object required to 'some'",
		errormsg);

var checker2="a string";
var testarray=Array();
var errormsg="";
try {
  result=testarray.some(checker2);
} catch (e) {
  errormsg=e.toString();
}
new TestCase(   SECTION,
		"some checker not a function",
		"TypeError: Function object required to 'some'",
		errormsg);


var msg="";
var testarray=[];
var result=testarray.some(checker1);
new TestCase(   SECTION,
                "some empty array result",
                false,
                result);
new TestCase(   SECTION,
                "some empty array message empty",
                "",
                msg);

var msg="";
var testarray=['a','b','c'];
var result=testarray.some(checker1);
new TestCase(   SECTION,
                "some small array result",
                false,
                result);
new TestCase(   SECTION,
                "some small array message",
                "(value=a,index=0,object=[a,b,c])(value=b,index=1,object=[a,b,c])(value=c,index=2,object=[a,b,c])",
                msg);

var msg="";
var testarray=['a','b','t','c','d'];
var result=testarray.some(checker1)
new TestCase(   SECTION,
                "some small array result with a true",
                true,
                result);
new TestCase(   SECTION,
                "some small array message with a true",
                "(value=a,index=0,object=[a,b,t,c,d])(value=b,index=1,object=[a,b,t,c,d])(value=t,index=2,object=[a,b,t,c,d])",
                msg);
var msg="";
var testarray=['a','b','t','c','d'];
var result=Array.some(testarray,checker1)
new TestCase(   SECTION,
                "static some small array result with a true",
                true,
                result);
new TestCase(   SECTION,
                "static some small array message with a true",
                "(value=a,index=0,object=[a,b,t,c,d])(value=b,index=1,object=[a,b,t,c,d])(value=t,index=2,object=[a,b,t,c,d])",
                msg);
test();