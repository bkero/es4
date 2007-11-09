/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Mozilla Communicator client code, released
 * March 31, 1998.
 *
 * The Initial Developer of the Original Code is
 * Netscape Communications Corporation.
 * Portions created by the Initial Developer are Copyright (C) 1998
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

/**
   File Name:          constructor-function.js
   ECMA Section:       15.4.1 The Array Constructor Called as a Function

   Description:        When Array is called as a function rather than as a
   constructor, it creates and initializes a new array
   object.  Thus, the function call Array(...) is
   equivalent to the object creationi new Array(...) with
   the same arguments.

   Author:             christine@netscape.com 7-Oct-1997
   Updated:            dschaffe@adobe.com 1-Nov-2007
*/

var SECTION = "15.4.1";
var VERSION = "ECMA_1";
startTest();
var TITLE   = "The Array Constructor Called as a Function";

writeHeaderToLog( SECTION + " "+ TITLE);

new TestCase(   SECTION,
		"Array() +''",
		"",
		Array() +"" );

new TestCase(   SECTION,
		"typeof Array()",
		"object",
		typeof Array() );

new TestCase(   SECTION,
		"var arr = Array(); arr.getClass = Object.prototype.toString; arr.getClass()",
		"[object Array]",
		eval("var arr = Array(); arr.getClass = Object.prototype.toString; arr.getClass()") );
new TestCase(   SECTION,
		"var arr = Array(); arr.toString == Array.prototype.toString",
		true,
		eval("var arr = Array(); arr.toString == Array.prototype.toString") );

new TestCase(   SECTION,
		"Array().length",
		0,
		Array().length );

new TestCase(   SECTION,
		"Array(1,2,3) +''",
		"1,2,3",
		Array(1,2,3) +"" );

new TestCase(   SECTION,
		"typeof Array(1,2,3)",
		"object",
		typeof Array(1,2,3) );

new TestCase(   SECTION,
		"var arr = Array(1,2,3); arr.getClass = Object.prototype.toString; arr.getClass()",
		"[object Array]",
		eval("var arr = Array(1,2,3); arr.getClass = Object.prototype.toString; arr.getClass()") );

new TestCase(   SECTION,
		"var arr = Array(1,2,3); arr.toString == Array.prototype.toString",
		true,
		eval("var arr = Array(1,2,3); arr.toString == Array.prototype.toString") );

new TestCase(   SECTION,
		"Array(1,2,3).length",
		3,
		Array(1,2,3).length );

new TestCase(   SECTION,
		"typeof Array(12345)",
		"object",
		typeof Array(12345) );

new TestCase(   SECTION,
		"var arr = Array(12345); arr.getClass = Object.prototype.toString; arr.getClass()",
		"[object Array]",
		eval("var arr = Array(12345); arr.getClass = Object.prototype.toString; arr.getClass()") );

new TestCase(   SECTION,
		"var arr = Array(1,2,3,4,5); arr.toString == Array.prototype.toString",
		true,
		eval("var arr = Array(1,2,3,4,5); arr.toString == Array.prototype.toString") );

new TestCase(   SECTION,
		"Array(12345).length",
		12345,
		Array(12345).length );
new TestCase( SECTION,	
	      "typeof Array(1,2)",        
	      "object",           
	      typeof Array(1,2) );

new TestCase( SECTION,	
	      "(Array(1,2)).toString",    
	      Array.prototype.toString,    
	      (Array(1,2)).toString );

new TestCase( SECTION,
	      "var arr = Array(1,2,3); arr.toString = Object.prototype.toString; arr.toString()",
	      "[object Array]",
	      eval("var arr = Array(1,2,3); arr.toString = Object.prototype.toString; arr.toString()") );

new TestCase( SECTION,	
	      "(Array(1,2)).length",      
	      2,                  
	      (Array(1,2)).length );

new TestCase( SECTION,	
	      "var arr = (Array(1,2)); arr[0]",  
	      1,           
	      eval("var arr = (Array(1,2)); arr[0]") );

new TestCase( SECTION,	
	      "var arr = (Array(1,2)); arr[1]",  
	      2,           
	      eval("var arr = (Array(1,2)); arr[1]") );

new TestCase( SECTION,	
	      "var arr = (Array(1,2)); String(arr)",  
	      "[object Array]",  
	      eval("var arr = (Array(1,2)); String(arr)") );

new TestCase( SECTION,  
	      "(Array()).length",             
	      0,                              
	      (Array()).length );

new TestCase( SECTION,	
	      "(Array(0)).length",            
	      0,                              
	      (Array(0)).length );

new TestCase( SECTION,	
	      "(Array(1)).length",            
	      1,                              
	      (Array(1)).length );

new TestCase( SECTION,	
	      "(Array(10)).length",           
	      10,                             
	      (Array(10)).length );

new TestCase( SECTION,	
	      "(Array('1')).length",          
	      1,                              
	      (Array('1')).length );

new TestCase( SECTION,	
	      "(Array(1000)).length",         
	      1000,                           
	      (Array(1000)).length );

new TestCase( SECTION,	
	      "(Array('1000')).length",       
	      1,                              
	      (Array('1000')).length );

new TestCase( SECTION,	
	      "(Array('8589934592')).length", 
	      1,                              
	      (Array("8589934592")).length );

new TestCase( SECTION,	
	      "(Array('4294967296')).length", 
	      1,                              
	      (Array("4294967296")).length );

new TestCase( SECTION,	
	      "(Array('a string')).length",   
	      1,                              
	      (Array("a string")).length );
new TestCase(   SECTION,
		"typeof Array()",
		"object",
		typeof Array() );
new TestCase(   SECTION,
		"MYARR = new Array();MYARR.getClass = Object.prototype.toString;MYARR.getClass()",
		"[object Array]",
		eval("MYARR = Array();MYARR.getClass = Object.prototype.toString;MYARR.getClass()") );

new TestCase(   SECTION,
		"(Array()).length",
		0,          
		(Array()).length );

new TestCase(   SECTION,
		"Array().toString()",
		"[object Array]",
		Array().toString() );
/*
 * these upper limit Array constructor tests fail
 * [todo] logged in trac:
new TestCase( SECTION,	
	      "(Array(1073741823)).length",   
	      ToUint32(1073741823),           
	      (Array(1073741823)).length );

new TestCase( SECTION,	
	      "(Array(4294967295)).length",   
	      ToUint32(4294967295),           
	      (Array(4294967295)).length );

new TestCase( SECTION,	
	      "(Array(Math.pow(2,31)-1)).length",     
	      ToUint32(Math.pow(2,31)-1),     
	      (Array(Math.pow(2,31)-1)).length );

new TestCase( SECTION,	
	      "(Array(1073741824)).length",   
	      ToUint32(1073741824),	        
	      (Array(1073741824)).length );
new TestCase( SECTION,	
	      "(Array(Math.pow(2,31))).length",       
	      ToUint32(Math.pow(2,31)),       
	      (Array(Math.pow(2,31))).length );

new TestCase( SECTION,	
	      "(Array(Math.pow(2,31)+1)).length",     
	      ToUint32(Math.pow(2,31)+1),     
	      (Array(Math.pow(2,31)+1)).length );
*/
test();

function ToUint32( n ) {
  n = Number( n );
  if( isNaN(n) || n == 0 || n == Number.POSITIVE_INFINITY ||
      n == Number.NEGATIVE_INFINITY ) {
    return 0;
  }
  var sign = n < 0 ? -1 : 1;

  return ( sign * ( n * Math.floor( Math.abs(n) ) ) ) % Math.pow(2, 32);
}
