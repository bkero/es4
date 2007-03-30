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
var completed = false;
var testcases = new Array();
var tc = testcases.length; 

var SECTION	= "";
var VERSION	= "";
var BUGNUMBER =	"";

/*
 * constant strings
 */
var	GLOBAL = "[object global]";
var PASSED = " PASSED!"
var FAILED = " FAILED! expected: ";

var DEBUG = false;

if (typeof version != "undefined") {
  version(120);
}

/* 
 * wrapper for test case constructor that doesn't require the SECTION
 * argument.
 */

function AddTestCase( description, expect, actual ) {
  new TestCase( SECTION, description, expect, actual );
}


function TestCase( n, d, e, a ) {
  this.path = (typeof gTestPath == 'undefined') ? '' : gTestPath;
  this.name        = n;
  this.description = d;
  this.expect      = e;
  this.actual      = a;
  this.passed      = true;
  this.reason      = "";

  this.passed = getTestCaseResult( this.expect, this.actual );
  if ( DEBUG ) {
    print( "added " + this.description );
  }
  /*
   * testcases are solely maintained in the TestCase
   * constructor. tc will _always_ point to one past the
   * last testcase. If an exception occurs during the call
   * to the constructor, then we are assured that the tc
   * index has not been incremented.
   */
  
  testcases[tc++] = this;
}

function startTest() {
  version(120);

  if ( BUGNUMBER ) {
    print ("BUGNUMBER: " + BUGNUMBER );
  }
}

function test() {
  for ( tc=0; tc < testcases.length; tc++ ) {
    try
    {
    testcases[tc].passed = writeTestCaseResult(
      testcases[tc].expect,
      testcases[tc].actual,
      testcases[tc].description +" = "+
      testcases[tc].actual );

    testcases[tc].reason += ( testcases[tc].passed ) ? "" : "wrong value ";
    }
    catch(e)
    {
      print('test(): empty testcase for tc = ' + tc + ' ' + e);
    }
  }
  stopTest();
  return ( testcases );
}

/*
 * Compare expected result to the actual result and figure out whether
 * the test case passed.
 */
function getTestCaseResult( expect, actual ) {
  //  because ( NaN == NaN ) always returns false, need to do
  //  a special compare to see if we got the right result.
  if ( actual != actual ) {
    if ( typeof actual == "object" ) {
      actual = "NaN object";
    } else {
      actual = "NaN number";
    }
  }
  if ( expect != expect ) {
    if ( typeof expect == "object" ) {
      expect = "NaN object";
    } else {
      expect = "NaN number";
    }
  }

  var passed = ( expect == actual ) ? true : false;

  //  if both objects are numbers, give a little leeway for rounding.
  if (    !passed
          && typeof(actual) == "number"
          && typeof(expect) == "number"
    ) {
    if ( Math.abs(actual-expect) < 0.0000001 ) {
      passed = true;
    }
  }

  //  verify type is the same
  if ( typeof(expect) != typeof(actual) ) {
    passed = false;
  }

  return passed;
}
/*
 * Begin printing functions.  These functions use the shell's
 * print function.  When running tests in the browser, these
 * functions, override these functions with functions that use
 * document.write.
 */

function writeTestCaseResult( expect, actual, string ) {
  var	passed = getTestCaseResult(	expect,	actual );
  writeFormattedResult( expect, actual, string, passed );
  return passed;
}
function writeFormattedResult( expect, actual, string, passed ) {
  var s = string ;
  s += ( passed ) ? PASSED : FAILED + expect;
  print( s);
  return passed;
}

function writeHeaderToLog( string )	{
	print( string );
}
/* end of print functions */

function stopTest() {
  var gc;
  if ( gc != undefined ) {
    gc();
  }
}

/* JavaScriptOptions
   encapsulate the logic for setting and retrieving the values
   of the javascript options.
   
   Note: in shell, options() takes an optional comma delimited list
   of option names, toggles the values for each option and returns the
   list of option names which were set before the call. 
   If no argument is passed to options(), it returns the current
   options with value true.

   Usage;

   // create and initialize object.
   jsOptions = new JavaScriptOptions();

   // set a particular option
   jsOptions.setOption(name, boolean);

   // reset all options to their original values.
   jsOptions.reset();
*/

function JavaScriptOptions()
{
  this.orig   = {};
  this.orig.strict = this.strict = false;
  this.orig.werror = this.werror = false;

  this.privileges = 'UniversalXPConnect UniversalPreferencesRead ' + 
                    'UniversalPreferencesWrite';

  if (typeof options == 'function')
  {
    // shell
    var optString = options();
    if (optString)
    {
      var optList = optString.split(',');
      for (var iOpt = 0; iOpt < optList.length; iOpt++)
      {
        optName = optList[iOpt];
        this[optName] = true;
      }
    }
  }
  else if (typeof netscape != 'undefined' && 'security' in netscape)
  {
    // browser
    netscape.security.PrivilegeManager.enablePrivilege(this.privileges);

    var preferences = Components.classes['@mozilla.org/preferences-service;1'];
    if (!preferences)
    {
      throw 'JavaScriptOptions: unable to get @mozilla.org/preferences-service;1';
    }

    var prefService = preferences.
      getService(Components.interfaces.nsIPrefService);

    if (!prefService)
    {
      throw 'JavaScriptOptions: unable to get nsIPrefService';
    }

    var pref = prefService.getBranch('');

    if (!pref)
    {
      throw 'JavaScriptOptions: unable to get prefService branch';
    }

    try
    {
      this.orig.strict = this.strict = 
        pref.getBoolPref('javascript.options.strict');
    }
    catch(e)
    {
    }

    try
    {
      this.orig.werror = this.werror = 
        pref.getBoolPref('javascript.options.werror');
    }
    catch(e)
    {
    }
  }
}

JavaScriptOptions.prototype.setOption = 
function (optionName, optionValue)
{
  if (typeof options == 'function')
  {
    // shell
    if (this[optionName] != optionValue)
    {
      options(optionName);
    }
  }
  else if (typeof netscape != 'undefined' && 'security' in netscape)
  {
    // browser
    netscape.security.PrivilegeManager.enablePrivilege(this.privileges);

    var preferences = Components.classes['@mozilla.org/preferences-service;1'];
    if (!preferences)
    {
      throw 'setOption: unable to get @mozilla.org/preferences-service;1';
    }

    var prefService = preferences.
    getService(Components.interfaces.nsIPrefService);

    if (!prefService)
    {
      throw 'setOption: unable to get nsIPrefService';
    }

    var pref = prefService.getBranch('');

    if (!pref)
    {
      throw 'setOption: unable to get prefService branch';
    }

    pref.setBoolPref('javascript.options.' + optionName, optionValue);
  }

  this[optionName] = optionValue;

  return;
}


JavaScriptOptions.prototype.reset = function ()
{
  this.setOption('strict', this.orig.strict);
  this.setOption('werror', this.orig.werror);
}
