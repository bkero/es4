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
 * The Original Code is JavaScript Engine testing utilities.
 *
 * The Initial Developer of the Original Code is
 * Netscape Communications Corp.
 * Portions created by the Initial Developer are Copyright (C) 2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   rogerl@netscape.com, pschwartau@netscape.com, lhansen@adobe.com
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
 * ***** END LICENSE BLOCK *****
 *
 *
 * Date:    23 May 2007
 * SUMMARY: RegExp conformance test
 *
 *   These testcases are derived from the examples in the ECMA-262 Ed.4 spec,
 *   [proposals:extend regexps]
 *
 */

var i = 0;
var bug = '(none)';
var summary = 'RegExp conformance test';
var status = '';
var statusmessages = new Array();
var pattern = '';
var patterns = new Array();
var string = '';
var strings = new Array();
var actualmatch = '';
var actualmatches = new Array();
var expectedmatch = '';
var expectedmatches = new Array();

/* Subsection 1, hex notation */

status = inSection(1);
pattern = new RegExp("\\u{41}*");
string = 'AAA';
actualmatch = string.match(pattern);
expectedmatch = Array('AAA');
addThis();

status = inSection(2);
pattern = new RegExp("\\x{00041}*");
string = 'AAA';
actualmatch = string.match(pattern);
expectedmatch = Array('AAA');
addThis();

/* The following test should work on both 16-bit and 32-bit Unicode
 * implementations, but on 16-bit Unicode depends on quantifiers being
 * handled properly when the Unicode character is split.  At the time
 * of writing, the meaning of "properly" is not known.
 *
 * See ticket #37.
 */

status = inSection(3);
pattern = new RegExp("\\x{1AFFE}+");
string = 'A\u{1AFFE}A';
actualmatch = string.match(pattern);
expectedmatch = Array('\u{1AFFE}');
addThis();


//-------------------------------------------------------------------------------------------------
test();
//-------------------------------------------------------------------------------------------------

function addThis()
{
  statusmessages[i] = status;
  patterns[i] = pattern;
  strings[i] = string;
  actualmatches[i] = actualmatch;
  expectedmatches[i] = expectedmatch;
  i++;
}

function test()
{
  enterFunc ('test');
  printBugNumber (bug);
  printStatus (summary);
  testRegExp(statusmessages, patterns, strings, actualmatches, expectedmatches);
  exitFunc ('test');
}

