/* -*- indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "String" object
 *
 * E262-3 15.5
 * E262-4 proposals:json_encoding_and_decoding
 * E262-4 proposals:string.prototype.trim
 * E262-4 proposals:bug_fixes
 * E262-4 proposals:static_generics
 *
 * FIXME: add "substr"
 *
 * FIXME: if String is subclassable then what do we say about overriding
 *        methods -- how does that impact other methods?
 *
 * Status: incomplete; not reviewed; not tested.
 *
 * Strings are constructed by:
 *    new String + magic::setStringValue
 *    magic::fromCharCode
 *    magic::stringAppend
 *
 * A string's length is obtained by:
 *    magic::stringLength
 *
 * Strings are picked apart by:
 *    magic::charCodeAt
 *
 * (We would expect "+" to map to magic::stringAppend().)
 */

package
{
    use namespace intrinsic;
    use strict;

    /* The January 2007 meeting resolved that String is open and
       dynamic, and the implementations will just have to work a
       little harder.  Go ECMA!  */
    dynamic class String
    {       
        /* E262-3 15.5.1: The String Constructor Called as a Function */
        static intrinsic function invoke(value)
            return arguments.length === 0 ? "" : ToString(value);

        /* 15.5.2 The String Constructor 
           Be careful to always return a new String object here, so don't
           optimize by returning the return value of 
         */
        function String(v) {
            if (arguments.length === 0)
                value = "";
            else if (v instanceof string)
                value = v;
            else if (v instanceof String)
                value = v.value;
            else
                value = ToString(value);
        }

        var value : string = "";

        /* E262-3 15.5.3.2: String.fromCharCode
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        public static function fromCharCode(...args)
            Function.apply(string.fromCharCodeHelper, null, args);

        intrinsic static function fromCharCode(...args) : double
            Function.apply(string.fromCharCodeHelper, null, args);

        /* E262-3 15.5.4.2: String.prototype.toString */
        prototype function toString(this : String)
            ToString(this);

        override intrinsic function toString() : String
            value;
        
        /* E262-3 15.5.4.3: String.prototype.valueOf */
        prototype function valueOf(this : String)
            this;

        intrinsic function valueOf() : Object
            this;

        /* E262-3 15.5.4.4: String.prototype.charAt
           E262-4 draft proposals:static_generics
        */
        prototype function charAt(pos)
            ToString(this).charAt(pos);

        public static function charAt(self, pos)
            ToString(self).charAt(pos);
            
        intrinsic function charAt(pos: double = 0) : String
            ToString(self).charAt(pos);

        /* E262-3 15.5.4.5: String.prototype.charCodeAt
           E262-4 draft proposals:static_generics
        */
        prototype function charCodeAt(pos)
            ToString(this).charCodeAt(pos);

        public static function charCodeAt(self, pos)
            ToString(self).charCodeAt(pos);

        intrinsic function charCodeAt(pos: double = 0) : double
            value.charCodeAt(pos);

        /* E262-3 15.5.4.6: String.prototype.concat.
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
         */
        prototype function concat(...args)
            Function.apply(string.concat, ToString(this), args);

        public static function concat(self, ...args)
            Function.apply(string.concat, ToString(self), args);

        intrinsic function concat(...args) : String
            Function.apply(string.concat, value, args);


        /* E262-3 15.5.4.7: String.prototype.indexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function indexOf(searchString, position)
            ToString(this).indexOf(searchString, position);

        public static function indexOf(self, searchString, position)
            ToString(self).indexOf(searchString, position);

        intrinsic function indexOf(searchString: String!, position: double = 0.0) : double 
            value.indexOf(searchString, position);

        /* E262-3 15.5.4.8: String.prototype.lastIndexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function lastIndexOf(searchString, position)
            ToString(this).lastIndexOf(searchString, position);

        public static function lastIndexOf(self, searchString, position)
            ToString(self).lastIndexOf(searchString, position);

        intrinsic function lastIndexOf(searchString: String!, position: double) : double 
            value.lastIndexOf(searchString, position);

        /* E262-3 15.5.4.9: String.prototype.localeCompare 
           E262-4 draft proposals:static_generics
         */
        prototype function localeCompare(that)
            ToString(this).localeCompare(that);

        public static function localeCompare(self, that)
            ToString(self).localeCompare(that);

        intrinsic function localeCompare(that : String!) : double
            value.localeCompare(that);

        /* E262-3 15.5.4.10: String.prototype.match
           E262-4 draft proposals:static_generics
        */
        prototype function match(regexp)
            ToString(this).match(regexp);

        public static function match(self, regexp)
            ToString(self).match(regexp);

        intrinsic function match(r) : Array 
            value.match(r);

        /* E262-3 15.5.4.11: String.prototype.replace 
           E262-4 draft proposals:static_generics
         */
        prototype function replace(searchValue, replaceValue)
            ToString(this).replace(searchValue, replaceValue);

        public static function replace(self, searchValue, replaceValue)
            ToString(self).replace(searchValue, replaceValue);

        intrinsic function replace(s, r) : String
            value.replace(searchValue, replaceValue);
        
        /* E262-3 15.5.4.12: String.prototype.search 
           E262-4 draft proposals:static_generics
         */
        prototype function search(regexp)
            ToString(this).search(regexp);

        public static function search(self, regexp)
            ToString(self).search(regexp);

        intrinsic function search(r) : double 
            value.search(r);

        /* E262-3 15.5.4.13: String.prototype.slice 
           E262-4 draft proposals:static_generics
         */
        prototype function slice(start, end)
            ToString(this).slice(start, end);

        public static function slice(self, start, end)
            ToString(self).slice(start, end);

        intrinsic function slice(s, e) : Array
            value.slice(s, e);
        
        /* ES262-3 15.5.4.14: String.prototype.split
           E262-4 draft proposals:static_generics
        */
        prototype function split(separator, limit)
            ToString(this).split(separator, limit);

        public static function split(self, separator, limit)
            ToString(self).split(separator, limit);

        intrinsic function split(separator, limit) : Array
            value.split(separator, limit);

        /* E262-3 15.5.4.15: String.prototype.substring
           E262-4 draft proposals:static_generics
         */
        prototype function substring(start, end)
            ToString(this).substring(start, end);

        public static function substring(self, start, end)
            ToString(self).substring(start, end)

        intrinsic function substring(start: double, end: double) : string
            value.substring(start, end);

        /* E262-3 15.5.4.16: String.prototype.toLowerCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLowerCase()
            ToString(this).toLowerCase();

        public static function toLowerCase(selft)
            ToString(self).toLowerCase();

        intrinsic function toLowerCase() : string
            value.toLowerCase();

        /* E262-3 15.5.4.17: String.prototype.toLocaleLowerCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLocaleLowerCase()
            ToString(this).toLocaleLowerCase();

        public prototype function toLocaleLowerCase(self)
            ToString(self).toLocaleLowerCase();

        intrinsic function toLocaleLowerCase() : string
            value.toLocaleLowerCase();

        /* E262-3 15.5.4.18: String.prototype.toUpperCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toUpperCase()
            ToString(this).toUpperCase();

        public static function toUpperCase(self)
            ToString(self).toUpperCase();

        intrinsic function toUpperCase() : string
            value.toUpperCase();

        /* E262-3 15.5.4.19: String.prototype.toLocaleUpperCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLocaleUpperCase()
            ToString(this).toLocaleUpperCase();

        public static function toLocaleUpperCase(self)
            ToString(self).toLocaleUpperCase();

        intrinsic function toLocaleUpperCase() : string
            value.toLocaleUpperCase();

        /* E262-4 draft proposals:json_encoding_and_decoding */
        prototype function parseJSON() 
            ToString(this).parseJSON();

        intrinsic function parseJSON(...args)
            Function.apply(string.parseJSON, value, args);

        /* E262-4 draft proposals:string.prototype.trim */
        prototype function trim()
            ToString(this).trim();

        intrinsic function trim() : string
            value.trim();

        /* E262-3 15.5.5.1: length. */
        public function get length() : uint
            magic::stringLength(this);
    }
}
