/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "String" wrapper object
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
    use default namespace public;
    use namespace intrinsic;
    use strict;
    
    /* The January 2007 meeting resolved that String is open and
     * dynamic
     */
    dynamic class String
    {       
        /* E262-3 15.5.1: The String Constructor Called as a Function */
        static meta function invoke(x="")
            string(x);

        /* 15.5.2 The String Constructor */
        function String(x="")
            magic::bindString(this, x);

        /* E262-3 15.5.3.2: String.fromCharCode
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        static function fromCharCode(...args)
            string.fromCharCodeHelper(args);

        intrinsic static function fromCharCode(...args) : double
            string.fromCharCodeHelper(args);

        /* E262-3 15.5.4.1: String.prototype.constructor */
        String.prototype.constructor = String;

        /* E262-3 15.5.4.2: String.prototype.toString */
        prototype function toString(this : String)
            ToString(this);

        override intrinsic function toString() : string
            private::toString();
        
        private final function toString() : string
            ToString(this);
        
        /* E262-3 15.5.4.3: String.prototype.valueOf */
        prototype function valueOf(this : String)
            this;
        
        override intrinsic function valueOf() : String
            private::valueOf();

        private final function valueOf() : String
            this;

        /* E262-3 15.5.4.4: String.prototype.charAt
           E262-4 draft proposals:static_generics
        */
        prototype function charAt(pos)
            ToString(this).charAt(pos);

        static function charAt(self, pos)
            ToString(self).charAt(pos);
            
        intrinsic function charAt(pos: double = 0) : String
            ToString(this).charAt(pos);

        /* E262-3 15.5.4.5: String.prototype.charCodeAt
           E262-4 draft proposals:static_generics
        */
        prototype function charCodeAt(pos)
            ToString(this).charCodeAt(pos);

        static function charCodeAt(self, pos)
            ToString(self).charCodeAt(pos);

        intrinsic function charCodeAt(pos: double = 0) : double
            ToString(this).charCodeAt(pos);

        /* E262-3 15.5.4.6: String.prototype.concat.
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
         */
        prototype function concat(...args)
            Function.apply(string.concat, ToString(this), args);

        static function concat(self, ...args)
            Function.apply(string.concat, ToString(self), args);

        intrinsic function concat(...args) : String
            Function.apply(string.concat, ToString(this), args);


        /* E262-3 15.5.4.7: String.prototype.indexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function indexOf(searchString, position)
            ToString(this).indexOf(searchString, position);

        static function indexOf(self, searchString, position)
            ToString(self).indexOf(searchString, position);

        intrinsic function indexOf(searchString: String!, position: double = 0.0) : double 
            ToString(this).indexOf(searchString, position);

        /* E262-3 15.5.4.8: String.prototype.lastIndexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function lastIndexOf(searchString, position)
            ToString(this).lastIndexOf(searchString, position);

        static function lastIndexOf(self, searchString, position)
            ToString(self).lastIndexOf(searchString, position);

        intrinsic function lastIndexOf(searchString: String!, position: double) : double 
            ToString(this).lastIndexOf(searchString, position);

        /* E262-3 15.5.4.9: String.prototype.localeCompare 
           E262-4 draft proposals:static_generics
         */
        prototype function localeCompare(that)
            ToString(this).localeCompare(that);

        static function localeCompare(self, that)
            ToString(self).localeCompare(that);

        intrinsic function localeCompare(that : String!) : double
            ToString(this).localeCompare(that);

        /* E262-3 15.5.4.10: String.prototype.match
           E262-4 draft proposals:static_generics
        */
        prototype function match(regexp)
            ToString(this).match(regexp);

        static function match(self, regexp)
            ToString(self).match(regexp);

        intrinsic function match(r) : Array 
            ToString(this).match(r);

        /* E262-3 15.5.4.11: String.prototype.replace 
           E262-4 draft proposals:static_generics
         */
        prototype function replace(searchValue, replaceValue)
            ToString(this).replace(searchValue, replaceValue);

        static function replace(self, searchValue, replaceValue)
            ToString(self).replace(searchValue, replaceValue);

        intrinsic function replace(s, r) : String
            ToString(this).replace(searchValue, replaceValue);
        
        /* E262-3 15.5.4.12: String.prototype.search 
           E262-4 draft proposals:static_generics
         */
        prototype function search(regexp)
            ToString(this).search(regexp);

        static function search(self, regexp)
            ToString(self).search(regexp);

        intrinsic function search(r) : double 
            ToString(this).search(r);

        /* E262-3 15.5.4.13: String.prototype.slice 
           E262-4 draft proposals:static_generics
         */
        prototype function slice(start, end)
            ToString(this).slice(start, end);

        static function slice(self, start, end)
            ToString(self).slice(start, end);

        intrinsic function slice(s, e) : Array
            ToString(this).slice(s, e);
        
        /* ES262-3 15.5.4.14: String.prototype.split
           E262-4 draft proposals:static_generics
        */
        prototype function split(separator, limit)
            ToString(this).split(separator, limit);

        static function split(self, separator, limit)
            ToString(self).split(separator, limit);

        intrinsic function split(separator, limit) : Array
            ToString(this).split(separator, limit);

        /* E262-3 15.5.4.15: String.prototype.substring
           E262-4 draft proposals:static_generics
         */
        prototype function substring(start=0.0, end=undefined)
            ToString(this).substring(start, end == undefined ? this.length : end);
        
        static function substring(self, start, end) 
            ToString(self).substring(start, end);
            
        intrinsic function substring(start: double, end: double) : string
            ToString(this).substring(start, end);

        /* E262-3 15.5.4.16: String.prototype.toLowerCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLowerCase()
            ToString(this).toLowerCase();

        static function toLowerCase(selft)
            ToString(self).toLowerCase();

        intrinsic function toLowerCase() : string
            ToString(this).toLowerCase();

        /* E262-3 15.5.4.17: String.prototype.toLocaleLowerCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLocaleLowerCase()
            ToString(this).toLocaleLowerCase();

        prototype function toLocaleLowerCase(self)
            ToString(self).toLocaleLowerCase();

        intrinsic function toLocaleLowerCase() : string
            ToString(this).toLocaleLowerCase();

        /* E262-3 15.5.4.18: String.prototype.toUpperCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toUpperCase()
            ToString(this).toUpperCase();

        static function toUpperCase(self)
            ToString(self).toUpperCase();

        intrinsic function toUpperCase() : string
            ToString(this).toUpperCase();

        /* E262-3 15.5.4.19: String.prototype.toLocaleUpperCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLocaleUpperCase()
            ToString(this).toLocaleUpperCase();

        static function toLocaleUpperCase(self)
            ToString(self).toLocaleUpperCase();

        intrinsic function toLocaleUpperCase() : string
            ToString(this).toLocaleUpperCase();

        /* E262-4 draft proposals:json_encoding_and_decoding */
        prototype function parseJSON() 
            ToString(this).parseJSON();

        intrinsic function parseJSON(...args)
            Function.apply(string.parseJSON, ToString(this), args);

        /* E262-4 draft proposals:string.prototype.trim */
        prototype function trim()
            ToString(this).trim();

        intrinsic function trim() : string
            ToString(this).trim();

        /* E262-3 15.5.5.1: length. */
        function get length() : uint
            magic::stringLength(this);

        /* Catchall indexing operation. */
        meta function get(pos) {
            let x : double = ToDouble(pos);
            if (isNaN(x))
                return undefined;
            return charAt(x);
        }
    }
}
