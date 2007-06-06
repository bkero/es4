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
 * Status: complete; not reviewed; not tested.
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
        /* E262-4 draft */
        meta static function convert(x) 
            string(x);

        /* E262-3 15.5.1: The String Constructor Called as a Function */
        meta static function invoke(x="")
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

        /* E262-3 15.5.4.2: String.prototype.toString */
        prototype function toString(this : String) {
            /* FIXME (Ticket #78): redundant check */
            if (this is String)
                return ToString(this);
            throw new Error("Implementation error: the 'this' constraint should have caught this one.");
        }

        override intrinsic function toString() : string
            ToString(this);
        

        /* E262-3 15.5.4.3: String.prototype.valueOf */
        prototype function valueOf(this : String) {
            /* FIXME (Ticket #78): redundant check */
            if (this is String)
                return ToString(this);
            throw new Error("Implementation error: the 'this' constraint should have caught this one.");
        }
        
        override intrinsic function valueOf() : String
            ToString(this);


        /* E262-3 15.5.4.4: String.prototype.charAt
           E262-4 draft proposals:static_generics
        */
        prototype function charAt(pos)
            string.charAt(this, pos);

        static function charAt(self, pos)
            string.charAt(self, pos);
            
        intrinsic function charAt(pos: double = 0) : string
            string.charAt(this, pos);

        /* E262-3 15.5.4.5: String.prototype.charCodeAt
           E262-4 draft proposals:static_generics
        */
        prototype function charCodeAt(pos)
            string.charCodeAt(this, pos);

        static function charCodeAt(self, pos)
            string.charCodeAt(self, pos);

        intrinsic function charCodeAt(pos: double = 0) : double
            string.charCodeAt(this, pos);

        /* E262-3 15.5.4.6: String.prototype.concat.
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
         */
        prototype function concat(...args)
            string.concatHelper(this, args);

        static function concat(self, ...args) : string
            string.concatHelper(self, args);

        intrinsic function concat(...args) : string
            string.concatHelper(this, args);


        /* E262-3 15.5.4.7: String.prototype.indexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function indexOf(searchString, position)
            string.indexOf(this, searchString, position);

        static function indexOf(self, searchString, position): double
            string.indexOf(this, searchString, position);

        intrinsic function indexOf(searchString: String!, position: double = 0.0) : double 
            string.indexOf(this, searchString, position);


        /* E262-3 15.5.4.8: String.prototype.lastIndexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function lastIndexOf(searchString, position)
            string.lastIndexOf(this, searchString, position);

        static function lastIndexOf(self, searchString, position) : double
            string.lastIndexOf(self, searchString, position);

        intrinsic function lastIndexOf(searchString: String!, position: double) : double 
            string.lastIndexOf(this, searchString, position);


        /* E262-3 15.5.4.9: String.prototype.localeCompare 
           E262-4 draft proposals:static_generics
         */
        prototype function localeCompare(that)
            string.localeCompare(this, that);

        static function localeCompare(self, that) : double
            string.localeCompare(self, that);

        intrinsic function localeCompare(that: String!) : double
            string.localeCompare(this, that);


        /* E262-3 15.5.4.10: String.prototype.match
           E262-4 draft proposals:static_generics
        */
        prototype function match(regexp)
            string.match(this, regexp);

        static function match(self, regexp) : Array
            string.match(self, regexp);

        intrinsic function match(regexp: RegExp!) : Array 
            string.match(this, regexp);


        /* E262-3 15.5.4.11: String.prototype.replace 
           E262-4 draft proposals:static_generics
         */
        prototype function replace(searchValue, replaceValue)
            string.replace(this, searchValue, replaceValue);

        static function replace(self, searchValue, replaceValue) : string
            string.replace(this, searchValue, replaceValue);

        intrinsic function replace(s: (RegExp!,String!), r: (String!,function(...):String!)) : string
            string.replace(this, searchValue, replaceValue);
        

        /* E262-3 15.5.4.12: String.prototype.search 
           E262-4 draft proposals:static_generics
         */
        prototype function search(regexp)
            string.search(this, regexp);

        static function search(self, regexp) : double
            string.search(self, regexp);

        intrinsic function search(regexp: RegExp!) : double 
            string.search(this, r);


        /* E262-3 15.5.4.13: String.prototype.slice 
           E262-4 draft proposals:static_generics
         */
        prototype function slice(start, end)
            string.slice(this, start, end);

        static function slice(self, start, end): string
            string.slice(self, start, end);

        intrinsic function slice(s: double, e: double): string
            string.slice(this, s, e);
        

        /* ES262-3 15.5.4.14: String.prototype.split
           E262-4 draft proposals:static_generics
        */
        prototype function split(separator, limit)
            string.split(this, separator, limit);

        static function split(self, separator, limit): Array!
            string.split(self, separator, limit);

        intrinsic function split(separator:(String!,RegExp!), limit: uint = uint.MAX_VALUE) : Array!
            string.split(this, separator, limit);


        /* E262-3 15.5.4.15: String.prototype.substring
           E262-4 draft proposals:static_generics
         */
        prototype function substring(start, end)
            string.substring(this, start, end);

        static function substring(self, start, end): string
            string.substring(self, start, end);

        intrinsic function substring(start: double, end: double=this.length) : string
            string.substring(this, start, end);


        /* E262-3 B.2.3: String.prototype.substr
           E262-4 draft proposals:static_generics
         */
        prototype function substr(start, length)
            string.substr(this, start, length);

        static function substring(self, start, length): string
            string.substr(self, start, length);
            
        intrinsic function substring(start: double, length: double): string
            string.substr(this, start, length);


        /* E262-3 15.5.4.16: String.prototype.toLowerCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLowerCase()
            string.toLowerCase(this);

        static function toLowerCase(self): string
            string.toLowerCase(self);

        intrinsic function toLowerCase(): string
            string.toLowerCase(this);


        /* E262-3 15.5.4.17: String.prototype.toLocaleLowerCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLocaleLowerCase()
            string.toLocaleLowerCase(this);

        prototype function toLocaleLowerCase(self): string
            string.toLocaleLowerCase(self);

        intrinsic function toLocaleLowerCase(): string
            string.toLocaleLowerCase(this);


        /* E262-3 15.5.4.18: String.prototype.toUpperCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toUpperCase()
            string.toUpperCase(this);

        static function toUpperCase(self): string
            string.toUpperCase(self);

        intrinsic function toUpperCase() : string
            string.toUpperCase(this);


        /* E262-3 15.5.4.19: String.prototype.toLocaleUpperCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLocaleUpperCase()
            string.toLocaleUpperCase(this);

        static function toLocaleUpperCase(self): string
            string.toLocaleUpperCase(self);

        intrinsic function toLocaleUpperCase() : string
            string.toLocaleUpperCase(this);


        /* E262-4 draft proposals:json_encoding_and_decoding */
        prototype function parseJSON(...args) 
            string.parseJSONHelper(this, args);

        intrinsic function parseJSON(...args)
            string.parseJSONHelper(this, args);


        /* E262-4 draft proposals:string.prototype.trim */
        prototype function trim()
            string.trim(this);

        intrinsic function trim() : string
            string.trim(this);


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
