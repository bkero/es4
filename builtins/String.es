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
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 *
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 *
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 *
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * End of Terms and Conditions
 *
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
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
    import ECMAScript4_Internal.*;

    intrinsic type Strings = (string,String!);

    /* The January 2007 meeting resolved that String is open and
     * dynamic.
     */
    dynamic class String
    {
        /* E262-4 draft */
        /* OBSOLETE */
        meta static function convert(x)
            string(x);

        /* E262-3 15.5.1: The String Constructor Called as a Function */
        meta static function invoke(value="")
            string(value);

        /* 15.5.2 The String Constructor */
        function String(value="")
            magic::bindString(this, value);

        /* E262-3 15.5.3.2: String.fromCharCode
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        static function fromCharCode(...args)
            string.helper::fromCharCode(args);

        override intrinsic function toString() : string
            string(this);

        override intrinsic function valueOf() : string
            intrinsic::toString();


        /* E262-3 15.5.4.4: String.prototype.charAt
           E262-4 draft proposals:static_generics
        */
        static function charAt(self, pos)
            string.charAt(self, pos);

        intrinsic function charAt(pos: double = 0) : string
            string.charAt(this, pos);

        /* E262-3 15.5.4.5: String.prototype.charCodeAt
           E262-4 draft proposals:static_generics
        */
        static function charCodeAt(self, pos)
            string.charCodeAt(self, pos);

        intrinsic function charCodeAt(pos: double = 0) : double
            string.charCodeAt(this, pos);

        /* E262-3 15.5.4.6: String.prototype.concat.
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
         */
        static function concat(self, ...args) : string
            string.helper::concat(self, args);

        intrinsic function concat(...args) : string
            string.helper::concat(this, args);


        /* E262-3 15.5.4.7: String.prototype.indexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        static function indexOf(self, searchString, position): double
            string.indexOf(self, searchString, position);

        intrinsic function indexOf(searchString: Strings, position: double = 0.0) : double
            string.indexOf(this, searchString, position);


        /* E262-3 15.5.4.8: String.prototype.lastIndexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        static function lastIndexOf(self, searchString, position) : double
            string.lastIndexOf(self, searchString, position);

        intrinsic function lastIndexOf(searchString: Strings, position: double) : double
            string.lastIndexOf(this, searchString, position);


        /* E262-3 15.5.4.9: String.prototype.localeCompare
           E262-4 draft proposals:static_generics
         */
        static function localeCompare(self, that) : double
            string.localeCompare(self, that);

        intrinsic function localeCompare(that: Strings) : double
            string.localeCompare(this, that);


        /* E262-3 15.5.4.10: String.prototype.match
           E262-4 draft proposals:static_generics
        */
        static function match(self, regexp) : Array
            string.match(self, regexp);

        intrinsic function match(regexp: RegExp!) : Array
            string.match(this, regexp);


        /* E262-3 15.5.4.11: String.prototype.replace
           E262-4 draft proposals:static_generics
         */
        static function replace(self, searchValue, replaceValue) : string
            string.replace(self, searchValue, replaceValue);

        intrinsic function replace(s: (RegExp!,Strings), r: (Strings,function(...):Strings)) : string
            string.replace(this, searchValue, replaceValue);


        /* E262-3 15.5.4.12: String.prototype.search
           E262-4 draft proposals:static_generics
         */
        static function search(self, regexp) : double
            string.search(self, regexp);

        intrinsic function search(regexp: RegExp!) : double
            string.search(this, r);


        /* E262-3 15.5.4.13: String.prototype.slice
           E262-4 draft proposals:static_generics
         */
        static function slice(self, start, end): string
            string.slice(self, start, end);

        intrinsic function slice(s: double, e: double): string
            string.slice(this, s, e);


        /* ES262-3 15.5.4.14: String.prototype.split
           E262-4 draft proposals:static_generics
        */
        static function split(self, separator, limit): Array!
            string.split(self, separator, limit);

        intrinsic function split(separator:(Strings,RegExp!), limit: uint = uint.MAX_VALUE) : Array!
            string.split(this, separator, limit);


        /* E262-3 15.5.4.15: String.prototype.substring
           E262-4 draft proposals:static_generics
         */
        static function substring(self, start, end): string
            string.substring(self, start, end);

        intrinsic function substring(start: double, end: double=Infinity) : string
            string.substring(this, start, end);


        /* E262-3 B.2.3: String.prototype.substr
           E262-4 draft proposals:static_generics
         */
        static function substr(self, start, length): string
            string.substr(self, start, length);

        intrinsic function substr(start: double, length: double): string
            string.substr(this, start, length);


        /* E262-3 15.5.4.16: String.prototype.toLowerCase
           E262-4 draft proposals:static_generics
         */
        static function toLowerCase(self): string
            string.toLowerCase(self);

        intrinsic function toLowerCase(): string
            string.toLowerCase(this);


        /* E262-3 15.5.4.17: String.prototype.toLocaleLowerCase
           E262-4 draft proposals:static_generics
         */
        static function toLocaleLowerCase(self): string
            string.toLocaleLowerCase(self);

        intrinsic function toLocaleLowerCase(): string
            string.toLocaleLowerCase(this);


        /* E262-3 15.5.4.18: String.prototype.toUpperCase
           E262-4 draft proposals:static_generics
         */
        static function toUpperCase(self): string
            string.toUpperCase(self);

        intrinsic function toUpperCase() : string
            string.toUpperCase(this);


        /* E262-3 15.5.4.19: String.prototype.toLocaleUpperCase
           E262-4 draft proposals:static_generics
         */
        static function toLocaleUpperCase(self): string
            string.toLocaleUpperCase(self);

        intrinsic function toLocaleUpperCase() : string
            string.toLocaleUpperCase(this);


        /* E262-4 draft proposals:json_encoding_and_decoding */
        static function parseJSON(self, ...args)
            string.helper::parseJSON(self, args);

        intrinsic function parseJSON(...args)
            string.helper::parseJSON(this, args);


        /* E262-4 draft proposals:string.prototype.trim */
        static function trim(self) : string
            string.trim(self);

        intrinsic function trim() : string
            string.trim(string(this));


        /* E262-3 15.5.5.1: length. */
        function get length() : uint
            magic::stringLength(this);


        /* Catchall indexing operation. */
        meta function get(pos) {
            let x = double(pos);
            if (isNaN(x))
                return undefined;
            return charAt(x);
        }


        prototype function toString(this: Strings)
            this.intrinsic::toString();

        prototype function valueOf(this: Strings)
            this.intrinsic::valueOf();

        prototype function charAt(pos)
            string.charAt(this, pos);

        prototype function charCodeAt(pos)
            string.charCodeAt(this, pos);

        prototype function concat(...args)
            string.helper::concat(this, args);

        prototype function indexOf(searchString, position)
            string.indexOf(this, searchString, position);

        prototype function lastIndexOf(searchString, position)
            string.lastIndexOf(this, searchString, position);

        prototype function localeCompare(that)
            string.localeCompare(this, that);

        prototype function match(regexp)
            string.match(this, regexp);

        prototype function replace(searchValue, replaceValue)
            string.replace(this, searchValue, replaceValue);

        prototype function search(regexp)
            string.search(this, regexp);

        prototype function slice(start, end)
            string.slice(this, start, end);

        prototype function split(separator, limit)
            string.split(this, separator, limit);

        prototype function substring(start, end)
            string.substring(this, start, end);

        prototype function substr(start, length)
            string.substr(this, start, length);

        prototype function toLowerCase()
            string.toLowerCase(this);

        prototype function toLocaleLowerCase()
            string.toLocaleLowerCase(this);

        prototype function toUpperCase()
            string.toUpperCase(this);

        prototype function toLocaleUpperCase()
            string.toLocaleUpperCase(this);

        prototype function parseJSON(...args)
            string.helper::parseJSON(this, args);

        prototype function trim()
            string.trim(this);
    }
}
