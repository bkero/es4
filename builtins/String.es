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
 *    new String + helper::setStringValue
 *    helper::fromCharCode
 *    helper::stringAppend
 *
 * A string's length is obtained by:
 *    helper::stringLength
 *
 * Strings are picked apart by:
 *    helper::charCodeAt
 *
 * (We would expect "+" to map to helper::stringAppend().)
 */

    use namespace intrinsic;

    __ES4__ type AnyString = (string|String!);

    /* The January 2007 meeting resolved that String is open and
     * dynamic.
     */
    dynamic class String
    {
        private var val : string;

        // A getter because String is loaded before int
        static function get length() { return 1 }

        /* E262-3 15.5.1: The String Constructor Called as a Function */
        static meta function invoke(value="")
            string(value);

        /* 15.5.2 The String Constructor */
        // Ticket #364, this does not work when called as Name's base class constructor
        //function String(value="") : val = string(value) {}
        function String(value="") { 
            val = string(value);
        }

        /* E262-3 15.5.3.2: String.fromCharCode
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        static function fromCharCode(...args)
            string.helper::fromCharCode(args);

        override intrinsic function toString() : string
            val;

        override intrinsic function toJSONString(pretty:boolean=false) : string
            JSON.formatString(val, pretty);

        override intrinsic function valueOf() : string
            val;


        /* E262-3 15.5.4.4: String.prototype.charAt
           E262-4 draft proposals:static_generics
        */
        static function charAt(self, pos)
            string.charAt(self, pos);

        intrinsic function charAt(pos: double = 0) : string
            string.charAt(val, pos);

        /* E262-3 15.5.4.5: String.prototype.charCodeAt
           E262-4 draft proposals:static_generics
        */
        static function charCodeAt(self, pos)
            string.charCodeAt(self, pos);

        intrinsic function charCodeAt(pos: double = 0) : double
            string.charCodeAt(val, pos);

        /* E262-3 15.5.4.6: String.prototype.concat.
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
         */
        static function concat(self, ...args) : string
            string.helper::concat(self, args);

        intrinsic function concat(...args) : string
            string.helper::concat(val, args);


        /* E262-3 15.5.4.7: String.prototype.indexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        static function indexOf(self, searchString, position): double
            string.indexOf(self, searchString, position);

        intrinsic function indexOf(searchString: AnyString, position: double = 0.0) : double
            string.indexOf(val, searchString, position);


        /* E262-3 15.5.4.8: String.prototype.lastIndexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        static function lastIndexOf(self, searchString, position) : double
            string.lastIndexOf(self, searchString, position);

        intrinsic function lastIndexOf(searchString: AnyString, position: double) : double
            string.lastIndexOf(val, searchString, position);


        /* E262-3 15.5.4.9: String.prototype.localeCompare
           E262-4 draft proposals:static_generics
         */
        static function localeCompare(self, that) : double
            string.localeCompare(self, that);

        intrinsic function localeCompare(that: AnyString) : double
            string.localeCompare(val, that);


        /* E262-3 15.5.4.10: String.prototype.match
           E262-4 draft proposals:static_generics
        */
        static function match(self, regexp) : Array
            string.match(self, regexp);

        intrinsic function match(regexp: RegExp!) : Array
            string.match(val, regexp);


        /* E262-3 15.5.4.11: String.prototype.replace
           E262-4 draft proposals:static_generics
         */
        static function replace(self, searchValue, replaceValue) : string
            string.replace(self, searchValue, replaceValue);

        intrinsic function replace(s: (RegExp!|AnyString), r: (AnyString|function(...):AnyString)) : string
            string.replace(val, searchValue, replaceValue);


        /* E262-3 15.5.4.12: String.prototype.search
           E262-4 draft proposals:static_generics
         */
        static function search(self, regexp) : double
            string.search(self, regexp);

        intrinsic function search(regexp: RegExp!) : double
            string.search(val, r);


        /* E262-3 15.5.4.13: String.prototype.slice
           E262-4 draft proposals:static_generics
         */
        static function slice(self, start, end, step): string
            string.slice(self, Number(start), Number(end), Number(step));

        intrinsic function slice(start: AnyNumber, end: AnyNumber, step: AnyNumber): string
            string.slice(val, start, end, step);


        /* ES262-3 15.5.4.14: String.prototype.split
           E262-4 draft proposals:static_generics
        */
        static function split(self, separator, limit): Array!
            string.split(self, separator, limit);

        intrinsic function split(separator:(AnyString|RegExp!), limit: double = double.MAX_VALUE) : Array!
            string.split(val, separator, limit);


        /* E262-3 15.5.4.15: String.prototype.substring
           E262-4 draft proposals:static_generics
         */
        static function substring(self, start, end): string
            string.substring(self, start, end);

        intrinsic function substring(start: double, end: double=Infinity) : string
            string.substring(val, start, end);


        /* E262-3 B.2.3: String.prototype.substr
           E262-4 draft proposals:static_generics
         */
        static function substr(self, start, length): string
            string.substr(self, start, length);

        intrinsic function substr(start: double, length: double): string
            string.substr(val, start, length);


        /* E262-3 15.5.4.16: String.prototype.toLowerCase
           E262-4 draft proposals:static_generics
         */
        static function toLowerCase(self): string
            string.toLowerCase(self);

        intrinsic function toLowerCase(): string
            string.toLowerCase(val);


        /* E262-3 15.5.4.17: String.prototype.toLocaleLowerCase
           E262-4 draft proposals:static_generics
         */
        static function toLocaleLowerCase(self): string
            string.toLocaleLowerCase(self);

        intrinsic function toLocaleLowerCase(): string
            string.toLocaleLowerCase(val);


        /* E262-3 15.5.4.18: String.prototype.toUpperCase
           E262-4 draft proposals:static_generics
         */
        static function toUpperCase(self): string
            string.toUpperCase(self);

        intrinsic function toUpperCase() : string
            string.toUpperCase(val);


        /* E262-3 15.5.4.19: String.prototype.toLocaleUpperCase
           E262-4 draft proposals:static_generics
         */
        static function toLocaleUpperCase(self): string
            string.toLocaleUpperCase(self);

        intrinsic function toLocaleUpperCase() : string
            string.toLocaleUpperCase(val);


        /* E262-4 draft proposals:json_encoding_and_decoding */
        static function parseJSON(self, filter=undefined)
            string.parseJSON(string(self), filter);

        intrinsic function parseJSON(filter=undefined)
            string.parseJSON(string(val), filter);


        /* E262-4 draft proposals:string.prototype.trim */
        static function trim(self) : string
            string.trim(self);

        intrinsic function trim() : string
            string.trim(string(val));


        /* E262-3 15.5.5.1: length. */
        function get length() : double
            val.length;


        /* Catchall indexing operation. */
        /* FIXME: remove use of intrinsic::get / set. Ticket #214 */
        meta function get(n) {
            let x = double(n);
            if (isNaN(x))
                return intrinsic::get(this, n);
            return charAt(x);
        }

        meta function set(n,v) {
            let x = double(n);
            if (isNaN(x))
                intrinsic::set(this, n, v);
        }


        prototype function toString(this: AnyString)
            this.intrinsic::toString();

        prototype function toJSONString(this: AnyString, pretty=false)
            this.intrinsic::toJSONString(pretty);

        prototype function valueOf(this: AnyString)
            this.intrinsic::valueOf();

        prototype function charAt(pos)
            string.charAt(this.toString(), pos);

        prototype function charCodeAt(pos)
            string.charCodeAt(this.toString(), pos);

        prototype function concat(...args)
            string.helper::concat(this.toString(), args);

        prototype function indexOf(searchString, position)
            string.indexOf(this.toString(), searchString, position);

        prototype function lastIndexOf(searchString, position)
            string.lastIndexOf(this.toString(), searchString, position);

        prototype function localeCompare(that)
            string.localeCompare(this.toString(), that);

        prototype function match(regexp)
            string.match(this.toString(), regexp);

        prototype function replace(searchValue, replaceValue)
            string.replace(this.toString(), searchValue, replaceValue);

        prototype function search(regexp)
            string.search(this.toString(), regexp);

        prototype function slice(start, end, step)
            string.slice(this.toString(), Number(start), Number(end), Number(step));

        prototype function split(separator, limit)
            string.split(this.toString(), separator, limit);

        prototype function substring(start, end)
            string.substring(this.toString(), start, end);

        prototype function substr(start, length)
            string.substr(this.toString(), start, length);

        prototype function toLowerCase()
            string.toLowerCase(this.toString());

        prototype function toLocaleLowerCase()
            string.toLocaleLowerCase(this.toString());

        prototype function toUpperCase()
            string.toUpperCase(this.toString());

        prototype function toLocaleUpperCase()
            string.toLocaleUpperCase(this.toString());

        prototype function parseJSON(this:AnyString, filter=undefined)
            string.parseJSON(this.toString(), filter);

        prototype function trim()
            string.trim(this.toString());
    }
