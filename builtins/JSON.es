/* -*- mode: java; indent-tabs-mode: nil -*-
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
 */

package JSON
{
    use namespace intrinsic;
    use namespace __ES4__;
    use strict;

    public function formatObject(object, pretty=false): string {
        pp = pretty;
        return fmtObject(object);
    }

    public function formatArray(array, pretty=false): string {
        pp = pretty;
        return fmtArray(array);
    }

    public function formatDate(date, pretty=false): string
        '"' + date.intrinsic::toISOString() + '"';

    public function formatString(s, pretty=false): string {
        // FIXME
        return "\"" + s + "\"";
    }

    public function formatNumber(n, pretty=false): string {
        if (isNaN(n) || !isFinite(n))
            return "null";
        return string(n);
    }

    public function formatBoolean(bool, pretty=false): string
        bool ? "true" : "false";

    public function parse(s: string, filter) {
        // FIXME
        return {};
    }

    var pp = false;    // true for pretty-printing
    var level = 0;    // indentation level
    var active = new ObjectMap;

    // The only way to ask if something is of a specific type (and not
    // one of the subtypes) is through the use of the meta-objects
    // system, and then only if you're able to create at least one
    // dummy instance of the type from which to obtain a Type object.

    var objectType = intrinsic::typeOf({});  // Type object for Object

    function isObject(v)
        objectType.isSubtypeOf(intrinsic::typeOf(v));

    function isEncodableValue(v) {
        return ((v is AnyString) ||
                (v is AnyBoolean) ||
                (v is AnyNumber) ||
                (v is null) ||
                (v is Date) ||
                (v is Array) ||
                isObject(v));
    }

    function isEncodableName(n) {
        return (n is uint || 
                n is string || 
                n is Name && n.qualifier == null);
    }

    function fmtObject(obj)
        indent + "{" + newline + fmtFields(obj, level+1) + newline + indent + "}";

    function fmtFields(obj, newlevel) {
        let oldlevel = level;
        try {
            let s = "";
            level = newlevel;
            active.activate(obj);
            let first = true;
            for ( let prop in obj ) {
                if (isEncodableName(prop) && obj.hasOwnProperty(prop)) {
                    let value = obj[prop];
                    if (isEncodableValue(value)) {
                        if (!first)
                            s += comma;
                        first = false;
                        s += (indent + "\"" + prop + "\"" + colon + 
                              value.public::toJSONString(pp));
                    }
                }
            }
            return s;
        }
        finally {
            level = oldlevel;
            active.deactivate(obj);
        }
    }

    function fmtArray(obj)
        indent + "[" + newline + fmtElements(obj, level+1) + newline + indent + "]";

    function fmtElements(obj, newlevel) {
        let oldlevel = level;
        try {
            let s = "";
            let first = true;
            level = newlevel;
            active.activate(obj);
            for ( let i=0, limit=Number(obj.length) ; i < limit ; i++ ) {
                let value = obj[i];
                if (isEncodableValue(value)) {
                    if (!first)
                        s += comma;
                    first = false;
                    s += (indent + value.public::toJSONString(pp));
                }
            }
        }
        finally {
            level = oldlevel;
            active.deactivate(obj);
        }
    }

    function get colon() pp ? ": " : ":";
    function get comma() pp ? ",\n" : ",";
    function get newline() pp ? "\n" : "";
    function get indent() pp ? (new Array(level*4 + 1)).join(" ") : "";

    // This could be a Map.<*,null>, say
    class ObjectMap
    {
        var active = [];   // list of active objects (circularity detection) (could be a Map)

        function activate(obj) {
            if (Array.indexOf(active, obj) != -1)
                throw new EncodingError("Circular structure can't be JSON encoded");
            active.push(obj);
        }

        function deactivate(obj) {
            let probe = Array.indexOf(active, obj);
            if (probe == -1)
                throw new Error("Internal error: object not in active list!");
            active.slice(probe, 1);
        }
    }
}
