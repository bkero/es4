/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Name" object
 *
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 * 
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 * 
 *    2. All liability and responsibility for the implementation or other
 * use of this Reference Implementation rests with the implementor, and
 * not with any of the parties who contribute to, or who own or hold any
 * copyright in, this Reference Implementation.
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

use namespace intrinsic;

intrinsic final class Name extends String {
    type NS = (Name, Namespace, string);
    type ID = (undefined, string);

    function Name(ns : NS, id : ID = undefined) {
        if (id is undefined) {
            if (ns is Name) {
                let n : Name = ns;
                identifier = n.identifier;
                qualifier = n.qualifier;
            } else {
                // FIXME #80: remove this line when qualifier defaults to null not undefined
                qualifier = null;
                identifier = ns;
            }
        } else {
            qualifier = ns;
            identifier = id;
        }
    }

    meta static function invoke(ns : NS, id : ID = undefined) : Name
        new Name(ns, id);

    meta static function convert(v : (Namespace, string))
        new Name(v);

    prototype function toString(this : Name)
        this.intrinsic::toString();

    intrinsic override function toString() : string {
        if (qualifier === null)
            return identifier;
        return qualifier + "::" + identifier;
    }

    prototype function valueOf(this : Name)
        this.intrinsic::valueOf();

    intrinsic override function valueOf() : string
        intrinsic::toString();

    // FIXME #42: use const again when it works
    public var   qualifier  : Namespace,
                 identifier : string;
}
