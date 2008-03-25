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

// Vile hack.  See Object.es for documentation
package org.ecmascript.vilehack.Name {
    public namespace Private = "Name private";
}

package 
{
    import org.ecmascript.vilehack.Name.*;

    use namespace __ES4__;

    __ES4__ final class Name extends String 
    {
        // IMPLEMENTATION ARTIFACT: A getter because Name is loaded before int.
        static public function get length() { return 2 }

        // FIXME, "is undefined" would be better than "=== undefined"
        // but doesn't work, ticket #364 I think.

        static helper function analyzeArgs (a, b) {
            if (a is Namespace)
                return analyzeWithNamespace(a, b);
            if (b === undefined) {
                if (a is Name)
                    return a;
                return analyzeWithNamespace(null, a);
            }
            throw new TypeError();                        
        
            function analyzeWithNamespace(ns, x) {
                if (x is AnyNumber && isIntegral(x) && x > 0 && x <= 0xFFFFFFFF || x is AnyString)
                    return { qualifier: ns, identifier: string(x) };
                throw TypeError();
            }
        }

        // FIXME: use abbreviations here when they are supported

        public function Name(a, b=undefined) 
            : { qualifier: qualifier, 
                identifier: identifier } = helper::analyzeArgs(a,b)
        {}
        
        static meta function invoke(a, b=undefined): Name
            new Name(a, b);
        
        prototype function toString(this : Name)
            this.Private::toString();
        
        override intrinsic function toString() : string
            Private::toString();

        Private function toString() : string {
            if (qualifier === null)
                return identifier;
            return string(qualifier) + "::" + identifier;
        }
        
        prototype function valueOf(this : Name)
            this.Private::toString();
        
        override intrinsic function valueOf() : string
            Private::toString();
        
        public const qualifier  : Namespace,
                     identifier : string;
    }
}
