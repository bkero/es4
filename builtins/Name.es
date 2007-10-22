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

package 
{
    use default namespace public;
    use namespace intrinsic;
    use namespace __ES4__;
    use strict;
    
    __ES4__ final class Name extends String 
    {
        
        // IMPLEMENTATION ARTIFACT: A getter because Name is loaded before int.
        static function get length() { return 2 }
        
        function Name(a, b=undefined) {
            if (a is Namespace && b is string) {
                qualifier = a;
                identifier = b;                
            }
            if (a is Name && b is undefined) {
                qualifier = a.qualifier;
                identifier = a.identifier;
            }
            if (a is string && b is undefined) {
                identifier = a;
            }
            throw new TypeError();
        }
        
        meta static function invoke(a, b=undefined): Name
            new Name(a, b);
        
        prototype function toString(this : Name)
            this.intrinsic::toString();
        
        override intrinsic function toString() : string {
            if (qualifier === null)
                return identifier;
            return string(qualifier) + "::" + identifier;
        }
        
        prototype function valueOf(this : Name)
            this.intrinsic::valueOf();
        
        override intrinsic function valueOf() : string
            intrinsic::toString();
        
        // FIXME #42: use const again when it works
        public var qualifier  : Namespace,
                   identifier : string;
    }
}
