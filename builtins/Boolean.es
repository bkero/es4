/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Boolean" wrapper object
 *
 * E262-3 15.6
 * E262-4 19.x
 * E262-4 proposals:builtin_classes
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

    // RI bug: the "this" constraint on methods can't use union types,
    // but the parser allows type names...
    __ES4__ type AnyBoolean = (boolean|Boolean!);

    dynamic class Boolean
    {
        private var val : boolean;

        // IMPLEMENTATION ARTIFACT: A getter because Boolean is loaded before int.
        static function get length() { return 1 }

        /* E262-3 15.6.1: The Boolean Constructor Called as a Function. */
        meta static function invoke(x=false) : boolean
            boolean(x);

        /* E262-3 15.6.2: The Boolean Constructor. */
        function Boolean(x=false) : val = boolean(x) {}

        /* E262-4 early-binding variant. */
        override intrinsic function toString() : string
            intrinsic::valueOf().intrinsic::toString();

        override intrinsic function toJSONString(pretty: boolean=false) : string
            intrinsic::valueOf().intrinsic::toJSONString();

        /* E262-4 early-binding variant. */
        override intrinsic function valueOf() : boolean
            val;

        // The boolean class uses the Boolean class's prototype too.

        /* E262-3 15.6.4.2: Boolean.prototype.toString.  */
        prototype function toString(this: AnyBoolean)
            this.intrinsic::toString();

        prototype function toJSONString(this: AnyBoolean, pretty=false)
            this.intrinsic::toJSONString(pretty);

        /* E262-3 15.6.4.3: Boolean.prototype.valueOf. */
        prototype function valueOf(this: AnyBoolean)
            this.intrinsic::valueOf();
    }
}
