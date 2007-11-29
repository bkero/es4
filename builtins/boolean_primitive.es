/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "boolean" object
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
    use strict;

    import JSON.*;

    /*
     * The boolean class is final and non-dynamic because most
     * implementations will in fact represent boolean objects not
     * using objects, but using some magic tagged value, of which
     * there will be only two, one for "true" and the other for
     * "false".
     *
     * The literals true and false denote instances of this class.
     */
    __ES4__ final class boolean!
    {
        // IMPLEMENTATION ARTIFACT: A getter because boolean is loaded before int.
        static function get length() { return 1 }

        /* E262-3 15.6.1: The boolean Constructor Called as a Function. */
        meta static function invoke(x=null) : boolean
            x is boolean ? x : new boolean(x);

        /* E262-4 early-binding variant. */
        override intrinsic function toString() : string
            this ? "true" : "false";

        override intrinsic function toJSONString(pretty: boolean=null) : string
            JSON.formatBoolean(this, pretty);

        /* E262-4 draft ch 19 */
        override intrinsic function valueOf() : boolean
            this;

        /* The E262-3 boolean primitive consumes all additional [[set]] operations. */
        meta function set(n,v) : void
        {
        }
    }
}
