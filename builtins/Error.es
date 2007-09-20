/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Error" object
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
 *
 * E262-3 15.11
 * E262-4 draft (intrinsic functions)
 *
 * Status: Incomplete; Not reviewed against spec; Not tested.
 *
 * To check:
 *
 * Can we use "prototype" with "var" like I do here?  It makes sense,
 * and is desirable, but probably not necessary.
 *
 * It's my belief that making eg EvalError extend Error makes
 * EvalError.prototype be an Error object, though that needs to be
 * verified.
 */

package
{
    use namespace intrinsic;
    use default namespace public;

    /* E262-3 15.11 */
    dynamic class Error
    {
        meta static function invoke(message)
            new Error(message);

        function Error(message) {
            if (message !== undefined)
                this.public::message = string(message);
        }

        /* E262-3 15.11.4.2: "name" property on prototype */
        prototype var name = "Error";

        /* E262-3 15.11.4.3: "message" property on prototype */
        /* INFORMATIVE */
        prototype var message = "Generic error";

        /* E262-3 15.11.4.4: toString */
        prototype function toString(this:Error)
            this.intrinsic::toString();

        /* INFORMATIVE */
        override intrinsic function toString() {
            if (this.public::message !== undefined)
                return this.public::name + ": " + this.public::message;   // "this" qualification in case they've been deleted
            else
                return this.public::name;
        }
    }
}
