/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "EncodingError" object
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
    use default namespace public;
    use namespace intrinsic;
    use namespace __ES4__;
    use strict;

    /* E262-3 15.11.6.1; 15.11.7 */
    dynamic class EncodingError extends Error
    {
        static const length = 1;

        meta static function invoke(message)
            new EncodingError(message);

        function EncodingError(message) 
            : super(message) 
        {
        }

        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "EncodingError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Bad encoding";
    }
}
