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

/* Primitive conversion functions.  These are all "intrinsic"-only, in
   E262-3 they are hidden in the implementation but there's no real
   reason not to expose them, though we could certainly hide them.
*/

package
{
    use namespace intrinsic;
    use namespace __ES4__;
    use strict;

    // Unused functions have been commented out.  lth / 2007-09-07

    /*
    intrinsic function IsPrimitive(value)
        magic::isPrimitive(value);
    */

    /*
    intrinsic function DefaultValue(obj, preferredType)
        magic::defaultValue(obj, preferredType);
    */

    intrinsic function ToPrimitive(value, preferredType)
        magic::toPrimitive(value, preferredType);

    /*
     * ES-262-3 9.9: ToObject.
     *
     * ES-262-4 draft: All values except undefined and null are
     * already objects, no conversion is necessary.
    */

    /*
    intrinsic function ToObject(value) : Object! {
        if (value === undefined || value === null)
            throw new TypeError("Can't convert undefined or null to Object");
        return new Object(value);
    }
    */
}
