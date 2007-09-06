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
    use strict;

    intrinsic function IsPrimitive(value)
        magic::isPrimitive(value);

    intrinsic function DefaultValue(obj, preferredType)
        magic::defaultValue(obj, preferredType);

    intrinsic function ToPrimitive(value, preferredType)
        magic::toPrimitive(value, preferredType);

    /* ES-262-3 9.4: The ToInteger operation */
    intrinsic function ToInteger(value) : Numeric {
        value = ToDouble(value);
        if (value !== value)
            return 0;
        if (value === 0 || !isFinite(value))
            return value;
        var sign:double = value < 0d ? -1d : 1d;
        return sign * Math.floor(Math.abs(value));
    }

    /*
     * ES-262-3 9.9: ToObject.
     *
     * ES-262-4 draft: All values except undefined and null are
     * already objects, no conversion is necessary.
    */

    intrinsic function ToObject(value) : Object! {
        if (value === undefined || value === null)
            throw new TypeError("Can't convert undefined or null to Object");
        return new Object(value);
    }

    /*
     * The remaining ES-262-3 9.x primitive conversions are formulated
     * in terms of calling the class meta::invoke of the associated
     * primitive, which generally calls the primitive constructor and
     * thus one of the native magic::bindFoo() primitive conversion
     * routines provided by the implementation.
     *
     * It is done this way because expressing the conversion
     * algorithms in ES4 code proved difficult: the code invariably
     * fed back on primitive constructors (for literals, control-flow
     * booleans, temporaries, etc). This meant that the conversions
     * could seldom be called from primitive constructors without
     * entering infinite loops. This was unacceptable since the
     * conversion algorithms are primarily *intended* to be called
     * from the bodies of primitive constructors.
     */

    intrinsic function ToBoolean(value) : boolean
        boolean(value);

    intrinsic function ToInt(v) : int
        int(value)

    intrinsic function ToUint(x) : uint
        uint(x);

    intrinsic function ToDouble(x) : double
        double(x);

    intrinsic function ToDecimal(x) : decimal
        decimal(x);

    intrinsic function ToString(x) : string
        string(x);

    intrinsic function ToNumeric(x) : Numeric {
        if (x is Numeric)
            return x;
        return ToDouble(x);
    }
}
