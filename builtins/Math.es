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

/* See ticket #8 for everything about the Math object.
 *
 * Summary: Math is a singleton instance of a hidden class called
 * "Math".  Early binding and inlining opportunities are provided for
 * by a strongly-typed global intrinsic const variable also called
 * "Math", and for intrinsic methods on objects of this type.
 */

/* NOTE!  *Do not* qualify any names by intrinsic:: explicitly,
 * intrinsic is always assumed for all predefined names.  But *do*
 * qualify public names by public:: explicitly.
 */

use namespace intrinsic;

helper type PrimitiveNumber = (double|decimal);

helper function toPrimitiveNumber(x): helper::PrimitiveNumber {
    if (x is helper::PrimitiveNumber)
        return x;
    return double(x);
}

helper function isPositiveZero(n)
    n == 0 && sign(n) > 0;

helper function isNegativeZero(n)
    n == 0 && sign(n) < 0;

helper function isPositive(n)
    n > 0 || helper::isPositiveZero(n);

helper function isOddInteger(n)
    isIntegral(n) && n % 2 == 1;
            
informative native function acosDouble(x: double): double;
informative native function acosDecimal(x: decimal): decimal;
informative native function asinDouble(x: double): double;
informative native function asinDecimal(x: decimal): decimal;
informative native function atanDouble(x: double): double;
informative native function atanDecimal(x: decimal): decimal;
informative native function atan2Double(y: double, x: double): double;
informative native function atan2Decimal(y: decimal, x: decimal): decimal;
informative native function ceilDouble(x: double): double;
informative native function ceilDecimal(x: decimal): decimal;
informative native function cosDouble(x: double): double;
informative native function cosDecimal(x: decimal): decimal;
informative native function expDouble(x: double): double;
informative native function expDecimal(x: decimal): decimal;
informative native function floorDouble(x: double): double;
informative native function floorDecimal(x: decimal): decimal;
informative native function logDouble(x: double): double;
informative native function logDecimal(x: decimal): decimal;
informative native function powDouble(x: double, y: double): double;
informative native function powDecimal(x: decimal, y: decimal): decimal;
informative native function roundDouble(x: double):double;
informative native function roundDecimal(x: decimal):decimal;
informative native function sinDouble(x: double):double;
informative native function sinDecimal(x: decimal):decimal;
informative native function sqrtDouble(x: double): double;
informative native function sqrtDecimal(x: decimal): decimal;
informative native function tanDouble(x: double):double;
informative native function tanDecimal(x: decimal):decimal;

helper dynamic final class Math
{
    /* The intrinsics can't be static, because if they are they
     * won't be visible through an object reference.
     */

    /* E262-3 15.8.2.1 */
    intrinsic function abs(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) {
            if (isNaN(n)) return n;
            if (x == 0) return 0;    // Handle -0 => 0
            return n < 0 ? -n : n;
        }
        case (n: decimal) {
            if (isNaN(n)) return n;
            if (x == 0m) return 0m;  // Handle -0 => 0
            return n < 0m ? -n : n;
        }
        }
    }

    // The signature here mirrors signatures elsewhere: doublerinsic functions are "narrow"
    // where non-intrinsics are not.  This aids performance, type checking, and so on.
    // But is it correct to use only the primitive number types here, or should we
    // allow Number, too, seeing as it is likely to become a sibling type to these
    // and not its supertype?

    intrinsic function acos(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) { 
            if (isNaN(n) || n > 1 || n < -1) return NaN;
            if (n == 1) return 0;
            return informative::acosDouble(n);
        }
        case (n: decimal) {
            if (isNaN(n) || n > 1m || n < 1m) return decimal.NaN;
            if (n == 1m) return 0m;
            return informative::acosDecimal(n);
        }
        }
    }

    intrinsic function asin(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) { 
            if (isNaN(n) || n > 1 || n < -1) return NaN;
            if (n == 0) return n; // Note, preserves -0
            return informative::asinDouble(n);
        }
        case (n: decimal) {
            if (isNaN(n) || n > 1m || n < 1m) return decimal.NaN;
            if (n == 0m) return n; // Note, preserves -0
            return informative::asinDecimal(n);
        }
        }
    }

    intrinsic function atan(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) { 
            if (isNaN(n) || n == 0) return n; // Note, preserves -0
            if (!isFinite(n)) 
                return copysign(double.PI / 2, n);
            return informative::atanDouble(n);
        }
        case (n: decimal) {
            if (isNaN(n) || n == 0m) return n; // Note, preserves -0
            if (!isFinite(n))
                return copysign(decimal.PI / 2m, n);
            return informative::atanDecimal(n);
        }
        }
    }

    // FIXME
    // Here we can make a helper::atan2.<T>(y:T, x:T) { ... } which
    // is made specific to double and decimal, if we like.  This avoids
    // generic arithmetic below.

    intrinsic function atan2(y: helper::PrimitiveNumber, x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        if (y is decimal && !(x is decimal)) 
            x = decimal(x);
        else if (x is decimal && !(y is decimal)) 
            y = decimal(y);

        let Type = (x is double) ? double : decimal;

        if (isNaN(x) || isNaN(y)) 
            return Type.NaN;
        if (y > 0 && x == 0) 
            return Type.PI/2;
        if (helper::isPositiveZero(y)) 
            return helper::isPositive(x) ? Type(+0) : Type.PI;
        if (helper::isNegativeZero(y))
            return helper::isPositive(x) ? Type(-0) : -Type.PI;
        if (y < 0 && x == 0) 
            return -Type.PI/2;
        if (y != 0 && isFinite(y) && !isFinite(x) && x > 0)
            return Type(copysign(0, y));
        if (y != 0 && isFinite(y) && !isFinite(x) && x < 0)
            return copysign(Type.PI, y);
        if (!isFinite(y) && isFinite(x)) 
            return copysign(Type.PI/2, y);
        if (!isFinite(y) && !isFinite(x)) 
            return copysign(x > 0 ? Type.PI/4 : 3*Type.PI/4, y);

        if (Type == double)
            return informative::atan2Double(y, x);
        return informative::atan2Decimal(y, x);
    }

    intrinsic function ceil(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) {
            if (!isFinite(n) || n == 0) return n;
            if (-1 < n && n < 0) return -0;
            return informative::ceilDouble(n);
        }
        case (n: decimal) {
            if (!isFinite(n) || n == 0m) return n;
            if (-1m < n && n < 0m) return -0m;
            return informative::ceilDecimal(n);
        }
        }
    }

    intrinsic function cos(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) {
            if (!isFinite(n)) return NaN;
            if (n == 0) return 1;
            return informative::cosDouble(n);
        }
        case (n: decimal) {
            if (!isFinite(n)) return decimal.NaN;
            if (n == 0m) return 1m;
            return informative::cosDecimal(n);
        }
        }
    }

    intrinsic function exp(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) { 
            if (isNaN(n)) return n;
            if (n == 0) return 1d;
            if (n == Infinity) return Infinity;
            if (n == -Infinity) return 0;
            return informative::expDouble(n);
        }
        case (n: decimal) {
            if (isNaN(n)) return n;
            if (n == 0m) return 1m;
            if (n == decimal.POSITIVE_INFINITY) return decimal.POSITIVE_INFINITY;
            if (n == decimal.NEGATIVE_INFINITY) return 0m;
            return informative::expDecimal(n);
        }
        }
    }

    intrinsic function floor(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) {
            if (!isFinite(n) || n == 0) return n;
            if (0 < n && n < 1) return +0;
            return informative::floorDouble(n);
        }
        case (n: decimal) {
            if (!isFinite(n) || n == 0m) return n;
            if (0m < n && n < 1m) return +0m;
            return informative::floorDecimal(n);
        }
        }
    }

    intrinsic function log(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) {
            if (isNaN(n) || n < 0) return NaN;
            if (n == 0) return -Infinity;
            if (n == 1) return +0;
            if (n == Infinity) return n;
            return informative::logDouble(n);
        }
        case (n: decimal) {
            if (isNaN(n) || n < 0m) return decimal.NaN;
            if (n == 0m) return decimal.NEGATIVE_INFINITY;
            if (n == 1m) return +0m;
            if (n == decimal.POSITIVE_INFINITY) return n;
            return informative::logDecimal(n);
        }
        }
    }

    intrinsic function max(x: helper::PrimitiveNumber, y: helper::PrimitiveNumber): helper::PrimitiveNumber {
        if (isNaN(x)) return x;
        if (isNaN(y)) return y;
        if (x > y) return x;
        if (y > x) return y;
        if (x != 0) return x;

        let x_sign = sign(x),
            y_sign = sign(y);
        if (x_sign > y_sign) return x;
        if (y_sign > x_sign) return y;
        return x;
    }

    intrinsic function min(x: helper::PrimitiveNumber, y: helper::PrimitiveNumber): helper::PrimitiveNumber {
        if (isNaN(x)) return x;
        if (isNaN(y)) return y;
        if (x < y) return x;
        if (y < x) return y;
        if (x != 0) return x;

        let x_sign = sign(x),
            y_sign = sign(y);
        if (x_sign < y_sign) return x;
        if (y_sign < x_sign) return y;
        return x;
    }

    intrinsic function pow(x: helper::PrimitiveNumber, y: helper::PrimitiveNumber): helper::PrimitiveNumber {
        if (x is decimal && !(y is decimal)) 
            y = decimal(y);
        else if (y is decimal && !(x is decimal)) 
            x = decimal(x);

        let Type = (x is double) ? double : decimal;

        if (isNaN(y)) return Type.NaN;
        if (y == 0) return Type(1);
        if (isNaN(x) && y != 0) return Type.NaN;
        if (abs(x) > 1 && y == Infinity) return Type.POSITIVE_INFINITY;
        if (abs(x) > 1 && y == -Infinity) return Type(+0);
        if (abs(x) == 1 && y == Infinity) return Type.NaN;
        if (abs(x) == 1 && y == -Infinity) return Type.NaN;
        if (abs(x) < 1 && y == Infinity) return Type(+0);
        if (abs(x) < 1 && y == -Infinity) return Type.POSITIVE_INFINITY;
        if (x == Infinity && y > 0) return Type.POSITIVE_INFINITY;
        if (x == Infinity && y < 0) return Type(+0);
        if (x == -Infinity && y > 0 && helper::isOddInteger(y)) return Type.NEGATIVE_INFINITY;
        if (x == -Infinity && y > 0 && !helper::isOddInteger(y)) return Type.POSITIVE_INFINITY;
        if (x == -Infinity && y < 0 && helper::isOddInteger(y)) return Type(-0);
        if (x == -Infinity && y < 0 && !helper::isOddInteger(y)) return Type(+0);
        if (x == 0 && y > 0) return Type(+0);
        if (x == 0 && y < 0) return Type.POSITIVE_INFINITY;
        if (helper::isNegativeZero(x) && y > 0 && helper::isOddInteger(y)) return Type(-0);
        if (helper::isNegativeZero(x) && y > 0 && !helper::isOddInteger(y)) return Type(+0);
        if (helper::isNegativeZero(x) && y < 0 && helper::isOddInteger(y)) return Type.NEGATIVE_INFINITY;
        if (helper::isNegativeZero(x) && y < 0 && !helper::isOddInteger(y)) return Type.POSITIVE_INFINITY;
        if (x < 0 && isFinite(x) && isFinite(y) && !isIntegral(y)) return Type.NaN;

        if (Type == double)
            return informative::powDouble(x, y);
        return informative::powDecimal(x, y);
    }

    /* Alternative, more efficient code for the decision tree inside powFloating.
       I'm not using it because it's substantially more verbose than what we have,
       which was transcribed directly from ES3, but it might actually be clearer.

        if (isNaN(y))
            res = NaN;
        else if (y == 0)
            res = 1;
        else if (isNaN(x))
            res = NaN;
        else if (x == Infinity) {
            if (y > 0)
                res = Infinity;
            else if (y < 0)
                res = +0;
        }
        else if (x == -Infinity) {
            if (y > 0) {
                if (isIntegral(y) && y % 2 == 0)
                    res = -Infinity;
                else
                    res = Infinity;
            }
            else {
                if (isIntegral(y) && y % 2 == 0)
                    res = -0;
                else
                    res = +0;
            }
        }
        else if (y == Infinity) {
            let n = abs(x);
            if (n > 1)
                res = Infinity;
            else if (n == 1)
                res = NaN;
            else
                res = +0;
        }
        else if (y == -Infinity) {
            let n = abs(x);
            if (n > 1)
                res = +0;
            else if (n == 1)
                res = NaN;
            else
                res = Infinity;
        }
        else if (x == 0) {
            if (sign(x) > 0) {
                if (y > 0)
                    res = +0;
                else
                    res = Infinity;
            }
            else {
                if (y > 0) {
                    if (isIntegral(y) && y % 2 == 0)
                        res = -0;
                    else
                        res = +0;
                }
                else if (y < 0) {
                    if (isIntegral(y) && y % 2 == 0)
                        res = -Infinity;
                    else
                        res = Infinity;
                }
            }
        }
        else if (x < 0 && isFinite(x) && isFinite(y) && !isIntegral(y))
            res = NaN;

        if (!(res is undefined)) {
            if (x is double)
                return double(res);
            return decimal(res);
        }

    */

    intrinsic native function random(): double;

    intrinsic function round(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) {
            if (!isFinite(n) || n == 0) return n;
            if (0 < n && n < 0.5) return +0;
            if (-0.5 < n && n < 0) return -0;
            return informative::roundDouble(n);
        }
        case (n: decimal) {
            if (!isFinite(n) || n == 0m) return n;
            if (0m < n && n < 0.5m) return +0m;
            if (-0.5m < n && n < 0m) return -0m;
            return informative::roundDecimal(n);
        }
        }
    }

    intrinsic function sin(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) {
            if (!isFinite(n)) return NaN;
            if (n == 0) return n;  // preserves -0
            return informative::sinDouble(n);
        }
        case (n: decimal) {
            if (!isFinite(n)) return decimal.NaN;
            if (n == 0m) return n;  // preserves -0
            return informative::sinDecimal(n);
        }
        }
    }

    intrinsic function sqrt(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) {
            if (isNaN(n) || n < 0) return NaN;
            if (n == 0 || n == Infinity) return n;  // Preserves -0
            return informative::sqrtDouble(n);
        }
        case (n: decimal) {
            if (isNaN(n) || n < 0m) return decimal.NaN;
            if (n == 0m || n == decimal.POSITIVE_INFINITY) return n;  // Preserves -0
            return informative::sqrtDecimal(n);
        }
        }
    }

    intrinsic function tan(x: helper::PrimitiveNumber): helper::PrimitiveNumber {
        switch type (x) {
        case (n: double) {
            if (!isFinite(n)) return NaN;
            if (n == 0) return n;  // preserves -0
            return informative::tanDouble(n);
        }
        case (n: decimal) {
            if (!isFinite(n)) return decimal.NaN;
            if (n == 0m) return n;  // preserves -0
            return informative::tanDecimal(n);
        }
        }
    }

    // 15.8.1 Value Properties of the Math Object.  These are {DD,DE,RO}.
    const E: double = double.E;
    const LN10: double = double.LN10;
    const LN2: double = double.LN2;
    const LOG2E: double = double.LOG2E;
    const LOG10E: double = double.LOG10E;
    const PI: double = double.PI;
    const SQRT1_2: double = double.SQRT1_2;
    const SQRT2: double = double.SQRT2;
}

intrinsic const Math : helper::Math = new helper::Math();

// 15.8.2 Public function Properties of the Math Object.  These
// are {DE} only.
//
// FIXME: these properties need to be set to Enumerable, but
// that's not yet possible because propertyIsEnumerable
// does not work and (maybe) because one can't construct
// Name objects in the public namespace.  See tickets #89
// and #90.
//
// Update 2008-06-25: Clearly we should use
// Object.prototype.__defineProperty__ or whatever that turns into, to
// create these.  But that one also does not work at present.

Math.public::abs = 
    function (x) Math.abs(helper::toPrimitiveNumber(x));

Math.public::acos = 
    function (x) Math.acos(helper::toPrimitiveNumber(x));

Math.public::asin = 
    function (x) Math.asin(helper::toPrimitiveNumber(x));
    
Math.public::atan = 
    function (x) Math.atan(helper::toPrimitiveNumber(x));
    
Math.public::atan2 = 
    function (y,x) 
        Math.atan2(helper::toPrimitiveNumber(y), helper::toPrimitiveNumber(x));
    
Math.public::ceil =
    function (x) Math.ceil(helper::toPrimitiveNumber(x));
    
Math.public::cos = 
    function (x) Math.cos(helper::toPrimitiveNumber(x));
    
Math.public::exp = 
    function (x) Math.exp(helper::toPrimitiveNumber(x));
    
Math.public::floor = 
    function (x) Math.floor(helper::toPrimitiveNumber(x));
    
Math.public::log = 
    function (x) Math.log(helper::toPrimitiveNumber(x));
    
// Do note that letting result start at -Infinity and having the loop
// run from 0 to xs.length produces a different result if there is
// one argument that is -decimal.Infinity, unless max
// always prefers one type over another.  (It doesn't.)
Math.public::max = 
    function max(...xs) {
        if (xs.length == 0) 
            return -Infinity;
        let result = helper::toPrimitiveNumber(xs[0]);
        for ( let i=1 ; i < xs.length; ++i ) {
            result = Math.max(result, helper::toPrimitiveNumber(xs[i]));
            if (isNaN(result))
                break;
        }
        return result;
    };
    
// Do note that letting result start at Infinity and having the loop
// run from 0 to xs.length produces a different result if there is
// one argument that is decimal.Infinity, unless min
// always prefers one type over another.  (It doesn't.)
Math.public::min = 
    function min(...xs) {
        if (xs.length == 0) 
            return Infinity;
        let result = helper::toPrimitiveNumber(xs[0]);
        for ( let i=1 ; i < xs.length; ++i ) {
            result = Math.min(result, helper::toPrimitiveNumber(xs[i]));
            if (isNaN(result))
                break;
        }
        return result;
    };
    
Math.public::pow = 
    function (x, y) 
        Math.pow(helper::toPrimitiveNumber(x), helper::toPrimitiveNumber(y));
    
Math.public::random =
    function () Math.random();
    
Math.public::round = 
    function (x) Math.round(helper::toPrimitiveNumber(x));

Math.public::sin = 
    function (x) Math.sin(helper::toPrimitiveNumber(x));
    
Math.public::sqrt = 
    function (x) Math.sqrt(helper::toPrimitiveNumber(x));
    
Math.public::tan = 
    function (x) Math.atan(helper::toPrimitiveNumber(x));

public var Math = Math;
