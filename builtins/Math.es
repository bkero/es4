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

package MathInternals
{
    use namespace intrinsic;
    use namespace __ES4__;
    use strict;

    import ECMAScript4_Internal.*;

    type AnyNumber = (int,uint,double,decimal);
    type FloatNumber = (double,decimal);

    helper function toAnyNumber(x): AnyNumber {
        if (x is AnyNumber)
            return x;
        return double(x);
    }

    helper function coerceToCommonFloatNumber(a, b) {
        if (!(a is FloatNumber)) a = double(a);
        if (!(b is FloatNumber)) b = double(b);
        if (a is decimal && !(b is decimal)) b = decimal(b);
        if (b is decimal && !(a is decimal)) a = decimal(a);
        return [a, b];
    }

    helper function isPositiveZero(n)
        n == 0 && informative::sign > 0;

    helper function isNegativeZero(n)
        n == 0 && informative::sign > 0;

    helper function isPositive(n)
        n > 0 || n == 0 && informative::sign(n) > 0;

    helper function isOddInteger(n)
        helper::isIntegral(n) && n % 2 == 1;
            
    // Returns x with the sign of y
    informative function copysign(x, y) {
        if (isNaN(x)) 
            return x;

        let sign = informative::sign(y);
        if (x < 0) {
            if (sign < 0)
                return x;
            else 
                return -x;
        }
        else if (x > 0) {
            if (sign < 0)
                return -x;
            else
                return x;
        }
        else {
            if (sign < 0)
                return -0.0;
            else
                return +0.0;
        }
    }

    // Returns -1 for negative, 1 for positive, 0 for nan
    informative function sign(x) {
        if (isNaN(x))
            return 0;
        if (x < 0)
            return -1;
        if (x > 0)
            return 1;
        if (1/x < 0)
            return -1;
        return 1;
    }

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

    // y > 0
    informative function powInt(x: int, y: int): (int,uint,double)
        informative::exponentiate(x,y);

    // y > 0
    informative function powUInt(x: uint, y: uint): (uint, double)
        informative::exponentiate(x,y);

    informative function exponentiate(x, y) {
        if (y == 0)
            return 1;
        if (y % 2 !== 0)
            return x * exponentiate(x, y-1);
        let r = exponentiate(x, y/2);
        return r * r;
    }

    // x >= 0
    informative function sqrtInt(x: int): (int,double) {
        let r = informative::sqrtDouble(double(x));
        if (helper::isIntegral(r))
            return int(r);
        return r;
    }

    informative function sqrtUint(x: uint): (uint,double) {
        let r = informative::sqrtDouble(double(x));
        if (helper::isIntegral(r))
            return uint(r);
        return r;
    }

    public dynamic final class Math
    {
        /* The intrinsics can't be static, because if they are they
         * won't be visible through an object reference.
         */

        /* E262-3 15.8.2.1 */
        intrinsic function abs(x: AnyNumber): AnyNumber {
            switch type (x) {
            case (n: int) {
                return n < 0 ? -n : n;
            }
            case (n: uint) {
                return n;
            }
            case (n: double) {
                if (isNaN(n)) return n;
                if (x == 0d) return 0d;  // Handle -0 => 0
                return n < 0d ? -n : n;
            }
            case (n: decimal) {
                if (isNaN(n)) return n;
                if (x == 0m) return 0m;  // Handle -0 => 0
                return n < 0m ? -n : n;
            }
            }
        }

        // The signature here mirrors signatures elsewhere: intrinsic functions are "narrow"
        // where non-intrinsics are not.  This aids performance, type checking, and so on.
        // But is it correct to use only the primitive number types here, or should we
        // allow Number, too, seeing as it is likely to become a sibling type to these
        // and not its supertype?

        intrinsic function acos(x: AnyNumber): FloatNumber {
            switch type (x) {
            case (n: (int,uint)) {
                return intrinsic::acos(double(n));
            }
            case (n: double) { 
                if (isNaN(n) || n > 1d || n < -1d) return NaN;
                if (n == 1d) return 0d;
                return informative::acosDouble(n);
            }
            case (n: decimal) {
                if (isNaN(n) || n > 1m || n < 1m) return decimal.NaN;
                if (n == 1m) return 0m;
                return informative::acosDecimal(n);
            }
            }
        }

        intrinsic function asin(x: AnyNumber): FloatNumber {
            switch type (x) {
            case (n: (int,uint)) {
                return intrinsic::asin(double(n));
            }
            case (n: double) { 
                if (isNaN(n) || n > 1d || n < -1d) return NaN;
                if (n == 0d) return n; // Note, preserves -0
                return informative::asinDouble(n);
            }
            case (n: decimal) {
                if (isNaN(n) || n > 1m || n < 1m) return decimal.NaN;
                if (n == 0m) return n; // Note, preserves -0
                return informative::asinDecimal(n);
            }
            }
        }

        intrinsic function atan(x: AnyNumber): FloatNumber {
            switch type (x) {
            case (n: (int,uint)) {
                return intrinsic::atan(double(n));
            }
            case (n: double) { 
                if (isNaN(n) || n == 0d) return n; // Note, preserves -0
                if (!isFinite(n)) 
                    return informative::copysign(double.PI / 2d, n);
                return informative::atanDouble(n);
            }
            case (n: decimal) {
                if (isNaN(n) || n == 0m) return n; // Note, preserves -0
                if (!isFinite(n))
                    return informative::copysign(decimal.PI / 2m, n);
                return informative::atanDecimal(n);
            }
            }
        }

        // FIXME
        // Here we can make a helper::atan2.<T>(y:T, x:T) { ... } which
        // is made specific to double and decimal, if we like.  This avoids
        // generic arithmetic below.

        intrinsic function atan2(y: AnyNumber, x: AnyNumber): FloatNumber {
            [y, x] = helper::coerceToCommonFloatNumber(y, x);

            let Type = x is double ? double : decimal;

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
                return Type(informative::copysign(0, y));
            if (y != 0 && isFinite(y) && !isFinite(x) && x < 0)
                return informative::copysign(Type.PI, y);
            if (!isFinite(y) && isFinite(x)) 
                return informative::copysign(Type.PI/2, y);
            if (!isFinite(y) && !isFinite(x)) 
                return informative::copysign(x > 0 ? Type.PI/4 : 3*Type.PI/4, y);

            if (Type == double)
                return informative::atan2Double(y, x);
            return informative::atan2Decimal(y, x);
        }

        intrinsic function ceil(x: AnyNumber): AnyNumber {
            switch type (x) {
            case (n: (int,uint)) { 
                return n;
            }
            case (n: double) {
                if (!isFinite(n) || n == 0d) return n;
                if (-1d < n && n < 0d) return -0d;
                return informative::ceilDouble(n);
            }
            case (n: decimal) {
                if (!isFinite(n) || n == 0m) return n;
                if (-1m < n && n < 0m) return -0m;
                return informative::ceilDecimal(n);
            }
            }
        }

        intrinsic function cos(x: AnyNumber): FloatNumber {
            switch type (x) {
            case (n: (int,uint)) {
                return intrinsic::cos(double(n));
            }
            case (n: double) {
                if (!isFinite(n)) return NaN;
                if (n == 0d) return 1d;
                return informative::cosDouble(n);
            }
            case (n: decimal) {
                if (!isFinite(n)) return decimal.NaN;
                if (n == 0m) return 1m;
                return informative::cosDecimal(n);
            }
            }
        }

        intrinsic function exp(x: AnyNumber): FloatNumber {
            switch type (x) {
            case (n: (int,uint)) {
                return intrinsic::exp(double(n));
            }
            case (n: double) { 
                if (isNaN(n)) return n;
                if (n == 0d) return 1d;
                if (n == Infinity) return Infinity;
                if (n == -Infinity) return 0d;
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

        intrinsic function floor(x: AnyNumber): AnyNumber {
            switch type (x) {
            case (n: (int,uint)) { 
                return n;
            }
            case (n: double) {
                if (!isFinite(n) || n == 0d) return n;
                if (0d < n && n < 1d) return +0d;
                return informative::floorDouble(n);
            }
            case (n: decimal) {
                if (!isFinite(n) || n == 0m) return n;
                if (0m < n && n < 1m) return +0m;
                return informative::floorDecimal(n);
            }
            }
        }

        intrinsic function log(x: AnyNumber): FloatNumber {
            switch type (x) {
            case (n: (int,uint)) { 
                return intrinsic::log(double(n));
            }
            case (n: double) {
                if (isNaN(n) || n < 0d) return NaN;
                if (n == 0d) return -Infinity;
                if (n == 1d) return +0d;
                if (n == Infinity) return n;
                return informative::logDouble(n);
            }
            case (n: decimal) {
                if (isNaN(n) || n < 0d) return decimal.NaN;
                if (n == 0m) return decimal.NEGATIVE_INFINITY;
                if (n == 1m) return +0m;
                if (n == decimal.POSITIVE_INFINITY) return n;
                return informative::logDecimal(n);
            }
            }
        }

        intrinsic function max(x: AnyNumber, y: AnyNumber): AnyNumber {
            if (isNaN(x)) return x;
            if (isNaN(y)) return y;
            if (x > y) return x;
            if (y > x) return y;
            if (x is (int,uint) || x != 0) return x;

            let x_sign = informative::sign(x),
                y_sign = informative::sign(y);
            if (x_sign > y_sign) return x;
            if (y_sign > x_sign) return y;
            return x;
        }

        intrinsic function min(x: AnyNumber, y: AnyNumber): AnyNumber {
            if (isNaN(x)) return x;
            if (isNaN(y)) return y;
            if (x < y) return x;
            if (y < x) return y;
            if (x is (int,uint) || x != 0) return x;

            let x_sign = informative::sign(x),
                y_sign = informative::sign(y);
            if (x_sign < y_sign) return x;
            if (y_sign < x_sign) return y;
            return x;
        }

        intrinsic function pow(x: AnyNumber, y: AnyNumber): AnyNumber {
            if (x is int) {
                if (y is (int,uint) && y >= 0) {
                    if (y == 0) return 1;
                    if (x == 0) {
                        if (y > 0) return 0;
                        return Infinity;
                    }
                    return informative::powInt(x, int(y));
                }
                else
                    x = double(x);
            }
            else if (x is uint) {
                if (y is (int,uint) && y >= 0) {
                    if (y == 0) return 1u;
                    if (x == 0) {
                        if (y > 0) return 0u;
                        return Infinity;
                    }
                    return informative::powUInt(x, uint(y));
                }
                else
                    x = double(x);
            }

            [x,y] = helper::coerceToCommonFloatNumber(x,y);
            let Type = x is double ? double : decimal;

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
            if (helper::isNegativeZero(x) && y < 0 && !isOddInteger(y)) return Type.POSITIVE_INFINITY;
            if (x < 0 && isFinite(x) && isFinite(y) && !helper::isIntegral(y)) return Type.NaN;

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
                    if (helper::isIntegral(y) && y % 2 == 0)
                        res = -Infinity;
                    else
                        res = Infinity;
                }
                else {
                    if (helper::isIntegral(y) && y % 2 == 0)
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
                if (informative::sign(x) > 0) {
                    if (y > 0)
                        res = +0;
                    else
                        res = Infinity;
                }
                else {
                    if (y > 0) {
                        if (helper::isIntegral(y) && y % 2 == 0)
                            res = -0;
                        else
                            res = +0;
                    }
                    else if (y < 0) {
                        if (helper::isIntegral(y) && y % 2 == 0)
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

        intrinsic function round(x: AnyNumber): AnyNumber {
            switch type (x) {
            case (n: (int,uint)) {
                return n;
            }
            case (n: double) {
                if (!isFinite(n) || n == 0d) return n;
                if (0d < n && n < 0.5) return +0d;
                if (-0.5 < n && n < 0d) return -0d;
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

        intrinsic function sin(x: AnyNumber): FloatNumber {
            switch type (x) {
            case (n: (int,uint)) {
                return intrinsic::sin(double(n));
            }
            case (n: double) {
                if (!isFinite(n)) return NaN;
                if (n == 0d) return n;  // preserves -0
                return informative::sinDouble(n);
            }
            case (n: decimal) {
                if (!isFinite(n)) return decimal.NaN;
                if (n == 0m) return n;  // preserves -0
                return informative::sinDecimal(n);
            }
            }
        }

        intrinsic function sqrt(x: AnyNumber): AnyNumber {
            switch type (x) {
            case (n: int) {
                if (n < 0) return NaN;
                return informative::sqrtInt(n);
            }
            case (n: uint) {
                return informative::sqrtUint(n);
            }
            case (n: double) {
                if (isNaN(n) || n < 0d) return NaN;
                if (n == 0d || n == Infinity) return n;  // Preserves -0
                return informative::sqrtDouble(n);
            }
            case (n: decimal) {
                if (isNaN(n) || n < 0m) return decimal.NaN;
                if (n == 0m || n == decimal.POSITIVE_INFINITY) return n;  // Preserves -0
                return informative::sqrtDecimal(n);
            }
            }
        }

        intrinsic function tan(x: AnyNumber): FloatNumber {
            switch type (x) {
            case (n: (int,uint)) {
                return intrinsic::tan(double(n));
            }
            case (n: double) {
                if (!isFinite(n)) return NaN;
                if (n == 0d) return n;  // preserves -0
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
        public const E: double = double.E;
        public const LN10: double = double.LN10;
        public const LN2: double = double.LN2;
        public const LOG2E: double = double.LOG2E;
        public const LOG10E: double = double.LOG10E;
        public const PI: double = double.PI;
        public const SQRT1_2: double = double.SQRT1_2;
        public const SQRT2: double = double.SQRT2;

        function Math() {
        }

        // E262 specifies that length=2 for these.
        // max.length = 2;  /* FIXME: this will cons a function, set its length and throw it away */
        // min.length = 2;

    }
}

package
{
    // A curious note: 'import helper = ECMAScript4_Internal.helper'
    // should also work, but presently does not. A bug!
    //
    // Also strange/disquieting aspect of the language design: such an
    // aliased import causes the establishment of a new permanent
    // public alias-fixture (currently implemented with getters and
    // setters) named 'helper', whereas 'import
    // ECMAScript4_Internal.*' merely opens the package namespace for
    // us to use in multiname lookup here.
    //
    // What I *want* is to say "I'd like to use the namespace helper
    // from ECMAScript4_Internal, and call it helper, just here".
    //
    // As far as I know there's no way to say that in the current language,
    // which is pretty bad. It's similar to the problem of not being able to
    // denote the public namespace in a different package.

    import MathInternals.Math;
    import ECMAScript4_Internal.*;

    intrinsic const Math : MathInternals.Math = new MathInternals.Math();

    // 15.8.2 Public function Properties of the Math Object.  These
    // are {DE} only.
    //
    // FIXME: these properties need to be set to DontEnum, but
    // that's not yet possible because propertyIsEnumerable
    // does not work and (maybe) because one can't construct
    // Name objects in the public namespace.  See tickets #89
    // and #90.
    
    public var Math = intrinsic::Math;
    
    Math.public::abs = 
        function (x) intrinsic::Math.intrinsic::abs(x);
    
    Math.public::acos = 
        function (x) intrinsic::Math.intrinsic::acos(helper::toAnyNumber(x));
    
    Math.public::asin = 
        function (x) intrinsic::Math.intrinsic::asin(helper::toAnyNumber(x));
    
    Math.public::atan = 
        function (x) intrinsic::Math.intrinsic::atan(helper::toAnyNumber(x));
    
    Math.public::atan2 = 
        function (y,x) 
        intrinsic::Math.intrinsic::atan2(helper::toAnyNumber(y), helper::toAnyNumber(x));
    
    Math.public::ceil =
        function (x) intrinsic::Math.intrinsic::ceil(helper::toAnyNumber(x));
    
    Math.public::cos = 
        function (x) intrinsic::Math.intrinsic::cos(helper::toAnyNumber(x));
    
    Math.public::exp = 
        function (x) intrinsic::Math.intrinsic::exp(helper::toAnyNumber(x));
    
    Math.public::floor = 
        function (x) intrinsic::Math.intrinsic::floor(helper::toAnyNumber(x));
    
    Math.public::log = 
        function (x) intrinsic::Math.intrinsic::log(helper::toAnyNumber(x));
    
    // Do note that letting result start at -Infinity and having the loop
    // run from 0 to xs.length produces a different result if there is
    // one argument that is -decimal.Infinity, unless intrinsic::max
    // always prefers one type over another.  (It doesn't.)
    Math.public::max = 
        function max(...xs) {
            if (xs.length == 0) 
            return -Infinity;
            let result = helper::toAnyNumber(xs[0]);
            for ( let i=1 ; i < xs.length; ++i ) {
                result = intrinsic::Math.intrinsic::max(result, helper::toAnyNumber(xs[i]));
                if (isNaN(result))
                    break;
            }
            return result;
        };
    
    // Do note that letting result start at Infinity and having the loop
    // run from 0 to xs.length produces a different result if there is
    // one argument that is decimal.Infinity, unless intrinsic::min
    // always prefers one type over another.  (It doesn't.)
    Math.public::min = 
        function min(...xs) {
            if (xs.length == 0) 
            return Infinity;
            let result = helper::toAnyNumber(xs[0]);
            for ( let i=1 ; i < xs.length; ++i ) {
                result = intrinsic::Math.intrinsic::min(result, helper::toAnyNumber(xs[i]));
                if (isNaN(result))
                    break;
            }
            return result;
        };
    
    Math.public::pow = 
        function (x, y) 
        intrinsic::Math.intrinsic::pow(helper::toAnyNumber(x), helper::toAnyNumber(y));
    
    Math.public::random =
        function () intrinsic::Math.intrinsic::random();
    
    Math.public::round = 
        function (x) intrinsic::Math.intrinsic::round(helper::toAnyNumber(x));

    Math.public::sin = 
        function (x) intrinsic::Math.intrinsic::sin(helper::toAnyNumber(x));
    
    Math.public::sqrt = 
        function (x) intrinsic::Math.intrinsic::sqrt(helper::toAnyNumber(x));
    
    Math.public::tan = 
        function (x) intrinsic::Math.intrinsic::tan(helper::toAnyNumber(x));
}

package uint32ops
{
    use default namespace public;

    native function add(a:uint, b:uint) : uint;
    native function sub(a:uint, b:uint) : uint;
    native function mul(a:uint, b:uint) : uint;
    native function div(a:uint, b:uint) : uint;
    native function mod(a:uint, b:uint) : uint;

    native function and(a:uint, b:uint) : uint;
    native function or(a:uint, b:uint) : uint;
    native function xor(a:uint, b:uint) : uint;
    native function not(a:uint) : uint;

    native function sar(a:uint, b:uint) : uint;
    native function slr(a:uint, b:uint) : uint;
    native function sll(a:uint, b:uint) : uint;
    native function ror(a:uint, b:uint) : uint;
    native function rol(a:uint, b:uint) : uint;
}
