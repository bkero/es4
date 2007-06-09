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

/* Math is a singleton object, not a class, but has the type "Math"
 * which suggests a private Math class that created the singleton.  In
 * this implementation there is a package MathInternals which exports
 * the Math class, but MathInternals will not be available to user
 * programs, so user programs can't create new instances of Math.
 */

package MathInternals
{
    use namespace intrinsic;
    use strict;

    public dynamic final class Math
    {
        /* E262-3 15.8.2.1 */
        intrinsic static function abs(x:Numeric = NaN):Numeric {
            if (isNaN(x))
                return x;
            if (x === 0.0)
                return 0.0;  /* Handles -0 => 0 */
            if (x < 0)
                return -x;
            return x;
        }

        intrinsic static function max(...args):Numeric
	{
	    let x = intrinsic::double.NEGATIVE_INFINITY;
	    for (let i : uint = 0; i < args.length; ++i) {
                let v = Number(args[i]);  /* FIXME: is this conversion right? */
                if (isNaN(v))
                    return v;
		if (!(v < x))  /* Handles -0 < +0 */
		    x = v;
	    }
	    return x;
	}

        intrinsic static function min(...args):Numeric
	{
	    let x = intrinsic::double.POSITIVE_INFINITY;
	    for (let i : uint = 0; i < args.length; ++i) {
                let v = Number(args[i]);  /* FIXME: is this conversion right? */
                if (isNaN(v))
                    return v;
		if (!(v > x))  /* Handles -0 < +0 */
		    x = v;
	    }
	    return x;
	}

        /* FIXME: These signatures are dodgy.  
         * 
         * Consider Math.floor(1.1m), which we might like to work.  On
         * the other hand do we really care about calling Math.cos on
         * decimal numbers?
         */
	intrinsic static native function acos(x):Number;
        intrinsic static native function asin(x):Number;
        intrinsic static native function atan(x):Number;
        intrinsic static native function atan2(x,y):Number;
        intrinsic static native function ceil(x):Number;
        intrinsic static native function cos(x):Number;
        intrinsic static native function exp(x):Number;
        intrinsic static native function floor(x):Number;
        intrinsic static native function log(x):Number;
        intrinsic static native function pow(x,y):Number;
        intrinsic static native function random():Number;
        intrinsic static native function round(x):Number;
        intrinsic static native function sin(x):Number;
        intrinsic static native function sqrt(x):Number;
        intrinsic static native function tan(x):Number;

        // 15.8.1 Value Properties of the Math Object.
        public const E = 2.7182818284590452354;   /* Approximately */
        public const LN10 = 2.302585092994046;    /* Approximately */
        public const LN2 = 0.6931471805599453;    /* Approximately */
        public const LOG2E = 1.4426950408889634;  /* Approximately */
        public const LOG10E = 0.4342944819032518; /* Approximately */
        public const PI = 3.1415926535897932;     /* Approximately */
        public const SQRT1_2 = 0.7071067811865476;/* Approximately */
        public const SQRT2 = 1.4142135623730951;  /* Approximately */

        // 15.8.2 Function Properties of the Math Object
        public var abs = intrinsic::abs;
        public var acos = intrinsic::acos;
        public var asin = intrinsic::asin;
        public var atan = intrinsic::atan;
        public var atan2 = intrinsic::atan2;
        public var ceil = intrinsic::ceil;
        public var cos = intrinsic::cos;
        public var exp = intrinsic::exp;
        public var floor = intrinsic::floor;
        public var log = intrinsic::log;
        public var max = intrinsic::max;
        public var min = intrinsic::min;
        public var pow = intrinsic::pow;
        public var random = intrinsic::random;
        public var round = intrinsic::round;
        public var sin = intrinsic::sin;
        public var sqrt = intrinsic::sqrt;
        public var tan = intrinsic::tan;

        // E262 specifies that length=2 for these.
        // max.length = 2;  /* FIXME: this will cons a function, set its length and throw it away */
        // min.length = 2;

    }
}

package 
{
    import MathInternals.*;

    /* FIXME #82: some imported types can't be used as annotations */
    intrinsic const Math /* : Math.MathInternals */ = new MathInternals.Math();

    public var Math = new MathInternals.Math;
}
