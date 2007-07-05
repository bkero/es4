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
    use strict;

    public dynamic final class Math
    {
        /* The intrinsics can't be static, because if they are they
         * won't be visible through an object reference.
         */

        /* E262-3 15.8.2.1 */
        intrinsic function abs(x:Numeric = NaN):Numeric {
            if (isNaN(x))
                return x;
            if (x === 0.0)
                return 0.0;  /* Handles -0 => 0 */
            if (x < 0)
                return -x;
            return x;
        }

        intrinsic function max(...args):Numeric
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

        intrinsic function min(...args):Numeric
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

        /* FIXME (ticket #83): These signatures are dodgy.
         *
         * Consider Math.floor(1.1m), which we might like to work.  On
         * the other hand do we really care about calling Math.cos on
         * decimal numbers?
         */
	intrinsic native function acos(x):Number;
        intrinsic native function asin(x):Number;
        intrinsic native function atan(x):Number;
        intrinsic native function atan2(x,y):Number;
        intrinsic native function ceil(x):Number;
        intrinsic native function cos(x):Number;
        intrinsic native function exp(x):Number;
        intrinsic native function floor(x):Number;
        intrinsic native function log(x):Number;
        intrinsic native function pow(x,y):Number;
        intrinsic native function random():Number;
        intrinsic native function round(x):Number;
        intrinsic native function sin(x):Number;
        intrinsic native function sqrt(x):Number;
        intrinsic native function tan(x):Number;

        // 15.8.1 Value Properties of the Math Object.  These are {DD,DE,RO}.
        public const E = 2.7182818284590452354;   /* Approximately */
        public const LN10 = 2.302585092994046;    /* Approximately */
        public const LN2 = 0.6931471805599453;    /* Approximately */
        public const LOG2E = 1.4426950408889634;  /* Approximately */
        public const LOG10E = 0.4342944819032518; /* Approximately */
        public const PI = 3.1415926535897932;     /* Approximately */
        public const SQRT1_2 = 0.7071067811865476;/* Approximately */
        public const SQRT2 = 1.4142135623730951;  /* Approximately */

        function Math() {
            // 15.8.2 Function Properties of the Math Object.  These
            // are {DE} only.
            //
            // FIXME: these properties need to be set to DontEnum, but
            // that's not yet possible because propertyIsEnumerable
            // does not work and (maybe) because one can't construct
            // Name objects in the public namespace.  See tickets #89
            // and #90.
            this.public::abs = intrinsic::abs;
            this.public::acos = intrinsic::acos;
            this.public::asin = intrinsic::asin;
            this.public::atan = intrinsic::atan;
            this.public::atan2 = intrinsic::atan2;
            this.public::ceil = intrinsic::ceil;
            this.public::cos = intrinsic::cos;
            this.public::exp = intrinsic::exp;
            this.public::floor = intrinsic::floor;
            this.public::log = intrinsic::log;
            this.public::max = intrinsic::max;
            this.public::min = intrinsic::min;
            this.public::pow = intrinsic::pow;
            this.public::random = intrinsic::random;
            this.public::round = intrinsic::round;
            this.public::sin = intrinsic::sin;
            this.public::sqrt = intrinsic::sqrt;
            this.public::tan = intrinsic::tan;
        }

        // E262 specifies that length=2 for these.
        // max.length = 2;  /* FIXME: this will cons a function, set its length and throw it away */
        // min.length = 2;

    }
}

package
{
    import MathInternals.*;

    /* FIXME #82: some imported types can't be used as annotations */

    //    intrinsic const math /* : MathInternals.Math */ = new MathInternals.Math();

    public var Math = new MathInternals.Math();
}
