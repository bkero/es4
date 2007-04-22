/* -*- indent-tabs-mode: nil -*- */

/* FIXME: The name hiding trick with MathInternals will work (provided
 * the visible properties on the Math class are changed to "public")
 * but does not work now due to a bug.  */

//package MathInternals
//{
    /* Math is a singleton object, not a class, but has the type "Math"
     * which suggests a private Math class that created the singleton.  
     */

/*public*/ intrinsic dynamic final class Math
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
	    let x = double.NEGATIVE_INFINITY;
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
	    let x = double.POSITIVE_INFINITY;
	    for (let i : uint = 0; i < args.length; ++i) {
                let v = Number(args[i]);  /* FIXME: is this conversion right? */
                if (isNaN(v))
                    return v;
		if (!(v > x))  /* Handles -0 < +0 */
		    x = v;
	    }
	    return x;
	}

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
        const E = 2.7182818284590452354;   /* Approximately */
        const LN10 = 2.302585092994046;    /* Approximately */
        const LN2 = 0.6931471805599453;    /* Approximately */
        const LOG2E = 1.4426950408889634;  /* Approximately */
        const LOG10E = 0.4342944819032518; /* Approximately */
        const PI = 3.1415926535897932;     /* Approximately */
        const SQRT1_2 = 0.7071067811865476;/* Approximately */
        const SQRT2 = 1.4142135623730951;  /* Approximately */

        // 15.8.2 Function Properties of the Math Object
        var abs = intrinsic::abs;
        var acos = intrinsic::acos;
        var asin = intrinsic::asin;
        var atan = intrinsic::atan;
        var atan2 = intrinsic::atan2;
        var ceil = intrinsic::ceil;
        var cos = intrinsic::cos;
        var exp = intrinsic::exp;
        var floor = intrinsic::floor;
        var log = intrinsic::log;
        var max = intrinsic::max;
        var min = intrinsic::min;
        var pow = intrinsic::pow;
        var random = intrinsic::random;
        var round = intrinsic::round;
        var sin = intrinsic::sin;
        var sqrt = intrinsic::sqrt;
        var tan = intrinsic::tan;

        // E262 specifies that length=2 for these.
        // max.length = 2;  /* FIXME: this will cons a function, set its length and throw it away */
        // min.length = 2;

    }
//}

/*package 
{
    import MathInternals.*;

    public var Math = new MathInternals.Math;
}*/

    var Math = new intrinsic::Math;
