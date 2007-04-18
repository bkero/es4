/* -*- indent-tabs-mode: nil -*- */

//package
//{

    // Math is a singleton object, not a class, but has the type "Math"
    // which suggests a private Math class that created the singleton.

    intrinsic dynamic final class Math extends Object
    {
        intrinsic static const E
        intrinsic static const LN10
        intrinsic static const LN2
        intrinsic static const LOG2E
        intrinsic static const LOG10E
        intrinsic static const PI
        intrinsic static const SQRT1_2
        intrinsic static const SQRT2

        intrinsic static function abs(x:Number):Number
	    x > 0 ? x : -x;

        intrinsic static function max(...args):Number
	{
	    var x : double = double.NEGATIVE_INFINITY;
	    for (let i : uint = 0; i < args.length; ++i) {
		if (args[i] > x) {
		    x = args[i];
		}
	    }
	    return x;
	}

        intrinsic static function min(...args):Number
	{
	    var x : double = double.POSITIVE_INFINITY;
	    for (let i : uint = 0; i < args.length; ++i) {
		if (args[i] < x) {
		    x = args[i];
		}
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

        // 15.8.1 Value Properties of the Math Object
        const E = intrinsic::E;    
        const LN10 = intrinsic::LN10;  
        const LN2 = intrinsic::LN2;    
        const LOG2E = intrinsic::LOG2E;    
        const LOG10E = intrinsic::LOG10E;  
        const PI = intrinsic::PI;  
        const SQRT1_2 = intrinsic::SQRT1_2;    
        const SQRT2 = intrinsic::SQRT2;    

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

    } // class
    public var Math = new intrinsic::Math;


//} // package
