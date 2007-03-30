/* -*- indent-tabs-mode: nil -*- */

package
{
    final class Math extends Object
    {       
        // 15.8.1 Value Properties of the Math Object
        static const E; 
        static const LN10;  
        static const LN2;   
        static const LOG2E; 
        static const LOG10E;    
        static const PI;    
        static const SQRT1_2;   
        static const SQRT2; 

        // 15.8.2 Function Properties of the Math Object
        static native function abs(x:Number):Number;
        static native function acos(x):Number;
        static native function asin(x):Number;
        static native function atan(x):Number;
        static native function atan2(x,y):Number;
        static native function ceil(x):Number;
        static native function cos(x):Number;
        static native function exp(x):Number;
        static native function floor(x):Number;
        static native function log(x):Number;
        static native function max(...args):Number;
        static native function min(...args):Number;
        static native function pow(x,y):Number;
        static native function random():Number;
        static native function round(x):Number;
        static native function sin(x):Number;
        static native function sqrt(x):Number;
        static native function tan(x):Number;

    } // class
    
    // Math is a singleton object, not a class, but has the type "Math"
    // which suggests a private Math class that created the singleton.

    var Math = new private::Math;

    dynamic class Math extends Object
    {       
        // 15.8.1 Value Properties of the Math Object
        const E = ECMA4::Math.E;    
        const LN10 = ECMA4::Math.LN10;  
        const LN2 = ECMA4::Math.LN2;    
        const LOG2E = ECMA4::Math.LOG2E;    
        const LOG10E = ECMA4::Math.LOG10E;  
        const PI = ECMA4::Math.PI;  
        const SQRT1_2 = ECMA4::Math.SQRT1_2;    
        const SQRT2 = ECMA4::Math.SQRT2;    

        // 15.8.2 Function Properties of the Math Object
        var abs = ECMA4::Math.abs;
        var acos = ECMA4::Math.acos;
        var asin = ECMA4::Math.asin;
        var atan = ECMA4::Math.atan;
        var atan2 = ECMA4::Math.atan2;
        var ceil = ECMA4::Math.ceil;
        var cos = ECMA4::Math.cos;
        var exp = ECMA4::Math.exp;
        var floor = ECMA4::Math.floor;
        var log = ECMA4::Math.log;
        var max = ECMA4::Math.max;
        var min = ECMA4::Math.min;
        var pow = ECMA4::Math.pow;
        var random = ECMA4::Math.random;
        var round = ECMA4::Math.round;
        var sin = ECMA4::Math.sin;
        var sqrt = ECMA4::Math.sqrt;
        var tan = ECMA4::Math.tan;

        // E262 specifies that length=2 for these.
        max.length = 2;
        min.length = 2;

    } // class
} // package
