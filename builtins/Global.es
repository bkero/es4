/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Global" object
 * ES-262-3 15.1
 * ES-262-4 draft
 *
 * Status: not reviewed against specs.
 */

package
{
    use default namespace public;

    // 15.1.1.1 NaN
    // 15.1.1.2 Infinity
    // 15.1.1.3 undefined
    // {DE,DD,RO} -- change from ECMA-262, which has them as {DE,DD}
    intrinsic const NaN = 0.0/0.0;
    intrinsic const Infinity = 1.0/0.0;
    intrinsic const undefined = void(0);
    
    // @todo: "dynamic function" is probably redundant at the top level, but is useful for clarity
    

    // 15.1.2.1 eval (x)
    intrinsic native function eval(x);

    // 15.1.2.2 parseInt (string , radix)
    intrinsic native function parseInt(string:string, radix:int);

    // 15.1.2.3 parseFloat (string)
    intrinsic native function parseFloat(string:string);

    // 15.1.2.4 isNaN (v)
    intrinsic function isNaN(v:*):boolean
    {
        let const n = intrinsic::ToDouble(v)
        return n == Number.NaN 
    }

    // 15.1.2.5 isFinite (number)
    intrinsic function isFinite(v:*):boolean 
    {
        let const n = intrinsic::ToDouble(v)
        return n != Number.NaN && 
               n != Number.NEGATIVE_INFINITY && 
               n != Number.POSTITVE_INFINITY 
    }
    
    
    // 15.1.3.1 decodeURI (encodedURI)
    intrinsic native function decodeURI(encodedURI);

    // 15.1.3.2 decodeURIComponent (encodedURIComponent)
    intrinsic native function decodeURIComponent(encodedURIComponent);
    
    // 15.1.3.3 encodeURI (uri)
    intrinsic native function encodeURI(uri);
    
    // 15.1.3.4 encodeURIComponent (uriComponent)
    intrinsic native function encodeURIComponent(uriComponent);

    // Mutable public properties defaulting to their intrinsic namesakes.
    var NaN = intrinsic::NaN;
    var Infinity = intrinsic::Infinity;
    var undefined = intrinsic::undefined;
    
    var eval = intrinsic::eval;
    var parseInt = intrinsic::parseInt;
    var parseFloat = intrinsic::parseFloat;
    var isNaN = intrinsic::isNaN;
    var isFinite = intrinsic::isFinite;
    var decodeURI = intrinsic::decodeURI;
    var decodeURIComponent = intrinsic::decodeURIComponent;
    var encodeURI = intrinsic::encodeURI;
    var encodeURIComponent = intrinsic::encodeURIComponent;
    
    // The non-virtual property get/set helpers.
    intrinsic native function get(obj:Object!, name:string) : *;
    intrinsic native function set(obj:Object!, name:string, val:*) : void;


}
