(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* Host functions provided to implement standard library. *)

structure Native = struct 

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[native] " :: ss) else ()
fun error ss = LogErr.hostError ss

fun nthAsA (f:Mach.VAL -> 'a) 
           (vals:Mach.VAL list) 
           (n:int) 
    : 'a = 
    if n >= length vals
    then error ["trying to fetch arg #",
                           (Int.toString n), 
                           " from arg list of length ",
                           (Int.toString (length vals))]
    else 
        f (List.nth (vals, n))


fun nthAsObj (vals:Mach.VAL list) 
             (n:int) 
    : Mach.OBJ = 
    let 
        fun f Mach.Undef = error ["Wanted Object, got Undef"]
          | f Mach.Null = error ["Wanted Object, got Null"]
          | f (Mach.Object ob) = ob
    in
        nthAsA f vals n
    end


fun nthAsObjAndCls (vals:Mach.VAL list) 
                   (n:int) 
    : (Mach.OBJ * Mach.CLS_CLOSURE) = 
    let
        val obj = nthAsObj vals n
        val Mach.Obj { magic, ... } = obj
    in
        case !magic of 
            SOME (Mach.Class c) => (obj, c)
          | _ => error ["Wanted class, got other"]
    end


fun nthAsStr (vals:Mach.VAL list) 
             (n:int) 
    : string = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.String s) => s
          | _ => error ["Wanted string, got other"]
    end


fun nthAsInt (vals:Mach.VAL list) 
             (n:int) 
    : Int32.int = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.Int n) => n
          | _ => error ["Wanted int, got other"]
    end

fun nthAsUInt (vals:Mach.VAL list) 
              (n:int) 
    : Word32.word = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.UInt n) => n
          | _ => error ["Wanted uint, got other"]
    end


fun nthAsBool (vals:Mach.VAL list) 
              (n:int) 
    : bool = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.Boolean b) => b
          | _ => error ["Wanted Boolean, got other"]
    end

fun nthAsByteArray (vals:Mach.VAL list) 
                   (n:int) 
    : Word8Array.array = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.ByteArray b) => b
          | _ => error ["Wanted ByteArray, got other"]
    end


fun propQuery (vals:Mach.VAL list) 
              (f:(Mach.PROP_BINDINGS -> Ast.NAME -> bool))
    : Mach.VAL = 
    let 
        val Mach.Obj { props, ...} = nthAsObj vals 0
        val id = nthAsStr vals 1
        val ns = Ast.Internal ""
    in
        Eval.newBoolean (f props {id=id, ns=ns})
    end

fun arrayToList (arr:Mach.OBJ) 
    : Mach.VAL list = 
    let 
        val ns = Ast.Internal ""
        val len = Word32.toInt 
                      (Eval.toUInt32 
                           (Eval.getValue (arr, {id="length", ns=ns})))
        fun build i vs = 
            if i < 0
            then vs
            else 
                let
                    val n = {id=(Int.toString i), ns=ns}
                    val curr = if Eval.hasValue arr n
                               then Eval.getValue (arr, n)
                               else Mach.Undef
                in
                    build (i-1) (curr::vs)
                end
    in
        build (len-1) []
    end


(* 
 * Given a class object, run the standard object-construction
 * protocol for it (and its base classes, initializers, settings,
 * ctors). Return the resulting instance, always an Object!
 *
 * magic native function construct(cls:Class!, args:[*]) : Object!;
 *)
fun construct (vals:Mach.VAL list)
    : Mach.VAL = 
    let
        val (obj, cls) = nthAsObjAndCls vals 0
        val args = arrayToList (nthAsObj vals 1)
    in
        Eval.constructClassInstance obj cls args
    end


(* 
 * Retrieve the [[Class]] property of o
 * 
 * magic native function getClassName(o : Object!) : string; 
 *)
fun getClassName (vals:Mach.VAL list) 
    : Mach.VAL = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals 0
        (* FIXME: is this right? *)
        val str = case !magic of 
                      SOME (Mach.Function _) => "Function"
                    | SOME (Mach.NativeFunction _) => "Function"
                    | _ => "Object"
    in
        Eval.newString str
    end

    
(* 
 * Retrieve the possibly null [[Prototype]] property of o 
 * 
 * magic native function getPrototype(o : Object!) : Object;
 *)
fun getPrototype (vals:Mach.VAL list) 
    : Mach.VAL = 
    let 
        val Mach.Obj { proto, ... } = nthAsObj vals 0
    in
        !proto
    end


(*
 * Return true iff o has a local property named by p.
 * 
 * magic native function hasOwnProperty(o : Object!, p : string) : Boolean;
 *)
fun hasOwnProperty (vals:Mach.VAL list) 
    : Mach.VAL = 
    propQuery vals Mach.hasProp


(*
 * Return true if the property p does exists locally on o and its
 * DontEnum bit is set
 * 
 * magic native function getPropertyIsDontEnum(o : Object!, p : string) : Boolean;
 *)
fun getPropertyIsDontEnum (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        fun f props n = 
            if Mach.hasProp props n
            then (#dontEnum (#attrs (Mach.getProp props n)))
            else false
    in
        propQuery vals f
    end


(* 
 * Return true if the property p does exists locally on o and its
 * DontDelete bit is set
 * 
 * magic native function getPropertyIsDontDelete(o : Object!, p : string) : Boolean;
 *)

fun getPropertyIsDontDelete (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        fun f props n = 
            if Mach.hasProp props n
            then (#dontDelete (#attrs (Mach.getProp props n)))
            else false
    in
        propQuery vals f
    end
    

(* Provided that the property p exists locally on o, set its DontEnum
 * flag according to f.  If the property p does not exist locally on
 * o, it does nothing.
 * 
 * magic native function setPropertyIsDontEnum(o : Object!, p : string, f : Boolean) : void;
 *)
fun setPropertyIsDontEnum (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val Mach.Obj { props, ...} = nthAsObj vals 0
        val id = nthAsStr vals 1
        val ns = Ast.Internal ""
        val n = { id = id, ns = ns }
        val b = nthAsBool vals 2
    in
        if Mach.hasProp props n
        then 
            let 
                val prop = Mach.getProp props n
                val attrs = (#attrs prop)
                val newProp = { ty = (#ty prop),
                                state = (#state prop),
                                attrs = { dontDelete = (#dontDelete attrs),
                                          dontEnum = b,
                                          readOnly = (#readOnly attrs),
                                          isFixed = (#isFixed attrs) } }
            in
                Mach.delProp props n;
                Mach.addProp props n newProp
            end
        else ();
        Mach.Undef
    end


(*
 * Copy the magic value slot from src to dst.
 * 
 *  magic native function copyValue(src: Object!, dst:Object!) : void;
 *)
fun copyValue (vals:Mach.VAL list) 
    : Mach.VAL = 
    let 
        val Mach.Obj src = nthAsObj vals 0
        val Mach.Obj dst = nthAsObj vals 1
    in
        (#magic dst) := !(#magic src);
        Mach.Undef
    end


(*
 * Given a function object, a this object, and an array of argument
 * values, call the function with the this object and arguments. 
 *
 * magic native function apply(fn : Function!, t : Object!, args : Array) : *;
 *)
fun apply (vals:Mach.VAL list) 
    : Mach.VAL = 
    let 
        val fnObj = nthAsObj vals 0
        val thisObj = nthAsObj vals 1
        val argsObj = nthAsObj vals 2
        val argsList = arrayToList argsObj
    in
        Eval.evalCallExpr (SOME thisObj) fnObj argsList
    end


(* Given a function object, arguments to bind and source to run, 
 * compiles the source and arguments into a magic function value
 * and sets the magic slot inside the function to contain it.
 *
 * magic native function compileInto(fn: Function!, argNames: [String!], src: String!): void;
 *)
fun compileInto (vals:Mach.VAL list) 
    : Mach.VAL = 
    (*
     * We synthesize a token stream here that feeds back into the parser.
     *)
    let 
        val funcObj = nthAsObj vals 0
        fun ident v = Token.Identifier (Eval.toString v)
        val argIdents = map ident (arrayToList (nthAsObj vals 1))
        val argList = case argIdents of
                          [] => []
                        | x::xs => ((x, {file="<no filename>", line=1}) :: (List.concat 
                                                   (map (fn i => [(Token.Comma, {file="<no filename>", line=1}), (i, {file="<no filename>", line=1})]) xs)))
        val source = nthAsStr vals 2
        val lines = [source] (* FIXME: split lines *)
        val lineTokens = Parser.lexLines lines
        val funcTokens = [(Token.Function, {file="<no filename>", line=1}), 
                          (Token.LeftParen, {file="<no filename>", line=1})]
                         @ argList
                         @ [(Token.RightParen, {file="<no filename>", line=1})]
                         @ [(Token.LeftBrace, {file="<no filename>", line=1})]
                         @ lineTokens
                         @ [(Token.RightBrace, {file="<no filename>", line=1})]
                         
        val (_,funcExpr) = Parser.functionExpression (funcTokens, 
                                                      Parser.NOLIST, 
                                                      Parser.ALLOWIN)

        val funcExpr = Defn.defExpr (Defn.topEnv()) funcExpr
        val funcVal = Eval.evalExpr (Eval.getGlobalScope ()) funcExpr
    in
        case funcVal of 
            Mach.Object obj =>
            let 
                val Mach.Obj { magic, ...} = funcObj
                val sname = {ns=Ast.Internal "", id="source"}
                val sval = (Eval.newString source)
            in
                Eval.setValue obj sname sval;
                magic := Mach.getObjMagic obj
            end
          | _ => error ["function did not compile to object"];
        Mach.Undef
    end


(* Given a string and a position in that string, return the
 * numeric value of the character at that position in the
 * string.
 *
 * magic native function charCodeAt(s : string, pos : uint) : uint;
 *)
fun charCodeAt (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val s = nthAsStr vals 0
        val i = nthAsUInt vals 1
    in
        Eval.newUInt (Word32.fromInt (Char.ord (String.sub (s, (Word32.toInt i)))))
    end


(*
 * Given a numeric character value, return a string of length 1
 * whose element 0 is the character with that same value.
 *
 * magic native function fromCharCode(ch : uint) : string;
 *)
fun fromCharCode (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val i = nthAsUInt vals 0
    in
        Eval.newString (Char.toString (Char.chr (Word32.toInt i)))
    end


(* 
 * Given a string object, return the number of characters in the
 * string.
 *
 * magic native function stringLength(s : string) : uint;
 *)
fun stringLength (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val s = nthAsStr vals 0
    in
        Eval.newUInt (Word32.fromInt (String.size s))
    end
    

(* 
 * Given two string objects A and B , return a new string object
 * containing the characters from A followed by the characters
 * from B.
 * 
 * magic native function stringAppend(a : string, b : string) : string;
 *)
fun stringAppend (vals:Mach.VAL list)
    : Mach.VAL = 
    let
        val a = nthAsStr vals 0
        val b = nthAsStr vals 1
    in
        Eval.newString (a ^ b)
    end
    
    
(*
 * Get the byte at index idx.  Unspecified behavior if that index
 * does not have data (it's OK to crash the system).
 * 
 * magic native function getByteArrayByte(ba : ByteArray!, idx : uint) : uint;
 *)
fun getByteArrayByte (vals:Mach.VAL list)
    : Mach.VAL =
    let 
        val b = nthAsByteArray vals 0
        val i = nthAsUInt vals 1
    in
        Eval.newUInt (Word32.fromInt (Word8.toInt (Word8Array.sub (b, (Word32.toInt i)))))
    end

(*
 * Set the byte at index idx to val, which will be truncated to
 * the low 8 bits before being stored.
 *
 * magic native function setByteArrayByte(ba : ByteArray!, idx : uint, val : uint) : void;
 *)
fun setByteArrayByte (vals:Mach.VAL list)
    : Mach.VAL =
    let 
        val b = nthAsByteArray vals 0
        val i = nthAsUInt vals 1
        val v = nthAsUInt vals 2
    in
        Word8Array.update(b, Word32.toInt i, Word8.fromInt (Word32.toInt v));
        Mach.Undef
    end


(* 
 * 15.1.2.1
 * intrinsic native function eval(x)
 *)

fun eval (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::eval"]
    
(* 
 * 15.1.2.2
 * intrinsic native function parseInt(string:string, radix:int)
 *)
fun parseInt (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::parseInt"]
    

(* 
 * 15.1.2.3 
 * intrinsic native function parseFloat(string:string);
 *)
fun parseFloat (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::parseFloat"]


(* 
 * 15.1.2.4
 * intrinsic native function isNaN( number:* ):boolean;
 *)
fun isNaN (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::isNaN"]


(* 
 * 15.1.2.5
 * intrinsic native function isFinite( number:* ):boolean;
 *)
fun isFinite (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::isFinite"]


(* 
 * 15.1.3.1
 * intrinsic native function decodeURI(encodedURI);
 *)
fun decodeURI (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::decodeURI"]


(* 
 * 15.1.3.2
 * intrinsic native function decodeURIComponent(encodedURIComponent);
 *)
fun decodeURIComponent (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::decodeURIComponent"]

(* 
 * 15.1.3.3
 * intrinsic native function encodeURI(uri);
 *)
fun encodeURI (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::encodeURI"]


(* 
 * 15.1.3.4
 * intrinsic native function encodeURIComponent(uriComponent);
 *)
fun encodeURIComponent (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::encodeURIComponent"]

(*
    Math natives

        addFn Name.intrinsicNS "abs" abs
        addFn Name.intrinsicNS "acos" acos
        addFn Name.intrinsicNS "asin" asin
        addFn Name.intrinsicNS "atan" atan
        addFn Name.intrinsicNS "atan2" atan2
        addFn Name.intrinsicNS "ceil" ceil
        addFn Name.intrinsicNS "cos" cos
        addFn Name.intrinsicNS "exp" exp
        addFn Name.intrinsicNS "floor" floor
        addFn Name.intrinsicNS "log" log
        addFn Name.intrinsicNS "max" max
        addFn Name.intrinsicNS "min" min
        addFn Name.intrinsicNS "pow" pow
        addFn Name.intrinsicNS "random" random
        addFn Name.intrinsicNS "round" round
        addFn Name.intrinsicNS "sin" sin
        addFn Name.intrinsicNS "sqrt" sqrt
        addFn Name.intrinsicNS "tan" tan;
*)

fun abs (v:Mach.VAL list)
    : Mach.VAL = 
    hd v

fun acos (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::acos"]

fun asin (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::acos"]

fun atan (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::acos"]

fun atan2 (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::acos"]

fun ceil (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::acos"]

fun cos (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::acos"]

fun exp (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::acos"]

fun floor (v:Mach.VAL list)
    : Mach.VAL =
    hd v

fun log (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::log"]

fun max (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::max"]

fun min (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::min"]

fun pow (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::pow"]

fun random (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::random"]

fun round (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::round"]

fun sin (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::sin"]

fun sqrt (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::sqrt"]

fun tan (v:Mach.VAL list)
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::tan"]


(* Some helpers not specified in the wiki at the moment. Maybe get rid
 * of them eventually? *)

fun print (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        fun printOne v = TextIO.print (Eval.toString v) 
    in
        List.app printOne vals; 
        TextIO.print "\n";
        Mach.Undef
    end
    
fun assert (vals:Mach.VAL list) 
    : Mach.VAL = 
    if nthAsBool vals 0
    then Mach.Undef
    else error ["intrinsic::assert() failed"]

fun typename (vals:Mach.VAL list) 
    : Mach.VAL = 
    Eval.newString 
    (case hd vals of 
        Mach.Null => "null"
      | Mach.Undef => "undefined"
      | Mach.Object (Mach.Obj ob) => 
        (case !(#magic ob) of
             NONE => "object"
           | SOME (Mach.UInt _) => "uint"
           | SOME (Mach.Int _) => "int"
           | SOME (Mach.Double _) => "double"
           | SOME (Mach.Decimal _) => "decimal"
           | SOME (Mach.ByteArray _) => "bytearray"
           | SOME (Mach.String _) => "string"
           | SOME (Mach.Boolean _) => "bool"
           | SOME (Mach.Namespace _) => "namespace"
           | SOME (Mach.Class _) => "class"
           | SOME (Mach.Interface _) => "interface"
           | SOME (Mach.Function _) => "function"
           | SOME (Mach.Type _) => "type"
           | SOME (Mach.NativeFunction _) => "native function"))

fun converter (convert:Mach.VAL -> 'a) 
              (construct:'a -> Mach.VAL)
    : ((Mach.VAL list) -> Mach.VAL) = 
    let
        fun cvt [] = Mach.Undef
          | cvt (x::_) = construct (convert x)
    in
        cvt
    end

(* Register all the native functions in this file. *)
fun registerNatives _ = 
    let
        fun addFn ns name f = 
            Mach.registerNativeFunction { ns = ns, id = name } f
    in
        addFn Name.magicNS "construct" construct;
        addFn Name.magicNS "getClassName" getClassName;
        addFn Name.magicNS "getPrototype" getPrototype;
        addFn Name.magicNS "hasOwnProperty" hasOwnProperty;
        addFn Name.magicNS "getPropertyIsDontEnum" getPropertyIsDontEnum;
        addFn Name.magicNS "getPropertyIsDontDelete" getPropertyIsDontDelete;
        addFn Name.magicNS "setPropertyIsDontEnum" setPropertyIsDontEnum;
        addFn Name.magicNS "copyValue" copyValue;
        addFn Name.magicNS "apply" apply;
        addFn Name.magicNS "compileInto" compileInto;
        addFn Name.magicNS "charCodeAt" charCodeAt;
        addFn Name.magicNS "fromCharCode" fromCharCode;
        addFn Name.magicNS "stringLength" stringLength;
        addFn Name.magicNS "stringAppend" stringAppend;
        addFn Name.magicNS "getByteArrayByte" getByteArrayByte;
        addFn Name.magicNS "setByteArrayByte" setByteArrayByte;

        addFn Name.magicNS "toInt" (converter Eval.toInt32 Eval.newInt);
        addFn Name.magicNS "toUint" (converter Eval.toUInt32 Eval.newUInt);
        addFn Name.magicNS "toDouble" (converter Eval.toDouble Eval.newDouble);
        addFn Name.magicNS "toDecimal" (converter (Eval.toDecimal 
                                                                      Decimal.defaultPrecision 
                                                                      Decimal.defaultRoundingMode) 
                                                                 Eval.newDecimal);

        addFn Name.intrinsicNS "eval" eval;
        addFn Name.intrinsicNS "parseInt" parseInt;
        addFn Name.intrinsicNS "parseFloat" parseFloat;
        addFn Name.intrinsicNS "isNaN" isNaN;
        addFn Name.intrinsicNS "isFinite" isFinite;
        addFn Name.intrinsicNS "decodeURI" decodeURI;
        addFn Name.intrinsicNS "decodeURIComponent" decodeURIComponent;
        addFn Name.intrinsicNS "encodeURI" encodeURI;
        addFn Name.intrinsicNS "encodeURIComponent" encodeURIComponent;

        (* FIXME: stubs to get double loading. Implement. *)
        addFn Name.intrinsicNS "toFixedStep10" (fn _ => Eval.newString(""));
        addFn Name.intrinsicNS "toExponential" (fn _ => Eval.newString(""));
        addFn Name.intrinsicNS "toPrecision" (fn _ => Eval.newString(""));

        (* FIXME: stubs to get Date loading. Implement. *)
        addFn Name.intrinsicNS "now" (fn _ => Eval.newDouble 0.0);
        addFn Name.internalNS "LocalTZA" (fn _ => Eval.newDouble 0.0);
        addFn Name.internalNS "DaylightSavingsTA" (fn _ => Eval.newDouble 0.0);
       
        (* Math.es natives *) 
        addFn Name.intrinsicNS "abs" abs;
        addFn Name.intrinsicNS "acos" acos;
        addFn Name.intrinsicNS "asin" asin;
        addFn Name.intrinsicNS "atan" atan;
        addFn Name.intrinsicNS "atan2" atan2;
        addFn Name.intrinsicNS "ceil" ceil;
        addFn Name.intrinsicNS "cos" cos;
        addFn Name.intrinsicNS "exp" exp;
        addFn Name.intrinsicNS "floor" floor;
        addFn Name.intrinsicNS "log" log;
        addFn Name.intrinsicNS "max" max;
        addFn Name.intrinsicNS "min" min;
        addFn Name.intrinsicNS "pow" pow;
        addFn Name.intrinsicNS "random" random;
        addFn Name.intrinsicNS "round" round;
        addFn Name.intrinsicNS "sin" sin;
        addFn Name.intrinsicNS "sqrt" sqrt;
        addFn Name.intrinsicNS "tan" tan;

        addFn Name.intrinsicNS "print" print;
        addFn Name.intrinsicNS "assert" assert;
        addFn Name.intrinsicNS "typename" typename

    end

end
