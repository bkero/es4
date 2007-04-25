(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* Host functions provided to implement standard library. *)

structure Native = struct 

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[native] " :: ss) else ()
fun error ss = LogErr.hostError ss

fun rawNth (vals:Mach.VAL list)
           (n:int) 
    : Mach.VAL =
    if n >= length vals
    then error ["trying to fetch arg #",
                (Int.toString n), 
                " from arg list of length ",
                (Int.toString (length vals))]
    else 
        List.nth (vals, n)


fun nthAsA (f:Mach.VAL -> 'a) 
           (vals:Mach.VAL list) 
           (n:int) 
    : 'a = 
    f (rawNth vals n)

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


fun nthAsFn (vals:Mach.VAL list) 
             (n:int) 
    : Mach.FUN_CLOSURE = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.Function f) => f
          | _ => error ["Wanted function, got other"]
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

fun nthAsDouble (vals:Mach.VAL list) 
                (n:int) 
    : Real64.real = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.Double d) => d
          | _ => error ["Wanted double, got other"]
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
        val len = Word32.toInt 
                      (Eval.toUInt32 
                           (Eval.getValue arr Name.public_length))
        fun build i vs = 
            if i < 0
            then vs
            else 
                let
                    val n = Name.public (Int.toString i)
                    val curr = if Eval.hasValue arr n
                               then Eval.getValue arr n
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
                    | SOME (Mach.String _) => "String"
                    | SOME (Mach.Decimal _) => "Number"
                    | SOME (Mach.Int _) => "Number"
                    | SOME (Mach.UInt _) => "Number"
                    | SOME (Mach.Double _) => "Number"
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

fun convertAndBindMagic (vals:Mach.VAL list) 
                        (cvt:(Mach.VAL -> 'a))
                        (mag:('a -> Mach.MAGIC)) 
    : Mach.VAL = 
    let
        val ob = nthAsObj vals 0
        val v = rawNth vals 1
        val p = cvt v
        val m = mag p
    in
        Mach.setMagic ob (SOME m);
        Mach.Undef
    end

(*
 * Given a target object and a value, select a magic representation for 
 * the value, of the type implied by the function name, and set the
 * target's magic slot to that representation.
 *
 * magic native function bindInt(target : Object!, value : * );
 * magic native function bindUInt(target : Object!, value : * );
 * magic native function bindBoolean(target : Object!, value : * );
 * magic native function bindDouble(target : Object!, value : * );
 * magic native function bindDecimal(target : Object!, value : * );
 * magic native function bindString(target : Object!, value : * );
 *)
fun bindUInt (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toUInt32) (Mach.UInt)

fun bindInt (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toInt32) (Mach.Int)

fun bindBoolean (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toBoolean) (Mach.Boolean)

fun bindDouble (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toDouble) (Mach.Double)

fun bindDecimal (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toDecimal 
                                  Decimal.defaultPrecision 
                                  Decimal.defaultRoundingMode) (Mach.Decimal)

fun bindString (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toString) (Mach.String)


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
        Eval.evalCallExpr thisObj fnObj argsList
    end

fun fnLength (vals:Mach.VAL list) 
    : Mach.VAL = 
    let 
        val {func=Ast.Func{ty, ...}, ...} = nthAsFn vals 0
    in
        Eval.newUInt (Word32.fromInt (length (#params ty)))
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
    let
        val x = rawNth vals 0
    in
        if not (Mach.isString x)
        then x
        else 
            let
                val s = nthAsStr vals 0
                val lines = [s] (* FIXME: split lines *)
                (* 
                 * FIXME: catch parse errors and throw a user SyntaxError
                 * exception here once natives grow the ability to throw 
                 * user exceptions. 
                 *)                            
                val prog = Parser.parseLines lines
            in
                (* 
                 * FIXME: maybe don't want to permit the full set of 
                 * program constructs (classes?) so possibly sanitize the
                 * result of parsing a bit, strip out some sorts of things...
                 *)
                Eval.evalProgram (Defn.defProgram prog)
            end
    end
    
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
 * intrinsic function get (obj: Object!, name: string) : *
 *)
fun get (vals:Mach.VAL list)
    : Mach.VAL = 
    (* FIXME: arg #1 should be a Name, and convert to Ast.Name. *)
    Eval.getValueOrVirtual 
        (nthAsObj vals 0) 
        {id=(nthAsStr vals 1), ns=Name.publicNS} 
        false

(* 
 * intrinsic function set (obj: Object!, name: string, val: * ) : void
 *)
fun set (vals:Mach.VAL list)
    : Mach.VAL = 
    (* FIXME: arg #1 should be a Name, and convert to Ast.Name. *)
    (Eval.setValueOrVirtual 
         (nthAsObj vals 0) 
         {id=(nthAsStr vals 1), ns=Name.publicNS} 
         (rawNth vals 2) 
         false;
     Mach.Undef)

(* 
 * Return the current time in milliseconds since January 1 1970 00:00:00 UTC.
 * 
 * static intrinsic native function now() : double;
 *)

fun now (vals:Mach.VAL list)
    : Mach.VAL = 
    Eval.newDouble ((Time.toReal (Time.now())) * 1000.0)

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

 NOTE: all of these only work on doubles. We need versions that
 also work on decimals.

*)

val random_state = Random.rand (37, 79)

fun random (v:Mach.VAL list)
    : Mach.VAL =
    Eval.newDouble (Random.randReal random_state)

fun unaryDoubleFn (f:(Real64.real -> Real64.real)) : 
    ((Mach.VAL list) -> Mach.VAL) =
 fn vals => Eval.newDouble (f (Eval.toDouble (rawNth vals 0)))
         
fun binaryDoubleFn (f:((Real64.real * Real64.real) -> Real64.real)) : 
    ((Mach.VAL list) -> Mach.VAL) =
 fn vals => Eval.newDouble (f ((Eval.toDouble (rawNth vals 0)),
                               (Eval.toDouble (rawNth vals 1))))

val abs = unaryDoubleFn Real64.abs 
val ceil = unaryDoubleFn Real64.realCeil
val floor = unaryDoubleFn Real64.realFloor
val round = unaryDoubleFn Real64.realRound

val acos = unaryDoubleFn Math.acos
val asin = unaryDoubleFn Math.asin
val atan = unaryDoubleFn Math.atan
val atan2 = binaryDoubleFn Math.atan2
val cos = unaryDoubleFn Math.cos
val exp = unaryDoubleFn Math.exp
val log = unaryDoubleFn Math.ln
val sin = unaryDoubleFn Math.sin
val sqrt = unaryDoubleFn Math.sqrt
val tan = unaryDoubleFn Math.tan

(* Math.pow in smlnj 110.60 has an error of 3.33e~6 on computing 2^32! *)

val pow = binaryDoubleFn (fn (a,b) => 
                             if Real64.compare((Real64.realFloor b), b) = EQUAL andalso b >= 0.0 andalso b <= 2147483647.0 then
                                 let fun exponentiate expt =
                                         if expt = 0 then
                                             1.0
                                         else if Int.mod(expt, 2) = 1 then
                                             a * (exponentiate (expt - 1))
                                         else 
                                             let val v = exponentiate (expt div 2)
                                             in 
                                                 v * v
                                             end
                                 in
                                     exponentiate (Real64.floor b)
                                 end
                             else
                                 Math.pow (a,b))

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

fun load (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val fname = nthAsStr vals 0
    in
        Eval.evalProgram (Defn.defProgram (Parser.parseFile fname));
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

fun inspect (vals:Mach.VAL list)
    : Mach.VAL = 
    let 
        val pad = "          "
        fun p 0 s = List.app TextIO.print s
          | p n s = (TextIO.print pad; p (n-1) s)

        fun nl _ = TextIO.print "\n";

        fun att {dontDelete,dontEnum,readOnly,isFixed} = 
            if not dontDelete 
               andalso not dontEnum
               andalso not readOnly
               andalso not isFixed
            then ""
            else
                (" (" 
                 ^ (if dontDelete then "DD," else "")
                 ^ (if dontEnum then "DE," else "")
                 ^ (if readOnly then "RO," else "")
                 ^ (if isFixed then "FX" else "")
                 ^ ") ")
                
        fun id (Mach.Obj ob) = Int.toString (#ident ob)

        fun tag (Mach.Obj ob) = 
            case (#tag ob) of 
                (* FIXME: elaborate printing of structural tags. *)
                Mach.ObjectTag _ => "<Obj>"
              | Mach.ArrayTag _ => "<Arr>"
              | Mach.FunctionTag _ => "<Fn>"
              | Mach.ClassTag n => ("<Class " ^ (LogErr.name n) ^ ">")
              | Mach.NoTag => "<NoTag>"

        (* FIXME: elaborate printing of type expressions. *)
        fun typ t = "<TypeExpr>"
        fun mag m = case m of 
                        Mach.String s => ("\"" ^ (String.toString s) ^ "\"")
                      | m => Eval.magicToString m
                                 
        fun printVal indent _ Mach.Undef = TextIO.print "undefined\n"
          | printVal indent _ Mach.Null = TextIO.print "null\n"
          | printVal indent 0 (Mach.Object (Mach.Obj ob)) = 
            (TextIO.print (case !(#magic ob) of 
                               NONE => tag (Mach.Obj ob)
                             | SOME m => mag m);
             TextIO.print "\n")

          | printVal indent n (Mach.Object obj) = 
            let
                fun subVal i v = printVal (i+1) (n-1) v
                fun prop np = 
                    let
                        val (n,{ty,state,attrs}) = np
                        val indent = indent + 1
                        val stateStr = 
                            case state of 
		                        Mach.TypeVarProp => "[typeVar]"
		                      | Mach.TypeProp => "[type]"
		                      | Mach.UninitProp => "[uninit]"
		                      | Mach.ValProp v => "[val]"
		                      | Mach.VirtualValProp _ => "[virtual val]"
                              | Mach.MethodProp _ => "[method]"
                              | Mach.NativeFunctionProp _ => "[native function]"
                              | Mach.NamespaceProp _ => "[namespace]"
                              | Mach.ValListProp _ => "[val list]"
                    in
                        p indent ["   prop = ", LogErr.name n, att attrs, "= "];
                        (* p indent ["   type = ", typ ty]; nl(); *)
                        case state of 
                            Mach.ValProp v => subVal indent v
                          | _ => TextIO.print (stateStr ^ "\n")
                    end
                val Mach.Obj { magic, props, proto, ... } = obj
            in
                TextIO.print "Obj {\n";
                (case !magic of 
                     SOME m => (p indent ["  magic = ", (mag m)]; nl())
                   | NONE => ());
                p indent ["    tag = ", (tag obj)]; nl();
                p indent ["  ident = ", (id obj)]; nl();
                p indent ["  proto = "]; subVal indent (!proto);
                p indent ["  props = ["]; nl();
                List.app prop (!props);
                p indent ["          ] }"]; nl()
            end

        val v = rawNth vals 0
        val d = if length vals > 1 then nthAsInt vals 1 else 1
    in
        printVal 0 d v;
        Mach.Undef
    end

fun proto (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val Mach.Obj { proto, ... } = nthAsObj vals 0
    in
        !proto
    end


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

        addFn Name.magicNS "bindInt" bindInt;
        addFn Name.magicNS "bindUInt" bindUInt;
        addFn Name.magicNS "bindDouble" bindDouble;
        addFn Name.magicNS "bindDecimal" bindDecimal;
        addFn Name.magicNS "bindBoolean" bindBoolean;
        addFn Name.magicNS "bindString" bindString;

        addFn Name.magicNS "apply" apply;
        addFn Name.magicNS "fnLength" fnLength;

        addFn Name.magicNS "charCodeAt" charCodeAt;
        addFn Name.magicNS "fromCharCode" fromCharCode;
        addFn Name.magicNS "stringLength" stringLength;
        addFn Name.magicNS "stringAppend" stringAppend;
        addFn Name.magicNS "getByteArrayByte" getByteArrayByte;
        addFn Name.magicNS "setByteArrayByte" setByteArrayByte;

        addFn Name.intrinsicNS "eval" eval;
        addFn Name.intrinsicNS "parseInt" parseInt;
        addFn Name.intrinsicNS "parseFloat" parseFloat;
        addFn Name.intrinsicNS "isNaN" isNaN;
        addFn Name.intrinsicNS "isFinite" isFinite;
        addFn Name.intrinsicNS "decodeURI" decodeURI;
        addFn Name.intrinsicNS "decodeURIComponent" decodeURIComponent;
        addFn Name.intrinsicNS "encodeURI" encodeURI;
        addFn Name.intrinsicNS "encodeURIComponent" encodeURIComponent;

        addFn Name.intrinsicNS "get" get;
        addFn Name.intrinsicNS "set" set;

        (* FIXME: stubs to get double loading. Implement. *)
        addFn Name.intrinsicNS "toFixedStep10" (fn _ => Eval.newString(""));
        addFn Name.intrinsicNS "toExponential" (fn _ => Eval.newString(""));
        addFn Name.intrinsicNS "toPrecision" (fn _ => Eval.newString(""));

        (* FIXME: stubs to get Date loading. Implement. *)
        addFn Name.intrinsicNS "now" now;
        addFn Name.publicNS "LocalTZA" (fn _ => Eval.newDouble 1.0);
        addFn Name.publicNS "DaylightSavingsTA" (fn _ => Eval.newDouble 1.0);
       
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
        addFn Name.intrinsicNS "pow" pow;
        addFn Name.intrinsicNS "random" random;
        addFn Name.intrinsicNS "round" round;
        addFn Name.intrinsicNS "sin" sin;
        addFn Name.intrinsicNS "sqrt" sqrt;
        addFn Name.intrinsicNS "tan" tan;

        addFn Name.intrinsicNS "print" print;
        addFn Name.intrinsicNS "load" load;
        addFn Name.intrinsicNS "assert" assert;
        addFn Name.intrinsicNS "typename" typename;
        addFn Name.intrinsicNS "inspect" inspect;
        addFn Name.intrinsicNS "proto" proto
    end

end
