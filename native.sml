(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(*
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
 *)
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


fun nthAsUstr (vals:Mach.VAL list) 
             (n:int) 
    : Ustring.STRING = 
    let 
        val Mach.Obj { magic, ... } = nthAsObj vals n
    in
        case !magic of 
            SOME (Mach.String s) => s
          | _ => error ["Wanted string, got other"]
    end


fun nthAsName (vals:Mach.VAL list) 
              (n:int) 
    : Ast.NAME = 
    let 
        val v = rawNth vals n
    in
        Eval.needNameOrString v
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
        val n = nthAsName vals 1
    in
        Eval.newBoolean (f props n)
    end

fun arrayToList (arr:Mach.OBJ) 
    : Mach.VAL list = 
    let 
        val len = Word32.toInt 
                      (Eval.toUInt32 
                           (Eval.getValue arr Name.nons_length))
        fun build i vs = 
            if i < 0
            then vs
            else 
                let
                    val n = Name.nons (Ustring.fromInt i)
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
fun construct (regs:Mach.REGS) 
              (vals:Mach.VAL list)
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
fun getClassName (regs:Mach.REGS) 
                 (vals:Mach.VAL list) 
    : Mach.VAL = 
    let 
        val Mach.Obj { magic, tag, ... } = nthAsObj vals 0
        (* FIXME: is this right? *)
        val ustr = case !magic of 
                      SOME (Mach.Function _) => Ustring.Function_
                    | SOME (Mach.NativeFunction _) => Ustring.Function_
                    | SOME (Mach.String _) => Ustring.String_
                    | SOME (Mach.Decimal _) => Ustring.Number_
                    | SOME (Mach.Int _) => Ustring.Number_
                    | SOME (Mach.UInt _) => Ustring.Number_
                    | SOME (Mach.Double _) => Ustring.Number_
                    | SOME (Mach.Boolean _) => Ustring.Boolean_
                    | _ => 
                      (case tag of 
                           Mach.ObjectTag _ => Ustring.Object_
                         | Mach.ArrayTag _ => Ustring.Array_
                         | Mach.FunctionTag _ => Ustring.Function_
                         | Mach.ClassTag {ns, id} => id
                         | Mach.NoTag => Ustring.Object_)                      
    in
        Eval.newString ustr
    end

    
(* 
 * Retrieve the possibly null [[Prototype]] property of o 
 * 
 * magic native function getPrototype(o : Object!) : Object;
 *)
fun getPrototype (regs:Mach.REGS) 
                 (vals:Mach.VAL list) 
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
fun hasOwnProperty (regs:Mach.REGS) 
                   (vals:Mach.VAL list) 
    : Mach.VAL = 
    propQuery vals Mach.hasProp


(*
 * Return true if the property p does exists locally on o and its
 * DontEnum bit is set
 * 
 * magic native function getPropertyIsDontEnum(o : Object!, p : string) : Boolean;
 *)
fun getPropertyIsDontEnum (regs:Mach.REGS) 
                          (vals:Mach.VAL list) 
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

fun getPropertyIsDontDelete (regs:Mach.REGS) 
                            (vals:Mach.VAL list) 
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
fun setPropertyIsDontEnum (regs:Mach.REGS) 
                          (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val Mach.Obj { props, ...} = nthAsObj vals 0
        val id = nthAsUstr vals 1
        val ns = Ast.Internal Ustring.empty
        val n = { id = id, ns = ns }
        val b = nthAsBool vals 2
    in
        Mach.setPropDontEnum props n b;
        Mach.Undef
    end

fun isPrimitive (regs:Mach.REGS) 
                (vals:Mach.VAL list)
    : Mach.VAL = 
    Eval.newBoolean (Eval.isPrimitive (rawNth vals 0))

fun toPrimitive (regs:Mach.REGS) 
                (vals:Mach.VAL list)
    : Mach.VAL = 
    let
        val v = rawNth vals 0
        val hint = rawNth vals 1
    in
        if Mach.isString hint
        then Eval.toPrimitive v (Eval.toUstring hint)
        else Eval.toPrimitive v Ustring.empty
    end

fun defaultValue (regs:Mach.REGS) 
                 (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val obj = nthAsObj vals 0
        val hint = nthAsUstr vals 1
    in
        Eval.defaultValue obj hint
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
fun bindUInt (regs:Mach.REGS) 
             (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toUInt32) (Mach.UInt)

fun bindInt (regs:Mach.REGS) 
            (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toInt32) (Mach.Int)

fun bindBoolean (regs:Mach.REGS) 
                (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toBoolean) (Mach.Boolean)

fun bindDouble (regs:Mach.REGS) 
               (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toDouble) (Mach.Double)

fun bindDecimal (regs:Mach.REGS) 
                (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals (Eval.toDecimal 
                                  Decimal.defaultPrecision 
                                  Decimal.defaultRoundingMode) (Mach.Decimal)

fun bindString (regs:Mach.REGS) 
               (vals:Mach.VAL list) 
    : Mach.VAL = 
    convertAndBindMagic vals Eval.toUstring Mach.String


fun newInt (regs:Mach.REGS) 
           (vals:Mach.VAL list) 
    : Mach.VAL = 
    Eval.newInt (Eval.toInt32 (rawNth vals 0))

fun newUInt (regs:Mach.REGS) 
            (vals:Mach.VAL list) 
    : Mach.VAL = 
    Eval.newUInt (Eval.toUInt32 (rawNth vals 0))

fun newDouble (regs:Mach.REGS) 
              (vals:Mach.VAL list) 
    : Mach.VAL = 
    Eval.newDouble (Eval.toDouble (rawNth vals 0))


(*
 * Given a function object, a this object, and an array of argument
 * values, call the function with the this object and arguments. 
 *
 * magic native function apply(fn : Function!, t : Object!, args : Array) : *;
 *)
fun apply (regs:Mach.REGS) 
          (vals:Mach.VAL list) 
    : Mach.VAL = 
    let 
        val fnObj = nthAsObj vals 0
        val thisObj = nthAsObj vals 1
        val argsObj = nthAsObj vals 2
        val argsList = arrayToList argsObj
    in
        Eval.evalCallExpr (Eval.withThis (Eval.getInitialRegs()) thisObj) fnObj argsList
    end

fun fnLength (regs:Mach.REGS) 
             (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val Mach.Obj { magic, ... } = nthAsObj vals 0
        val len = case !magic of                           
                      SOME (Mach.Function {func=Ast.Func{ty, ...}, ...}) => (length (#params ty))
                    | SOME (Mach.NativeFunction {length, ...}) => length
                    | _ => error ["wrong kind of magic to fnLength"]
    in
        Eval.newUInt (Word32.fromInt len)
    end


(* Given a string and a position in that string, return the
 * numeric value of the character at that position in the
 * string.
 *
 * magic native function charCodeAt(s : string, pos : uint) : uint;
 *)
fun charCodeAt (regs:Mach.REGS) 
               (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val s = nthAsUstr vals 0
        val i = nthAsUInt vals 1
    in
        Eval.newUInt (Word32.fromInt (Ustring.charCodeAt s (Word32.toInt i)))
    end


(*
 * Given a numeric character value, return a string of length 1
 * whose element 0 is the character with that same value.
 *
 * magic native function fromCharCode(ch : uint) : string;
 *)
fun fromCharCode (regs:Mach.REGS) 
                 (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val i = nthAsUInt vals 0
    in
        Eval.newString (Ustring.fromCharCode (Word32.toInt (Word32.andb(i, 0wx1FFFFF))))
    end


(* 
 * Given a string object, return the number of characters in the
 * string.
 *
 * magic native function stringLength(s : string) : uint;
 *)
fun stringLength (regs:Mach.REGS) 
                 (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val s = nthAsUstr vals 0
    in
        Eval.newUInt (Word32.fromInt (Ustring.stringLength s))
    end
    

(* 
 * Given two string objects A and B , return a new string object
 * containing the characters from A followed by the characters
 * from B.
 * 
 * magic native function stringAppend(a : string, b : string) : string;
 *)
fun stringAppend (regs:Mach.REGS) 
                 (vals:Mach.VAL list)
    : Mach.VAL = 
    let
        val a = nthAsUstr vals 0
        val b = nthAsUstr vals 1
    in
        Eval.newString (Ustring.stringAppend a b)
    end
    
    
(*
 * Get the byte at index idx.  Unspecified behavior if that index
 * does not have data (it's OK to crash the system).
 * 
 * magic native function getByteArrayByte(ba : ByteArray!, idx : uint) : uint;
 *)
fun getByteArrayByte (regs:Mach.REGS) 
                     (vals:Mach.VAL list)
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
fun setByteArrayByte (regs:Mach.REGS) 
                     (vals:Mach.VAL list)
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

fun eval (regs:Mach.REGS) 
         (vals:Mach.VAL list) 
    : Mach.VAL =
    if length vals = 0 
    then Mach.Undef 
    else 
        let
            val x = rawNth vals 0
        in
            if not (Mach.isString x)
            then x
            else 
                let
                    val s = nthAsUstr vals 0
                    val lines = [Ustring.sourceFromUstring s] (* FIXME: split lines *)
                    (* 
                     * FIXME: catch parse errors and throw a user SyntaxError
                     * exception here once natives grow the ability to throw 
                     * user exceptions. 
                     *)
                    fun str s = Eval.newString (Ustring.fromString s)
                    val prog = Parser.parseLines lines 
                        handle LogErr.LexError le => raise Eval.ThrowException (str le)
                             | LogErr.ParseError pe => raise Eval.ThrowException (str pe)
                in
                    (* 
                     * FIXME: maybe don't want to permit the full set of 
                     * program constructs (classes?) so possibly sanitize the
                     * result of parsing a bit, strip out some sorts of things...
                     *)
                    Eval.evalProgram regs (Verify.verifyProgram (Defn.defProgram prog))
                    handle LogErr.DefnError de => raise Eval.ThrowException (str de)
                         | LogErr.VerifyError ve => raise Eval.ThrowException (str ve)
                         | LogErr.NameError ne => raise Eval.ThrowException (str ne)
                         | LogErr.EvalError ee => raise Eval.ThrowException (str ee)
                end
        end
    
(* 
 * 15.1.2.2
 * intrinsic native function parseInt(string:string, radix:int)
 *)
fun parseInt (regs:Mach.REGS) 
             (vals:Mach.VAL list) 
    : Mach.VAL =
    let
        val strVal = rawNth vals 0 
        val radixVal = rawNth vals 1
        val str = Eval.toUstring strVal
        val radix = Int32.toInt (Eval.toInt32 radixVal)
        val chars = Ustring.explode str
        fun stripWs c = case c of 
                            x::xs => if Ustring.isWs x 
                                     then stripWs xs
                                     else x::xs
                          | [] => []
        val chars = map Ustring.charCodeOf (stripWs chars)
        val (sign, chars) = case chars of 
                                (0x2D (* '-' *) :: rest) => (~1.0, rest)
                              | (0x2B (* '+' *) :: rest) => (1.0, rest)
                              | _ => (1.0, chars)

        fun nan _ = Eval.newDouble (0.0 / 0.0)

        fun digitVal (charcode:int) 
            : int option = 
            if 0x2F < charcode andalso charcode < 0x3A
            then SOME (charcode - 0x30)
            else if 0x60 < charcode andalso charcode < 0x7B
            then SOME (10 + (charcode - 0x61))
            else if 0x40 < charcode andalso charcode < 0x5B
            then SOME (10 + (charcode - 0x41))
            else NONE

        fun finishWith (accum:LargeInt.int option) 
            : Mach.VAL = 
            case accum of 
                NONE => nan()
              | SOME li => Eval.newDouble (Real64.* (sign, (Real64.fromLargeInt li)))

        fun parseWithRadix (accum:LargeInt.int option) (radix:int) (chars:int list) 
            : Mach.VAL = 
            case chars of 
                [] => finishWith accum
              | x::xs => case digitVal x of 
                             NONE => finishWith accum
                           | SOME v => if v < radix
                                       then case accum of 
                                                NONE => parseWithRadix (SOME (Int.toLarge v)) radix xs 
                                              | SOME acc => parseWithRadix 
                                                                (SOME (LargeInt.+ 
                                                                       (LargeInt.* (acc, (LargeInt.fromInt radix)), 
                                                                        (Int.toLarge v)))) 
                                                                radix xs
                                       else finishWith accum

        fun parseWithInferredHex (radix:int) (chars:int list) 
            : Mach.VAL = 
            case chars of 
                [] => nan()
              (* Handle 0x... and 0X... prefixes *)
              | 0x30 :: 0x78 :: rest => parseWithRadix NONE 16 rest
              | 0x30 :: 0x58 :: rest => parseWithRadix NONE 16 rest
              | _ => parseWithRadix NONE radix chars

        fun parseWithInferredOctalAndHex (radix:int) (chars:int list) 
            : Mach.VAL = 
            case chars of 
                [] => nan()
              | 0x30 :: _ => parseWithInferredHex 8 chars
              | _ => parseWithInferredHex radix chars
    in
        if radix = 0 
        then parseWithInferredOctalAndHex 10 chars
        else if radix < 2 orelse radix > 36
        then nan()
        else if radix = 16 
        then parseWithInferredHex radix chars
        else parseWithRadix NONE radix chars
    end
    

(* 
 * 15.1.2.3 
 * intrinsic native function parseFloat(string:string);
 *)
fun parseFloat (regs:Mach.REGS) 
               (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::parseFloat"]


(* 
 * 15.1.2.4
 * intrinsic native function isNaN( number:* ):boolean;
 *)
fun isNaN (regs:Mach.REGS) 
          (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::isNaN"]


(* 
 * 15.1.2.5
 * intrinsic native function isFinite( number:* ):boolean;
 *)
fun isFinite (regs:Mach.REGS) 
             (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::isFinite"]


(* 
 * 15.1.3.1
 * intrinsic native function decodeURI(encodedURI);
 *)
fun decodeURI (regs:Mach.REGS) 
              (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::decodeURI"]


(* 
 * 15.1.3.2
 * intrinsic native function decodeURIComponent(encodedURIComponent);
 *)
fun decodeURIComponent (regs:Mach.REGS) 
                       (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::decodeURIComponent"]

(* 
 * 15.1.3.3
 * intrinsic native function encodeURI(uri);
 *)
fun encodeURI (regs:Mach.REGS) 
              (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::encodeURI"]


(* 
 * 15.1.3.4
 * intrinsic native function encodeURIComponent(uriComponent);
 *)
fun encodeURIComponent (regs:Mach.REGS) 
                       (vals:Mach.VAL list) 
    : Mach.VAL =
    LogErr.unimplError ["intrinsic::encodeURIComponent"]

(* 
 * intrinsic function get (obj: Object!, name: string) : *
 *)
fun get (regs:Mach.REGS) 
        (vals:Mach.VAL list)
    : Mach.VAL = 
    (* FIXME: arg #1 should be a Name, and convert to Ast.Name. *)
    Eval.getValueOrVirtual 
        (nthAsObj vals 0) 
        (nthAsName vals 1)
        false

(* 
 * intrinsic function set (obj: Object!, name: string, val: * ) : void
 *)
fun set (regs:Mach.REGS) 
        (vals:Mach.VAL list)
    : Mach.VAL = 
    (* FIXME: arg #1 should be a Name, and convert to Ast.Name. *)
    (Eval.setValueOrVirtual 
         (nthAsObj vals 0) 
         (nthAsName vals 1)
         (rawNth vals 2) 
         false;
     Mach.Undef)

(* 
 * Return the current time in milliseconds since January 1 1970 00:00:00 UTC.
 * 
 * static intrinsic native function now() : double;
 *)

fun now (regs:Mach.REGS) 
        (vals:Mach.VAL list)
    : Mach.VAL = 
    Eval.newDouble (Real.realFloor ((Time.toReal (Time.now())) * 1000.0))

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

fun random (regs:Mach.REGS) 
           (v:Mach.VAL list)
    : Mach.VAL =
    Eval.newDouble (Random.randReal random_state)

fun unaryDoubleFn (f:(Real64.real -> Real64.real)) : 
    (Mach.REGS -> (Mach.VAL list) -> Mach.VAL) =
    fn regs => 
    fn vals => if length vals = 0 
               then Eval.newDouble (0.0/0.0)
               else Eval.newDouble (f (Eval.toDouble (rawNth vals 0)))
         
fun binaryDoubleFn (f:((Real64.real * Real64.real) -> Real64.real)) : 
    (Mach.REGS -> (Mach.VAL list) -> Mach.VAL) =
    fn regs => 
    fn vals => if length vals = 0 orelse length vals = 1 
               then Eval.newDouble (0.0/0.0)
               else Eval.newDouble (f ((Eval.toDouble (rawNth vals 0)),
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

fun print (regs:Mach.REGS) 
          (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        fun printOne v = TextIO.print (Ustring.toAscii (Eval.toUstring v))
    in
        List.app printOne vals; 
        TextIO.print "\n";
        Mach.Undef
    end

fun load (regs:Mach.REGS) 
         (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val fname = Ustring.toFilename (nthAsUstr vals 0)
    in        
        Eval.evalProgram regs (Verify.verifyProgram (Defn.defProgram (Parser.parseFile fname)))
        handle x => raise Eval.ThrowException (Eval.newString (Ustring.fromString "error while loading"));
        Mach.Undef
    end
    
fun assert (regs:Mach.REGS) 
           (vals:Mach.VAL list) 
    : Mach.VAL = 
    if nthAsBool vals 0
    then Mach.Undef
    else error ["intrinsic::assert() failed"]

fun typename (regs:Mach.REGS) 
             (vals:Mach.VAL list) 
    : Mach.VAL = 
    Eval.newString 
    (case hd vals of 
        Mach.Null => Ustring.null_
      | Mach.Undef => Ustring.undefined_
      | Mach.Object (Mach.Obj ob) => 
        (case !(#magic ob) of
             NONE => Ustring.object_
           | SOME (Mach.UInt _) => Ustring.uint_
           | SOME (Mach.Int _) => Ustring.int_
           | SOME (Mach.Double _) => Ustring.double_
           | SOME (Mach.Decimal _) => Ustring.decimal_
           | SOME (Mach.ByteArray _) => Ustring.bytearray_
           | SOME (Mach.String _) => Ustring.string_
           | SOME (Mach.Boolean _) => Ustring.bool_
           | SOME (Mach.Namespace _) => Ustring.namespace_
           | SOME (Mach.Class _) => Ustring.class_
           | SOME (Mach.Interface _) => Ustring.interface_
           | SOME (Mach.Function _) => Ustring.function_
           | SOME (Mach.Type _) => Ustring.type_
           | SOME (Mach.NativeFunction _) => Ustring.native_function_))

fun dumpFunc (regs:Mach.REGS) 
             (vals:Mach.VAL list)
    : Mach.VAL = 
    let
        val v = rawNth vals 0
    in
        if Mach.isFunction v 
        then 
            case Mach.needMagic v of 
                Mach.Function { func, ... } => Pretty.ppFunc func
              | _ => ()
        else
            ();
        Mach.Undef
    end

fun inspect (regs:Mach.REGS) 
            (vals:Mach.VAL list)
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
              | Mach.ClassTag n => "<Class " ^ (LogErr.name n) ^ ">"
              | Mach.NoTag => "<NoTag>"

        (* FIXME: elaborate printing of type expressions. *)
        fun typ t = Verify.typeToString t
        fun mag m = case m of 
                        Mach.String s => ("\"" ^ (Ustring.toAscii s) ^ "\"")
                      | m => Ustring.toAscii (Eval.magicToUstring m)
                                 
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
                        p indent ["   prop = ", LogErr.name n, ": ", typ ty, att attrs,  " = "];
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
                NameMap.appi prop (!props);
                p indent ["          ] }"]; nl()
            end

        val v = rawNth vals 0
        val d = if length vals > 1 then nthAsInt vals 1 else 1
    in
        printVal 0 d v;
        Mach.Undef
    end

fun proto (regs:Mach.REGS) 
          (vals:Mach.VAL list) 
    : Mach.VAL = 
    let
        val Mach.Obj { proto, ... } = nthAsObj vals 0
    in
        !proto
    end


(* Register all the native functions in this file. *)
fun registerNatives _ = 
    let
        fun addFn (len:int) (name:Ast.NAME) f = 
            Mach.registerNativeFunction name {length=len, func=f}
    in
        addFn 2 Name.magic_construct construct;
        addFn 1 Name.magic_getClassName getClassName;
        addFn 1 Name.magic_getPrototype getPrototype;
        addFn 2 Name.magic_hasOwnProperty hasOwnProperty;
        addFn 2 Name.magic_getPropertyIsDontEnum getPropertyIsDontEnum;
        addFn 2 Name.magic_getPropertyIsDontDelete getPropertyIsDontDelete;
        addFn 3 Name.magic_setPropertyIsDontEnum setPropertyIsDontEnum;

        addFn 2 Name.magic_toPrimitive toPrimitive;
        addFn 1 Name.magic_isPrimitive isPrimitive;
        addFn 2 Name.magic_defaultValue defaultValue;

        addFn 2 Name.magic_bindInt bindInt;
        addFn 2 Name.magic_bindUInt bindUInt;
        addFn 2 Name.magic_bindDouble bindDouble;
        addFn 2 Name.magic_bindDecimal bindDecimal;
        addFn 2 Name.magic_bindBoolean bindBoolean;
        addFn 2 Name.magic_bindString bindString;

        addFn 1 Name.magic_newInt newInt;
        addFn 1 Name.magic_newUInt newUInt;
        addFn 1 Name.magic_newDouble newDouble;

        addFn 3 Name.magic_apply apply;
        addFn 1 Name.magic_fnLength fnLength;

        addFn 2 Name.magic_charCodeAt charCodeAt;
        addFn 1 Name.magic_fromCharCode fromCharCode;
        addFn 1 Name.magic_stringLength stringLength;
        addFn 2 Name.magic_stringAppend stringAppend;
        addFn 2 Name.magic_getByteArrayByte getByteArrayByte;
        addFn 3 Name.magic_setByteArrayByte setByteArrayByte;

        addFn 1 Name.intrinsic_eval eval;
        addFn 2 Name.intrinsic_parseInt parseInt;
        addFn 1 Name.intrinsic_parseFloat parseFloat;
        addFn 1 Name.intrinsic_isNaN isNaN;
        addFn 1 Name.intrinsic_isFinite isFinite;
        addFn 1 Name.intrinsic_decodeURI decodeURI;
        addFn 1 Name.intrinsic_decodeURIComponent decodeURIComponent;
        addFn 1 Name.intrinsic_encodeURI encodeURI;
        addFn 1 Name.intrinsic_encodeURIComponent encodeURIComponent;

        addFn 2 Name.intrinsic_get get;
        addFn 3 Name.intrinsic_set set;

        (* FIXME: stubs to get double loading. Implement. *)
        addFn 1 Name.intrinsic_toFixedStep10 (fn _ => fn _ => Eval.newString Ustring.empty);
        addFn 1 Name.intrinsic_toExponential (fn _ => fn _ => Eval.newString Ustring.empty);
        addFn 1 Name.intrinsic_toPrecision (fn _ => fn _ => Eval.newString Ustring.empty);

        (* FIXME: stubs to get Date loading. Implement. *)
        addFn 0 Name.intrinsic_now now;
        addFn 0 Name.nons_LocalTZA (fn _ => fn _ => Eval.newDouble 0.0);
        addFn 0 Name.nons_DaylightSavingsTA (fn _ => fn _ => Eval.newDouble 0.0);
       
        (* Math.es natives *) 
        addFn 1 Name.intrinsic_abs abs;
        addFn 1 Name.intrinsic_acos acos;
        addFn 1 Name.intrinsic_asin asin;
        addFn 1 Name.intrinsic_atan atan;
        addFn 2 Name.intrinsic_atan2 atan2;
        addFn 1 Name.intrinsic_ceil ceil;
        addFn 1 Name.intrinsic_cos cos;
        addFn 1 Name.intrinsic_exp exp;
        addFn 1 Name.intrinsic_floor floor;
        addFn 1 Name.intrinsic_log log;
        addFn 2 Name.intrinsic_pow pow;
        addFn 1 Name.intrinsic_random random;
        addFn 1 Name.intrinsic_round round;
        addFn 1 Name.intrinsic_sin sin;
        addFn 1 Name.intrinsic_sqrt sqrt;
        addFn 1 Name.intrinsic_tan tan;

        addFn 1 Name.intrinsic_print print;
        addFn 1 Name.intrinsic_load load;
        addFn 1 Name.intrinsic_assert assert;
        addFn 1 Name.intrinsic_typename typename;
        addFn 1 Name.intrinsic_dumpFunc dumpFunc;
        addFn 1 Name.intrinsic_inspect inspect;
        addFn 1 Name.intrinsic_proto proto
    end

end
