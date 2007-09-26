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


fun nthAsName (regs:Mach.REGS)
              (vals:Mach.VAL list)
              (n:int)
    : Ast.NAME =
    let
        val v = rawNth vals n
    in
        Eval.needNameOrString regs v
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


fun propQuery (regs:Mach.REGS)
              (vals:Mach.VAL list)
              (f:(Mach.PROP_BINDINGS -> Ast.NAME -> bool))
    : Mach.VAL =
    let
        val Mach.Obj { props, ...} = nthAsObj vals 0
        val n = nthAsName regs vals 1
    in
        Eval.newBoolean regs (f props n)
    end

fun arrayToList (regs:Mach.REGS)
                (arr:Mach.OBJ)
    : Mach.VAL list =
    let
        val len = Word32.toInt
                      (Eval.toUInt32 regs
                           (Eval.getValue regs arr Name.nons_length))
        fun build i vs =
            if i < 0
            then vs
            else
                let
                    val n = Name.nons (Ustring.fromInt i)
                    val curr = if Eval.hasValue arr n
                               then Eval.getValue regs arr n
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
        val args = arrayToList regs (nthAsObj vals 1)
    in
        Eval.constructClassInstance regs obj cls args
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
                         | Mach.ClassTag ity => (#id (#name ity))
                         | Mach.NoTag => Ustring.Object_)
    in
        Eval.newString regs ustr
    end


(*
 * Meta-object interface:
 * Retrieve the class of an object instance.
 *
 * magic native function getClassOfObject(o: Object!) : Class;
 *)
fun getClassOfObject (regs:Mach.REGS)
                     (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val Mach.Obj { magic, tag, ... } = nthAsObj vals 0
        val globalScope = Eval.getGlobalScope regs
    in
(*  This is wrong, because eg "Number" is classified as "Double" because it
 *  stores a double value magically.
        case !magic of
            SOME (Mach.Function _) => Eval.findVal globalScope Name.nons_Function
          | SOME (Mach.NativeFunction _) => Eval.findVal globalScope Name.nons_Function
          | SOME (Mach.String _) => Eval.findVal globalScope Name.intrinsic_string
          | SOME (Mach.Decimal _) => Eval.findVal globalScope Name.intrinsic_decimal
          | SOME (Mach.Int _) => Eval.findVal globalScope Name.intrinsic_int
          | SOME (Mach.UInt _) => Eval.findVal globalScope Name.intrinsic_uint
          | SOME (Mach.Double _) => Eval.findVal globalScope Name.intrinsic_double
          | SOME (Mach.Boolean _) => Eval.findVal globalScope Name.intrinsic_boolean
          | _ =>
            (case tag of
                 Mach.ObjectTag _ => Eval.findVal globalScope Name.nons_Object
               | Mach.ArrayTag _ => Eval.findVal globalScope Name.nons_Array
               | Mach.FunctionTag _ => Eval.findVal globalScope Name.nons_Function
               | Mach.ClassTag name => Eval.findVal globalScope name
               | Mach.NoTag => Eval.findVal globalScope Name.nons_Object)
*)
        case tag of
            Mach.ObjectTag _ => Eval.findVal regs globalScope Name.nons_Object
          | Mach.ArrayTag _ => Eval.findVal regs globalScope Name.nons_Array
          | Mach.FunctionTag _ => Eval.findVal regs globalScope Name.nons_Function
          | Mach.ClassTag ity => Mach.Object (Eval.instanceClass regs ity)
          | Mach.NoTag => Eval.findVal regs globalScope Name.nons_Object
    end


(*
 * Meta-object interface:
 * Retrieve the possibly null base class of cls.
 *
 * magic native function getSuperClass(cls : Class!) : Class;
 *)
fun getSuperClass (regs:Mach.REGS)
                  (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val { cls, env, ... } = Mach.needClass (rawNth vals 0)
        val Ast.Cls { extends, ... } = cls
        val regs = Eval.withScope regs env 
    in
        (case extends of 
             SOME ty => Mach.Object 
                            (Eval.instanceClass 
                                 regs (AstQuery.needInstanceType (Eval.evalTy regs ty)))
           | _ => Mach.Null)
    end


(*
 * Meta-object interface:
 * Retrieve the possibly null kth implemented interface of cls.
 *
 * magic native function getClassExtends(cls : Class!, k: uint) : Class;
 *)
fun getImplementedInterface (regs:Mach.REGS)
                            (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val { cls, env, ... } = Mach.needClass (rawNth vals 0)
        val k = Word32.toInt (nthAsUInt vals 1)
        val Ast.Cls { implements, ... } = cls
    in
        if k >= (List.length implements) then
            Mach.Null
        else
            Mach.Object 
                (Eval.instanceInterface 
                     regs (AstQuery.needInstanceType 
                               (Eval.evalTy 
                                    regs (List.nth(implements, k)))))
    end


(* 
 * Meta-object interface:
 * Retrieve the possibly null kth base interface of iface.
 *
 * magic native function getSuperInterface(iface: Interface!, k: uint): Interface;
 *)
fun getSuperInterface (regs:Mach.REGS)
                      (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val { iface, env, ... } = Mach.needInterface (rawNth vals 0)
        val k = Word32.toInt(nthAsUInt vals 1)
        val Ast.Iface { extends, ... } = iface
    in
        if k >= (List.length extends) then
            Mach.Null
        else
            Mach.Object
                (Eval.instanceInterface 
                     regs (AstQuery.needInstanceType 
                               (Eval.evalTy 
                                    regs (List.nth(extends, k)))))
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
    propQuery regs vals Mach.hasProp


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
        propQuery regs vals f
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
        propQuery regs vals f
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
    Eval.newBoolean regs (Eval.isPrimitive (rawNth vals 0))

fun toPrimitive (regs:Mach.REGS)
                (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val v = rawNth vals 0
        val hint = rawNth vals 1
    in
        if Mach.isString hint
        then Eval.toPrimitive regs v (Eval.toUstring regs hint)
        else Eval.toPrimitive regs v Ustring.empty
    end

fun defaultValue (regs:Mach.REGS)
                 (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val obj = nthAsObj vals 0
        val hint = nthAsUstr vals 1
    in
        Eval.defaultValue regs obj hint
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
    convertAndBindMagic vals (Eval.toUInt32 regs) (Mach.UInt)

fun bindInt (regs:Mach.REGS)
            (vals:Mach.VAL list)
    : Mach.VAL =
    convertAndBindMagic vals (Eval.toInt32 regs) (Mach.Int)

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
    convertAndBindMagic vals 
                        (Eval.toDecimal
                             Decimal.defaultPrecision
                             Decimal.defaultRoundingMode) 
                        (Mach.Decimal)
    
fun bindString (regs:Mach.REGS)
               (vals:Mach.VAL list)
    : Mach.VAL =
    convertAndBindMagic vals (Eval.toUstring regs) Mach.String

fun newBoolean (regs:Mach.REGS)
               (vals:Mach.VAL list)
    : Mach.VAL =
    Eval.newBoolean regs (Eval.toBoolean (rawNth vals 0))

fun newString (regs:Mach.REGS)
              (vals:Mach.VAL list)
    : Mach.VAL =
    Eval.newString regs (Eval.toUstring regs (rawNth vals 0))

fun newInt (regs:Mach.REGS)
           (vals:Mach.VAL list)
    : Mach.VAL =
    Eval.newInt regs (Eval.toInt32 regs (rawNth vals 0))

fun newUInt (regs:Mach.REGS)
            (vals:Mach.VAL list)
    : Mach.VAL =
    Eval.newUInt regs (Eval.toUInt32 regs (rawNth vals 0))

fun newDouble (regs:Mach.REGS)
              (vals:Mach.VAL list)
    : Mach.VAL =
    Eval.newDouble regs (Eval.toDouble (rawNth vals 0))

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
        val argsList = arrayToList regs argsObj
    in
        Eval.evalCallExpr (Eval.withThis regs thisObj) fnObj argsList
    end

fun fnLength (regs:Mach.REGS)
             (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val Mach.Obj { magic, ... } = nthAsObj vals 0
        val len = 
            case !magic of
                SOME (Mach.Function ({ func = Ast.Func { ty, ... }, ...}))
                => AstQuery.minArgsOfFuncTy ty
              | SOME (Mach.NativeFunction {length, ...}) => length
                    | _ => error ["wrong kind of magic to fnLength"]
    in
        Eval.newUInt regs (Word32.fromInt len)
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
        Eval.newUInt 
            regs
            (Word32.fromInt 
                 (Ustring.charCodeAt s (Word32.toInt i)))
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
        Eval.newString 
            regs 
            (Ustring.fromCharCode 
                 (Word32.toInt 
                      (Word32.andb(i, 0wx1FFFFF))))
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
        Eval.newUInt regs (Word32.fromInt (Ustring.stringLength s))
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
        Eval.newString regs (Ustring.stringAppend a b)
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
        Eval.newUInt regs 
                     (Word32.fromInt 
                          (Word8.toInt 
                               (Word8Array.sub 
                                    (b, (Word32.toInt i)))))
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
                    fun str s = Eval.newString regs (Ustring.fromString s)
                    val frag = Parser.parseLines lines
                        handle LogErr.LexError le => raise Eval.ThrowException (str le)
                             | LogErr.ParseError pe => raise Eval.ThrowException (str pe)
                    val (prog, frag) = (Defn.defTopFragment (#prog regs) frag
                                        handle
                                        LogErr.DefnError de => raise Eval.ThrowException (str de))
                    val frag = (Verify.verifyTopFragment prog true frag
                                handle
                                LogErr.VerifyError ve => raise Eval.ThrowException (str ve))

                    val regs = Eval.withProg regs prog
                in
                    (*
                     * FIXME: maybe don't want to permit the full set of
                     * program constructs (classes?) so possibly set a flag in defn's env
                     * that says we're in an eval context, and should only permit a 
                     * subset of definitions.
                     *)
                    
                    Eval.evalTopFragment regs frag
                    handle 
                    LogErr.NameError ne => raise Eval.ThrowException (str ne)
                  | LogErr.EvalError ee => raise Eval.ThrowException (str ee)
                end
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
 * intrinsic function get (obj: Object!, name: string) : *
 *)
fun get (regs:Mach.REGS)
        (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val obj = (nthAsObj vals 0)
        val name = (nthAsName regs vals 1)
        fun propNotFound (curr:Mach.OBJ) : Mach.VAL =
            Eval.throwRefErr1 regs ["getting nonexistent property ", LogErr.name name]
    in
        Eval.getValueOrVirtual regs obj name false propNotFound
    end

(*
 * intrinsic function set (obj: Object!, name: string, val: * ) : void
 *)
fun set (regs:Mach.REGS)
        (vals:Mach.VAL list)
    : Mach.VAL =
    (Eval.setValueOrVirtual
         regs
         (nthAsObj vals 0)
         (nthAsName regs vals 1)
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
    Eval.newDouble regs 
                   (Real.realFloor 
                        ((Time.toReal (Time.now())) * 1000.0))

val random_state = Random.rand (37, 79)

fun random (regs:Mach.REGS)
           (v:Mach.VAL list)
    : Mach.VAL =
    Eval.newDouble regs (Random.randReal random_state)

fun unaryDoubleFn (f:(Real64.real -> Real64.real)) :
    (Mach.REGS -> (Mach.VAL list) -> Mach.VAL) =
    fn regs =>
    fn vals => if length vals = 0
               then Eval.newDouble regs (0.0/0.0)
               else Eval.newDouble regs (f (Eval.toDouble (rawNth vals 0)))

fun unaryDecimalFn (f:(Decimal.DEC -> Decimal.DEC)) :
    (Mach.REGS -> (Mach.VAL list) -> Mach.VAL) =
    fn regs =>
    fn vals => if length vals = 0
               then Eval.newDecimal regs Decimal.NaN
               else Eval.newDecimal regs (f (Eval.toDecimal Decimal.defaultPrecision Decimal.defaultRoundingMode (rawNth vals 0)))

fun binaryDoubleFn (f:((Real64.real * Real64.real) -> Real64.real)) :
    (Mach.REGS -> (Mach.VAL list) -> Mach.VAL) =
    fn regs =>
    fn vals => if length vals = 0 orelse length vals = 1
               then Eval.newDouble regs (0.0/0.0)
               else Eval.newDouble regs (f ((Eval.toDouble (rawNth vals 0)),
                                            (Eval.toDouble (rawNth vals 1))))

fun binaryDecimalFn (f:((Decimal.DEC * Decimal.DEC) -> Decimal.DEC)) :
    (Mach.REGS -> (Mach.VAL list) -> Mach.VAL) =
    fn regs =>
    fn vals => if length vals = 0 orelse length vals = 1
               then Eval.newDecimal regs Decimal.NaN
               else Eval.newDecimal regs (f ((Eval.toDecimal Decimal.defaultPrecision Decimal.defaultRoundingMode (rawNth vals 0)),
                                        (Eval.toDecimal Decimal.defaultPrecision Decimal.defaultRoundingMode (rawNth vals 1))))

val ceilDouble = unaryDoubleFn Real64.realCeil
val ceilDecimal = unaryDecimalFn Decimal.ceil
val floorDouble = unaryDoubleFn Real64.realFloor
val floorDecimal = unaryDecimalFn Decimal.floor
val roundDouble = unaryDoubleFn Real64.realRound
val roundDecimal = unaryDecimalFn Decimal.round

val acosDouble = unaryDoubleFn Math.acos
val acosDecimal = unaryDecimalFn Decimal.acos
val asinDouble = unaryDoubleFn Math.asin
val asinDecimal = unaryDecimalFn Decimal.asin
val atanDouble = unaryDoubleFn Math.atan
val atanDecimal = unaryDecimalFn Decimal.atan
val atan2Double = binaryDoubleFn Math.atan2
val atan2Decimal = binaryDecimalFn Decimal.atan2
val cosDouble = unaryDoubleFn Math.cos
val cosDecimal = unaryDecimalFn Decimal.cos
val expDouble = unaryDoubleFn Math.exp
val expDecimal = unaryDecimalFn Decimal.exp
val logDouble = unaryDoubleFn Math.ln
val logDecimal = unaryDecimalFn Decimal.log
val sinDouble = unaryDoubleFn Math.sin
val sinDecimal = unaryDecimalFn Decimal.sin
val sqrtDouble = unaryDoubleFn Math.sqrt
val sqrtDecimal = unaryDecimalFn Decimal.sqrt
val tanDouble = unaryDoubleFn Math.tan
val tanDecimal = unaryDecimalFn Decimal.tan
val powDouble = binaryDoubleFn Math.pow
val powDecimal = binaryDecimalFn Decimal.pow

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


(* Takes a double and 0u or 1u and returns the high part
 * (sign+expt+high bits of significand) or low part (low 32 bits of
 * significand), respectively, as an unsigned integer.
 *
 * Uses LargeInt, among other things.  There is an optional library
 * in SML for unpacking doubles (PackReal), but it's not supported
 * by SML/NJ from what I can tell.
 *
 * This function is here to support ESC, but it is generally useful
 * and it should probably be added to the standard set of builtins.
 *)
fun explodeDouble (regs:Mach.REGS)
                  (vals:Mach.VAL list)
    : Mach.VAL =
    let val p1 = nthAsDouble vals 0
        val p2 = nthAsUInt vals 1
        val r  = Real64.toManExp p1
        val k  = Real64.toLargeInt IEEEReal.TO_ZERO ((#man r) * 9007199254740992.0)
        fun assemble hi lo =
            Word32.orb(Word32.<<(Word32.fromLargeInt hi, 0w16), Word32.fromLargeInt lo)

    in
        Eval.newUInt regs 
                     (if p2 = 0w0 then
                          let val hi_lo    = IntInf.andb(IntInf.~>>(k, 0w32), IntInf.fromInt 65535)
                              val sign_exp = IntInf.orb(IntInf.fromInt(if p1 < 0.0 then 2048 else 0),
                                                        IntInf.fromInt((#exp r) + 1022))
                              val hi_hi    = IntInf.orb(IntInf.<<(sign_exp, 0w4),
                                                        IntInf.andb(IntInf.~>>(k, 0w48), IntInf.fromInt 15))
                          in
                              assemble hi_hi hi_lo
                          end
                      else
                          let val lo_lo = IntInf.andb(k, IntInf.fromInt 65535)
                              val lo_hi = IntInf.andb(IntInf.~>>(k, 0w16), IntInf.fromInt 65535)
                          in
                              assemble lo_hi lo_lo
                          end)
    end


(* Some helpers not specified in the wiki at the moment. Maybe get rid
 * of them eventually? *)

fun print (regs:Mach.REGS)
          (vals:Mach.VAL list)
    : Mach.VAL =
    let
        fun printOne v = TextIO.print (Ustring.toAscii (Eval.toUstring regs v))
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
        val (prog, frag) = 
            (* (Verify.verifyProgram *) 
            Defn.defFragment 
                (Defn.mkTopEnv (#prog regs)) 
                (Parser.parseFile fname)
            (* ) *)
            handle x => 
                   raise Eval.ThrowException 
                             (Eval.newString regs
                                  (Ustring.fromString "error while loading"))
    in
        (* 
         * FIXME: This is a bit odd. In eval(), we probably -- possibly? -- don't
         * want the eval'ed string extending the program fixtures. But possibly
         * we want load() to do so. Or not? Need to discuss.
         *)
        Eval.evalFragment (Eval.withProg regs prog) frag
            handle x => 
                   raise Eval.ThrowException 
                             (Eval.newString regs
                                             (Ustring.fromString "error while loading"));
        Mach.Undef
    end

fun readFile (regs:Mach.REGS)
             (vals:Mach.VAL list)
    : Mach.VAL =
    let
        fun mkReader filename =
            let
                val stream = TextIO.openIn filename
            in
                fn _ => case TextIO.inputLine stream of
                            SOME line => (trace ["read line ", line]; SOME line)
                          | NONE => (TextIO.closeIn stream; NONE)
            end

        val fname = Ustring.toFilename (nthAsUstr vals 0)
        val reader = mkReader fname

        fun readSrc (src: Ustring.SOURCE)
            : Ustring.SOURCE =
            case reader() of
                NONE => src
              | SOME newSrc => readSrc (src @ (Ustring.fromSource newSrc))

        val str = implode (map Ustring.wcharToChar (readSrc Ustring.emptySource))

    in
        Eval.newString regs (Ustring.fromString str)
    end

fun writeFile (regs:Mach.REGS)
              (vals:Mach.VAL list)
    : Mach.VAL =
    let val s        = Ustring.toAscii (nthAsUstr vals 0)
        val filename = Ustring.toFilename (nthAsUstr vals 1)
        val out      = TextIO.openOut filename
    in
        TextIO.output(out, s);
        TextIO.closeOut out;
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
    Eval.newString regs
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
        val v = rawNth vals 0
        val d = if length vals > 1 then nthAsInt vals 1 else 1
    in
        Mach.inspect v d;
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

fun id (regs:Mach.REGS)
       (vals:Mach.VAL list)
    : Mach.VAL =
    let
        val Mach.Obj { ident, ... } = nthAsObj vals 0
    in
        Eval.newInt regs (Int32.fromInt ident)
    end


(* Register all the native functions in this file. *)
fun registerNatives _ =
    let
        fun addFn (len:int) (name:Ast.NAME) f =
            Mach.registerNativeFunction name {length=len, func=f}
    in
        addFn 2 Name.magic_construct construct;
        addFn 1 Name.magic_getClassName getClassName;
        addFn 1 Name.magic_getClassOfObject getClassOfObject;
        addFn 1 Name.magic_getSuperClass getSuperClass;
        addFn 2 Name.magic_getImplementedInterface getImplementedInterface;
        addFn 2 Name.magic_getSuperInterface getSuperInterface;
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

        addFn 1 Name.magic_newBoolean newBoolean;
        addFn 1 Name.magic_newString newString;
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
        addFn 1 Name.intrinsic_parseFloat parseFloat;

        addFn 2 Name.intrinsic_get get;
        addFn 3 Name.intrinsic_set set;

        (* FIXME: stubs to get double loading. Implement. *)
        addFn 1 Name.intrinsic_toFixedStep10 (fn regs => fn _ => Eval.newString regs Ustring.empty);
        addFn 1 Name.intrinsic_toExponential (fn regs => fn _ => Eval.newString regs Ustring.empty);
        addFn 1 Name.intrinsic_toPrecision (fn regs => fn _ => Eval.newString regs Ustring.empty);

        (* FIXME: stubs to get Date loading. Implement. *)
        addFn 0 Name.intrinsic_now now;
        addFn 0 Name.nons_LocalTZA (fn regs => fn _ => Eval.newDouble regs 0.0);
        addFn 0 Name.nons_DaylightSavingsTA (fn regs => fn _ => Eval.newDouble regs 0.0);

        (* Math.es natives *)
        addFn 1 Name.informative_acosDouble acosDouble;
        addFn 1 Name.informative_acosDecimal acosDecimal;
        addFn 1 Name.informative_asinDouble asinDouble;
        addFn 1 Name.informative_asinDecimal asinDecimal;
        addFn 1 Name.informative_atanDouble atanDouble;
        addFn 1 Name.informative_atanDecimal atanDecimal;
        addFn 2 Name.informative_atan2Double atan2Double;
        addFn 2 Name.informative_atan2Decimal atan2Decimal;
        addFn 1 Name.informative_ceilDouble ceilDouble;
        addFn 1 Name.informative_ceilDecimal ceilDecimal;
        addFn 1 Name.informative_cosDouble cosDouble;
        addFn 1 Name.informative_cosDecimal cosDecimal;
        addFn 1 Name.informative_expDouble expDouble;
        addFn 1 Name.informative_expDecimal expDecimal;
        addFn 1 Name.informative_floorDouble floorDouble;
        addFn 1 Name.informative_floorDecimal floorDecimal;
        addFn 1 Name.informative_logDouble logDouble;
        addFn 1 Name.informative_logDecimal logDecimal;
        addFn 2 Name.informative_powDouble powDouble;
        addFn 2 Name.informative_powDecimal powDecimal;
        addFn 1 Name.informative_roundDouble roundDouble;
        addFn 1 Name.informative_roundDecimal roundDecimal;
        addFn 1 Name.informative_sinDouble sinDouble;
        addFn 1 Name.informative_sinDecimal sinDecimal;
        addFn 1 Name.informative_sqrtDouble sqrtDouble;
        addFn 1 Name.informative_sqrtDecimal sqrtDecimal;
        addFn 1 Name.informative_tanDouble tanDouble;
        addFn 1 Name.informative_tanDecimal tanDecimal;

        addFn 1 Name.intrinsic_random random;

        addFn 1 Name.intrinsic_print print;
        addFn 1 Name.intrinsic_load load;
        addFn 1 Name.intrinsic_readFile readFile;
        addFn 2 Name.intrinsic_writeFile writeFile;

        addFn 1 Name.intrinsic_explodeDouble explodeDouble;
        addFn 1 Name.intrinsic_assert assert;
        addFn 1 Name.intrinsic_typename typename;
        addFn 1 Name.intrinsic_dumpFunc dumpFunc;
        addFn 1 Name.intrinsic_inspect inspect;
        addFn 1 Name.intrinsic_proto proto;
        addFn 1 Name.intrinsic_id id

    end

end
