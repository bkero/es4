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

fun rawNth (vals:Mach.VALUE list)
           (n:int)
    : Mach.VALUE =
    if n >= length vals
    then error ["trying to fetch arg #",
                (Int.toString n),
                " from arg list of length ",
                (Int.toString (length vals))]
    else
        List.nth (vals, n)


fun nthAsA (f:Mach.VALUE -> 'a)
           (vals:Mach.VALUE list)
           (n:int)
    : 'a =
    f (rawNth vals n)

fun nthAsObj (vals:Mach.VALUE list)
             (n:int)
    : Mach.OBJ =
    let
        fun f Mach.Undefined = error ["Wanted Object, got Undefined"]
          | f Mach.Null = error ["Wanted Object, got Null"]
          | f (Mach.Object ob) = ob
    in
        nthAsA f vals n
    end


fun nthAsObjAndClass (vals:Mach.VALUE list)
                   (n:int)
    : (Mach.OBJ * Ast.CLASS) =
    let
        val obj = nthAsObj vals n
        val c = Mach.needClass (Mach.Object obj)
    in
        (obj, c)
    end

fun nthAsUstr (vals:Mach.VALUE list)
             (n:int)
    : Ustring.STRING =
    Mach.needString (rawNth vals n)

fun nthAsName (regs:Mach.REGS)
              (vals:Mach.VALUE list)
              (n:int)
    : Ast.NAME =
    let
        val v = rawNth vals n
    in
        Eval.needNameOrString regs v
    end


fun nthAsFn (vals:Mach.VALUE list)
            (n:int)
    : Mach.FUN_CLOSURE =
    Mach.needFunction (rawNth vals n)


fun nthAsInt (regs:Mach.REGS)
             (vals:Mach.VALUE list)
             (n:int)
    : Int32.int =
    Eval.doubleToInt (Eval.toInt32 regs (rawNth vals n))


fun nthAsUInt (regs:Mach.REGS)
             (vals:Mach.VALUE list)
             (n:int)
    : Word32.word =
    Eval.doubleToWord (Eval.toUInt32 regs (rawNth vals n))


fun nthAsDouble (vals:Mach.VALUE list)
                (n:int)
    : Real64.real =
    Mach.needDouble (rawNth vals n)


fun nthAsBool (vals:Mach.VALUE list)
              (n:int)
    : bool =
    Mach.needBoolean (rawNth vals n)


fun propQuery (regs:Mach.REGS)
              (vals:Mach.VALUE list)
              (f:(Mach.PROPERTY_BINDINGS -> Ast.NAME -> bool))
    : Mach.VALUE =
    let
        val Mach.Obj { props, ...} = nthAsObj vals 0
        val n = nthAsName regs vals 1
    in
        Eval.newBoolean regs (f props n)
    end


(*
 * Given a class object, run the standard object-construction
 * protocol for it (and its base classes, initializers, settings,
 * ctors). Return the resulting instance, always an Object!
 *
 * helper native function construct(cls:Class!, args:[*]) : Object!;
 *)
fun construct (regs:Mach.REGS)
              (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val (obj, cls) = nthAsObjAndClass vals 0
        val args = Eval.arrayToList regs (nthAsObj vals 1)
    in
        Eval.constructClassInstance regs obj cls args
    end


(*
 * Retrieve the [[Class]] property of o
 *
 * informative native function getClassName(o : Object!) : string;
 *)
fun getClassName (regs:Mach.REGS)
                 (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Mach.Obj { tag, ... } = nthAsObj vals 0
    in
        Eval.newString regs (#id (Mach.nominalBaseOfTag tag))
    end


(*
 * Meta-object interface:
 * Retrieve the class of an object instance.
 *
 * helper native function getClassOfObject(o: Object!) : Class;
 *)
fun getClassOfObject (regs:Mach.REGS)
                     (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Mach.Obj { tag, ... } = nthAsObj vals 0
    in
        Eval.getValue regs (#global regs) (Mach.nominalBaseOfTag tag)
    end


(*
 * Meta-object interface:
 * Retrieve the possibly null base class of cls.
 *
 * helper native function getSuperClass(cls : Class!) : Class;
 *)
fun getSuperClass (regs:Mach.REGS)
                  (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val (classObj, Ast.Class { extends, ... }) = nthAsObjAndClass vals 0
        val regs = Eval.withScope regs (Eval.getClassScope regs classObj)
    in
        (case extends of 
             SOME ty => Mach.Object 
                            (Eval.instanceClass regs(Eval.evalTy regs ty))                        
           | _ => Mach.Null)
    end


(*
 * Meta-object interface:
 * Retrieve the possibly null kth implemented interface of cls.
 *
 * helper native function getClassExtends(cls : Class!, k: uint) : Class;
 *)
fun getImplementedInterface (regs:Mach.REGS)
                            (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val cls = Mach.needClass (rawNth vals 0)
        val k = Word32.toInt (nthAsUInt regs vals 1)
        val Ast.Class { implements, ... } = cls
    in
        if k >= (List.length implements) then
            Mach.Null
        else
            Mach.Object 
            (Eval.instanceInterface regs 
                                    (Eval.evalTy 
                                         regs (List.nth(implements, k))))
    end


(* 
 * Meta-object interface:
 * Retrieve the possibly null kth base interface of iface.
 *
 * helper native function getSuperInterface(iface: Interface!, k: uint): Interface;
 *)
fun getSuperInterface (regs:Mach.REGS)
                      (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Ast.Interface { extends, ... } = Mach.needInterface (rawNth vals 0)
        val k = Word32.toInt(nthAsUInt regs vals 1)
    in
        if k >= (List.length extends) then
            Mach.Null
        else
            Mach.Object
                (Eval.instanceInterface 
                     regs (Eval.evalTy 
                               regs (List.nth(extends, k))))
    end


(*
 * Retrieve the array of enumerable properties of o, in property creation order.
 *
 * helper native function getEnumerableIds(o : Object) : iterator::EnumerableIdArray;
 *)
fun getEnumerableIds (regs:Mach.REGS)
                     (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val v = rawNth vals 0
    in
        case v of
            Mach.Undefined => Eval.newArray regs []
          | Mach.Null => Eval.newArray regs []
          | Mach.Object (Mach.Obj { props, ... }) =>
            let
                val { bindings, ... } = !props
                val bindingList = NameMap.listItemsi bindings
                fun select (name, { seq, prop }) = 
                    case prop of 
                        { state = Mach.ValProp _,
                          attrs = { enumerable = true, removable, writable, fixed },
                          ty } => SOME (name, seq)
                      | _ => NONE
                val filteredList = List.mapPartial select bindingList
                val bindingArray = Array.fromList filteredList
                fun sort ((_, seq1), (_, seq2)) = Int.compare (seq2,seq1)
                val _ = ArrayQSort.sort sort bindingArray
                fun project ((name:Ast.NAME, _), (curr:Mach.VALUE list)) =
                    (* FIXME: what about numeric? string? *)
                    (Eval.newName regs name) :: curr
                val vals = Array.foldl project [] bindingArray
                (*val iterator = Eval.needObj regs (Eval.newArray regs vals)*)
            in
                Eval.newArray regs vals
            end
    end


(*
 * Retrieve the possibly null [[Prototype]] property of o
 *
 * helper native function getPrototype(o : Object!) : Object;
 *)
fun getPrototype (regs:Mach.REGS)
                 (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Mach.Obj { proto, ... } = nthAsObj vals 0
    in
        proto
    end


(*
 * Return true iff o has a local property named by p.
 *
 * helper native function hasOwnProperty(o : Object!, p : string) : Boolean;
 *)
fun hasOwnProperty (regs:Mach.REGS)
                   (vals:Mach.VALUE list)
    : Mach.VALUE =
    propQuery regs vals Mach.hasProp


(*
 * Return true if the property p does exists locally on o and its
 * Enumerable bit is set
 *
 * helper native function getPropertyIsEnumerable(o : Object!, p : string) : Boolean;
 *)
fun getPropertyIsEnumerable (regs:Mach.REGS)
                          (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        fun f props n =
            if Mach.hasProp props n
            then (#enumerable (#attrs (Mach.getProp props n)))
            else false
    in
        propQuery regs vals f
    end


(*
 * Return true if the property p does exists locally on o and its
 * removable bit is set
 *
 * helper native function getPropertyIsRemovable(o : Object!, p : string) : Boolean;
 *)

fun getPropertyIsRemovable (regs:Mach.REGS)
                           (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        fun f props n =
            if Mach.hasProp props n
            then (#removable (#attrs (Mach.getProp props n)))
            else false
    in
        propQuery regs vals f
    end


(* Provided that the property p exists locally on o, set its Enumerable
 * flag according to f.  If the property p does not exist locally on
 * o, it does nothing.
 *
 * helper native function setPropertyIsEnumerable(o : Object!, p : string, f : Boolean) : void;
 *)
fun setPropertyIsEnumerable (regs:Mach.REGS)
                          (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Mach.Obj { props, ...} = nthAsObj vals 0
        val n = nthAsName regs vals 1
        val b = nthAsBool vals 2
    in
        Mach.setPropEnumerable props n b;
        Mach.Undefined
    end

fun isPrimitive (regs:Mach.REGS)
                (vals:Mach.VALUE list)
    : Mach.VALUE =
    Eval.newBoolean regs (Eval.isPrimitive (rawNth vals 0))

fun toPrimitive (regs:Mach.REGS)
                (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val v = rawNth vals 0
        val hint = rawNth vals 1
    in
        if Mach.isString hint
        then Eval.toPrimitive regs v (Eval.toUstring regs hint)
        else Eval.toPrimitive regs v Ustring.empty
    end

fun defaultValue (regs:Mach.REGS)
                 (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val obj = nthAsObj vals 0
        val hint = nthAsUstr vals 1
    in
        Eval.defaultValue regs obj hint
    end


(*
 * Given a function object, a this object, and an array of argument
 * values, call the function with the this object and arguments.
 *
 * helper native function apply(fn : Function!, t : Object!, args : Array) : *;
 *)
fun apply (regs:Mach.REGS)
          (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val fnObj = nthAsObj vals 0
        val thisObj = nthAsObj vals 1
        val argsObj = nthAsObj vals 2
        val argsList = Eval.arrayToList regs argsObj
    in
        Eval.evalCallByObj (Eval.withThis regs thisObj) fnObj argsList
    end

fun fnLength (regs:Mach.REGS)
             (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Mach.Obj { tag, ... } = nthAsObj vals 0
        val len = 
            case tag of
                Mach.PrimitiveTag (Mach.FunctionPrimitive ({ func = Ast.Func { ty, ... }, ...}))
                => AstQuery.minArgsOfFuncTy ty
              | Mach.PrimitiveTag (Mach.NativeFunctionPrimitive {length, ...}) => length
              | _ => error ["wrong kind of object to fnLength"]
    in
        Eval.newDouble regs (Real64.fromInt len)
    end

fun genSend (regs:Mach.REGS)
            (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Mach.Obj { tag, ... } = nthAsObj vals 0
        val arg = rawNth vals 1
    in
        case tag of
            Mach.PrimitiveTag (Mach.GeneratorPrimitive gen) => Eval.sendToGen regs gen arg
          | _ => error ["wrong kind of object to genSend"]
    end

fun genThrow (regs:Mach.REGS)
             (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Mach.Obj { tag, ... } = nthAsObj vals 0
        val arg = rawNth vals 1
    in
        case tag of
            Mach.PrimitiveTag (Mach.GeneratorPrimitive gen) => Eval.throwToGen regs gen arg
          | _ => error ["wrong kind of object to genSend"]
    end

fun genClose (regs:Mach.REGS)
             (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Mach.Obj { tag, ... } = nthAsObj vals 0
    in
        case tag of
            Mach.PrimitiveTag (Mach.GeneratorPrimitive gen) => Eval.closeGen regs gen
          | _ => error ["wrong kind of object to genSend"];
        Mach.Undefined
    end


(* Given a string and a position in that string, return the
 * numeric value of the character at that position in the
 * string.
 *
 * informative native function charCodeAt(s : string, pos : uint) : uint;
 *)
fun charCodeAt (regs:Mach.REGS)
               (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val s = nthAsUstr vals 0
        val i = nthAsUInt regs vals 1
    in
        Eval.newDouble 
            regs
            (Real64.fromInt 
                 (Ustring.charCodeAt s (Word32.toInt i)))
    end


(*
 * Given a numeric character value, return a string of length 1
 * whose element 0 is the character with that same value.
 *
 * informative native function fromCharCode(ch : uint) : string;
 *)
fun fromCharCode (regs:Mach.REGS)
                 (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val i = nthAsUInt regs vals 0
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
 * informative native function stringLength(s : string) : uint;
 *)
fun stringLength (regs:Mach.REGS)
                 (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val s = nthAsUstr vals 0
    in
        Eval.newDouble regs (Real64.fromInt (Ustring.stringLength s))
    end


(*
 * Given two string objects A and B , return a new string object
 * containing the characters from A followed by the characters
 * from B.
 *
 * informative native function stringAppend(a : string, b : string) : string;
 *)
fun stringAppend (regs:Mach.REGS)
                 (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val a = nthAsUstr vals 0
        val b = nthAsUstr vals 1
    in
        Eval.newString regs (Ustring.stringAppend a b)
    end

(*
 * 15.1.2.1
 * intrinsic native function eval(x)
 *)

fun eval (regs:Mach.REGS)
         (vals:Mach.VALUE list)
    : Mach.VALUE =
    if length vals = 0
    then Mach.Undefined
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
                    val (rootRib, frag) = (Defn.defTopFragment (#rootRib regs) frag (Mach.getLangEd regs)
                                           handle
                                           LogErr.DefnError de => raise Eval.ThrowException (str de))
                    val _ = (Verify.verifyTopFragment rootRib false frag
                             handle
                             LogErr.VerifyError ve => raise Eval.ThrowException (str ve))

                    val regs = Eval.withRootRib regs rootRib
                    val regs = Eval.withScope regs (Eval.getGlobalScope regs)
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
               (vals:Mach.VALUE list)
    : Mach.VALUE =
    LogErr.unimplError ["intrinsic::parseFloat"]


(*
 * intrinsic function get (obj: Object!, name: string) : *
 *)
fun get (regs:Mach.REGS)
        (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val obj = (nthAsObj vals 0)
        val name = (nthAsName regs vals 1)
    in
        Eval.getValueOrVirtual regs obj name false 
    end

(*
 * intrinsic function set (obj: Object!, name: string, val: * ) : void
 *)
fun set (regs:Mach.REGS)
        (vals:Mach.VALUE list)
    : Mach.VALUE =
    (Eval.setValueOrVirtual
         regs
         (nthAsObj vals 0)
         (nthAsName regs vals 1)
         (rawNth vals 2)
         false;
     Mach.Undefined)

(* 
 * informative native function objectHash ( ob:Object! ) : uint;
 *)


fun objectHash (regs:Mach.REGS)
               (vals:Mach.VALUE list)
    : Mach.VALUE = 
    let
        val Mach.Obj { ident, ... } = nthAsObj vals 0
    in
       Eval.newDouble regs (Real64.fromInt ident)
    end
                    
(*
 * Return the current time in milliseconds since January 1 1970 00:00:00 UTC.
 *
 * static intrinsic native function now() : double;
 *)

fun now (regs:Mach.REGS)
        (vals:Mach.VALUE list)
    : Mach.VALUE =
    Eval.newDouble regs 
                   (Real.realFloor 
                        ((Time.toReal (Time.now())) * 1000.0))

val random_state = Random.rand (37, 79)

fun random (regs:Mach.REGS)
           (v:Mach.VALUE list)
    : Mach.VALUE =
    Eval.newDouble regs (Random.randReal random_state)

fun unaryDoubleFn (f:(Real64.real -> Real64.real)) :
    (Mach.REGS -> (Mach.VALUE list) -> Mach.VALUE) =
    fn regs =>
    fn vals => if length vals = 0
               then Eval.newDouble regs (0.0/0.0)
               else Eval.newDouble regs (f (Eval.toDouble (rawNth vals 0)))

fun unaryDecimalFn (f:(Decimal.DEC -> Decimal.DEC)) :
    (Mach.REGS -> (Mach.VALUE list) -> Mach.VALUE) =
    fn regs =>
    fn vals => if length vals = 0
               then Eval.newDecimal regs Decimal.NaN
               else Eval.newDecimal regs (f (Eval.toDecimal (rawNth vals 0)))

fun binaryDoubleFn (f:((Real64.real * Real64.real) -> Real64.real)) :
    (Mach.REGS -> (Mach.VALUE list) -> Mach.VALUE) =
    fn regs =>
    fn vals => if length vals = 0 orelse length vals = 1
               then Eval.newDouble regs (0.0/0.0)
               else Eval.newDouble regs (f ((Eval.toDouble (rawNth vals 0)),
                                            (Eval.toDouble (rawNth vals 1))))

fun binaryDecimalFn (f:((Decimal.DEC * Decimal.DEC) -> Decimal.DEC)) :
    (Mach.REGS -> (Mach.VALUE list) -> Mach.VALUE) =
    fn regs =>
    fn vals => if length vals = 0 orelse length vals = 1
               then Eval.newDecimal regs Decimal.NaN
               else Eval.newDecimal regs (f ((Eval.toDecimal (rawNth vals 0)),
                                             (Eval.toDecimal (rawNth vals 1))))

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
                  (vals:Mach.VALUE list)
    : Mach.VALUE =
    let val p1 = nthAsDouble vals 0
        val p2 = nthAsUInt regs vals 1
        val r  = Real64.toManExp p1
        val k  = Real64.toLargeInt IEEEReal.TO_ZERO ((#man r) * 9007199254740992.0)
        fun assemble hi lo =
            Eval.wordToDouble (Word32.orb(Word32.<<(Word32.fromLargeInt hi, 0w16), Word32.fromLargeInt lo))

    in
        Eval.newDouble regs 
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
          (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        fun printOne v = TextIO.print (Ustring.toAscii (Eval.toUstring regs v))
    in
        List.app printOne vals;
        TextIO.print "\n";
        Mach.Undefined
    end

fun load (regs:Mach.REGS)
         (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        fun str s = Eval.newString regs (Ustring.fromString s)
        val fname = Ustring.toFilename (nthAsUstr vals 0)
        val frag = Parser.parseFile fname
            handle LogErr.LexError le => raise Eval.ThrowException (str le)
                 | LogErr.ParseError pe => raise Eval.ThrowException (str pe)
        val (rootRib, frag) = (Defn.defTopFragment (#rootRib regs) frag (Mach.getLangEd regs)
                            handle
                            LogErr.DefnError de => raise Eval.ThrowException (str de))
        val _ = (Verify.verifyTopFragment rootRib false frag
                 handle
                 LogErr.VerifyError ve => raise Eval.ThrowException (str ve))
        val regs = Eval.withRootRib regs rootRib
    in
        
        Eval.evalTopFragment regs frag
        handle 
        LogErr.NameError ne => raise Eval.ThrowException (str ne)
      | LogErr.EvalError ee => raise Eval.ThrowException (str ee)
    end

fun readFile (regs:Mach.REGS)
             (vals:Mach.VALUE list)
    : Mach.VALUE =
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

fun readHTTP (regs:Mach.REGS)
             (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val server = Ustring.toAscii (nthAsUstr vals 0)
        val port = nthAsUInt regs vals 1
        val page = Ustring.toAscii (nthAsUstr vals 2)
        val method = Ustring.toAscii (nthAsUstr vals 3)
        val headers = Ustring.toAscii (nthAsUstr vals 4)

        fun str s = Eval.newString regs (Ustring.fromString s)

        val addr = case NetHostDB.getByName server of
                       SOME ip => INetSock.toAddr (NetHostDB.addr ip, Word32.toInt port)
                     | NONE => raise Eval.ThrowException (str "unknown host")
        val sock = INetSock.TCP.socket ()

        fun close () =
            Socket.close sock handle OS.SysErr _ => ()
        fun throw s = (close(); raise Eval.ThrowException (str s))

        val bufSize = 16384
        val buf = Word8Array.array (bufSize, Word8.fromInt 0)

        fun send s =
            Socket.sendVec (sock, Word8VectorSlice.full (Byte.stringToBytes s))
        fun recv () =
            Socket.recvArr (sock, Word8ArraySlice.full buf)

        fun slicePrefix n = Word8ArraySlice.slice (buf, 0, SOME n)
        fun sliceSource slice =
            let
                fun cons (b, ls) = (Char.chr (Word8.toInt b))::ls
                val chars = Word8ArraySlice.foldr cons [] slice
            in
                Ustring.fromSource (String.implode chars)
            end

        fun reader () : Ustring.SOURCE option =
            (case recv () of
                 0 => NONE
               | bytesRead => SOME (sliceSource (slicePrefix bytesRead)))

        fun readSrc (chunks:Ustring.SOURCE list)
            : Ustring.SOURCE =
            (case reader() of
                 NONE => (close(); List.concat (List.rev chunks))
               | SOME chunk => readSrc (chunk::chunks))
    in
        let
            val _ = Socket.connect (sock, addr)
            val _ = send (method ^ " " ^ page ^ " HTTP/1.0\r\n")
            val _ = send headers
            val _ = send "\r\n"
        in
            str (implode (map Ustring.wcharToChar (readSrc [])))
        end
        handle OS.SysErr (msg, _) => (close(); throw ("socket error: " ^ msg))
    end

fun writeFile (regs:Mach.REGS)
              (vals:Mach.VALUE list)
    : Mach.VALUE =
    let val s        = Ustring.toAscii (nthAsUstr vals 0)
        val filename = Ustring.toFilename (nthAsUstr vals 1)
        val out      = TextIO.openOut filename
    in
        TextIO.output(out, s);
        TextIO.closeOut out;
        Mach.Undefined
    end

fun assert (regs:Mach.REGS)
           (vals:Mach.VALUE list)
    : Mach.VALUE =
    if nthAsBool vals 0
    then Mach.Undefined
    else error ["intrinsic::assert() failed"]

fun typename (regs:Mach.REGS)
             (vals:Mach.VALUE list)
    : Mach.VALUE =    
    case hd vals of
        Mach.Null => Eval.newString regs Ustring.null_
      | Mach.Undefined => Eval.newString regs Ustring.undefined_
      | Mach.Object (Mach.Obj {tag, ...}) =>
        let 
            val name = Mach.nominalBaseOfTag tag
        in
            Eval.newString regs (#id name)
        end

fun dumpFunc (regs:Mach.REGS)
             (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val v = rawNth vals 0
    in
        if Mach.isFunction v
        then
            case Mach.needPrimitive v of
                Mach.FunctionPrimitive { func, ... } => Pretty.ppFunc func
              | _ => ()
        else
            ();
        Mach.Undefined
    end

fun inspect (regs:Mach.REGS)
            (vals:Mach.VALUE list)
    : Mach.VALUE = 
    let
        val v = rawNth vals 0
        val d = if length vals > 1 then nthAsInt regs vals 1 else 1
    in
        Mach.inspect v d;
        Mach.Undefined
    end


fun proto (regs:Mach.REGS)
          (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Mach.Obj { proto, ... } = nthAsObj vals 0
    in
        proto
    end

fun id (regs:Mach.REGS)
       (vals:Mach.VALUE list)
    : Mach.VALUE =
    let
        val Mach.Obj { ident, ... } = nthAsObj vals 0
    in
        Eval.newDouble regs (Real64.fromInt ident)
    end


(* Register all the native functions in this file. *)
fun registerNatives _ =
    let
        fun addFn (len:int) (name:Ast.NAME) f =
            Mach.registerNativeFunction name {length=len, func=f}
    in
        addFn 2 Name.helper_construct construct;
        addFn 1 Name.informative_getClassName getClassName;
        addFn 1 Name.helper_getClassOfObject getClassOfObject;
        addFn 1 Name.helper_getSuperClass getSuperClass;
        addFn 2 Name.helper_getImplementedInterface getImplementedInterface;
        addFn 2 Name.helper_getSuperInterface getSuperInterface;
        addFn 1 Name.helper_getEnumerableIds getEnumerableIds;
        addFn 1 Name.helper_getPrototype getPrototype;
        addFn 2 Name.helper_hasOwnProperty hasOwnProperty;
        addFn 2 Name.helper_getPropertyIsEnumerable getPropertyIsEnumerable;
        addFn 2 Name.helper_getPropertyIsRemovable getPropertyIsRemovable;
        addFn 3 Name.helper_setPropertyIsEnumerable setPropertyIsEnumerable;

        addFn 2 Name.helper_toPrimitive toPrimitive;
        addFn 1 Name.helper_isPrimitive isPrimitive;
        addFn 2 Name.helper_defaultValue defaultValue;

        addFn 3 Name.helper_apply apply;
        addFn 1 Name.helper_fnLength fnLength;
        addFn 2 Name.helper_genSend genSend;
        addFn 2 Name.helper_genThrow genThrow;
        addFn 1 Name.helper_genClose genClose;

        addFn 2 Name.informative_charCodeAt charCodeAt;
        addFn 1 Name.informative_fromCharCode fromCharCode;
        addFn 1 Name.informative_stringLength stringLength;
        addFn 2 Name.informative_stringAppend stringAppend;

        addFn 1 Name.intrinsic_eval eval;
        addFn 1 Name.intrinsic_parseFloat parseFloat;

        addFn 2 Name.intrinsic_get get;
        addFn 3 Name.intrinsic_set set;

        addFn 1 Name.informative_objectHash objectHash;

        (* FIXME: stubs to get double loading. Implement. *)
        addFn 1 Name.intrinsic_toFixedStep10 (fn regs => fn _ => Eval.newString regs Ustring.empty);
        addFn 1 Name.intrinsic_toExponential (fn regs => fn _ => Eval.newString regs Ustring.empty);
        addFn 1 Name.intrinsic_toPrecision (fn regs => fn _ => Eval.newString regs Ustring.empty);

        (* FIXME: stubs to get Date loading. Implement. *)
        addFn 0 Name.intrinsic_now now;
        addFn 0 Name.intrinsic_LocalTZA (fn regs => fn _ => Eval.newDouble regs 0.0);
        addFn 0 Name.intrinsic_DaylightSavingsTA (fn regs => fn _ => Eval.newDouble regs 0.0);

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
        addFn 2 Name.informative_powDouble pow;
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
        addFn 5 Name.intrinsic_readHTTP readHTTP;

        addFn 1 Name.intrinsic_explodeDouble explodeDouble;
        addFn 1 Name.intrinsic_assert assert;
        addFn 1 Name.intrinsic_typename typename;
        addFn 1 Name.intrinsic_dumpFunc dumpFunc;
        addFn 1 Name.intrinsic_inspect inspect;
        addFn 1 Name.intrinsic_proto proto;
        addFn 1 Name.intrinsic_id id

    end

end
