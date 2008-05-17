(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Control = Control (type RESULT = Mach.GEN_SIGNAL);

structure Eval = struct
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

open Name
open Ast
open Mach
open LogErr


fun log (ss:string list) = log ("[eval] " :: ss)

val doTrace = ref false
val doTraceConstruct = ref false

fun fmtName n = if (!doTrace orelse !doTraceConstruct) then name n else ""
fun fmtNameExpr n = if (!doTrace orelse !doTraceConstruct) then nameExpr n else ""

fun trace (ss:string list) = 
    if (!doTrace) then log ss else ()

fun traceConstruct (ss:string list) = 
    if (!doTraceConstruct) then log ss else ()

fun error (regs:REGS) 
          (ss:string list) =
    (log ("[stack] " :: [stackString (stackOf regs)]);
     evalError ss)


fun normalize (regs:REGS)
              (ty:TYPE)
    : TYPE = 
    let
        val { scope, prog, ... } = regs
        val ribs = getRibs scope
    in
        Type.normalize ribs ty
    end

fun evalTy (regs:REGS)
           (ty:TYPE)
    : TYPE = normalize regs ty

(* Exceptions for object-language control transfer. *)
exception ContinueException of (IDENTIFIER option)
exception BreakException of (IDENTIFIER option)
exception ThrowException of VALUE
exception ReturnException of VALUE
exception StopIterationException

exception InternalError

fun throwExn (v:VALUE)
    : 'a =
    raise (ThrowException v)

infix 4 <*;
fun tsub <* tsup = Type.compatibleSubtype tsub tsup

fun mathOp (v:VALUE)
           (decimalFn:(Decimal.DEC -> 'a) option)
           (doubleFn:(Real64.real -> 'a) option)
           (default:'a)
    : 'a =
    let
        fun fnOrDefault fo v = case fo of
                                   NONE => default
                                 | SOME f => f v
    in
        if isDecimal v
        then fnOrDefault decimalFn (needDecimal v)
        else 
            if isDouble v
            then fnOrDefault doubleFn (needDouble v)
            else default
    end

(*
 Extend a scope object (p) with object (ob) of kind (kind)
 *)

fun extendScope (p:SCOPE)
                (ob:OBJ)
                (kind:SCOPE_KIND)
    : SCOPE =
    let
        val parent = SOME p
        val temps = ref []
    in
        Scope { parent = parent,
                object = ob,
                temps = temps,
                kind = kind }
    end

(*
 Extend the scope in registers (r) with object (ob) of kind (kind)
 *)

fun extendScopeReg (r:REGS)
                   (ob:OBJ)
                   (kind:SCOPE_KIND)
    : REGS =
    let
        val { scope, this, thisFun, thisGen, global, prog, aux } = r
        val scope = extendScope scope ob kind
    in
        { scope = scope,
          this = this,
          thisFun = thisFun,
          thisGen = thisGen,
          global = global,
          prog = prog,
          aux = aux }
    end

fun withThis (r:REGS)
             (newThis:OBJ)
    : REGS =
    let
        val { scope, this, thisFun, thisGen, global, prog, aux } = r
    in
        { scope = scope, 
          this = newThis, 
          thisFun = thisFun,
          thisGen = thisGen,
          global = global, 
          prog = prog,
          aux = aux }
    end

fun withThisFun (r:REGS)
                (newThisFun:OBJ option)
    : REGS =
    let
        val { scope, this, thisFun, thisGen, global, prog, aux } = r
    in
        { scope = scope, 
          this = this, 
          thisFun = newThisFun,
          thisGen = thisGen,
          global = global, 
          prog = prog,
          aux = aux }
    end

fun withThisGen (r:REGS)
                (newThisGen:OBJ option)
    : REGS =
    let
        val { scope, this, thisFun, thisGen, global, prog, aux } = r
    in
        { scope = scope, 
          this = this, 
          thisFun = thisFun,
          thisGen = newThisGen,
          global = global, 
          prog = prog,
          aux = aux }
    end

fun withScope (r:REGS)
              (newScope:SCOPE)
    : REGS =
    let
        val { scope, this, thisFun, thisGen, global, prog, aux } = r
    in
        { scope = newScope, 
          this = this, 
          thisFun = thisFun,
          thisGen = thisGen,
          global = global, 
          prog = prog,
          aux = aux }
    end

fun withProg (r:REGS)
             (newProg:Fixture.PROGRAM)
    : REGS =
    let
        val { scope, this, thisFun, thisGen, global, prog, aux } = r
    in
        { scope = scope, 
          this = this, 
          thisFun = thisFun,
          thisGen = thisGen,
          global = global, 
          prog = newProg,
          aux = aux }
    end
    
    
fun getObjId (obj:OBJ)
    : OBJ_IDENTIFIER =
    let
        val Obj { ident, ... } = obj
    in
        ident
    end


fun slotObjId (regs:REGS) 
              (slotFunc:REGS -> (OBJ option) ref) 
    : OBJ_IDENTIFIER =
    let
        val slot = slotFunc regs
        val slotVal = !slot
    in
        case slotVal of 
            SOME obj => getObjId obj
          | NONE => ~1
    end


fun getScopeObj (scope:SCOPE)
    : OBJ =
    let
        val Scope { object, ... } = scope
    in
        object
    end


fun getScopeId (scope:SCOPE)
    : OBJ_IDENTIFIER = 
    let 
        val scopeObj = getScopeObj scope
    in 
        getObjId scopeObj
    end


fun getScopeTemps (scope:SCOPE)
    : TEMPS =
    let
        val Scope { temps, ... } = scope
    in
        temps
    end

fun getTemps (regs:REGS)
    : TEMPS =
    let
        val { scope, ... } = regs
    in        
        getScopeTemps scope
    end

(*
 * The global object and scope.
 *)

fun getGlobalScope (regs:REGS) 
    : SCOPE =
    let
        val { global, ... } = regs
    in
        makeGlobalScopeWith global
    end 

fun getClassScope (regs:REGS)
                  (classObj:OBJ)
    : SCOPE = 
    extendScope (getGlobalScope regs) classObj InstanceScope
    
(*
 * A small number of functions do not fully evaluate to VALUE
 * values, but instead to REFs; these are temporary artifacts of
 * evaluation, not first-class values in the language.
 *)

type REF = (OBJ * NAME)

datatype NUMBER_TYPE = DoubleNum | DecimalNum 
                                   
fun numLE (a:NUMBER_TYPE)
          (b:NUMBER_TYPE) =
    if a = b
    then true
    else 
        case (a,b) of
            (DoubleNum, DecimalNum) => true
          | _ => false
                 
fun promoteToCommon (regs:REGS) 
                    (a:NUMBER_TYPE) 
                    (b:NUMBER_TYPE)
    : NUMBER_TYPE =
    if a = b
    then a
    else 
        case if numLE a b then (a,b) else (b,a) of 
            (DoubleNum, DecimalNum) => DecimalNum
          | _ => error regs ["unexpected number combination in promoteToCommon"]

(* Fundamental object methods *)

fun allocRib (regs:REGS)
             (obj:OBJ)
             (this:OBJ option)
             (temps:TEMPS)
             (f:RIB)
    : unit =
    let
        
        val Obj { props, ident, ... } = obj
        val _ = traceConstruct ["allocating rib on object id #", Int.toString ident]
        val _ = if (isBooting regs andalso 
                    getObjId (#global regs) = getObjId obj)
                then ()
                else setRib obj (f @ (getRib obj))
        val {scope, ...} = regs
        val methodScope = extendScope scope obj ActivationScope                 
        val attrs0 = { removable = false,
                       enumerable = false,
                       writable = ReadOnly,
                       fixed = true }
        fun allocFixture (n, f) =
            case n of
                TempName t => allocTemp regs f t temps
              | PropName pn =>
                let
                    val _ = traceConstruct ["allocating fixture for prop ", fmtName pn]
                    fun allocProp state p =
                        if hasProp props pn
                        then
                            (* FIXME: 
                             * This is ugly: boot code has no instances of fixture-replacement.
                             * Some ES3 code -- notably the spidermonkey testsuite -- does in fact
                             * have instances of fixture-replacing. We need to decide on the exact
                             * rule here, and its interaction with the fixture-merging code in Fixture.sml
                             *
                             * For the time being I'm only permitting replacement of methods and vars,
                             * in non-boot code, since those are the only things ES3 is supposed to know 
                             * about.
                             *)
                            let
                                fun fail _ = error regs ["allocating duplicate property name: ", name pn]
                                fun permit _ = (traceConstruct ["replacing ", state, " property ", fmtName pn];
                                                delProp props pn; 
                                                addProp props pn p)
                                val state = (#state (getProp props pn))
                            in
                                if isBooting regs 
                                then fail ()
                                else case state of
                                         ValProp _ => permit ()
                                       | MethodProp _ => permit ()
                                       | NativeFunctionProp _ => permit ()
                                       | VirtualValProp _ => permit ()
                                       | _ => fail ()
                            end
                        else addProp props pn p
                in
                    case f of
                        TypeFixture (typeParams, ty) =>
                        allocProp "type"
                                  { ty = normalize regs ty,   (* FIXME: handle typeParams *)
                                    state = TypeProp,
                                    attrs = attrs0 }
                        
                      | MethodFixture { func, ty, writable, ... } =>
                        let
                            val Func { native, ... } = func
                            val p = if native
                                    then NativeFunctionProp (getNativeFunction pn)
                                    else MethodProp (newFunClosure methodScope func this)
                        in
                            allocProp "method"
                                      { ty = normalize regs ty,
                                        state = p,
                                        attrs = { removable = false,
                                                  enumerable = false,
                                                  writable = if writable 
                                                             then Writable
                                                             else WriteOnce,
                                                  fixed = true } }
                        end
                        
                      | ValFixture { ty, writable, ... } =>
                        let
                            val ty = evalTy regs ty
                        in
                            allocProp "value"
                                      { ty = ty,
                                        state = if writable
                                                then valAllocState regs ty
                                                else UninitProp,
                                        attrs = { removable = false,
                                                  enumerable = false, 
                                                  writable = if writable 
                                                             then Writable
                                                             else WriteOnce,
                                                  fixed = true } }
                        end

                      | VirtualValFixture { ty, getter, setter, ... } =>
                        let
                            val getFn = case getter of
                                            NONE => NONE
                                          | SOME f => SOME (newFunClosure methodScope f this)
                            val setFn = case setter of
                                            NONE => NONE
                                          | SOME f => SOME (newFunClosure methodScope f this)
                        in
                            allocProp "virtual value"
                                      { ty = evalTy regs ty,
                                        state = VirtualValProp { getter = getFn,
                                                                 setter = setFn },
                                        attrs = { removable = false,
                                                  enumerable = false, 
                                                  writable = ReadOnly,
                                                  fixed = true } }
                        end
                        
                      | ClassFixture cls =>
                        let
                            val Class {classRib, ...} = cls
                            val _ = traceConstruct ["allocating class object for class ", fmtName pn]
                            val classObj = needObj regs (newClass regs cls)
                            val _ = traceConstruct ["allocating class rib on class ", fmtName pn]
                            (* FIXME: 'this' binding in class objects might be wrong here. *)
                            val _ = allocObjRib regs classObj NONE classRib
                        in
                            allocProp "class"
                                      { ty = typename intrinsic_Class,
                                        state = ValProp (Object classObj),
                                        attrs = attrs0 }
                        end

                      | NamespaceFixture ns =>
                        allocProp "namespace"
                                  { ty = typename ES4_Namespace,
                                    state = NamespaceProp ns,
                                    attrs = attrs0 }

                      | TypeVarFixture _ =>
                        allocProp "type variable"
                                  { ty = typename intrinsic_Type,
                                    state = TypeVarProp,
                                    attrs = attrs0 }

                      | InterfaceFixture iface =>  (* FIXME *)
                        let
                            val _ = traceConstruct ["allocating interface object for interface ", fmtName pn]
                            val ifaceObj = needObj regs (newInterface regs iface)
                        in
                            allocProp "interface"
                                      { ty = typename intrinsic_Type,
                                        state = ValProp (Object ifaceObj),
                                        attrs = attrs0 }
                        end

                (* | _ => error regs ["Shouldn't happen: failed to match in Eval.allocRib#allocFixture."] *)

                end
    in
        List.app allocFixture f
    end


and allocObjRib (regs:REGS)
                (obj:OBJ)
                (this:OBJ option)
                (f:RIB)
    : unit =
    let
        val (temps:TEMPS) = ref []
    in
        allocRib regs obj this temps f;
        if not ((length (!temps)) = 0)
        then error regs ["allocated temporaries in non-scope object"]
        else ()
    end


and allocScopeRib (regs:REGS)
                  (rib:RIB)
    : unit =
    let
        val { scope, ... } = regs
        val Scope { object, temps, ... } = scope
    in
        allocRib regs object NONE temps rib
    end


and allocTemp (regs:REGS) 
              (f:FIXTURE)
              (t:int)
              (temps:TEMPS) 
    : unit =
    let
        val ty = case f of 
                     ValFixture { ty, ... } => ty
                   | _ => error regs ["allocating non-value temporary"]
        val tmps = !temps
        val tlen = List.length tmps
        val emptyTmp = (AnyType, UninitTemp)
    in
        if t = tlen
        then 
            let
                val newTemps = tmps @ [emptyTmp]
            in
                traceConstruct ["allocating temp #", Int.toString t];
                temps := newTemps
            end
        else 
            if t < tlen
            then 
                let
                    val prefix = List.take (tmps, t-1)
                    val suffix = List.drop (tmps, t)
                    val tempTy = evalTy regs ty
                    val tempCell = (tempTy, UninitTemp)
                    val newTemps = prefix @ (tempCell :: suffix)
                in
                    traceConstruct ["reallocating temp #", Int.toString t, " of ", Int.toString tlen];
                    temps := newTemps
                end                                 
            else 
                let
                    val prefixLen = t-tlen
                    val prefix = List.tabulate (prefixLen, (fn _ => emptyTmp))
                    val newTemps = prefix @ (emptyTmp :: tmps)
                in
                    traceConstruct ["extending temps to cover temp #", Int.toString t];
                    temps := newTemps
                end                                
    end


and valAllocState (regs:REGS) 
                  (ty:TYPE)
    : PROPERTY_STATE =
    
    (* Every value fixture has a type, and every type has an
     * associated "allocated state". Note that
     * this is *not* the same as saying that every type
     * has an associated default value; for *some* types
     * the allocated state is a default value; for
     * types that are non-nullable, however, the allocated
     * state is UninitProp. This property
     * state should never be observable to a user. It is
     * always a hard error to read a property in
     * UninitProp state, and it is always a hard
     * error to complete the initialization phase of an
     * object with any properties remaining in
     * UninitProp state. 
     *)
    
    case ty of
        AnyType =>
        ValProp (Undefined)
        
      | NullType =>
        ValProp (Null)
        
      | UndefinedType =>
        ValProp (Undefined)
      (* 
      | VoidType =>
        error regs ["attempt to allocate void-type property"]
       *)
      | UnionType [] => 
        UninitProp
        
      | UnionType ts => 
        let
            fun firstType [] = UninitProp
              | firstType (x::xs) = 
                (case valAllocState regs x of
                     UninitProp => firstType xs
                   | other => other)
                
            fun firstSimpleType [] = firstType ts
              | firstSimpleType ((AnyType)::xs) = ValProp (Undefined)
              | firstSimpleType ((NullType)::xs) = ValProp (Null)
              | firstSimpleType ((UndefinedType)::xs) = ValProp (Undefined)
              | firstSimpleType ((ClassType (Class {nonnullable=false, ...}))::xs) = ValProp (Null)
              | firstSimpleType ((InterfaceType (Interface {nonnullable=false, ...}))::xs) = ValProp (Null)
              | firstSimpleType ((AppType (ClassType (Class {nonnullable=false, ...}), _))::xs) = ValProp (Null)
              | firstSimpleType ((AppType (InterfaceType (Interface {nonnullable=false, ...}), _))::xs) = ValProp (Null)
              | firstSimpleType (x::xs) = firstSimpleType xs
        in
            firstSimpleType ts
        end
        
      | ArrayType _ =>
        ValProp (Null)
        
      | FunctionType _ =>
        UninitProp
        
      | RecordType _ =>
        ValProp (Null)
        
      | AppType (base, _) =>
        valAllocState regs base

      | NonNullType expr =>
        UninitProp
        
      | TypeName ident =>
        error regs ["allocating fixture with unresolved type name: ", LogErr.ty ty]
        
      | TypeIndexReferenceType _ =>
        error regs ["allocating fixture of unresolved element type reference"]
        
      | TypeNameReferenceType _ =>
        error regs ["allocating fixture of unresolved field type reference"]
        
      | ClassType (Class {name, nonnullable, ...}) =>
        (* It is possible that we're booting and the class n doesn't even exist yet. *)
        if (not (isBooting regs)) orelse 
           isClass (getValue regs (#global regs) name)
        then            
            let 
                val clsid = getObjId (needObj regs (getValue regs (#global regs) name))
            in
                case allocSpecial regs clsid of
                    SOME v => ValProp v
                  | NONE => if nonnullable 
                            then UninitProp
                            else ValProp (Null)
            end
        else
            if nonnullable
            then UninitProp
            else ValProp (Null)

      | InterfaceType (Interface {name, ...}) => 
        UninitProp


and allocSpecial (regs:REGS)
                 (id:OBJ_IDENTIFIER)
    : VALUE option =
    let
        fun findSpecial [] = NONE
          | findSpecial ((q,f)::rest) = 
            let
                val ident = slotObjId regs q
            in
                if ident = id 
                then 
                    (traceConstruct ["allocating special builtin"]; SOME (f ()))
                else 
                    findSpecial rest
            end
    in
        findSpecial 
            [
             (getBooleanClassSlot, (fn _ => newBoolean regs false)),

             (getDoubleClassSlot, (fn _ => newDouble regs 0.0)),
             (getDecimalClassSlot, (fn _ => newDecimal regs Decimal.zero)),

             (getStringClassSlot, (fn _ => newString regs Ustring.empty))
            ]
    end


and asArrayIndex (v:VALUE)
    : Word32.word =
    case v of
        Object (Obj { tag, ... }) =>
        (case tag of
             PrimitiveTag (DoublePrimitive d) => 
             if isIntegral d 
                andalso 0.0 <= d 
                andalso d < 4294967295.0
             then
                 doubleToWord d
             else
                 0wxFFFFFFFF
           | PrimitiveTag (DecimalPrimitive d) => 
             0wxFFFFFFFF (* FIXME *)
           | _ => 0wxFFFFFFFF)
      | _ => 0wxFFFFFFFF
             

and hasOwnProperty (regs : REGS)
                   (obj  : OBJ)
                   (n    : NAME)
    : bool =
    let
        val Obj { props, ... } = obj
    in
        if hasFixedProp props n then 
            true
        else if hasFixedProp props meta_has then 
            let 
                val v = evalNamedMethodCall 
                            regs obj meta_has [newName regs n]
            in
                toBoolean v
            end
            handle ThrowException e => 
                   let
                       val ty = typeOfVal regs e
                       val defaultBehaviorClassTy = 
                           instanceType regs ES4_DefaultBehaviorClass []
                   in
                       if ty <* defaultBehaviorClassTy then
                           hasProp props n
                       else 
                           throwExn e
                   end
        else
            hasProp props n
    end


and hasProperty (regs:REGS)
                (obj:OBJ)
                (n:NAME)
    : bool =
    if hasOwnProperty regs obj n
    then true
    else 
        let
            val Obj { proto, ... } = obj
        in
            case proto of
                Object p => hasProperty regs p n
              | _ => false
        end

(*
 * *Similar to* ES-262-3 8.7.1 GetValue(V), there's
 * no Reference type in ES4.
 *)
and getValueOrVirtual (regs:REGS)
                      (obj:OBJ)
                      (name:NAME)
                      (doVirtual:bool)
                      (propNotFound:(OBJ -> VALUE))
    : VALUE =
    let
        val _ = trace ["getting property ", fmtName name, 
                       " on obj #", Int.toString (getObjId obj)]
        val Obj { props, ... } = obj
        fun upgraded (currProp:PROPERTY) newVal =
            let
                val { ty, attrs, ... } = currProp
                val newProp = { state = ValProp newVal,
                                ty = ty,
                                attrs = attrs }
            in
                delProp props name;
                addProp props name newProp;
                trace ["upgraded property ", fmtName name ];
                newVal
            end
    in
        case findProp props name of
            SOME prop =>
            (case (#state prop) of
                 TypeProp =>
                 throwExn (newTypeErr regs ["getValue on a type property: ",
                                            LogErr.name name])

               | TypeVarProp =>
                 throwExn (newTypeErr regs ["getValue on a type variable property: ",
                                            LogErr.name name])

               | UninitProp =>
                 throwExn (newTypeErr regs ["getValue on an uninitialized property: ",
                                            LogErr.name name])

               | VirtualValProp { getter, ... } =>
                 if doVirtual
                 then
                     case getter of
                         SOME g => invokeFuncClosure (withThis regs obj) g NONE []
                       | _ => Undefined
                 else
                     (* FIXME: possibly throw here? *)
                     Undefined

               | NamespaceProp n =>
                 upgraded prop (newNamespace regs n)

               | NativeFunctionProp nf =>
                 upgraded prop (newNativeFunction regs nf)

               | MethodProp closure =>
                 upgraded prop (newFunctionFromClosure regs closure)

               | ValListProp vals =>
                 (* FIXME: The 'arguments' object can't be an array. *)
                 upgraded prop (newArray regs vals)

               | ValProp v => v)
          | NONE =>
            let
                fun catchAll _ =
                    (* FIXME: need to use builtin es object here, when that file exists. *)
                    (trace ["running meta::get(\"", 
                            (Ustring.toAscii (#id name)), 
                            "\") catchall on obj #", 
                            Int.toString (getObjId obj)];
                     (evalCallByRef (withThis regs obj) 
                                    (obj, meta_get) 
                                    [newString regs (#id name)]
                                    false))
            in
                if doVirtual
                then 
                    case findProp props meta_get of
                        SOME { state = MethodProp _, ... } => catchAll ()
                      | SOME { state = NativeFunctionProp _, ... } => catchAll ()
                      | _ => propNotFound obj
                else 
                    propNotFound obj
            end
    end


and getValue (regs:REGS)
             (obj:OBJ)
             (name:NAME)
    : VALUE =
    let
        fun propNotFound (curr:OBJ)
            : VALUE =
            let
                val Obj { proto, ... } = curr
            in
                case proto of
                    Object ob => 
                    getValueOrVirtual regs ob name true propNotFound
                  | _ =>
                    if isDynamic regs obj
                    then Undefined
                    else throwExn (newTypeErr
                                       regs
                                       ["attempting to get nonexistent property ",
                                        LogErr.name name,
                                        " from non-dynamic object"])
            end
    in
        getValueOrVirtual regs obj name true propNotFound
    end


and newTypeOpFailure (regs:REGS)
                     (prefix:string)
                     (v:VALUE)
                     (tyExpr:TYPE)
    : VALUE =
    newTypeErr regs [prefix, ": val=", approx v,
                     " type=", ty (typeOfVal regs v),
                     " wanted=", ty tyExpr]
(*
 throwTypeErr regs [prefix, ": val=", approx v,
                    " type=", ty (typeOfVal regs v),
                    " wanted=", ty tyExpr]
 *)
    
and checkAndConvert (regs:REGS)
                    (v:VALUE)
                    (ty:TYPE)
    : VALUE =
    let
        val tyExpr = evalTy regs ty
    in                         
        if evalOperatorIs regs v tyExpr
        then v
        else
            let
                val (classType:TYPE) =
                    case Type.findSpecialConversion (typeOfVal regs v) tyExpr of
                        NONE => throwExn (newTypeOpFailure regs "incompatible types w/o conversion" v tyExpr)
                      | SOME n => n
                val (classTy:TYPE) = AstQuery.needClassType classType
                val (classObj:OBJ) = instanceClass regs classTy
                (* FIXME: this will call back on itself! *)
                val converted = evalCallByRef (withThis regs classObj) (classObj, meta_invoke) [v]
                                              true
            in
                typeCheck regs converted tyExpr
            end
    end


and getObjTag (obj:OBJ)
    : TAG = 
    let
        val Obj { tag, ... } = obj
    in
        tag
    end


and isDynamic (regs:REGS)
              (obj:OBJ)
    : bool =
    let
        fun typeIsDynamic (UnionType tys) = List.exists typeIsDynamic tys
          | typeIsDynamic (ArrayType _) = true
          | typeIsDynamic (FunctionType _) = true
          | typeIsDynamic (RecordType _) = true
          (*    | typeIsDynamic (LamType { params, body }) = typeIsDynamic body *)
          | typeIsDynamic (NonNullType t) = typeIsDynamic t
          | typeIsDynamic (ClassType (Class { dynamic, ... })) = dynamic
          | typeIsDynamic _ = false
    in
        typeIsDynamic (typeOfVal regs (Object obj))
    end


and badPropAccess (regs:REGS)
                  (accessKind:string)
                  (propName:NAME)
                  (existingPropState:PROPERTY_STATE)
    : unit =
    let
        val existingPropKind = 
            case existingPropState of 
                TypeVarProp => "type variable"
              | TypeProp => "type"
              | UninitProp => "uninitialized"
              | ValProp _ => "value"
              | NamespaceProp _ => "namespace"
              | MethodProp _ => "method"
              | ValListProp _ => "value-list"
              | NativeFunctionProp _ => "native function"
              | VirtualValProp _ => "virtual"
    in
        throwExn (newTypeErr regs ["bad property ", accessKind,
                                   " on ", existingPropKind, 
                                   " ", name propName])
    end


and setValueOrVirtual (regs:REGS)
                      (obj:OBJ)
                      (name:NAME)
                      (v:VALUE)
                      (doVirtual:bool)
    : unit =
    let
        val Obj { props, ... } = obj
    in
        case findProp props name of
            SOME existingProp =>
            let
                val { state, attrs, ty, ... } = existingProp
                val { removable, enumerable, fixed, writable } = attrs
                fun newProp _ = 
                    { state = ValProp (checkAndConvert regs v ty),
                      ty = ty,
                      attrs = { removable = removable,
                                enumerable = enumerable,
                                fixed = fixed,
                                writable = case writable of 
                                               (* If we got here we were actually upgrading an uninitprop. *)
                                               ReadOnly => ReadOnly
                                             | WriteOnce => ReadOnly
                                             | Writable => Writable } }
                fun write _ = 
                    let
                        val np = newProp ()
                    in
                        (* FIXME: change to "replaceProp" that presrves insert-order. *)
                        delProp props name;
                        addProp props name np
                    end

                fun maybeWrite throwIfReadOnly =
                    case writable of 
                        ReadOnly => if throwIfReadOnly
                                    then throwExn (newTypeErr regs ["setValue on non-writable property: ",
                                                                    LogErr.name name])
                                    else ()
                      | WriteOnce => write ()
                      | Writable => write ()
            in
                case state of
                    
                    (* FIXME: change UninitProp state to another state in the Writability datatype. *)
                    UninitProp => write ()
                  | MethodProp _ => maybeWrite true
                  | VirtualValProp { setter, ... } =>
                    if doVirtual
                    then 
                        case setter of 
                            NONE => throwExn (newTypeErr regs ["attempting to write to a virtual property without a setter: ",
                                                               LogErr.name name])
                          | SOME s => (invokeFuncClosure (withThis regs obj) s NONE [v]; ())
                    else 
                        maybeWrite true

                  (* FIXME: predicate throw/ignore on the presence of runtime strict-mode flag. *)
                  | ValProp _ => maybeWrite false                  
                  | _ => badPropAccess regs "setValue" name state
            end
          | NONE =>
            let
                fun newProp _ =
                    let
                        val prop = { state = ValProp v,
                                     ty = AnyType,
                                     attrs = { removable = true,
                                               enumerable = true,
                                               writable = Writable,
                                               fixed = false } }
                    in
                        if isDynamic regs obj
                        then addProp props name prop
                        else throwExn (newTypeErr regs ["attempting to add property to non-dynamic object"])
                    end
                fun catchAll _ =
                    (* FIXME: need to use builtin es object here, when that file exists. *)
                    (trace ["running meta::set(\"", 
                            (Ustring.toAscii (#id name)), 
                            "\", ", approx v,
                            ") catchall on obj #", 
                            Int.toString (getObjId obj)];
                     (evalCallByRef (withThis regs obj) 
                                    (obj, meta_set) 
                                    [newString regs (#id name), v]
                                    false; 
                      ()))
            in
                if doVirtual
                then
                    case findProp props meta_set of
                        SOME { state = MethodProp _, ... } => catchAll ()
                      | SOME { state = NativeFunctionProp _, ... } => catchAll ()
                      | _ => newProp ()
                else
                    newProp ()
            end
    end


and setValue (regs:REGS)
             (base:OBJ)
             (name:NAME)
             (v:VALUE)
    : unit =
    setValueOrVirtual regs base name v true

and defValue (regs:REGS)
             (base:OBJ)
             (name:NAME)
             (v:VALUE)
    : unit =
    setValueOrVirtual regs base name v false

and instantiateGlobalClass (regs:REGS)
                           (n:NAME)
                           (args:VALUE list)
    : VALUE =
    let
        val _ = traceConstruct ["instantiating global class ", fmtName n];
        val (cls:VALUE) = getValue regs (#global regs) n
    in
        case cls of
            Object ob => evalNewObj regs ob args
          | _ => error regs ["global class name ", name n,
                             " did not resolve to object"]
    end

and newExn (regs:REGS)
           (name:NAME)
           (args:string list)
    : VALUE =
    if isBooting regs
    then error regs ["trapped ThrowException during boot: ", 
                     LogErr.name name, "(", String.concat args, ")"]
    else instantiateGlobalClass
             regs name
             [((newString regs) o Ustring.fromString o String.concat) args]

and newTypeErr (regs:REGS)
               (args:string list)
    : VALUE =
    newExn regs public_TypeError args

and newRefErr (regs:REGS)
              (args:string list)
    : VALUE =
    newExn regs public_ReferenceError args

and needNameOrString (regs:REGS)
                     (v:VALUE)
    : NAME =
    case v of
        Object obj => 
        if (typeOfVal regs v) <* (instanceType regs ES4_Name [])
        then
            let
                val nsval = getValue regs obj public_qualifier
                val idval = getValue regs obj public_identifier
            in
                { id = (toUstring regs idval), ns = (needNamespaceOrNull nsval) }
            end
        else
            public (toUstring regs v)
      | _ => public (toUstring regs v)

and needObj (regs:REGS)
            (v:VALUE)
    : OBJ =
    case v of
        Object ob => ob
      | _ => throwExn (newTypeErr regs ["need object"])

and newPublicObject (regs:REGS) =
    instantiateGlobalClass 
        regs public_Object []
    
and newPublicObj (regs:REGS) =
    needObj regs (newPublicObject regs)
    
and newArray (regs:REGS)
             (vals:VALUE list)
    : VALUE =
    (*
     * NB: Do not reorganize this to call the public Array constructor with the val list
     * directly: that constructor is a bit silly. It interprets a single-argument list as
     * a number, and sets the array to that length. We want to always return an array from
     * this call containing as many values as we were passed, no more no less.
     *)
    let val a = instantiateGlobalClass 
                    regs public_Array 
                    [newDouble regs (Real64.fromInt (List.length vals))]
        fun init a _ [] = ()
          | init a k (x::xs) =
            (setValue regs a (public (Ustring.fromInt k)) x ;
             init a (k+1) xs)
    in
        init (needObj regs a) 0 vals;
        a
    end

(* SPEC

fun evalRegExpInitialiser (env: ENV)
                          (string: Ustring.STRING)
    : VALUE =
 (* FINISH ME *)

 *)

and newRegExp (regs:REGS)
              (pattern:Ustring.STRING)
              (flags:Ustring.STRING)
    : VALUE =
    instantiateGlobalClass 
        regs public_RegExp 
        [newString regs pattern, newString regs flags]


and newPrimitive (regs:REGS)
                 (prim:PRIMITIVE)
                 (getter: REGS -> (OBJ option) ref)
    : VALUE =
    let
        val args = [Object (newObject (PrimitiveTag prim) Null)]
        val (class, closure) = getClassObjectAndClass regs getter
    in
        constructClassInstance regs class closure args
    end
    

(* SPEC

fun evalDecimalLiteral (env: ENV)
                       (decimal: Decimal.DEC)
    : VALUE =
 (* FINISH ME *)

 *)

and newDecimal (regs:REGS) 
               (n:Decimal.DEC)
    : VALUE =
    newPrimitive regs (DecimalPrimitive n) getDecimalClassSlot

(* SPEC

fun evalDoubleLiteral (env: ENV)
                      (double: Real64.real)
    : VALUE =
 (* FINISH ME *)

 *)

and newDouble (regs:REGS) 
              (n:Real64.real)
    : VALUE =
    newPrimitive regs (DoublePrimitive n) getDoubleClassSlot


(* SPEC

fun evalStringLiteral (env: ENV)
                      (s: Ustring.STRING)
    : VALUE =
 (* FINISH ME *)

 *)


and newString (regs:REGS)
              (s:Ustring.STRING)
    : VALUE =
    newPrimitive regs (StringPrimitive s) getStringClassSlot


(* SPEC

fun evalBooleanLiteral (env: ENV)
                       (b: bool)
    : VALUE =
 (* FINISH ME *)

 *)

and newBoolean (regs:REGS)
               (b:bool)
    : VALUE =
    newPrimitive regs (BooleanPrimitive b) getBooleanClassSlot

and newNamespace (regs:REGS)
                 (n:NAMESPACE)
    : VALUE =
    newPrimitive regs (NamespacePrimitive n) getNamespaceClassSlot

and newName (regs:REGS)
            (n:NAME)
    : VALUE =
    case findInNmCache regs n of
        SOME obj => Object obj
      | NONE => 
        let 
            val nsval = newNamespace regs (#ns n)
            val idval = newString regs (#id n)
            val v = instantiateGlobalClass 
                        regs ES4_Name 
                        [nsval, idval]
        in 
            Object (updateNmCache regs (n, needObj regs v))
        end

and newClass (regs:REGS)
             (class:CLASS)
    : VALUE =
    newPrimitive regs (ClassPrimitive class) getClassClassSlot 

and newInterface (regs:REGS)
                 (iface:INTERFACE)
    : VALUE =
    newPrimitive regs (InterfacePrimitive iface) getInterfaceClassSlot

and newFunClosure (e:SCOPE)
                  (f:FUNC)
                  (this:OBJ option)
    : FUN_CLOSURE =
    { func = f, this = this, env = e }

and getClassObjectAndClass regs getter = 
    let
        val classObj = case !(getter regs) of 
                           NONE => error regs ["midding special class"]
                         | SOME c => c
        val class = needClass (Object classObj)
    in
        (classObj, class)
    end

and getFunctionClassObjectAndClass (regs:REGS) 
    : (OBJ * CLASS) = 
    (* LDOTS *)
    getClassObjectAndClass regs getFunctionClassSlot

and getObjectClassObjectAndClass (regs:REGS) 
    : (OBJ * CLASS) = 
    (* LDOTS *)
    getClassObjectAndClass regs getObjectClassSlot


and newFunctionFromClosure (regs:REGS)
                           (closure:FUN_CLOSURE) =
    let
        val (funClassObj, funClass) = getFunctionClassObjectAndClass regs
        val funProto = getPrototype regs funClassObj
                       
        (* This is a little weird: we're construction function f, but f.prototype needs to 
         * point to an instance of public::Function -- in order to behave "functiony" in the
         * sense of prototypes and private implementation methods -- even though f.prototype
         * is not going to have any function *primitive* associated with it. 
         * 
         * We also wire the new f.prototype.__proto__ value to Object.prototype.
         * 
         * f.__proto__ = Function.prototype
         * f.prototype = (new Function())    // essentially, minus any function primitive
         *
         *)

        val originalObjectProto = getOriginalObjectPrototype regs
        val newProtoObj = constructStandard regs funClassObj funClass originalObjectProto []
        val newProto = Object newProtoObj
        val _ = traceConstruct ["built new prototype chained to Object.prototype"]

        val tag = PrimitiveTag (FunctionPrimitive closure)
        val obj = constructStandardWithTag regs funClassObj funClass tag funProto []                  
        val Obj { props=newProtoProps, ... } = newProtoObj
    in
        setPrototype regs obj newProto;
        setValueOrVirtual regs newProtoObj public_constructor (Object obj) false;
        setPropEnumerable newProtoProps public_constructor false;
        Object obj
    end


and newFunctionFromFunc (regs:REGS)
                        (scope:SCOPE)
                        (f:FUNC)
    : VALUE =
    newFunctionFromClosure regs (newFunClosure scope f NONE)


and newNativeFunction (regs:REGS)
                      (f:NATIVE_FUNCTION) =
    let 
        val (classObj, class) = getFunctionClassObjectAndClass regs
        val tag = PrimitiveTag (NativeFunctionPrimitive f)
        val proto = getPrototype regs classObj
        val obj = constructStandardWithTag regs classObj class tag proto [] 
    in
        Object obj
    end
    
and getIteratorNamespace (regs:REGS)
    : NAMESPACE =
    needNamespace (getValue regs (#global regs) ES4_iterator)

and getStopIteration (regs:REGS) =
    getValue regs (#global regs) { id = Ustring.StopIteration_,
                                   ns = getIteratorNamespace regs }

and isStopIteration (regs:REGS)
                    (v:VALUE)
    : bool =
    let
        val stopIteration = needObj regs (getStopIteration regs)
    in
        case v of
            Object obj => (getObjId obj) = (getObjId stopIteration)
          | _ => false
    end

and newGen (execBody:unit -> VALUE)
    : GEN =
    let
        (* temporary state, reassigned below *)
        val state = ref ClosedGen 
    in
        (* this must be done via assignment because of the recursive reference to `state' *)
        state := NewbornGen (fn () =>
                                Control.reset (fn () =>
                                                  ((execBody (); CloseSig)
                                                   handle ThrowException v => ThrowSig v
                                                        | StopIterationException => StopSig)
                                                  before state := ClosedGen));
        Gen state
    end

and newGenerator (regs:REGS)
                 (execBody:OBJ -> VALUE)
    : VALUE =
    let
        val classObj = case !(getGeneratorClassSlot regs) of 
                           NONE => error regs ["cannot find helper::GeneratorImpl"]
                         | SOME ob => ob
        val class = needClass (Object classObj)
        val objRef = ref (newObjectNoTag ())
        val gen = newGen (fn () => execBody (!objRef))
        val tag = PrimitiveTag (GeneratorPrimitive gen)
        val proto = getPrototype regs classObj
        val obj = constructStandardWithTag regs classObj class tag proto [] 
    in
        objRef := obj;
        Object obj
    end

and yieldFromGen (regs:REGS)
                 (Gen state)
                 (v : VALUE)
    : VALUE =
    case !state of
        RunningGen => (case Control.shift (fn k => (state := DormantGen k;
                                                    YieldSig v)) of
                           SendSig v' => v'
                         | ThrowSig v' => raise (ThrowException v')
                         | StopSig => raise StopIterationException
                         | _ => error regs ["generator protocol"])
      | _ => error regs ["yield from dormant or dead generator"]

and sendToGen (regs:REGS)
              (Gen state)
              (v : VALUE)
    : VALUE =
    case !state of
        RunningGen => error regs ["already running"]
      | ClosedGen => raise StopIterationException
      | NewbornGen f =>
        if isUndef v then
            (state := RunningGen;
             case f () of
                 YieldSig v' => v'
               | ThrowSig v' => raise (ThrowException v')
               | StopSig => raise StopIterationException
               | CloseSig => raise StopIterationException
               | _ => error regs ["generator protocol"])
        else
            let val s = Ustring.toAscii (toUstring regs v)
                    (* FIXME: what's the best thing to do here? *)
                    handle ThrowException _ => "<<value>>"
                         | StopIterationException => "StopIteration"
            in
                throwExn (newTypeErr regs ["attempt to send ", s, " to newborn generator"])
            end
      | DormantGen k =>
        (state := RunningGen;
         case k (SendSig v) of
             YieldSig v' => v'
           | ThrowSig v' => raise (ThrowException v')
           | StopSig => raise StopIterationException
           | CloseSig => raise StopIterationException
           | _ => error regs ["generator protocol"])

and throwToGen (regs:REGS)
               (Gen state)
               (v : VALUE)
    : VALUE =
    case !state of
        RunningGen => error regs ["already running"]
      | ClosedGen => raise (ThrowException v)
      | NewbornGen f =>
        (* FIXME: confirm this semantics with be *)
        (state := ClosedGen; raise (ThrowException v))
      | DormantGen k =>
        (state := RunningGen;
         case k (ThrowSig v) of
             YieldSig v' => v'
           | ThrowSig v' => raise (ThrowException v')
           | StopSig => raise StopIterationException
           | CloseSig => raise (ThrowException v)
           | _ => error regs ["generator protocol"])

and closeGen (regs:REGS)
             (Gen state)
    : unit =
    case !state of
        RunningGen => error regs ["already running"]
      | _ => state := ClosedGen


(*
 * ES-262-3 9.8 ToString.
 *
 * We do it down here because we have some actual callers who
 * need it inside the implementation of the runtime. Most of the rest
 * is done up in Conversions.es.
 *)

and toUstring (regs:REGS)
              (v:VALUE)
    : Ustring.STRING =
    case v of
        Undefined => Ustring.undefined_
      | Null => Ustring.null_
      | Object (Obj { tag = (PrimitiveTag prim), ... }) =>
        primitiveToUstring prim
      | _ => toUstring regs (toPrimitiveWithStringHint regs v)

(*
 * ES-262-3 9.2: The ToBoolean operation
 *)

and toBoolean (v:VALUE) : bool =
    case v of
        Undefined => false
      | Null => false
      | Object (Obj { tag, ... }) =>
        (case tag of
             PrimitiveTag (BooleanPrimitive b) => b
           | PrimitiveTag (DoublePrimitive x) => 
             not (Real64.==(x,(Real64.fromInt 0))
                  orelse
                  Real64.isNan x)
           | PrimitiveTag (DecimalPrimitive x) => 
             not ((x = Decimal.zero)
                  orelse
                  (Decimal.isNaN x))
           | PrimitiveTag (StringPrimitive s) => 
             not (Ustring.stringLength s = 0)
           | _ => true)


(*
 * ES-262-3 8.6.2.6: The [[DefaultValue]] operation
 *)

and defaultValue (regs:REGS)
                 (obj:OBJ)
                 (preferredType:Ustring.STRING)
    : VALUE =
    let
        val (na, nb) = if preferredType = Ustring.String_
                       then (public_toString, public_valueOf)
                       else (public_valueOf, public_toString)
        val va = if hasProperty regs obj na
                 then evalNamedMethodCall regs obj na []
                 else Undefined
        val vb = if not (isPrimitive va) andalso hasProperty regs obj nb
                 then evalNamedMethodCall regs obj nb []
                 else va
    in
        if isPrimitive vb
        then vb
        else throwExn (newTypeErr regs ["defaultValue"])
    end

and isPrimitive (v:VALUE)
    : bool =
    isNull v orelse
    isUndef v orelse
    isNumeric v orelse
    isString v orelse
    isBoolean v

(*
 * ES-262-3 9.1: The ToPrimitive operation
 *)

and toPrimitive (regs:REGS)
                (v:VALUE) 
                (hint:Ustring.STRING)
    : VALUE =
    if isPrimitive v
    then v
    else defaultValue regs (needObj regs v) hint

and toPrimitiveWithStringHint (regs:REGS)
                              (v:VALUE)
    : VALUE =
    toPrimitive regs v Ustring.String_

and toPrimitiveWithNumberHint (regs:REGS)
                              (v:VALUE)
    : VALUE =
    toPrimitive regs v Ustring.Number_

and toPrimitiveWithNoHint (regs:REGS)
                          (v:VALUE)
    : VALUE =
    toPrimitive regs v Ustring.empty

(*
 * Arithmetic operations.
 *)

and toNumeric (regs:REGS)
              (v:VALUE)
    : VALUE =
    let
        fun NaN _ = newDouble regs (Real64.posInf / Real64.posInf)
        fun zero _ = newDouble regs (Real64.fromInt 0)
        fun one _ = newDouble regs (Real64.fromInt 1)
    in
        case v of
            Undefined => NaN ()
          | Null => zero ()
          | Object (Obj { tag, ... }) =>
            (case tag of
                 PrimitiveTag (DoublePrimitive _) => v
               | PrimitiveTag (DecimalPrimitive _) => v
               | PrimitiveTag (BooleanPrimitive false) => zero ()
               | PrimitiveTag (BooleanPrimitive true) => one ()
               (*
                * FIXME: This is not the correct definition of ToNumber applied to string.
                * See ES-262-3 9.3.1. We need to talk it over.
                *)
               | PrimitiveTag (StringPrimitive us) =>
                 let val s = Ustring.toAscii us
                 in
                     case Real64.fromString s of
                         SOME s' => newDouble regs s'
                       | NONE => NaN ()
                 end
               (*
                * FIXME: ES-262-3 9.3 defines ToNumber on objects in terms of primitives. We've
                * reorganized the classification of primitives vs. objects. Revisit this.
                *)
               | _ => zero ())
    end


and toDecimal (v:VALUE)
    : Decimal.DEC =
    let 
        val fromStr = Decimal.fromString 
                          Decimal.defaultPrecision 
                          Decimal.defaultRoundingMode
    in
        case v of
            Undefined => Decimal.NaN
          | Null => Decimal.zero
          | Object (Obj { tag, ... }) =>
            (case tag of
                 PrimitiveTag (DoublePrimitive d) =>
                 (* NB: Lossy. *)
                 (case fromStr (Real64.toString d) of
                      SOME d' => d'
                    | NONE => Decimal.NaN)
               | PrimitiveTag (DecimalPrimitive d) => d
               | PrimitiveTag (BooleanPrimitive false) => Decimal.zero
               | PrimitiveTag (BooleanPrimitive true) => Decimal.one
               (*
                * FIXME: This is not the correct definition either. See toNumeric.
                *)
               | PrimitiveTag (StringPrimitive us) =>
                 let 
                     val s = Ustring.toAscii us
                 in
                     case fromStr s of
                         SOME s' => s'
                       | NONE => Decimal.NaN
                 end
               (*
                * FIXME: Possibly wrong here also. See comment in toNumeric.
                *)
               | _ => Decimal.zero)
    end


and toDouble (v:VALUE)
    : Real64.real =
    let
        fun NaN _ = (Real64.posInf / Real64.posInf)
        fun zero _ = (Real64.fromInt 0)
        fun one _ = (Real64.fromInt 1)
    in
        case v of
            Undefined => NaN ()
          | Null => zero ()
          | Object (Obj {tag, ...}) =>
            (case tag of
                 PrimitiveTag (DoublePrimitive d) => d
               | PrimitiveTag (DecimalPrimitive d) =>
                 (* NB: Lossy. *)
                 (case Real64.fromString (Decimal.toString d) of
                      SOME d' => d'
                    | NONE => NaN ())
               | PrimitiveTag (BooleanPrimitive false) => zero ()
               | PrimitiveTag (BooleanPrimitive true) => one ()
               (*
                * FIXME: This is not the correct definition either. See toNumeric.
                *)
               | PrimitiveTag (StringPrimitive us) =>
                 let 
                     val s = Ustring.toAscii us
                 in
                     case Real64.fromString s  of
                         SOME s' => s'
                       | NONE => NaN()
                 end
               (*
                * FIXME: Possibly wrong here also. See comment in toNumeric.
                *)
               | _ => zero ())
    end


and isPositiveZero (v:VALUE)
    : bool =
    let
        fun doubleIsPosZero x =
            Real64.class x = IEEEReal.ZERO
            andalso not (Real64.signBit x)
    in
        mathOp v
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsPosZero)
               false
    end


and isNegativeZero (v:VALUE)
    : bool =
    let
        fun doubleIsNegZero x =
            Real64.class x = IEEEReal.ZERO
            andalso Real64.signBit x
    in
        mathOp v
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsNegZero)
               false
    end


and isPositiveInf (v:VALUE)
    : bool =
    let
        fun doubleIsPosInf x =
            Real64.class x = IEEEReal.INF
            andalso not (Real64.signBit x)
    in
        mathOp v
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsPosInf)
               false
    end


and isNegativeInf (v:VALUE)
    : bool =
    let
        fun doubleIsNegInf x =
            Real64.class x = IEEEReal.INF
            andalso Real64.signBit x
    in
        mathOp v
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsNegInf)
               false
    end


and isNaN (v:VALUE)
    : bool =
    mathOp v
           (SOME Decimal.isNaN)
           (SOME Real64.isNan)
           false


and sign (v:VALUE)
    : int =
    let
        (*
         * FIXME: this implemented 'sign' function returns 1, 0, or -1
         * depending on proximity to 0. Some definitions only return 1 or 0,
         * or only return 1 or -1. Don't know which one the ES-262-3 spec means.
         *)

        fun decimalSign d = case Decimal.compare Decimal.defaultPrecision
                                                 Decimal.defaultRoundingMode
                                                 d Decimal.zero
                             of
                                LESS => ~1
                              | EQUAL => 0
                              | GREATER => 1
    in
        mathOp v
               (SOME decimalSign)
               (SOME Real64.sign)
               0
    end


and floor (v:VALUE)
    : LargeInt.int =
    mathOp v
           (SOME Decimal.floorInt)
           (SOME (Real64.toLargeInt IEEEReal.TO_NEGINF))
           (LargeInt.fromInt 0)


and signFloorAbs (v:VALUE)
    : LargeInt.int =
    let
        val sign = Int.toLarge (sign v)
        val floor = floor v
    in
        LargeInt.*(sign, (LargeInt.abs floor))
    end


(* ES-262-3 9.5 ToInt32 *)

and toInt32 (regs:REGS)
            (v:VALUE)
    : Real64.real =
    let
        val v' = toNumeric regs v
    in
        if (isNaN v' orelse
            isPositiveInf v' orelse
            isNegativeInf v' orelse
            isPositiveZero v' orelse
            isNegativeZero v')
        then 0.0
        else
            let
                val l31 = IntInf.pow (2, 31)
                val l32 = IntInf.pow (2, 32)
                val v'' = IntInf.mod (signFloorAbs v', l32)
            in
                Real64.fromLargeInt (if LargeInt.>= (v'', l31)
                                     then LargeInt.- (v'', l32)
                                     else v'')
            end
    end

and toUIntNN (regs:REGS)
             (nn:int)
             (v:VALUE)
    : Real64.real =
    let
        val v' = toNumeric regs v
    in
        if (isNaN v' orelse
            isPositiveInf v' orelse
            isNegativeInf v' orelse
            isPositiveZero v' orelse
            isNegativeZero v')
        then 0.0
        else
            let
                val mask = IntInf.pow (2, nn)
            in
                Real64.fromLargeInt (LargeInt.mod (signFloorAbs v', mask))
            end
    end


(* ES-262-3 9.6 ToUInt32 *)

and toUInt32 (regs:REGS)
             (v:VALUE)
    : Real64.real =
    toUIntNN regs 32 v


(* ES-262-3 9.6 ToUInt16 *)

and toUInt16 (regs:REGS)
             (v:VALUE)
    : Real64.real =
    toUIntNN regs 16 v


and typeCheck (regs:REGS)
              (v:VALUE)
              (tyExpr:TYPE)
    : VALUE =
    if evalOperatorIs regs v tyExpr
    then v
    else error regs ["typecheck failed, val=", approx v,
                     " type=", ty (typeOfVal regs v),
                     " wanted=", ty tyExpr]

(*

 fun evalExpression (env: ENV)
                    (expr: EXPRESSION)
     : VALUE =
     case expr of
         LiteralNull => 
         Null
       | LiteralUndefined => 
         Undefined
       | LiteralDouble r => 
         newDouble env r
       | LiteralDecimal d => 
         newDecimal env d
       | LiteralBoolean b => 
         newBoolean env b
       | LiteralString s => 
         newString env s
       | LiteralFunction f => 
         newFunctionFromFunc env f
       | LiteralObject {expr, ty} => 
         evalObjectInitialiser env expr ty
       | LiteralArray (ListExpr exprs, typeExpr) =>
         evalArrayInitialiser env exprs typeExpr
       | LiteralRegExp regexp => 
         evalRegExpInitialiser env (#str re)
       | GetExpr ref =>
         evalGetExpr env ref
       | LetExpr {defs, body, head} => 
         evalLetExpr env (valOf head) body
       | ConditionalExpr (aexpr, bexpr, cexpr) => 
         evalCondExpr env aexpr bexpr cexpr
       | BinaryExpr (bop, aexpr, bexpr) => 
         evalBinaryOp env bop aexpr bexpr
       | UnaryExpr (unop, expr) => 
         evalUnaryOp env unop expr
       | TypeExpr ty => 
         evalTypeExpr env (evalTy env ty)
       | ThisExpr kind => 
         evalThisExpr env kind
       | SuperExpr _ => 
         error env ["super expression in illegal context"]
       | SetExpr (aop, pat, expr) => 
         evalSetExpr env aop pat expr
       | CallExpr { func, actuals } => 
         evalCallExpr env func actuals
       | NewExpr { obj, actuals } => 
         evalNewExpr env obj actuals
       | GetTemp n => 
         evalGetTemp env n
       | InitExpr (target,tempHead,inits) => 
         evalInitExpr env target tempHead inits
       | BinaryTypeExpr (typeOp, expr, tyExpr) => 
         evalBinaryTypeOp env typeOp expr tyExpr
       | ApplyTypeExpr { expr, actuals } => 
         evalApplyTypeExpr env expr (map (evalTy env) actuals)
       | _ => unimplError ["unhandled expression type"]    

 *)

and evalExpr (regs:REGS)
             (expr:EXPRESSION)
    : VALUE =
    case expr of
        LiteralExpr lit =>
        evalLiteralExpr regs lit

      | ListExpr es =>
        evalListExpr regs es

      | LexicalReference { name, loc } =>
        evalLexicalReference regs name
        
      | ObjectNameReference { object, name, loc } =>
        evalObjectNameReference regs expr

      | ObjectIndexReference { object, index, loc } =>
        evalObjectIndexReference regs expr

      | LetExpr {defs, body, head} =>
        evalLetExpr regs (valOf head) body

      | TernaryExpr (aexpr, bexpr, cexpr) =>
        evalCondExpr regs aexpr bexpr cexpr

      | BinaryExpr (bop, aexpr, bexpr) =>
        evalBinaryOp regs bop aexpr bexpr

      | UnaryExpr (unop, expr) =>
        evalUnaryOp regs unop expr

      | TypeExpr ty =>
        evalTypeExpr regs (evalTy regs ty)

      | ThisExpr kind =>
        evalThisExpr regs kind

      (* 
       * Naked super-exprs on their own are not permitted. Super is only 
       * permitted in a few contexts, which we explicitly handle elsewhere
       * (CallExprs and constructor settings).
       *)
      | SuperExpr _ => 
        error regs ["super expression in illegal context"]

      | SetExpr (aop, pat, expr) =>
        evalSetExpr regs aop pat expr

      | CallExpr { func, actuals } =>
        evalCallExpr regs func actuals

      | NewExpr { obj, actuals } =>
        evalNewExpr regs obj actuals

      | GetTemp n =>
        evalGetTemp regs n

      | InitExpr (target,tempHead,inits) =>
        evalInitExpr regs target tempHead inits

      | BinaryTypeExpr (typeOp, expr, tyExpr) =>
        evalBinaryTypeOp regs typeOp expr tyExpr

      | GetParam n =>
        unimplError ["unhandled GetParam expression"]

      | ApplyTypeExpr { expr, actuals } =>
        evalApplyTypeExpr regs expr (map (evalTy regs) actuals)

      | YieldExpr expr =>
        evalYieldExpr regs expr

      | _ => unimplError ["unhandled expression type"]    

(* SPEC

fun evalGetExpr (env: ENV)
                (ref: REFERENCE)
    : VALUE =
    let
        val (_, (obj, name)) = evalReference env ident true
    in
        getValue env obj name
    end

 *)

and evalLexicalReference (regs:REGS)
                         (nameExpr:NAME_EXPRESSION)
    : VALUE =
    let
        val (obj, name) = resolveLexicalReference regs nameExpr true
    in
        getValue regs obj name
    end


and evalObjectNameReference (regs:REGS)
                            (expr:EXPRESSION)
    : VALUE =
    let
        val (this, (obj, name)) = resolveObjectReference regs expr
    in
        getValue regs obj name
    end

and evalObjectIndexReference (regs:REGS)
                             (expr:EXPRESSION)
    : VALUE =
    let
        val (this, (obj, name)) = resolveObjectReference regs expr
    in
        getValue regs obj name
    end

(*

 fun evalThisExpression (env: ENV)
                        (kind: THIS_KIND option)
     : VALUE =
     let
         val { this, thisFunction, thisGenerator, ... } = env
     in
         case kind of
             FunctionThis => Object thisFunction
           | GeneratorThis => Object thisGenerator
           | _ => Object this
     end

 *)

and evalThisExpr (regs:REGS)
                 (kind:THIS_KIND option)
    : VALUE = 
    let
        val { this, thisFun, thisGen, ... } = regs
    in
        case kind of
            SOME FunctionThis => 
            (case thisFun of
                 SOME obj => Object obj
               (* this error should never occur, since it will be raised earlier in defn *)
               | _ => error regs ["error: 'this function' used in a non-function context"])
          | SOME GeneratorThis =>
            (case thisGen of
                 SOME obj => Object obj
               (* DAH: this error should also never occur? *)
               | _ => error regs ["error: 'this generator' used in a non-generator-function context"])
          | _ => Object this
    end

and arrayToList (regs:REGS)
                (arr:OBJ)
    : VALUE list =
    let
        val len = doubleToInt
                      (toUInt32 regs
                                (getValue regs arr public_length))
        fun build i vs =
            if (i <  (0:Int32.int))
            then vs
            else
                let
                    val n = public (Ustring.fromInt32 i)
                    val curr = if hasProperty regs arr n
                               then getValue regs arr n
                               else Undefined
                in
                    build (i - (1:Int32.int)) (curr::vs)
                end
    in
        build (len - (1:Int32.int)) []
    end

and evalExprsAndSpliceSpreads (regs:REGS)
                              (exprs:EXPRESSION list)
    : (VALUE list) = 
    let 
        (* 
         * A Spread expression is only allowed in certain syntactic
         * contexts but those are enforced by the parser. Here we
         * splice *any* unary spread operator we encounter in the value
         * list we're producing.
         *
         * Note: spread expressions should allow an array *or*
         * arguments object as its operand. Currently, an arguments
         * object *is* an array, so this is a vacuous distinction.
         *)
        fun f (UnaryExpr (Spread, expr)) = 
            let
                val v = evalExpr regs expr
                val t = typeOfVal regs v
            in
                if t <* (instanceType regs public_Array [])
                then arrayToList regs (needObj regs v)
                else (error regs ["spread expression requires an array or arguments object as its operand; ",
                                  "found instead: ", ty t];
                      [])
            end
          | f e = [evalExpr regs e]
    in
        List.concat (map f exprs)
    end

(* SPEC

fun evalCallExpr (env: ENV)
                 (func: EXPRESSION)
                 (actuals: EXPRESSION list)
    : VALUE =
 (* FINISH ME *)

 *)

and evalCallExpr (regs:REGS)
                 (func:EXPRESSION)
                 (actuals:EXPRESSION list)
    : VALUE = 
    let
        fun args _ = evalExprsAndSpliceSpreads regs actuals
    in
        case func of
            LexicalReference _ => evalCallMethodByExpr regs func (args ())
                                  
          | ObjectNameReference { object = SuperExpr NONE, name, loc } => 
            evalSuperCall regs (#this regs) name (args ())
            
          | ObjectNameReference { object = SuperExpr (SOME b), name, loc } => 
            evalSuperCall regs (needObj regs (evalExpr regs b)) name (args ())
            
          | ObjectNameReference _ => evalCallMethodByExpr regs func (args ())
                                     
          | _ =>
            let
                val funcVal = evalExpr regs func
                val funcObj = needObj regs funcVal
                val args = args ()
            in
                evalCallByObj regs funcObj args
            end
    end

(* SPEC

fun evalNewExpr (env: ENV)
                (func: EXPRESSION)
                (actuals: EXPRESSION list)
    : VALUE =
 (* FINISH ME *)

 *)

and evalNewExpr (regs:REGS)
                (obj:EXPRESSION)
                (actuals:EXPRESSION list)
    : VALUE = 
    let
        fun args _ = evalExprsAndSpliceSpreads regs actuals
        val rhs = evalExpr regs obj
    in
        case rhs of
            Object ob => evalNewObj regs ob (args())
          | _ => throwExn (newTypeErr regs ["not a constructor"])
    end


and evalGetTemp (regs:REGS)
                (n:int)
    : VALUE = 
    let
        val { scope, ... } = regs
        val scopeId = getScopeId scope
        val temps = getScopeTemps scope
    in
        trace ["GetTemp ", Int.toString n, " on scope #", Int.toString scopeId];
        getTemp temps n
    end

(* SPEC

fun evalInitExpr (env: ENV)
                 (target: INIT_TARGET)
                 (head: HEAD)
                 (inits: INITS)
    : VALUE =
 (* FINISH ME *)

 *)


and evalInitExpr (regs:REGS)
                 (target:INIT_TARGET) 
                 (tempHead:HEAD)
                 (inits:INITS) 
    : VALUE = 
    let
        (* 
         * We are aiming to initialize properties on 'target'.
         * 
         * The properties we want to initialize are described in 'inits'.
         * 
         * In order to initialize the target properties, we may have
         * to allocate and initialize a bunch of temps -- to
         * destructure fully -- which are presented here in a HEAD,
         * 'tempHead'. But we do not want to build a transient block
         * scope for tempHead; we want to allocate those temps in the
         * present scope. We are going to *use* the temps allocated
         * by tempHead in the target property inits.
         *
         * NB: do not reorganize this to use evalHead, since it
         * allocates a new scope object containing the head. If you so
         * so, the final evalScopeInits call may fail if there is any
         * deep destructuring.
         *)

        val (Head (tempRib, tempInits)) = tempHead
    in
        (* Allocate and init the temp head in the current scope. *)
        allocScopeRib regs tempRib;
        evalScopeInits regs Local tempInits;
        
        (* Allocate and init the target props. *)
        evalScopeInits regs target inits;
        Undefined
    end
    


and evalSuperCall (regs:REGS)
                  (object:OBJ)
                  (nameExpr:NAME_EXPRESSION)
                  (args:VALUE list)
    : VALUE = 
    let                                         
        val Obj { tag, ... } = object
        val thisType = case tag of 
                           InstanceTag c  => ClassType c
                         | _ => error regs ["missing ClassType tag during super call"]
                                
        fun getSuperClassObjs t = 
            case t of 
                (ClassType (Class {extends, ...})) =>
                (instanceClass regs t) :: 
                (case extends of 
                     NONE => []
                   | SOME t' => getSuperClassObjs t')
              | _ => error regs ["non-class type in extends clause during super call"]

        fun instanceRibOf (Class { instanceRib, ... }) = instanceRib

        val superClassObjs = getSuperClassObjs thisType 

        val superRibs = map (instanceRibOf 
                             o needClass 
                             o (Object))
                            superClassObjs
                            
                            
        val superRibs = case superRibs of 
                            [] => []
                          | x::xs => xs
                                     
        val (n, superClassInstanceRib, func) =
            case Fixture.resolveNameExpr superRibs nameExpr of 
                ((rib::ribs), _, MethodFixture { func, ... }) => 
                ((length superClassObjs) - (length (rib::ribs)), rib, func)
              | _ => error regs ["non-method fixture in super expression"]
                     

        val superClassObj = List.nth (superClassObjs,n)
        val superClassEnv = getClassScope regs superClassObj
        val env = extendScope superClassEnv (#this regs) InstanceScope
        val funcClosure = { func = func, env = env, this = SOME (#this regs) }

        (* Note: although bound methods defined on an object's own class are memoized,
         * those defined on one of the object's ancestor classes will not be, so
         * identity tests work for the former and not the latter... *)
        val thisFun = SOME (needObj regs (newFunctionFromClosure regs funcClosure))

    in
        invokeFuncClosure regs funcClosure thisFun args
    end


and applyTypes (regs:REGS)
               (base:TYPE)
               (args:TYPE list)
    : TYPE = 
    let
        val fullTy = 
            case args of 
                [] => base
              | _ => AppType ( base, args )
    in
        evalTy regs fullTy
    end    


and instanceType (regs:REGS)
                 (name:NAME)
                 (args:TYPE list)
    : TYPE = 
    let
        val instanceTy = Type.instanceTy (#prog regs) name
    in
        applyTypes regs instanceTy args
    end

and traceScope (s:SCOPE)
    : unit =
    if !doTrace
    then 
        let
            val Scope { object, parent, ... } = s
            val Obj { ident, props, ... } = getScopeObj s
            val { bindings, ... } = !props
            val names = map (fn (n,_) => n) (NameMap.listItemsi bindings)
        in    
            trace ["scope: ", Int.toString ident, " = ",
                   (if length names > 5
                    then " ...lots... "
                    else (join ", " (map name names)))];
            case parent of 
                NONE => ()
              | SOME p => traceScope p
        end
    else
        ()

and bindTypes (regs:REGS)
              (typeParams:IDENTIFIER list)
              (typeArgs:TYPE list)
              (env:SCOPE)
    : SCOPE = 
    let 
        val _ = 
            if not (length typeArgs = length typeParams)
            then error regs ["argument length mismatch when binding type args in env"]
            else ()
        val (scopeObj:OBJ) = newObjectNoTag ()
        val _ = trace ["binding ", Int.toString (length typeArgs), 
                       " type args to scope #", Int.toString (getObjId scopeObj)]
        val env = extendScope env scopeObj TypeArgScope
        val paramFixtureNames = map (fn id => PropName (public id)) typeParams
        val argFixtures = map (fn t => TypeFixture ([], t)) typeArgs
        val typeRib = ListPair.zip (paramFixtureNames, argFixtures)
        val _ = allocObjRib regs scopeObj NONE typeRib
    in
        env
    end

(* Types of various kinds are have a dual representation: both as TYPEs
 * and as a primitive within VALUE, such as a CLASS_CLOSURE.
 * Really would love to unify these two representations,
 * which might enable nice downstream simplifications. - cf
 *)

and applyTypesToClass (regs:REGS)
                      (classVal:VALUE)
                      (typeArgs:TYPE list)
    : VALUE = 
    let
        val cls = needClass classVal
        val Class c = cls
        val newCls = Class { name = (#name c), 
                             privateNS = (#privateNS c),
                             protectedNS = (#protectedNS c),
                             parentProtectedNSs = (#parentProtectedNSs c),
                             typeParams = (#typeParams c),
                             nonnullable = (#nonnullable c),
                             dynamic = (#dynamic c),
                             (* FIXME: apply to base types when logic for this is present in defn. *)
                             extends = (#extends c),
                             implements = (#implements c),
                             classRib = (#classRib c),
                             instanceRib = (#instanceRib c),
                             instanceInits = (#instanceInits c),
                             constructor = (#constructor c),
                             classType = (#classType c)  }
        val baseClassObj = needObj regs classVal
    in
        newClass regs newCls
    end


and applyTypesToInterface (regs:REGS)
                          (interfaceVal:VALUE)
                          (typeArgs:TYPE list)
    : VALUE = 
    let
        val iface = needInterface interfaceVal
        val Interface i = iface
        val newIface = Interface { name = (#name i), 
                                   typeParams = (#typeParams i),
                                   nonnullable = (#nonnullable i),
                                   extends = (#extends i),
                                   instanceRib = (#instanceRib i) }
    in
        newInterface regs newIface
    end
    
    
and applyTypesToFunction (regs:REGS)
                         (functionVal:VALUE)
                         (typeArgs:TYPE list)
    : VALUE = 
    let
        val funClosure = needFunction functionVal
        val { func, this, env } = funClosure
        val Func { ty, ... } = func
    in
        if Type.isGroundType ty
        then functionVal
        else 
            let
                fun applyArgs t = applyTypes regs t typeArgs
                val Func f = func
                val FunctionSignature { typeParams, ... } = (#fsig f)
                val newFunc = Func { name = (#name f), 
                                     fsig = (#fsig f),
                                     native = (#native f),
                                     generator = (#generator f),
                                     block = (#block f),
                                     param = (#param f),
                                     defaults = (#defaults f),
                                     ty = applyArgs (#ty f),
                                     loc = (#loc f) }
                val newClosure = { func = newFunc,
                                   this = this,
                                   env = bindTypes regs typeParams typeArgs env }
            in
                newFunctionFromClosure regs newClosure
            end
    end


and instanceClass (regs:REGS)
                  (ity:TYPE)
    : OBJ = 
    let 
        fun fetch n = getValue regs (#global regs) n
    in
        case ity of 
            ClassType (Class { name, ... }) => 
            needObj regs (fetch name)
            
          | AppType (ClassType (Class {name, ... }), types) => 
            needObj regs (applyTypesToClass regs (fetch name) types)
            
          | _ => error regs ["unexpected type in instanceClass"]
    end


and instanceInterface (regs:REGS)
                      (ity:TYPE)
    : OBJ = 
    let 
        fun fetch n = getValue regs (#global regs) n
    in
        case ity of 
            InterfaceType (Interface { name, ... }) => 
            needObj regs (fetch name)
            
          | AppType (InterfaceType (Interface {name, ... }), types) => 
            needObj regs (applyTypesToInterface regs (fetch name) types)
            
          | _ => error regs ["unexpected type in instanceInterface"]
    end

(* SPEC

fun evalApplyTypeExpr (env: ENV)
                      (expr: EXPRESSION)
                      (args: TYPE list)
    : VALUE =

 *)

and evalApplyTypeExpr (regs:REGS)
                      (expr:EXPRESSION)
                      (args:TYPE list)
    : VALUE =
    let
        val v = evalExpr regs expr
    in
        if isFunction v
        then applyTypesToFunction regs v args
        (* cf: these are not allowed, I think
        else 
            if isClass v
            then applyTypesToClass regs v args
            else
                if isInterface v
                then applyTypesToInterface regs v args
         *)
        else 
            throwExn (newTypeErr regs ["applying types to unknown base value: ",
                                       approx v])
    end


and evalYieldExpr (regs:REGS)
                  (expr:EXPRESSION option)
    : VALUE =
    let
        val { thisGen, ... } = regs
    in
        case thisGen of
            SOME (Obj { tag, ... }) =>
            (case tag of
                 PrimitiveTag (GeneratorPrimitive gen) =>
                 let
                     val v = case expr of
                                 NONE => Undefined
                               | SOME expr => evalExpr regs expr
                 in
                     yieldFromGen regs gen v
                 end
               | _ => error regs ["missing Generator tag on object in yield"])
          (* this should never happen *)
          | NONE => error regs ["yield expression in a non-generator function context"]
    end

(* SPEC

fun evalArrayInitialiser (env: ENV)
                         (elements: VALUE list)
                         (typeExpr: TYPEESSION)
    : VALUE =
 (* FINISH ME *)

 *)

and evalLiteralArrayExpr (regs:REGS)
                         (exprs:EXPRESSION list)
                         (ty:TYPE option)
    : VALUE =
    let
        val vals = evalExprsAndSpliceSpreads regs exprs
        val (newTag, newClassVal) = 
            case Option.map (evalTy regs) ty of 
                NONE => 
                ((ArrayTag ([], (SOME AnyType))), 
                 getValue regs (#global regs) public_Array)
              | SOME (ArrayType (tys,tyo)) =>
                (ArrayTag (tys, tyo),
                 getValue regs (#global regs) public_Array)
              | SOME ty => 
                let
                    val cv = Object (instanceClass regs ty)
                in
                    (InstanceTag (needClass cv), cv)
                end

        val newClass = needClass newClassVal
        val newClassObj = needObj regs newClassVal
        val proto = getPrototype regs newClassObj 
        val obj = constructStandardWithTag regs newClassObj newClass newTag proto [] 
        val (Obj {props, ...}) = obj
        fun putVal n [] = n
          | putVal n (v::vs) =
            let
                val name = public (Ustring.fromInt n)
            in
                setValue regs obj name v;
                putVal (n+1) vs
            end
        val numProps = putVal 0 vals
    in
        setValue regs obj public_length (newDouble regs (Real64.fromInt numProps));
        Object obj
    end

(*
 fun evalObjectInitialiser (env: ENV)
                           (fields: FIELD list)
                           (typeExpr: TYPEESSION)
     : VALUE =
 (* FINISH ME *)
 *)

and evalLiteralObjectExpr (regs:REGS)
                          (fields:FIELD list)
                          (ty:TYPE option)
    : VALUE =
    let
        fun searchFieldTypes n [] = AnyType
          | searchFieldTypes n ((name,ty)::ts) =
            if n = (evalNameExpr regs name)
            then ty
            else searchFieldTypes n ts
        fun getObjClassVal _ = case !(getObjectClassSlot regs) of
                                   NONE => error regs ["missing class public::Object"]
                                 | SOME ob => Object ob
        val (newTag, newClassVal, tyExprs) = 
            case Option.map (evalTy regs) ty of 
                NONE => ((ObjectTag []), getObjClassVal(), [])
              | SOME (RecordType fields) => 
                (ObjectTag fields, getObjClassVal(), fields)
              | SOME ty => 
                let 
                    val cv = Object (instanceClass regs ty)
                in
                    (InstanceTag (needClass cv), cv, [])
                end

        val newClass = needClass newClassVal
        val newClassObj = needObj regs newClassVal
        val proto = getPrototype regs newClassObj
        val obj = constructStandardWithTag regs newClassObj newClass newTag proto [] 
        val (Obj {props, ...}) = obj
                                 
        fun getPropState (v:VALUE) : PROPERTY_STATE =
            case v of
                Object (Obj {tag, ...}) =>
                (case tag of
                     PrimitiveTag (FunctionPrimitive closure) => 
                     let 
                         val Func { name, ... } = (#func closure)
                         val kind = (#kind name)
                     in
                         if kind = Get
                         then VirtualValProp { getter = SOME closure,
                                               setter = NONE }
                         else if kind = Set
                         then VirtualValProp { getter = NONE,
                                               setter = SOME closure }
                         else ValProp v
                     end
                   | _ => ValProp v)
              | _ => ValProp v

        fun mergePropState (existingProp:PROPERTY option) 
                           (newState:PROPERTY_STATE) 
            : PROPERTY_STATE =
            let
                fun merge existing new =
                    case new of
                        SOME a => new
                      | NONE => existing
            in
                case newState of
                    VirtualValProp { getter = ng, setter = ns } => 
                    (case existingProp of
                         SOME { state = VirtualValProp { getter = eg, 
                                                         setter = es }, ... } =>
                         VirtualValProp { getter = merge eg ng,
                                          setter = merge es ns }
                       | _ => newState)
                  | _ => newState
            end  

        fun processField {kind, name, init} =
            let
                val writability = case kind of
                                      Const => WriteOnce
                                    | LetConst => WriteOnce
                                    | _ => Writable
                val removable = case kind of 
                                    Const => false
                                  | LetConst => false
                                  | _ => true
                val (n:NAME) = evalNameExpr regs name
                val v = evalExpr regs init
                val ty = searchFieldTypes n tyExprs
                val attrs = { removable = removable,
                              enumerable = true,
                              writable = writability,
                              fixed = false }
                val state = getPropState v
                val existingProp = findProp props n
                val prop = { ty = ty,
                             attrs = attrs,
                             state = mergePropState existingProp state }
            in
                addProp props n prop
            end
    in
        List.app processField fields;
        Object obj
    end

(*

 fun evalRegExpInitialiser (env: ENV)
                           (regexp: Ustring.STRING)
     : MACH.VALUE =
 (* FIXME *)

 *)

and evalLiteralRegExp (regs:REGS)
                      (re:Ustring.STRING)
    : VALUE =
    let fun findSplit 0 = 0
          | findSplit n =
            if Ustring.charCodeAt re n = Char.ord #"/" then
                n
            else
                findSplit (n-1)
        val len = Ustring.stringLength re
        val split = findSplit (len - 1)
    in
        newRegExp regs 
                  (Ustring.substring re      1             (split - 1) )
                  (Ustring.substring re (split + 1) (len - (split + 1)))
    end


and evalLiteralExpr (regs:REGS)
                    (lit:LITERAL)
    : VALUE =
    case lit of
        LiteralNull => Null
      | LiteralUndefined => Undefined
      | LiteralDouble r => newDouble regs r
      | LiteralDecimal d => newDecimal regs d
      | LiteralBoolean b => newBoolean regs b
      | LiteralString s => newString regs s
      | LiteralArray {exprs=ListExpr exprs, ty} => evalLiteralArrayExpr regs exprs ty (* FIXME handle comprehensions *)
      | LiteralArray _ => unimplError ["unhandled non-ListExpr array literal"]
      | LiteralXML _ => unimplError ["unhandled literal XML"]
      | LiteralNamespace n => newNamespace regs n                
      | LiteralObject {expr, ty} => evalLiteralObjectExpr regs expr ty
      | LiteralFunction f => newFunctionFromFunc regs (#scope regs) f
      | LiteralRegExp re => evalLiteralRegExp regs (#str re)

and evalListExpr (regs:REGS)
                 (es:EXPRESSION list)
    : VALUE =
    case es of
        [] => Undefined
      | [e] => evalExpr regs e
      | (e::ez) => ((evalExpr regs e); (evalListExpr regs ez))


(* 
 * ES3 13.2.2 [[Construct]] on a Function object.
 *)

and constructObjectViaFunction (regs:REGS)
                               (ctorObj:OBJ)
                               (ctor:FUN_CLOSURE)
                               (args:VALUE list)
    : VALUE =
    let
        val ctorProto = getPrototype regs ctorObj
        val proto = case ctorProto of 
                        Object ob => ctorProto
                      | _ => getOriginalObjectPrototype regs

        val (objClassObj, objClass) = getObjectClassObjectAndClass regs
        val newObject = constructStandard regs objClassObj objClass proto []
        val constructorRegs = withThis regs newObject
        val constructorResult = invokeFuncClosure constructorRegs ctor (SOME ctorObj) args
    in
        if isObject constructorResult
        then constructorResult
        else Object newObject
    end
    

and evalNewObj (regs:REGS)
               (obj:OBJ)
               (args:VALUE list)
    : VALUE =
    case obj of
        Obj { tag, ... } =>
        case tag of
            PrimitiveTag (ClassPrimitive c) => constructClassInstance regs obj c args
          | PrimitiveTag (FunctionPrimitive f) => constructObjectViaFunction regs obj f args
          | _ => throwExn (newTypeErr regs ["operator 'new' applied to unexpected object type"])


and evalCallMethodByExpr (regs:REGS)
                         (func:EXPRESSION)
                         (args:VALUE list)
    : VALUE =
    let
        (*
         * If we have a method or native function *property*, we can just
         * call it directly without manufacturing a temporary Function
         * wrapper object.
         *)
        val _ = trace [">>> evalCallMethodByExpr"]
        val (thisObjOpt, r) = resolveRefExpr regs func true
        val thisObj = case thisObjOpt of 
                          NONE => (#this regs)
                        | SOME obj => obj
        val _ = trace ["resolved thisObj=#", (Int.toString (getObjId thisObj)), " for call"]
        val result = evalCallByRef (withThis regs thisObj) r args true
    in
        trace ["<<< evalCallMethodByExpr"];
        result
    end


and evalNamedMethodCall (regs:REGS)
                        (obj:OBJ)
                        (name:NAME)
                        (args:VALUE list)
    : VALUE = 
    let
        val refOpt = resolveName regs [obj] (nameExprOf name)
    in
        case refOpt of 
            NONE => error regs ["unable to resolve method: ", LogErr.name name]
          | SOME r => evalCallByRef (withThis regs obj) r args true
    end

(* 
 * Note: unless you are part of the function-calling machinery, you probably do 
 * not want to call evalCallByRef. It assumes you're passing in a sane 
 * ref and a sane 'this' register. If you just want to synthesize 
 * "a call to x.y()" use evalNamedMethodCall, above.
 *)
and evalCallByRef (regs:REGS)
                  (r:REF)
                  (args:VALUE list)
                  (useThisFun:bool)
    : VALUE =
    let
        val (obj, name) = r
        val _ = trace [">>> evalCallByRef ", fmtName name]
        val Obj { props, ... } = obj
        val res = 
            case (#state (getProp props name)) of
                NativeFunctionProp { func, ...} => func regs args
              | MethodProp f => 
                let 
                    val thisFun = if useThisFun andalso not (isBooting regs)
                                  then SOME (needObj regs (getValue regs obj name))
                                  else NONE
                in
                    invokeFuncClosure (withThis regs obj) f thisFun args
                end
              | _ =>
                (trace ["evalCallByRef: non-method property ",
                        "referenced, getting and calling"];
                 evalCallByObj regs (needObj regs (getValue regs obj name)) args)
        val _ = trace ["<<< evalCallByRef ", fmtName name]
    in
        res
    end

and evalCallByObj (regs:REGS)
                  (fobj:OBJ)
                  (args:VALUE list)
    : VALUE =
    case fobj of
        Obj { tag, ... } =>
        case tag of
            PrimitiveTag (NativeFunctionPrimitive { func, ... }) =>
            (trace ["evalCallByObj: entering native function"];
             func regs args)
          | PrimitiveTag (FunctionPrimitive f) =>
            (trace ["evalCallByObj: entering standard function"];
             invokeFuncClosure regs f (SOME fobj) args)
          | _ =>
            if hasOwnProperty regs fobj meta_invoke
            then
                (trace ["evalCallByObj: redirecting through meta::invoke"];
                 evalCallByRef regs (fobj, meta_invoke) args true)
            else throwExn (newTypeErr regs ["calling non-callable object"])

(* SPEC

fun evalSetExpr (env: ENV)
                (assignOp: ASSIGNOP)
                (leftSide: EXPRESSION)
                (rightSide: EXPRESSION)
    : VALUE =
 (* FINISH ME *)

 *)

and evalSetExpr (regs:REGS)
                (aop:ASSIGNOP)
                (lhs:EXPRESSION)
                (rhs:EXPRESSION)
    : VALUE =
    let
        val _ = trace ["evalSetExpr"]
        val (thisOpt, (obj, name)) = resolveRefExpr regs lhs false
        val v =
            let
                fun modifyWith bop =
                    let val v = evalExpr regs rhs
                    in performBinop 
                           regs bop 
                           (getValue regs obj name) 
                           v
                    end
            in
                case aop of
                    Assign => evalExpr regs rhs
                  | AssignPlus => modifyWith Plus
                  | AssignMinus => modifyWith Minus
                  | AssignTimes => modifyWith Times
                  | AssignDivide => modifyWith Divide
                  | AssignRemainder => modifyWith Remainder
                  | AssignLeftShift => modifyWith LeftShift
                  | AssignRightShift => modifyWith RightShift
                  | AssignRightShiftUnsigned => modifyWith RightShiftUnsigned
                  | AssignBitwiseAnd => modifyWith BitwiseAnd
                  | AssignBitwiseOr => modifyWith BitwiseOr
                  | AssignBitwiseXor => modifyWith BitwiseXor
                  | AssignLogicalAnd =>
                    let
                        val a = getValue regs obj name
                    in
                        if toBoolean a
                        then evalExpr regs rhs
                        else a
                    end
                  | AssignLogicalOr =>
                    let
                        val a = getValue regs obj name
                    in
                        if toBoolean a
                        then a
                        else evalExpr regs rhs
                    end
            end
    in
        trace ["setExpr assignment to slot ", fmtName name];
        (case thisOpt of 
             NONE => setValue regs obj name v
           | SOME this => setValue regs this name v);
        v
    end


and numberOfSimilarType (regs:REGS)
                        (v:VALUE)
                        (d:Real64.real)
    : VALUE = 
    let
        val nt = numTypeOf regs v
    in
        case nt of 
            _ => newDouble regs d
    end


and evalCrement (regs:REGS)
                (bop:BINOP)
                (pre:bool)
                (expr:EXPRESSION)
    : VALUE = 
    let
        val (_, (obj, name)) = resolveRefExpr regs expr false
        val v = getValue regs obj name
        val v' = toNumeric regs v
        val i = numberOfSimilarType regs v' 1.0
        val v'' = performBinop regs bop v' i
    in
        setValue regs obj name v'';
        if pre
        then v''
        else v'
    end        
    
(* SPEC

fun evalUnaryExpr (env: ENV)
                  (unaryOp: ASSIGNOP)
                  (expr: EXPRESSION)
    : VALUE =
 (* FINISH ME *)

 *)


and evalUnaryOp (regs:REGS)
                (unop:UNOP)
                (expr:EXPRESSION)
    : VALUE =
    let
    in
        case unop of
            Delete => 
            let
                val (_, (Obj {props, ...}, name)) = resolveRefExpr regs expr false
            in
                if (hasProp props name)
                then if (#removable (#attrs (getProp props name)))
                     then (delProp props name; newBoolean regs true)
                     else newBoolean regs false
                else newBoolean regs true
            end

          | PreIncrement => evalCrement regs Plus true expr
          | PreDecrement => evalCrement regs Minus true expr
          | PostIncrement => evalCrement regs Plus false expr
          | PostDecrement => evalCrement regs Minus false expr
          | BitwiseNot =>
            newDouble regs (wordToDouble
                                (Word32.notb
                                     (doubleToWord 
                                          (toUInt32 regs
                                                    (evalExpr regs expr)))))
            
          | LogicalNot =>
            newBoolean regs (not (toBoolean (evalExpr regs expr)))

          | UnaryPlus =>
            toNumeric regs (evalExpr regs expr)

          | UnaryMinus =>            
            let
                val v = toNumeric regs (evalExpr regs expr)
            in
                performBinop regs Minus (newDouble regs 0.0) v
            end

          | Void => Undefined

          | Spread => error regs ["spread operator in unexpected context"]

          | Type =>
            (*
             * FIXME: not clear what this operator does; I thought it just
             * affected parse context.
             *)
            evalExpr regs expr

          | Typeof =>
            (*
             * ES-262-3 1.4.3 backward-compatibility operation.
             *)
            let
                fun typeNameOfVal (v:VALUE) =
                    case v of
                        Null => Ustring.object_
                      | Undefined => Ustring.undefined_
                      | Object (Obj ob) =>
                        let
                            val n = nominalBaseOfTag (#tag ob)
                        in
                            if n = ES4_double orelse
                               n = ES4_decimal
                            then Ustring.number_
                            else
                                (if n = ES4_boolean
                                 then Ustring.boolean_
                                 else
                                     (if n = public_Function
                                      then Ustring.function_
                                      else
                                          (if n = ES4_string
                                           then Ustring.string_
                                           else Ustring.object_)))
                        end
            in
                newString regs
                          (case expr of
                               LexicalReference { name, loc } =>
                               let
                                  val (obj, name) = resolveLexicalReference regs name true
                               in
                                   typeNameOfVal (getValue regs obj name)
                               end
                             | _ => typeNameOfVal (evalExpr regs expr))
            end
    end

(* SPEC

fun evalYieldExpr (env: ENV)
                  (expr: EXPRESSION)
    : VALUE =
 (* FINISH ME *)

 *)


and evalTypeExpr (regs:REGS)
                 (te:TYPE)
    : VALUE =
    case te of
        AnyType => Null (* FIXME *)
      (*  | VoidType => Null (* FIXME *) *)
      | NullType => Null (* FIXME *)
      | UndefinedType => Null (* FIXME *)
      | UnionType ut => Null (* FIXME *)
      | ArrayType a => Null (* FIXME *)
      | TypeName (tn, _) => evalExpr regs (LexicalReference { name=tn, loc=NONE })
      | FunctionType ft => Null (* FIXME *)
      | RecordType ot => Null (* FIXME *)
      | NonNullType _ => Null (* FIXME *)
      | ClassType c => Null (* FIXME *)
      | InterfaceType t => Null (* FIXME *)
      | _ => Null (* FIXME *)

and wordToDouble (x:Word32.word) 
    : Real64.real =
    (Real64.fromLargeInt (Word32.toLargeInt x))

and doubleToWord (d:Real64.real)
    : Word32.word =
    Word32.fromLargeInt (Real64.toLargeInt IEEEReal.TO_NEAREST d)

and doubleToInt (d:Real64.real)
    : Int32.int =
    Int32.fromLarge (Real64.toLargeInt IEEEReal.TO_NEAREST d)


and numTypeOf (regs:REGS) 
              (v:VALUE) 
    : NUMBER_TYPE = 
    let
        val ty = typeOfVal regs v 
        fun sameItype t2 = 
            case (ty, t2) of
                (ClassType (Class {name=name1, ...}), ClassType (Class {name=name2, ...})) => 
                nameEq name1 name2
              | _ => false
    in
        (* Don't use <* here, it is willing to convert numeric types! *)
        if sameItype (instanceType regs ES4_double [])
        then DoubleNum
        else
            if sameItype (instanceType regs ES4_decimal [])
            then DecimalNum
            else error regs ["unexpected type in numTypeOf: ", LogErr.ty ty]
    end


and performDecimalBinop (regs:REGS)
                        (bop:BINOP)
                        (va:VALUE)
                        (vb:VALUE)
    : VALUE = 
    let
        val precision = Decimal.defaultPrecision 
        val mode = Decimal.defaultRoundingMode
        val da = toDecimal va
        val db = toDecimal vb
        val dc = case bop of 
                     Plus => Decimal.add precision mode da db
                   | Minus => Decimal.subtract precision mode da db
                   | Times => Decimal.multiply precision mode da db
                   | Divide => Decimal.divide precision mode da db
                   | Remainder => Decimal.remainder precision mode da db
                   | _ => error regs ["unexpected binary operator in performDecimalBinop"]
    in
        newDecimal regs dc
    end

    
and realRem (x,y) =
    let
        val n = Real.realTrunc(x / y)
    in
        x - ( n * y)
    end


and performNumericBinop (regs:REGS)
                        (bop:BINOP)
                        (va:VALUE)
                        (vb:VALUE)
    : VALUE = 
    let
        val va' = toNumeric regs va
        val vb' = toNumeric regs vb
        val na = numTypeOf regs va'
        val nb = numTypeOf regs vb'
        val commonNumType = promoteToCommon regs na nb
    in
        if commonNumType = DecimalNum
        then performDecimalBinop regs bop va' vb'
        else 
            let
                val da = toDouble va'
                val db = toDouble vb'
                val dc = case bop of 
                             Plus => da + db
                           | Minus => da - db
                           | Times => da * db
                           | Divide => da / db
                           | Remainder => realRem (da,db)
                           | _ => error regs ["unexpected binary operator in performNumericBinop"]
            in
                newDouble regs dc
            end
    end


and performBinop (regs:REGS)
                 (bop:BINOP)
                 (va:VALUE)
                 (vb:VALUE)
    : VALUE =

    let
        val precision = Decimal.defaultPrecision
        val mode = Decimal.defaultRoundingMode

        fun dispatchComparison cmp =            
            let
                val va = toPrimitiveWithNumberHint regs va
                val vb = toPrimitiveWithNumberHint regs vb
            in
                (*
                 * ES-262-3 11.8.5 Abstract Relational Comparison Algorithm
                 *)
                if isString va andalso isString vb
                then newBoolean 
                         regs 
                         (cmp (Ustring.compare 
                                   (toUstring regs va)
                                   (toUstring regs vb)))
                else
                    let
                        val va = toNumeric regs va
                        val vb = toNumeric regs vb
                        val commonNumType = promoteToCommon regs 
                                                            (numTypeOf regs va) 
                                                            (numTypeOf regs vb)
                    in
                        if isNaN va orelse 
                           isNaN vb
                        then newBoolean regs false
                        else newBoolean regs 
                                        (case commonNumType of 
                                             DoubleNum => 
                                             cmp (Real64.compare 
                                                      ((toDouble va),
                                                       (toDouble vb)))
                                             
                                           | DecimalNum => 
                                             cmp (Decimal.compare 
                                                      precision mode 
                                                      (toDecimal va) 
                                                      (toDecimal vb)))
                    end
            end

        fun masku5 (x:Word32.word) : Word.word =
            Word.fromInt (Word32.toInt (Word32.andb (x, (valOf (Word32.fromString "0x1F")))))

        fun bitwiseWordOp f =
            newDouble regs (wordToDouble (f ((doubleToWord (toUInt32 regs va),
                                              (doubleToWord (toUInt32 regs vb))))))
            
        (*
         * ES-262-3 11.9.6 Strict Equality Comparison Algorithm
         *
         *  (with extension to cover namespaces)
         *)

        fun tripleEquals _ =
            if isSameType va vb
            then
                if isUndef va orelse 
                   isNull va
                then newBoolean regs true
                else
                    if isNumeric va
                    then if isNaN va orelse 
                            isNaN vb
                         then newBoolean regs false
                         else dispatchComparison (fn x => x = EQUAL)
                    else
                        if isString va
                        then case Ustring.compare (toUstring regs va) (toUstring regs vb) of
                                 EQUAL => newBoolean regs true
                               | _ => newBoolean regs false
                        else
                            if isBoolean va
                            then newBoolean regs (toBoolean va = toBoolean vb)
                            else 
                                if isNamespace va andalso isNamespace vb
                                then newBoolean regs (Fixture.compareNamespaces  ((needNamespace va),
                                                                                  (needNamespace vb)))                                               
                                else newBoolean regs ((getObjId (needObj regs va)) = (getObjId (needObj regs vb)))
            else
                newBoolean regs false

        (*
         * ES-262-3 11.9.3 Abstract Equality Comparison Algorithm
         *)

        fun doubleEquals' _ =
            if isSameType va vb
            then tripleEquals ()
            else
                if (isNull va andalso 
                    isUndef vb)
                   orelse 
                   (isUndef va andalso 
                    isNull vb)
                then newBoolean regs true
                else
                    if (isNumeric va andalso 
                        isString vb) 
                       orelse 
                       (isString va andalso 
                        isNumeric vb)
                    then
                        performBinop 
                            regs Equals 
                            (toNumeric regs va) 
                            (toNumeric regs vb)
                    else
                        if isBoolean va
                        then performBinop 
                                 regs Equals
                                 (toNumeric regs va) vb
                        else
                            if isBoolean vb
                            then performBinop 
                                     regs Equals va 
                                     (toNumeric regs vb)
                            else
                                if (isString va orelse 
                                    isNumeric va) andalso 
                                   isObject vb
                                then performBinop 
                                         regs Equals va 
                                         (toPrimitiveWithNoHint regs vb)
                                else
                                    if isObject va andalso 
                                       (isString vb orelse 
                                        isNumeric vb)
                                    then performBinop 
                                             regs Equals
                                             (toPrimitiveWithNoHint regs va) vb
                                    else newBoolean regs false

        val binOpName =
            case bop of
                Plus => "+"
              | Minus => "-"
              | Times => "*"
              | Divide => "/"
              | Remainder => "%"
              | LeftShift => "<<"
              | RightShift => ">>"
              | RightShiftUnsigned => ">>>"
              | BitwiseAnd => "&"
              | BitwiseOr => "|"
              | BitwiseXor => "^"
              | LogicalAnd => "&&"
              | LogicalOr => "||"
              | InstanceOf => "instanceof"
              | In => "in"
              | Equals => "=="
              | NotEquals => "!="
              | StrictEquals => "==="
              | StrictNotEquals => "!=="
              | Less => "<"
              | LessOrEqual => "<="
              | Greater => ">"
              | GreaterOrEqual => ">="
              | Comma => ","
        val res =

            case bop of 

                Plus => (* ES3 11.6.1 The Addition operator (+) *)
                let
                    val a = toPrimitiveWithNoHint regs va
                    val b = toPrimitiveWithNoHint regs vb
                in
                    if isString a orelse isString b
                    then 
                        let
                            val sa = toUstring regs va
                            val sb = toUstring regs vb
                            val sc = Ustring.stringAppend sa sb
                        in
                            newString regs sc
                        end
                    else
                        performNumericBinop regs bop a b
                end
                
              | Minus => performNumericBinop regs bop va vb                
              | Times => performNumericBinop regs bop va vb
              | Divide => performNumericBinop regs bop va vb
              | Remainder => performNumericBinop regs bop va vb

              | LeftShift =>
                newDouble regs (wordToDouble (Word32.<< ((doubleToWord (toInt32 regs va)),
                                                         (masku5 (doubleToWord (toUInt32 regs vb))))))
                
              | RightShift =>
                newDouble regs (wordToDouble (Word32.>> ((doubleToWord (toInt32 regs va)),
                                                         (masku5 (doubleToWord (toUInt32 regs vb))))))

              | RightShiftUnsigned =>
                newDouble regs (wordToDouble (Word32.~>> ((doubleToWord (toUInt32 regs va)),
                                                          (masku5 (doubleToWord (toUInt32 regs vb))))))

              (* FIXME: should we return int if we do int|int or int&int ? *)
              | BitwiseAnd => bitwiseWordOp (Word32.andb)
              | BitwiseOr => bitwiseWordOp (Word32.orb)
              | BitwiseXor => bitwiseWordOp (Word32.xorb)

              | Equals => doubleEquals' ()

              | NotEquals =>
                newBoolean regs (not (toBoolean (doubleEquals' ())))

              | StrictEquals =>
                tripleEquals ()

              | StrictNotEquals =>
                newBoolean regs (not (toBoolean (tripleEquals ())))

              | Less =>
                dispatchComparison 
                    (fn x => x = LESS)

              | LessOrEqual =>
                dispatchComparison 
                    (fn x => (x = LESS) orelse (x = EQUAL))

              | Greater =>
                dispatchComparison 
                    (fn x => x = GREATER)

              | GreaterOrEqual =>
                dispatchComparison 
                    (fn x => (x = GREATER) orelse (x = EQUAL))

              | _ => error regs ["unexpected binary operator in performBinOp"]
    in
        trace ["binop: ", 
               approx va, " ", 
               binOpName, " ", 
               approx vb, " -> ", 
               approx res];
        res
    end


and doubleEquals (regs:REGS)
                 (a:VALUE)
                 (b:VALUE)
    : bool =
    let
        val b = performBinop regs Equals a b
    in
        toBoolean b
    end


and typeOfTag (regs:REGS)
              (tag:TAG)
    : (TYPE) =
    let
        fun primitiveClassType getter = 
            let
                val cell = getter regs 
            in
                case !cell of 
                    SOME (Obj { tag = PrimitiveTag (ClassPrimitive c), 
                                ...}) => 
                    ClassType c
                  | _ => error regs ["error fetching primitive instance type"]
            end
    in
        case tag of
            InstanceTag ity => ClassType ity
          | ObjectTag tys => RecordType tys
          | ArrayTag (tys,tyo) => ArrayType (tys,tyo)
          | PrimitiveTag (BooleanPrimitive _) => primitiveClassType getBooleanClassSlot
          | PrimitiveTag (DoublePrimitive _) => primitiveClassType getDoubleClassSlot
          | PrimitiveTag (DecimalPrimitive _) => primitiveClassType getDecimalClassSlot
          | PrimitiveTag (StringPrimitive _) => primitiveClassType getStringClassSlot
          | PrimitiveTag (NamespacePrimitive _) => primitiveClassType getNamespaceClassSlot
          | PrimitiveTag (ClassPrimitive _) => primitiveClassType getClassClassSlot
          | PrimitiveTag (InterfacePrimitive _) => primitiveClassType getInterfaceClassSlot
          | PrimitiveTag (TypePrimitive _) => primitiveClassType getTypeInterfaceSlot
          | PrimitiveTag (NativeFunctionPrimitive _) => primitiveClassType getFunctionClassSlot
          | PrimitiveTag (GeneratorPrimitive _) => primitiveClassType getGeneratorClassSlot
          | PrimitiveTag (FunctionPrimitive {func=Func { ty, ...}, ...}) => ty
                                                                            
          | NoTag =>
            (* FIXME: this would be a hard error if we didn't use NoTag values
             * as temporaries. Currently we do, so there are contexts where we
             * want them to have a type in order to pass a runtime type test.
             * this is of dubious value to me. -graydon.
             *
             * error regs ["typeOfVal on NoTag object"])
             *)
            AnyType
    end

    
and typeOfVal (regs:REGS)
              (v:VALUE)
    : TYPE =
    let
        val te = case v of
                     Undefined => UndefinedType
                   | Null => NullType
                   | Object obj => 
                     let 
                         val tag = getObjTag obj
                     in
                         typeOfTag regs tag
                     end
    in
        evalTy regs te
    end


and evalLogicalAnd (regs:REGS)
                   (aexpr:EXPRESSION)
                   (bexpr:EXPRESSION)
    : VALUE = 
    let
        val a = evalExpr regs aexpr
    in
        if toBoolean a
        then evalExpr regs bexpr
        else a
    end


and evalLogicalOr (regs:REGS)
                  (aexpr:EXPRESSION)
                  (bexpr:EXPRESSION)
    : VALUE = 
    let
        val a = evalExpr regs aexpr
    in
        if toBoolean a
        then a
        else evalExpr regs bexpr
    end

and evalOperatorIs (regs:REGS)
                   (v:VALUE)
                   (te:TYPE)
    : bool = 
    let
        val vt = typeOfVal regs v 
    in
        vt <* te
    end

(* SPEC

fun evalBinaryTypeExpr (env: ENV)
                       (binaryTypeOp: BINTYPEOP)
                       (expr: EXPRESSION)
                       (typeExpr: TYPE)
    : VALUE =
 (* FINISH ME *)

 *)

and evalBinaryTypeOp (regs:REGS)
                     (bop:BINTYPEOP)
                     (expr:EXPRESSION)
                     (ty:TYPE)
    : VALUE =
    let
        val v = evalExpr regs expr
    in
        case bop of
            Cast =>
            if evalOperatorIs regs v (evalTy regs ty)
            then v
            else throwExn (newTypeOpFailure regs "cast failed" v ty)
          | Is => newBoolean regs (evalOperatorIs regs v (evalTy regs ty))
          | Like => 
            let
                val vt = typeOfVal regs v 
                fun isLike (Object obj) (RecordType fields) = List.all (objHasLikeField obj) fields
                  | isLike v lte = (typeOfVal regs v) <* lte
                and objHasLikeField obj (name, ty) = 
                    let
                        val name = evalNameExpr regs name 
                    in
                        if hasOwnProperty regs obj name
                        then 
                            let 
                                val v2 = getValue regs obj name
                            in
                                isLike v2 ty
                            end
                        else 
                            false
                    end
            in
                newBoolean regs (isLike v ty)
            end
    end


and hasInstance (regs:REGS)
                (obj:OBJ)
                (v:VALUE)
    : bool = 
    let
        val proto = getPrototype regs obj
        val targId = case proto of 
                         (Object (Obj { ident, ... })) => ident
                       | _ => throwExn (newTypeErr regs ["no 'prototype' property found in [[hasInstance]]"])
        fun tryVal v' = 
            case v' of 
                Null => false
              | Undefined => false
              | Object ob =>
                if getObjId ob = targId
                then true
                else 
                    let
                        val Obj { proto, ... } = ob
                    in
                        tryVal (proto)
                    end
    in
        tryVal v
    end


and objHasInstance (regs:REGS)
                   (ob:OBJ)
                   (a:VALUE)                   
    : VALUE = 
    let
        val h = hasInstance regs ob a
    in
        newBoolean regs h
    end


and evalInstanceOf (regs:REGS)
                   (aexpr:EXPRESSION)
                   (bexpr:EXPRESSION)
    : VALUE = 
    let
        val a = evalExpr regs aexpr
        val b = evalExpr regs bexpr
        val ob = needObj regs b
    in
        case getObjPrimitive ob of 
            SOME (ClassPrimitive _) => objHasInstance regs ob a
          | SOME (InterfacePrimitive _) => objHasInstance regs ob a
          | SOME (FunctionPrimitive _) => objHasInstance regs ob a
          | SOME (NativeFunctionPrimitive _) => objHasInstance regs ob a
          | _ => throwExn (newTypeErr regs ["operator 'instanceof' applied object with no [[hasInstance]] method"])
    end


and evalOperatorIn (regs:REGS)
                   (aexpr:EXPRESSION)
                   (bexpr:EXPRESSION)
    : VALUE = 
    let
        val a = evalExpr regs aexpr
        val b = evalExpr regs bexpr
        val aname = needNameOrString regs a
    in
        case b of
            Object obj =>
            newBoolean regs (hasProperty regs obj aname)
          | _ => throwExn (newTypeErr regs ["operator 'in' applied to non-object"])
    end


and evalOperatorComma (regs:REGS)
                      (aexpr:EXPRESSION)
                      (bexpr:EXPRESSION)
    : VALUE = 
    (evalExpr regs aexpr;
     evalExpr regs bexpr)


and evalBinaryOp (regs:REGS)
                 (bop:BINOP)
                 (aexpr:EXPRESSION)
                 (bexpr:EXPRESSION)
    : VALUE =
    case bop of
        LogicalAnd => evalLogicalAnd regs aexpr bexpr
      | LogicalOr => evalLogicalOr regs aexpr bexpr
      | InstanceOf => evalInstanceOf regs aexpr bexpr
      | In => evalOperatorIn regs aexpr bexpr
      | Comma => evalOperatorComma regs aexpr bexpr
      | _ => performBinop regs bop
                          (evalExpr regs aexpr)
                          (evalExpr regs bexpr)

(* SPEC

fun evalConditionalExpr (env: ENV)
                        (condExpr: EXPRESSION)
                        (thenExpr: EXPRESSION)
                        (elseExpr: EXPRESSION)
    : VALUE =
 (* FINISH ME *)

 *)



and evalCondExpr (regs:REGS)
                 (cond:EXPRESSION)
                 (thn:EXPRESSION)
                 (els:EXPRESSION)
    : VALUE =
    let
        val v = evalExpr regs cond
        val b = toBoolean v
    in
        if b
        then evalExpr regs thn
        else evalExpr regs els
    end

and evalObjectExpr (regs:REGS)
                   (object:EXPRESSION)
    : OBJ =
    let 
        val v = evalExpr regs object
    in
        case v of 
            Object ob
            => ob
               
          | Null
            => throwExn (newRefErr regs ["object reference on null value"])

          | Undefined 
            => throwExn (newRefErr regs ["object reference on undefined value"])
    end

and resolveObjectReference (regs:REGS)
                           (ObjectNameReference { object, name, ... }: EXPRESSION)
    : (OBJ option * (OBJ * NAME)) =
    let
        val obj = evalObjectExpr regs object
    in
        case name of
            UnqualifiedName { identifier, openNamespaces, ... }
            => (SOME obj, 
                resolveUnqualifiedObjectReference regs 
                                                  obj 
                                                  identifier 
                                                  openNamespaces)

          | QualifiedName { namespace, identifier }
            => resolveQualifiedObjectReference regs obj identifier namespace
    end

  | resolveObjectReference regs 
                           (ObjectIndexReference {object, index, ...}) = 
    let
        val obj = evalObjectExpr regs object
        val idx = evalExpr regs index
        val identifier = toUstring regs idx  
        (* FIXME if its an Name, then don't convert *)
        val namespace = Namespace publicNS
    in
        resolveQualifiedObjectReference regs obj identifier namespace
    end

  | resolveObjectReference  regs  _  =                 (* INFORMATIVE *)
    error regs ["need object reference expression"]    (* INFORMATIVE *)

and resolveQualifiedObjectReference (regs: REGS)
                                    (object: OBJ)
                                    (identifier: IDENTIFIER)
                                    (namespaceExpr: NAMESPACE_EXPRESSION)
    : (OBJ option * (OBJ * NAME)) =
    let
        val namespace = evalNamespaceExpr regs namespaceExpr
    in
        (SOME object, (object, {ns=namespace, id=identifier}))
    end

and resolveUnqualifiedObjectReference (regs: REGS)
                                      (object: OBJ)
                                      (identifier: IDENTIFIER)
                                      (openNamespaces: OPEN_NAMESPACES)
    : (OBJ * NAME) =
    let
        val namespaces = List.concat openNamespaces
        val result = searchObject (SOME object, identifier, namespaces, false)
    in 
        case result of
            NONE 
            => (object, {ns=publicNS, id=identifier})

          | SOME (object, namespaces) 
            => let
                   val instanceRibs = instanceRibsOf (object)
                   val result = Fixture.selectNamespaces (identifier, 
                                                          namespaces, 
                                                          instanceRibs, 
                                                          openNamespaces)
               in 
                   case result of
                       [] 
                       => internalError ["empty namespace set"]

                      | namespace :: []
                       => (object, {ns=namespace, id=identifier})

                      | _
                       => error regs ["ambiguous reference"]
               end
    end

and resolveRefExpr (regs:REGS)
                   (expr:EXPRESSION)
                   (errIfNotFound:bool)
    : (OBJ option * REF) =
    let
    in
        case expr of
            Ast.LexicalReference {name, ...} => (NONE, resolveLexicalReference regs name errIfNotFound)
          | ObjectNameReference _ => resolveObjectReference regs expr
          | ObjectIndexReference _ => resolveObjectReference regs expr
          | _ => error regs ["need lexical or object-reference expression"]
    end

and evalLetExpr (regs:REGS)
                (head:HEAD)
                (body:EXPRESSION)
    : VALUE =
    let
        val letRegs = evalHead regs head
    in
        evalExpr letRegs body
    end

(*
 * ES-262-3 11.2.1: Resolving member expressions to REFs.
 *)

and resolveLexicalReference (regs            : REGS)
                            (nameExpression  : NAME_EXPRESSION)
                            (errorIfNotFound : bool)
    : (OBJ * NAME) =
    let
        val {scope, ...} = regs
    in 
        case nameExpression of

            QualifiedName {identifier, namespace} 
            => resolveQualifiedLexicalReference regs identifier namespace

          | UnqualifiedName { identifier, openNamespaces, ... }
            => resolveUnqualifiedLexicalReference regs identifier openNamespaces
    end

and resolveQualifiedLexicalReference (regs          : REGS)
                                     (identifier    : IDENTIFIER)
                                     (namespaceExpr : NAMESPACE_EXPRESSION)
    : (OBJ * NAME) =
    let
        val {scope, global, ...} = regs
        val namespace = evalNamespaceExpr regs namespaceExpr
        val result = searchScopeChain (SOME scope, identifier, [namespace])
    in 
        case result of
            NONE
            => (global, {ns=publicNS, id=identifier})

          | SOME (object, namespaces)
            => (object, {ns=namespace, id=identifier})
    end
    
and resolveUnqualifiedLexicalReference (regs           : REGS)
                                       (identifier     : IDENTIFIER)
                                       (openNamespaces : OPEN_NAMESPACES)
    : (OBJ * NAME) =
    let
        val {scope, global, ...} = regs
        val namespaces = List.concat openNamespaces
        val result = searchScopeChain (SOME scope, identifier, namespaces)
    in 
        case result of
            NONE 
            => (global, {ns=publicNS, id=identifier})

          | SOME (object, namespaces) 
            => let
                   val classRibs = instanceRibsOf (object)
                   val result = Fixture.selectNamespaces (identifier, 
                                                          namespaces, 
                                                          classRibs, 
                                                          openNamespaces)
               in 
                   case result of
                       [namespace] 
                       => (object, {ns=namespace, id=identifier})
                          
                     | _ 
                       => error regs ["ambiguous reference"]
               end
    end

and instanceRibsOf (object: OBJ) = []  (* FIXME *)

(*
 * Scans provided object and prototype chain looking for a slot that
 * matches name (or multiname). Returns a REF to the exact object found.
 *)

and resolveName (regs:REGS)
                (objects:OBJ list)
                (nameExpr:NAME_EXPRESSION)
    : REF option =
    let
        val (identifier, openNamespaces) = 
            case nameExpr of
                QualifiedName {identifier, namespace} => (identifier, [[evalNamespaceExpr regs namespace]])
              | UnqualifiedName { identifier, openNamespaces } => (identifier, openNamespaces)
    in
        findName ((#global regs), objects, identifier, openNamespaces)
    end

(* FIXME: evalNameExpr is mostly for field names; the handling of field names is presently a little confused. *)
and evalNameExpr (regs:REGS)
                 (nameExpr:NAME_EXPRESSION)
    : NAME = 
    case nameExpr of 
        QualifiedName { identifier, namespace } => 
        { id = identifier, ns = evalNamespaceExpr regs namespace }
      | UnqualifiedName { identifier, ... } => 
        public identifier

and evalNamespaceExpr (regs:REGS)
                      (nsExpr:NAMESPACE_EXPRESSION)
    : NAMESPACE = 
    case nsExpr of 
        Namespace ns => ns
      | NamespaceName ne => 
        (*
         error regs ["unresolved namespace ", nameExpr ne]
         *)
        let
           val (obj, name) = resolveLexicalReference regs ne true
        in
            needNamespace (getValue regs obj name)
        end

and labelMatch (stmtLabels:IDENTIFIER list)
               (exnLabel:IDENTIFIER option)
    : bool =
    let
        val lab = case exnLabel of 
                      NONE => Ustring.empty
                    | SOME lab => lab
        fun equalsLab x = lab = x
    in
        List.exists equalsLab stmtLabels
    end


and evalStmts (regs:REGS)
              (stmts:STATEMENT list)
    : VALUE =
    case stmts of
        [s] => evalStmt regs s
      | (s::ss) => (evalStmt regs s;
                    evalStmts regs ss)
      (* FIXME: need to keep the last non-empty value and fixup abrupt completions here *)
      | [] => Undefined

(* SPEC

fun evalStatement (env: ENV)
                  (stmt: STATEMENT)
    : VALUE =
    case stmt of
        EmptyStmt => 
        Undefined
      | ExprStmt e => 
        evalExpr env e
      | IfStmt {cnd,thn,els} => 
        evalIfStmt env cnd thn els
      | WhileStmt w => 
        evalWhileStmt env w
      | DoWhileStmt w => 
        evalDoWhileStmt env w
      | WithStmt { obj, ty, body } => 
        evalWithStmt env obj ty body
      | SwitchStmt { cond, cases, labels } => 
        evalSwitchStmt env cond cases labels
      | ForStmt w => 
        evalForStmt env w
      | ReturnStmt r => 
        evalReturnStmt env r
      | BreakStmt lbl => 
        evalBreakStmt env lbl
      | ContinueStmt lbl => 
        evalContinueStmt env lbl
      | ThrowStmt t => 
        evalThrowStmt env t
      | LabeledStmt (lab, s) => 
        evalLabelStmt env lab s
      | BlockStmt b => 
        evalBlock env b
      | ClassBlock c => 
        evalClassBlock env c
      | LetStmt b => 
        evalBlock env b
      | TryStmt { block, catches, finally } => 
        evalTryStmt env block catches finally
      | SwitchTypeStmt { cond, ty, cases } => 
        evalSwitchTypeStmt env cond ty cases
      | ForInStmt w => 
        evalForInStmt env w
      | _ => error env ["Shouldn't happen: failed to match in Eval.evalStmt."]

 *)

and evalStmt (regs:REGS)
             (stmt:STATEMENT)
    : VALUE =
    case stmt of
        EmptyStmt => Undefined
      | ExprStmt e => evalExpr regs e
      | IfStmt {cnd,thn,els} => evalIfStmt regs cnd thn els
      | WhileStmt w => evalWhileStmt regs w
      | DoWhileStmt w => evalDoWhileStmt regs w
      | WithStmt { obj, ty, body } => evalWithStmt regs obj ty body
      | SwitchStmt { cond, cases, labels } =>
        evalSwitchStmt regs cond cases labels
      | ForStmt w => evalForStmt regs w
      | ReturnStmt r => evalReturnStmt regs r
      | BreakStmt lbl => evalBreakStmt regs lbl
      | ContinueStmt lbl => evalContinueStmt regs lbl
      | ThrowStmt t => evalThrowStmt regs t
      | LabeledStmt (lab, s) => evalLabelStmt regs lab s
      | BlockStmt b => evalBlock regs b
      | ClassBlock c => evalClassBlock regs c
      | LetStmt b => evalBlock regs b
      | TryStmt { block, catches, finally } =>
        evalTryStmt regs block catches finally
      | SwitchTypeStmt { cond, ty, cases } =>
        evalSwitchTypeStmt regs cond ty cases
      | ForInStmt w => evalForInStmt regs w
      | _ => error regs ["Shouldn't happen: failed to match in Eval.evalStmt."]


and checkAllPropertiesInitialized (regs:REGS)
                                  (obj:OBJ)
    : unit =
    let
        fun checkOne (n:NAME, {prop, seq}) =
            let 
                val { ty, state, attrs} = prop 
            in
                case state of
                    UninitProp => 
                    error regs ["uninitialized property: ", name n]
                  | _ => ()
            end
        val Obj { props, ... } = obj
        val { bindings, ... } = !props
    in
        NameMap.appi checkOne bindings
    end


and invokeFuncClosure (regs:REGS)
                      (closure:FUN_CLOSURE)
                      (thisFun:OBJ option)
                      (args:VALUE list)
    : VALUE =
    let
        val { func, this, env } = closure
        val _ = trace ["entering func closure in scope #", 
                       Int.toString (getObjId (getScopeObj env))]
        val _ = traceScope env
        val Func { name, block, generator, param=Head (paramRib, paramInits), ty, ... } = func
        val this = case this of
                       SOME t => (trace ["using bound 'this' #", 
                                         Int.toString (getObjId t)]; t)
                     | NONE => (trace ["using caller 'this' #", 
                                       Int.toString (getObjId (#this regs))]; (#this regs))
        val regs = withThis regs this
        val regs = withScope regs env
        (* NB: leave this here, it faults if we have a non-ground type, which is the point. *)
        val tyExpr = evalTy regs ty
    in
        let
            val idStr = Ustring.toAscii (#ident name)
            val strname = case (#kind name) of
                              Ordinary => idStr
                            | Operator => "operator " ^ idStr
                            | Get => "get " ^ idStr
                            | Set => "set " ^ idStr
                            | Call => "call " ^ idStr
                            | Has => "has " ^ idStr

            val _ = push regs strname args

            val (varObj:OBJ) = newObjectNoTag ()
            val (varRegs:REGS) = extendScopeReg regs varObj ActivationScope
            val (varScope:SCOPE) = (#scope varRegs)
            val (Obj {props, ...}) = varObj
        in
            trace ["invokeFuncClosure: allocating scope rib"];
            allocScopeRib varRegs paramRib;
            trace ["invokeFuncClosure: binding args"];
            bindArgs regs varScope func args;
            trace ["invokeFuncClosure: evaluating scope inits on scope obj #",
                   Int.toString (getScopeId varScope)];
            evalScopeInits varRegs Local paramInits;
            checkAllPropertiesInitialized regs varObj;
            trace ["invokeFuncClosure: evaluating block"];
            let
                val blockRegs = withThisFun varRegs thisFun 
                val res = case block of
                              NONE => Undefined
                            | SOME b =>
                              if generator then
                                  let
                                      fun body (thisGen:OBJ) =
                                          (evalBlock (withThisGen blockRegs (SOME thisGen)) b;
                                           Undefined)
                                          handle ReturnException v => v
                                  in
                                      (* FIXME: this is the wrong REGS *)
                                      newGenerator regs body
                                  end
                              else
                                  ((evalBlock (withThisGen blockRegs NONE) b;
                                    Undefined)
                                   handle ReturnException v => v)
            in
                pop regs;
                res
            end
        end
    end

and catch (regs:REGS)
          (e:VALUE)
          (clauses:CATCH_CLAUSE list)
    : VALUE option =
    case clauses of
        [] => NONE
      | {ty, rib, inits, block, ...}::cs =>
        if evalOperatorIs regs e (evalTy regs ty)
        then
            let
                val fixs = valOf rib
                val head = Head (valOf rib, [])
                val regs = evalHead regs head
                val scope = (#scope regs)
                val obj = (#this regs)
                val temps = getScopeTemps scope
            in
                (if List.null fixs
                 then ()
                 else defTemp temps 0 e;
                 evalScopeInits regs Local (valOf inits);
                 SOME (evalBlock regs block))
            end
        else
            catch regs e cs

(* SPEC

fun evalTryStmt (env: ENV)
                (block: BLOCK)
                (catches: CATCH_CLAUSE list)
                (finally: BLOCK option)
    : VAL =
 (* FINISH ME *)

 *)

and evalTryStmt (regs:REGS)
                (block:BLOCK)
                (catches:CATCH_CLAUSE list)
                (finally:BLOCK option)
    : VALUE =
    let
        fun finishWith (v:VALUE)
            : VALUE =
            case finally of
                NONE => v
              (*
               * FIXME: Not sure how to read the spec wrt. labels;
               * are we supposed to return the finally-block's value
               * or the main block's value?
               *)
              | SOME f => evalBlock regs f
    in
        evalBlock regs block
        handle ThrowException v =>
               (case catch regs v catches of
                    SOME fix => finishWith fix
                  | NONE => (finishWith v;
                             raise ThrowException v))
             | StopIterationException =>
               let
                   val v = getStopIteration regs
               in
                   case catch regs v catches of
                       SOME fix => finishWith fix
                     | NONE => (finishWith v;
                                raise StopIterationException)
               end
    end


and bindArgs (regs:REGS)
             (argScope:SCOPE)
             (func:FUNC)
             (args:VALUE list)
    : unit =
    let
        val Scope { object = Obj { props, ... }, ... } = argScope
        val Func { defaults, ty, ... } = func
        val hasRest = AstQuery.funcTyHasRest ty

        (* If we have:
         *
         * P formal parameters
         * D parameter defaults (at the end)
         * A args
         *
         * Then we must have A + D >= P, and we let
         * I = (A+D) - P, the number of ignored defaults.
         *
         * We assign the args to temps numbered [0, A),
         * and assign the last D-I defaults to temps numbered [A, P-A).
         *
         * If the function has a ...rest arg and A <= (P-1), we do as above.
         *
         * If the function has a ...rest arg and A > (P-1), we do the above
         * for the (P-1) args and then take the A-(P-1) args and put them
         * in a new array, bound to the ...rest name.
         *)

        val p = length (AstQuery.paramTysOfFuncTy ty)
        val d = length defaults
        val a = length args
        val i = Int.min (d, Int.max(0, (a+d) - p));
        val _ = trace ["bindArgs",
                       ": p=", Int.toString p,
                       ", d=", Int.toString d,
                       ", a=", Int.toString a,
                       ", i=", Int.toString i,
                       ", hasRest=", Bool.toString hasRest]

        val argTemps = getScopeTemps argScope
        fun bindArg _ [] = ()
          | bindArg (n:int) ((arg:VALUE)::args) =
            (trace ["defining temp ", Int.toString n];
             defTemp argTemps n arg;
             bindArg (n+1) args)

        fun bind (finalArgs:VALUE list) =
            (*
             * FIXME: this is a random guess at the appropriate form
             * of 'arguments'.
             *)
            (addProp props arguments { state = ValListProp args,  
                                       (* args is a better approximation than finalArgs *)
                                       ty = typename public_Object,
                                       attrs = { removable = false,
                                                 enumerable = false,
                                                 writable = Writable,
                                                 fixed = true } };
             bindArg 0 finalArgs)

    (*
     * FIXME: should handle a flag in func that indicates whether
     * to insist on precise number of args.
     *)
    in
        if hasRest andalso a > (p-1)
        then
            let
                val _ = trace ["dropping ", Int.toString i, " defaults"]
                val defExprs = List.drop (defaults, i)
                val defVals = List.map (evalExpr regs) defExprs
                val restArgs = List.drop (args, (p-1))
                val _ = trace ["dropping ", Int.toString (p-1),
                               " args, binding ", Int.toString (List.length restArgs),
                               " rest args"];
                val allArgs = (List.take (args, (p-1))) @ defVals @ [newArray regs restArgs]
            in
                bind allArgs
            end
        else
            let
                val padding = if a < (p-d)
                              then (List.tabulate (((p-d) - a), (fn _ => Undefined)))
                              else []
                val defExprs = List.drop (defaults, i)
                val defVals = List.map (evalExpr regs) defExprs
                val allArgs = args @ padding @ defVals
            in
                bind (List.take (allArgs, p))
            end
    end


and evalInits (regs:REGS)
              (obj:OBJ)
              (temps:TEMPS)
              (inits:INITS)
    : unit =
    evalInitsMaybePrototype regs obj temps inits false


and evalInitsMaybePrototype (regs:REGS)
                            (obj:OBJ)
                            (temps:TEMPS)
                            (inits:INITS)
                            (isPrototypeInit:bool)
    : unit =
    let
        fun evalInit (n,e) =
            let
                val idStr = case n of
                                PropName { id, ... } => Ustring.toAscii id
                              | TempName t => ("#" ^ Int.toString t)
                val _ = push regs ("init " ^ idStr) []
                val v = evalExpr regs e
            in
                case n of
                    PropName pn =>
                    (traceConstruct ["evalInit assigning to prop ", fmtName pn,
                                     " on object #", (Int.toString (getObjId obj))];
                     (*                     if isPrototypeInit then
                                                log ["Propname: ", fmtName pn]
                                            else
                                                () ; *)
                     if isPrototypeInit
                     then
                         let
                             val Obj { props, ... } = obj
                         in
                             setValue regs obj pn v;
                             setPropEnumerable props pn false
                         end
                     else defValue regs obj pn v)
                  | TempName tn =>
                    (traceConstruct ["evalInit assigning to temp ", (Int.toString tn),
                                     " on object #", (Int.toString (getObjId obj))];
                     defTemp temps tn v);
                pop regs
            end
    in
        List.app evalInit inits
    end

(*
 Evaluate INITs targeting an obj, in scope of temporaries
 *)

and evalObjInits (regs:REGS)
                 (instanceObj:OBJ)
                 (head:HEAD)
    : unit =
    let
        val (Head (rib,inits)) = head
        val tempRegs = evalHead regs (Head (rib,[]))
        val temps = getScopeTemps (#scope tempRegs)
    in
        evalInits tempRegs instanceObj temps inits
    end


and findTargetObj (regs:REGS)
                  (scope:SCOPE)
                  (target:INIT_TARGET) =
    let
        val Scope { object, kind, parent, ...} = scope
        val Obj {props,...} = object
        val { bindings, ... } = !props
    in
        traceConstruct ["considering init target as object #", Int.toString (getScopeId scope)];
        case target of
            Local =>
            if (NameMap.numItems bindings) > 0 orelse 
               not (Option.isSome parent)
            then object
            else findTargetObj regs (valOf parent) target
          (* if there are no props then it is at temp scope *)
                 
          | Hoisted =>
            if kind = InstanceScope orelse
               kind = ActivationScope orelse
               not (Option.isSome parent)
            then object
            else findTargetObj regs (valOf parent) target
                 
          | Prototype =>
            if kind = InstanceScope
            then needObj regs (getPrototype regs object)
            else findTargetObj regs (valOf parent) target
    end


and evalScopeInits (regs:REGS)
                   (target:INIT_TARGET)
                   (inits:INITS)
    : unit =
    let
        val { scope, ... } = regs
        val Scope { temps, ...} = scope
        val obj = findTargetObj regs scope target
        val Obj { ident, ... } = obj
        val _ = traceConstruct ["resolved init target to object id #", Int.toString ident]
    in
        evalInitsMaybePrototype regs obj temps inits (target=Prototype)
    end


and initializeAndConstruct (regs:REGS)
                           (class:CLASS)
                           (classObj:OBJ)
                           (args:VALUE list)
                           (instanceObj:OBJ)
    : unit =
    let
        fun idStr ob = Int.toString (getObjId ob)
        val _ = traceConstruct ["initializeAndConstruct: this=#", (idStr (#this regs)),
                                ", constructee=#", (idStr instanceObj),
                                ", class=#", (idStr classObj)]
        val _ = if getObjId (#this regs) = getObjId instanceObj
                then ()
                else error regs ["constructor running on non-this value"]
        val Class { name,
                  extends,
                  instanceInits,
                  constructor,
                  ... } = class
        fun initializeAndConstructSuper (superArgs:VALUE list) =
            case extends of
                NONE =>
                (traceConstruct ["checking all properties initialized at root class ", fmtName name];
                 checkAllPropertiesInitialized regs instanceObj)
              | SOME parentTy =>
                let
                    val parentTy = evalTy regs parentTy
                    val _ = traceConstruct ["initializing and constructing superclass ", Type.fmtType parentTy]
                    val superObj = instanceClass regs (AstQuery.needClassType parentTy)
                    val superClass = needClass (Object superObj)
                    val superScope = getClassScope regs superObj
                    val superRegs = withThis (withScope regs superScope) instanceObj
                in
                    initializeAndConstruct
                        superRegs superClass superObj superArgs instanceObj
                end
    in
        traceConstruct ["evaluating instance initializers for ", fmtName name];
        evalObjInits regs instanceObj instanceInits;
        case constructor of
            NONE => initializeAndConstructSuper []
          | SOME (Ctor { settings, superArgs, func }) =>
            let
                val _ = push regs ("ctor " ^ (Ustring.toAscii (#id name))) args
                val Func { block, param=Head (paramRib,paramInits), ... } = func
                val (varObj:OBJ) = newObjectNoTag ()
                val (varRegs:REGS) = extendScopeReg regs
                                                    varObj
                                                    ActivationScope
                val varScope = (#scope varRegs)
                val (instanceRegs:REGS) = extendScopeReg regs
                                                         instanceObj
                                                         InstanceScope
                val (ctorRegs:REGS) = extendScopeReg instanceRegs
                                                     varObj
                                                     ActivationScope
            in
                traceConstruct ["allocating scope rib for constructor of ", fmtName name];
                allocScopeRib varRegs paramRib;
                traceConstruct ["binding constructor args of ", fmtName name];
                bindArgs regs varScope func args;
                traceConstruct ["evaluating inits of ", fmtName name,
                                " in scope #", Int.toString (getScopeId varScope)];
                evalScopeInits varRegs Local paramInits;
                traceConstruct ["evaluating settings"];
                evalObjInits varRegs instanceObj settings;
                traceConstruct ["initializing and constructing superclass of ", fmtName name];
                initializeAndConstructSuper (evalExprsAndSpliceSpreads varRegs superArgs);
                traceConstruct ["entering constructor for ", fmtName name];
                (case block of 
                     NONE => Undefined
                   | SOME b => (evalBlock (withThisFun ctorRegs (SOME classObj)) b
                                handle ReturnException v => v));
                pop regs;
                ()
            end
    end

and constructStandard (regs:REGS)
                      (classObj:OBJ)
                      (class:CLASS)
                      (proto:VALUE)
                      (args:VALUE list)
    : OBJ =
    let
        val regs = withScope regs (getClassScope regs classObj)
        val tag = InstanceTag class
    in
        constructStandardWithTag regs classObj class tag proto args 
    end

and constructStandardWithTag (regs:REGS)
                             (classObj:OBJ)
                             (class:CLASS)
                             (tag:TAG)
                             (proto:VALUE)
                             (args:VALUE list)
    : OBJ =
    let
        val Class { name, instanceRib, ...} = class
        val instanceObj = newObject tag proto
        (* FIXME: might have 'this' binding wrong in class scope here. *)
        val classScope = getClassScope regs classObj
        val regs = withThis (withScope regs classScope) instanceObj
    in
        traceConstruct ["allocating ", Int.toString (length instanceRib), " instance rib for new ", fmtName name];
        allocObjRib regs instanceObj (SOME instanceObj) instanceRib;
        traceConstruct ["entering most derived constructor for ", fmtName name];
        initializeAndConstruct regs class classObj args instanceObj;
        traceConstruct ["finished constructing new ", fmtName name];
        instanceObj
    end

and parseFunctionFromArgs (regs:REGS)
                          (args:VALUE list)
    : (Ustring.STRING * EXPRESSION) =
    let
        (*
         * We synthesize a full function expression here, then feed it back into the parser.
         *)
        val args = if List.null args then [newString regs Ustring.empty] else args
        val bodyStr = (toUstring regs (List.last args))
        val argArgs = List.take (args, (length args)-1)
        fun joinWithComma [] = []
          | joinWithComma [x] = [x]
          | joinWithComma (x::xs) = x :: Ustring.comma :: (joinWithComma xs)
        val argStr = Ustring.append (joinWithComma (map (fn x => (toUstring regs x)) argArgs))
        val fullStr = Ustring.append [Ustring.function_,
                                      Ustring.lparen,
                                      argStr,
                                      Ustring.rparen,
                                      Ustring.lbrace,
                                      bodyStr,
                                      Ustring.rbrace]

        val (_,funcExpr) = Parser.functionExpression
                               (Parser.lexLines
                                    [Ustring.sourceFromUstring fullStr],
                                Parser.AllowColon,
                                Parser.AllowIn)

        val funcExpr = Defn.defExpr (Defn.mkTopEnv (#prog regs) (getLangEd regs)) funcExpr
    in
        (fullStr, funcExpr)
    end


and specialFunctionConstructor (regs:REGS)
                               (classObj:OBJ)
                               (class:CLASS)
                               (args:VALUE list)
    : OBJ =
    let
        val (source, funcExpr) = parseFunctionFromArgs regs args
        val sname = public_source
        val sval = newString regs source
        val fv = case funcExpr of
                     LiteralExpr (LiteralFunction f) =>
                     newFunctionFromFunc regs (getGlobalScope regs) f
                   | _ => error regs ["function did not parse"];
        val fo = needObj regs fv
    in
        setValue regs fo sname sval;
        fo
    end


and specialArrayConstructor (regs:REGS)
                            (classObj:OBJ)
                            (class:CLASS)
                            (args:VALUE list) :
    OBJ =
    let
        val proto = getPrototype regs classObj
        val instanceObj = constructStandard regs classObj class proto args
        val Obj { props, ... } = instanceObj
        fun bindVal _ [] = ()
          | bindVal n (x::xs) =
            (setValue regs instanceObj (public (Ustring.fromInt n)) x;
             bindVal (n+1) xs)
    in
        case args of
            [] => setValue regs instanceObj public_length (newDouble regs 0.0)
          | [k] => let val idx = asArrayIndex k
                   in
                       if not (idx = 0wxFFFFFFFF) then
                           setValue regs instanceObj public_length k
                       else
                           bindVal 0 args
                   end
          | _ => bindVal 0 args;
        instanceObj
    end

(*
 * ES-262-3 15.2.2.1 The Object Constructor
 *)

and specialPrimitiveCopyingConstructor (regs:REGS)
                                       (classObj:OBJ)
                                       (class:CLASS)
                                       (args:VALUE list)
    : OBJ =
    let
        val primitive = 
            case args of 
                [] => error regs ["called special primitive-copying constructor with no args"]
              | v :: _ => needPrimitive v
        val tag = (PrimitiveTag primitive)
        val proto = getPrototype regs classObj
        val obj = constructStandardWithTag regs classObj class tag proto []
    in
        obj
    end

and specialObjectConstructor (regs:REGS)
                             (classObj:OBJ)
                             (class:CLASS)
                             (args:VALUE list)
    : OBJ =
    let
        val proto = getPrototype regs classObj
        fun instantiate _ = constructStandard regs classObj class proto args
    in
        case args of
            [] => instantiate ()
          | (Null :: _) => instantiate ()
          | (Undefined :: _) => instantiate ()
          | (Object (Obj { tag, ...}) :: _) =>
            case tag of 
                (*
                 * FIXME: This part is dubioius. ES-262-3 says to call ToObject
                 * on non-Object primitives. We do not do so here: rather, ToObject
                 * implements its lower half (primitive values) by calling into *here*,
                 * so here is where we do any cloning and primitive-copying.
                 *)
                PrimitiveTag mt => constructStandardWithTag regs classObj class (PrimitiveTag mt) proto args
              | _ => instantiate()
    end


and specialBooleanConstructor (regs:REGS)
                              (classObj:OBJ)
                              (class:CLASS)
                              (args:VALUE list)
    : OBJ =
    let
        val b = case args of 
                    [] => false
                  | v :: _ => toBoolean v
        val cell = 
            if b 
            then getBooleanTrueSlot regs 
            else getBooleanFalseSlot regs
    in
        case !cell of 
            SOME obj => obj
          | NONE => 
            let 
                val tag = PrimitiveTag (BooleanPrimitive b)
                val proto = getPrototype regs classObj
                val obj = constructStandardWithTag regs classObj class tag proto []
            in 
                cell := SOME obj;
                obj
            end
    end


and specialDoubleConstructor (regs:REGS)
                             (classObj:OBJ)
                             (class:CLASS)
                             (args:VALUE list)
    : OBJ =
    let
        val n = case args of 
                    [] => 0.0
                  | v :: _ => toDouble v
        fun build _ = 
            let
                val tag = PrimitiveTag (DoublePrimitive n)
                val proto = getPrototype regs classObj
                val obj = constructStandardWithTag regs classObj class tag proto []
            in
                obj
            end
    in
        if Real64.isNan n 
        then 
            let
                val dn = getDoubleNaNSlot regs
            in
                case !dn of
                    NONE => 
                    let 
                        val obj = build ()
                    in
                        dn := SOME obj;
                        obj
                    end
                  | SOME obj => obj
            end
        else 
            case findInDoubleCache regs n of
                SOME obj => obj
              | NONE => 
                let 
                    val obj = build ()
                in 
                    updateDoubleCache regs (n, obj);
                    obj
                end
    end


and specialDecimalConstructor (regs:REGS)
                              (classObj:OBJ)
                              (class:CLASS)
                              (args:VALUE list)
    : OBJ =
    let
        val n = case args of 
                    [] => toDecimal (newDouble regs 0.0)
                  | v :: _ => toDecimal v
        val tag = PrimitiveTag (DecimalPrimitive n)
        val proto = getPrototype regs classObj
        val obj = constructStandardWithTag regs classObj class tag proto []
    in
        obj
    end
    

and specialStringConstructor (regs:REGS)
                             (classObj:OBJ)
                             (class:CLASS)
                             (args:VALUE list)
    : OBJ =
    let 
        val s = case args of 
                    [] => Ustring.empty
                  | v :: _ => toUstring regs v
    in
        case findInStrCache regs s of
            SOME obj => obj
          | NONE => 
            let 
                val tag = PrimitiveTag (StringPrimitive s)
                val proto = getPrototype regs classObj
                val obj = constructStandardWithTag regs classObj class tag proto [] 
            in 
                updateStrCache regs (s, obj);
                obj
            end
    end
    

and constructSpecial (regs:REGS)
                     (id:OBJ_IDENTIFIER)
                     (classObj:OBJ)
                     (class:CLASS)
                     (args:VALUE list) :
    OBJ option =
    let
        val Class { name, ... } = class
                                
        fun findSpecial [] = NONE
          | findSpecial ((q,f)::rest) = 
            let
                val ident = slotObjId regs q
            in
                if ident = id 
                then 
                    (trace ["running special ctor for instance of class #", 
                            Int.toString id, " = ", fmtName name];
                     SOME (f regs classObj class args))
                else 
                    findSpecial rest
            end
    in
        findSpecial 
            [
             (getClassClassSlot, specialPrimitiveCopyingConstructor),
             (getInterfaceClassSlot, specialPrimitiveCopyingConstructor),
             (getNamespaceClassSlot, specialPrimitiveCopyingConstructor),

             (getObjectClassSlot, specialObjectConstructor),
             (getFunctionClassSlot, specialFunctionConstructor),
             (getArrayClassSlot, specialArrayConstructor),

             (getBooleanClassSlot, specialBooleanConstructor),
             (getDoubleClassSlot, specialDoubleConstructor),
             (getDecimalClassSlot, specialDecimalConstructor),

             (getStringClassSlot, specialStringConstructor)
            ]
    end


and bindAnySpecialIdentity (regs:REGS)
                           (obj:OBJ) =
    if not (isBooting regs)
    then ()
    else
        let
            val Obj { ident, tag, ... } = obj
        in
            case tag of 
                PrimitiveTag (ClassPrimitive (Class { name, ... })) =>
                let
                    val bindings = [
                        (intrinsic_Type, getTypeInterfaceSlot),
                        (intrinsic_Class, getClassClassSlot),
                        (intrinsic_Interface, getInterfaceClassSlot),
                        (ES4_Namespace, getNamespaceClassSlot),
                        
                        (public_Object, getObjectClassSlot),
                        (public_Array, getArrayClassSlot),
                        (public_Function, getFunctionClassSlot),
                        
                        (public_String, getStringWrapperClassSlot),
                        (ES4_string, getStringClassSlot),
                        
                        (public_Number, getNumberClassSlot),
                        (ES4_double, getDoubleClassSlot),
                        (ES4_decimal, getDecimalClassSlot),
                        
                        (ES4_boolean, getBooleanClassSlot),
                        (public_Boolean, getBooleanWrapperClassSlot),
                        
                        (helper_GeneratorImpl, getGeneratorClassSlot)
                    ]
                    fun f (n,id) = nameEq name n
                in
                    case List.find f bindings of
                        NONE => ()
                      | SOME (_,func) => 
                        let
                            (*val _ = TextIO.print ("binding special identity for class " ^ name name ^ "\n")*)
                            val _ = trace ["binding special identity for class ", fmtName name]
                            val cell = func regs
                        in
                            cell := SOME obj
                        end
                end
              | _ => ()
        end
        

and setPrototype (regs:REGS)
                 (obj:OBJ)
                 (proto:VALUE)
    : unit = 
    let
        val Obj { props, ... } = obj
        val n = public_prototype
        val prop = { ty = AnyType,
                     state = ValProp proto,
                     attrs = { removable = false,
                               enumerable = false, (* FIXME: is this wrong? (DAH) *)
                               writable = Writable,
                               fixed = true } }
    in
        if hasProp props n
        then delProp props n
        else ();
        addProp props n prop
    end


and getPrototype (regs:REGS)
                 (obj:OBJ)
    : VALUE = 
    let
        val Obj { props, ... } = obj
    in
        (* 
         * NB: Do not refactor this; it has to handle a variety of
         * unwelcome circumstances for the .prototype slot: 
         * null-valued, unallocated, and uninitialized.
         *)
        case findProp props public_prototype of 
            SOME { state = ValProp v, ... } => v
          | _ => Null
    end

and getOriginalObjectPrototype (regs:REGS)
    : VALUE = 
    case !(getObjectClassSlot regs) of 
        SOME obj => getPrototype regs obj
      | NONE => Null
                
and getSpecialPrototype (regs:REGS)
                        (id:OBJ_IDENTIFIER)
    : (VALUE * bool) option =
    if not (isBooting regs)
    then NONE
    else 
        let
            fun getExistingProto (q:REGS -> (OBJ option) ref)
                : (VALUE * bool) option =
                let
                    val _ = trace ["fetching existing proto"]
                    val objOptRef = q regs 
                in
                    case !objOptRef of 
                        NONE => NONE
                      | SOME obj => 
                        SOME (getPrototype regs obj, false)
                end
                
            fun findSpecial [] = NONE
              | findSpecial ((q,f)::xs) = 
                let
                    val ident = slotObjId regs q
                in
                    if ident = id
                    then f ()
                    else findSpecial xs
                end
        in
            findSpecial 
                [       
                 (* 
                  * Function.prototype = Function.__proto__;
                  * Yes, it's the most special case. 
                  *)
                 (getFunctionClassSlot,
                  (fn _ => case !(getFunctionClassSlot regs) of 
                               SOME ob => SOME (getProto ob, true)
                             | NONE => error regs ["Missing function class slot ",
                                                   "when extracting special prototype"])),

                 (getClassClassSlot,
                  (fn _ => getExistingProto getFunctionClassSlot)),
                 (getInterfaceClassSlot,
                  (fn _ => getExistingProto getFunctionClassSlot)),

                 (getStringClassSlot, 
                  (fn _ => SOME (newPublicObject regs, true))),
                 (getStringWrapperClassSlot,
                  (fn _ => getExistingProto getStringClassSlot)),
                 
                 (getDecimalClassSlot,
                  (fn _ => getExistingProto getDoubleClassSlot)),
                 (getNumberClassSlot, 
                  (fn _ => getExistingProto getDoubleClassSlot)),
                 
                 (getBooleanClassSlot,
                  (fn _ => SOME (newPublicObject regs, true))),
                 (getBooleanWrapperClassSlot,
                  (fn _ => getExistingProto getBooleanClassSlot)),
                 
                 (getArrayClassSlot,
                  (fn _ => SOME (newPublicObject regs, true)))
                ]
        end

and initClassPrototype (regs:REGS)
                       (obj:OBJ)
    : unit =
    let
        val Obj { ident, props, tag, ... } = obj
    in
        case tag of 
            PrimitiveTag (ClassPrimitive (Class {name, extends,...})) => 
            let
                val baseProtoVal =
                    case extends of
                        NONE => Null
                      | SOME baseClassTy =>
                        let
                            val ty = AstQuery.needClassType (evalTy regs baseClassTy)
                            val ob = instanceClass regs ty
                        in 
                            getPrototype regs ob
                        end
                val _ = traceConstruct ["initializing prototype for class ", fmtName name]
                val (newPrototype, setConstructor) = 
                    case getSpecialPrototype regs ident of
                        SOME (p,b) =>
                        (traceConstruct ["(special proto)"];
                         (needObj regs p, b))
                      | NONE => 
                        let 
                            val (classObj, class) = getObjectClassObjectAndClass regs
                        in 
                            traceConstruct ["(standard chained-to-base-class proto instance of Object)"];
                            (constructStandard regs classObj class baseProtoVal [], true)
                        end
                val Obj { props=newProtoProps, ... } = newPrototype
            in
                traceConstruct ["initializing proto on (obj #", Int.toString ident, 
                                "): ", fmtName name, ".prototype = ", 
                                "(obj #", Int.toString (getObjId newPrototype), ")"];
                setPrototype regs obj (Object newPrototype);
                if setConstructor
                then 
                    (setValueOrVirtual regs newPrototype 
                                       public_constructor 
                                       (Object obj) 
                                       false;
                     setPropEnumerable newProtoProps public_constructor false)
                else 
                    ();
                traceConstruct ["finished initialising class prototype"]
            end
          | _ => ()
    end
    
and constructClassInstance (regs:REGS)
                           (classObj:OBJ)
                           (class:CLASS)
                           (args:VALUE list)
    : VALUE =
    let
        val Obj { ident, ... } = classObj

        (* INFORMATIVE *) val Class { name, ...} = class 
        (* INFORMATIVE *) val _ = push regs ("new " ^ (Ustring.toAscii (#id name))) args 
        val obj = 
            case constructSpecial regs ident classObj class args of
                SOME ob => ob
              | NONE => constructStandard regs classObj class (getPrototype regs classObj) args
    in
        bindAnySpecialIdentity regs obj;
        initClassPrototype regs obj;
        (* INFORMATIVE *) pop regs; 
        Object obj
    end


(*
 * ES-262-3 8.6.2.1 [[Get]](P)
 *
 * FIXME: no idea if this makes the most sense given
 * the ES-262-3 meaning of the operation.
 *)

and get (regs:REGS)
        (obj:OBJ)
        (n:NAME)
    : VALUE =
    let
        fun tryObj ob =
            if hasOwnProperty regs ob n
            then getValue regs ob n
            else
                case obj of
                    Obj { proto, ... } =>
                    (case proto of
                         Object p => tryObj p
                       | _ => Undefined)
    in
        tryObj obj
    end


and evalPragmas (regs:REGS)
                (pragmas:PRAGMA list)
    : REGS = 
    regs

(*
 * Evaluate a block head (head) in the environment (regs).
 * - destructure head into its fixture bindings (rib) and initializers (inits)
 * - create a new object to become the scope object
 * - extend the environment (reg) scope with object (obj) and kind (BlockScope)
 * - allocate property bindings for fixture bindings (rib) in the extended environment (newRegs)
 * - initialize properties of target object (obj) with initializers (inits) in environment (regs)
 *   with temporaries (getScopeTemps scope)
 * - return the updated environment (newRegs)
 *)

and evalHead (regs:REGS)
             (head:HEAD)
    : REGS =
    let
        val (Head (rib,inits)) = head
        val obj = newObjectNoTag ()
        val newRegs = extendScopeReg regs obj BlockScope
        val {scope,...} = newRegs
        val _ = traceConstruct ["built temp scope #",
                                Int.toString (getScopeId scope),
                                " for head"]
    in
        allocScopeRib newRegs rib;
        evalInits regs obj (getScopeTemps scope) inits;
        newRegs
    end

(*
 BLOCK
 *)

and evalBlock (regs:REGS)
              (block:BLOCK)
    : VALUE =
    let
        val Block {pragmas, head, body, loc, ...} = block
        val blockRegs = evalPragmas regs pragmas 
        val _ = setLoc loc
        val blockRegs = evalHead blockRegs (valOf head)
        val _ = setLoc loc
        val res = evalStmts blockRegs body
        val _ = setLoc loc
    in
        res
    end


and evalClassBlock (regs:REGS)
                   (classBlock:CLASS_BLOCK)
    : VALUE =

    (* 
     * The property that holds the class object was allocated when the
     * rib of the outer scope were allocated. Still to do is
     * initialising the properties *of* the class object.
     *)

    let
        val {name, block, ...} = classBlock
        val {scope, ...} = regs

        val _ = trace ["evaluating class stmt for ", fmtName (valOf name)]
        val classVal = getValue regs (#global regs) (valOf name)
        val classObj = needObj regs classVal
                       
        (* FIXME: might have 'this' binding wrong in class scope *)
        val _ = trace ["extending scope for class block of ", fmtName (valOf name)]
        val classRegs = extendScopeReg regs classObj InstanceScope
    in
        trace ["evaluating class block of ", fmtName (valOf name)];
        evalBlock classRegs block
    end

(* SPEC

fun evalIfStatement (env: ENV)
                    (conditionExpr: EXPRESSION)
                    (thenStmt: STATEMENT)
                    (elseStmt: STATEMENT)
    : VALUE =
    let
        val conditionValue = evalExpr regs conditionExpr
        val booleanConditionValue = toBoolean conditionValue
    in
        if booleanConditionValue
        then evalStmt env thenStmt
        else evalStmt env elseStmt
    end

 *)

and evalIfStmt (regs:REGS)
               (cnd:EXPRESSION)
               (thn:STATEMENT)
               (els:STATEMENT)
    : VALUE =
    let
        val v = evalExpr regs cnd
        val b = toBoolean v
    in
        if b
        then evalStmt regs thn
        else evalStmt regs els
    end

(* SPEC

fun evalLabeledStatement (env: ENV)
                         (label: IDENTIFIER)
                         (stmt: STATEMENT)
    : VALUE =
    evalStmt env stmt
    handle BreakException exnLabel =>
           if labelMatch [label] exnLabel
           then Undefined
           else raise BreakException exnLabel

 *)

and evalLabelStmt (regs:REGS)
                  (lab:IDENTIFIER)
                  (s:STATEMENT)
    : VALUE =
    evalStmt regs s
    handle BreakException exnLabel =>
           if labelMatch [lab] exnLabel
           then Undefined
           else raise BreakException exnLabel


and evalWhileStmt (regs:REGS)
                  (whileStmt:WHILE_STATEMENT)
    : VALUE =
    case whileStmt of
        { cond, body, rib, labels } =>
        let
            val accum = ref Undefined
            fun loop _ =
                if toBoolean (evalExpr regs cond)
                then
                    (accum := evalStmt regs body;
                     (* FIXME: doesn't this need to happen in evalStmts? *)
                     loop ())
                    handle ContinueException exnLabel =>
                           if labelMatch labels exnLabel
                           then loop ()
                           else raise ContinueException exnLabel

                         | BreakException exnLabel =>
                           if labelMatch labels exnLabel
                           then (!accum)
                           else raise BreakException exnLabel
                else
                    (!accum)
        in
            loop ()
        end


and evalDoWhileStmt (regs:REGS)
                    (whileStmt:WHILE_STATEMENT)
    : VALUE =
    case whileStmt of
        { cond, body, rib, labels } =>
        let
            val accum = ref Undefined
            fun loop _ =
                let
                    fun bottom _ =
                        if toBoolean (evalExpr regs cond)
                        then loop ()
                        else (!accum)
                in
                    (accum := evalStmt regs body;
                     bottom ())
                    handle ContinueException exnLabel =>
                           if labelMatch labels exnLabel
                           then bottom ()
                           else raise ContinueException exnLabel

                         | BreakException exnLabel =>
                           if labelMatch labels exnLabel
                           then (!accum)
                           else raise BreakException exnLabel
                end
        in
            loop ()
        end


and evalWithStmt (regs:REGS)
                 (expr:EXPRESSION)
                 (ty:TYPE)
                 (body:STATEMENT)
    : VALUE =
    let
        val v = evalExpr regs expr
        val obj = needObj regs v
        val { scope, ... } = regs
        val scope' = extendScope scope obj WithScope
        val regs' = withScope regs scope'
        val regs'' = withThis regs' obj
    in
        evalStmt regs'' body
    end

and evalSwitchStmt (regs:REGS)
                   (cond:EXPRESSION)
                   (cases:CASE list)
                   (labels:IDENTIFIER list)
    : VALUE =
    let
        fun tryCases (v:VALUE) [] = Undefined
          | tryCases (v:VALUE) ({label, inits, body}::cs) =
            if (case label of
                    NONE => true
                  | SOME e => doubleEquals regs v (evalExpr regs e))
            then
                (* FIXME: This will change when switch stmt bodies are
                 * reorganized and get a proper head. *)
                let
                    val head = Head ([], case inits of NONE => []
                                                     | SOME i => i)
                    val caseRegs = evalHead regs head
                    val first = evalBlock caseRegs body
                    fun evalRest accum [] = accum
                      | evalRest accum ({label, inits, body}::xs) =
                        let
                            val a = evalBlock caseRegs body
                        in
                            evalRest a xs
                        end
                in
                    evalRest first cs
                end
            else
                tryCases v cs
    in
        tryCases (evalExpr regs cond) cases
        handle BreakException exnLabel =>
               if labelMatch labels exnLabel
               then Undefined
               else raise BreakException exnLabel
    end


and evalSwitchTypeStmt (regs:REGS)
                       (cond:EXPRESSION)
                       (ty:TYPE)
                       (cases:CATCH_CLAUSE list)
    : VALUE =
    let
        val v = evalExpr regs cond
    in
        case catch regs v cases of
            NONE => Undefined
          | SOME v => v
    end

(*
 FOR_STATEMENT
     structural subtype test in evalIterable
                                    TODO isEach
 *)

and evalIterable (regs:REGS)
                 (obj:EXPRESSION)
    : OBJ =
    let
        val v = evalExpr regs obj
        fun finishWith v = 
            case v of
                Object ob => ob
              | Undefined => newPublicObj regs
              | Null => newPublicObj regs            
    in
        (*
         * Implement the IE JScript quirk where for (i in null) and
         * for (i in undefined) do not loop at all by returning an
         * empty object for Undefined and Null.
         *)
        finishWith v
    end

and callIteratorGet (regs:REGS)
                    (iterable:OBJ)
    : OBJ =
    let
        val iteratorGET = { id = Ustring.GET_, ns = getIteratorNamespace regs }
        val args = [Object iterable, newBoolean regs true]
        val iterator = evalNamedMethodCall regs (#global regs) iteratorGET args
    in
        needObj regs iterator
    end

and callIteratorNext (regs:REGS)
                     (iterator:OBJ)
    : VALUE =
    (evalNamedMethodCall regs iterator public_next [])
    handle e as ThrowException v => raise (if isStopIteration regs v
                                           then StopIterationException
                                           else e)

and evalForInStmt (regs:REGS)
                  (forInStmt:FOR_ENUM_STATEMENT)
    : VALUE =
    case forInStmt of
        { isEach, defn, obj, rib, next, labels, body, ... } =>
        let
            val iterable = evalIterable regs obj
            val iterator = callIteratorGet regs iterable
            val forInRegs = evalHead regs (Head (valOf rib, []))

            (*
             * The following code is ugly but it needs to handle the cases
             * when there is a binding on the lhs of 'in' and not. If there
             * is no binding on the lhs, then the parser uses a LetExpr to
             * allocate a temporary fixture for the iteration value and
             * possibly some for desugaring. The associated inits (i) are
             * separated out as 'nextInits' for execution after the iteration
             * value is set each time around the loop.
             * 
             * FIXME: maybe unify 'next' as a new kind of expr. This would
             * trade redundancy for clarity. Not sure it's worth it.
             *)
            val (nextTarget, nextHead, nextInits, nextExpr) =
                case next of
                    ExprStmt e =>
                    (case e of
                         InitExpr (target, head, inits) =>
                         (target, head, inits, NONE)
                       | LetExpr {defs, body, head = SOME (Head (f, i))} =>
                         (Hoisted, Head (f, []), i, SOME body)
                       | _ => internalError ["evalForInStmt: invalid expr structure"])
                  | _ => internalError ["evalForInStmt: invalid stmt structure"]
                         
            (*
             * A binding init on the lhs of 'in' will be included in nextHead
             * and evaluated once when the tempRegs is created here
             *)

            val tempRegs = evalHead forInRegs nextHead
            val temps = getScopeTemps (#scope tempRegs)

            fun loop (accum:VALUE option) =
                let
                    val v = (SOME (callIteratorNext forInRegs iterator)
                             handle StopIterationException =>
                                    NONE)
                    val b = (case v of
                                 NONE => false
                               | x => true)
                in
                    if b
                    then
                        let
                            val _ = defTemp temps 0 (valOf v)   (* def the iteration value *)
                            val _ = evalScopeInits tempRegs nextTarget nextInits
                            val _ = Option.map (evalExpr tempRegs) nextExpr
                            val curr = (SOME (evalStmt forInRegs body)
                                        handle ContinueException exnLabel =>
                                               if labelMatch labels exnLabel
                                               then NONE
                                               else raise ContinueException exnLabel)
                            val nextVal = (case curr of
                                               NONE => accum
                                             | x => x)
                        in
                            loop nextVal handle BreakException exnLabel =>
                                                if labelMatch labels exnLabel
                                                then accum
                                                else raise BreakException exnLabel
                        end
                    else
                        accum
                end
        in
            case
                loop NONE handle BreakException exnLabel =>
                                 if labelMatch labels exnLabel
                                 then NONE
                                 else raise BreakException exnLabel
             of
                NONE => Undefined
              | SOME v => v
        end

and evalForStmt (regs:REGS)
                (forStmt:FOR_STATEMENT)
    : VALUE =
    case forStmt of
        { rib, init, cond, update, labels, body, ... } =>
        let
            val forRegs = evalHead regs (Head (valOf rib, []))

            fun loop (accum:VALUE option) =
                let
                    val b = case cond of
                                ListExpr [] => true
                              | _ => toBoolean (evalExpr forRegs cond)
                in
                    if b
                    then
                        let
                            val curr = (SOME (evalStmt forRegs body)
                                        handle ContinueException exnLabel =>
                                               if labelMatch labels exnLabel
                                               then NONE
                                               else raise ContinueException exnLabel)
                            val next = (case curr of
                                            NONE => accum
                                          | x => x)
                        in
                            evalExpr forRegs update;
                            loop next handle BreakException exnLabel =>
                                             if labelMatch labels exnLabel
                                             then accum
                                             else raise BreakException exnLabel
                        end
                    else
                        accum
                end
        in
            evalStmts forRegs init;
            case
                loop NONE handle BreakException exnLabel =>
                                 if labelMatch labels exnLabel
                                 then NONE
                                 else raise BreakException exnLabel
             of
                NONE => Undefined
              | SOME v => v
        end


and evalReturnStmt (regs:REGS)
                   (e:EXPRESSION)
    : VALUE =
    raise (ReturnException (evalExpr regs e))


and evalThrowStmt (regs:REGS)
                  (e:EXPRESSION)
    : VALUE =
    raise (ThrowException (evalExpr regs e))


and evalBreakStmt (regs:REGS)
                  (lbl:IDENTIFIER option)
    : VALUE =
    (trace ["raising BreakException ",
            case lbl of NONE => "empty" 
                      | SOME id => Ustring.toAscii id];
     raise (BreakException lbl))

    
and evalContinueStmt (regs:REGS)
                     (lbl:IDENTIFIER option)
    : VALUE =
    raise (ContinueException lbl)


and evalAnonFragment (regs:REGS)
                     (block:BLOCK)
    : VALUE =
    let
        fun findHoistingScopeObj scope = 
            case scope of 
                Scope { kind = InstanceScope, ... } => scope
              | Scope { kind = ActivationScope, ... } => scope
              | Scope { parent = NONE, ... } => scope
              | Scope { parent = SOME p, ...} => findHoistingScopeObj p
        val scope = findHoistingScopeObj (#scope regs)
        val regs = withScope regs scope
                   
        val Block { pragmas, head, body, loc, ... } = block
        val (rib, inits) = case head of 
                               NONE => ([], [])
                             | SOME (Head (r,i)) => (r,i)
        val Scope { object, temps, ... } = scope

        val _ = setLoc loc
        val _ = allocScopeRib regs rib

        val _ = setLoc loc
        val _ = evalInits regs object temps inits

        val _ = setLoc loc
        val res = evalStmts regs body
        val _ = setLoc loc
    in
        res
    end

and evalFragment (regs:REGS)
                 (frag:FRAGMENT)
    : VALUE =
    let
        fun lastVal [] = Undefined
          | lastVal x = List.last x
    in
        (case frag of 
             Anon (Block {head=NONE, ...}) => 
             error regs ["top-level block with no head"]
           | Anon (Block {head=SOME (Head (rib, inits)), body, loc, ...}) => 
             (* 
              * NB: do *not* do evalBlock here. It's not a "normal" block. The ribs 
              * and inits are not intended for a temporary scope, but rather the 
              * scope that you'd search for as a hoisting target (either the activation 
              * scope enclosing an eval, or the global scope)
              *)
             let
                 val _ = trace ["entering anon unit block"]
                 val { scope, ... } = regs
                 val Scope { temps, ...} = scope
                 val obj = findTargetObj regs scope Hoisted
                 val Obj { ident, ... } = obj
             in
                 trace ["resolved anonymous fragment target to obj #", Int.toString ident];
                 setLoc loc;
                 allocObjRib regs obj NONE rib;
                 setLoc loc;
                 trace ["allocating anonymous fragment inits on obj #", Int.toString ident];
                 evalInits regs obj temps;
                 setLoc loc;
                 trace ["running anonymous fragment stmts"];
                 evalStmts regs body
             end)
        handle ThrowException v =>
               let
                   val loc = !loc
                   val exnStr = Ustring.toAscii (toUstring regs v)
               in
                   setLoc loc;
                   error regs ["uncaught exception: ", exnStr]
               end
             | StopIterationException =>
               let
                   val loc = !loc
               in
                   setLoc loc;
                   error regs ["uncaught StopIteration exception"]
               end
    end

and evalTopFragment (regs:REGS)
                    (frag:FRAGMENT)
    : VALUE =
    let
        val _ = setLoc NONE
        val res = evalFragment regs frag
        val _ = reportProfile regs
    in
        res
    end
end
