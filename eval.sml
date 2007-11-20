(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
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

fun log (ss:string list) = LogErr.log ("[eval] " :: ss)

val doTrace = ref false
fun fmtName n = if (!doTrace) then LogErr.name n else ""
fun fmtMultiname n = if (!doTrace) then LogErr.multiname n else ""

fun trace (ss:string list) = 
    if (!doTrace) then log ss else ()

fun error (regs:Mach.REGS) 
          (ss:string list) =
    (LogErr.log ("[stack] " :: [Mach.stackString (Mach.stackOf regs)]);
     LogErr.evalError ss)

fun makeTy (tyExpr:Ast.TYPE_EXPR) 
    : Ast.TY = 
    (* 
     * NB: when we have a TYPE_EXPR at runtime, it's always in a context
     * where it's a ground term and its environment is irrelevant: a value
     * has been built and/or we've extracted it from a TY by normalization 
     * and ground-term extraction. So it's harmless API-compatibility to
     * repackage one of these in an empty environment to pass back into 
     * functions of the form Type.foo (ty:TY) that are expecting TY values;
     * ground TYPE_EXPR values in empty environments are a semantic
     * subset (though not an ML-type-theoretic subset) of TY values.
     *)
    Ast.Ty { expr = tyExpr,
             ribId = NONE }


fun extractRuntimeTypeRibs (regs:Mach.REGS)
                           (scope:Mach.SCOPE) 
                           (ribs:Ast.RIBS) 
    : Ast.RIBS = 
      let
          fun typePropToFixture (n:Ast.NAME, prop:Mach.PROP) : 
              (Ast.FIXTURE_NAME * Ast.FIXTURE) = 
              case (#state prop) of 
                  Mach.TypeProp => (Ast.PropName n, Ast.TypeFixture (#ty prop))
                | _ => error regs ["non-type property in type-arg scope"]
                       
          fun typePropsToRib (props:Mach.PROP_BINDINGS) 
              : Ast.RIB = 
              map typePropToFixture (NameMap.listItemsi (!props))
              
          val Mach.Scope {kind, object=Mach.Obj {props, ...}, parent, ...} = scope
          val ribs' = case kind of 
                          Mach.TypeArgScope => (typePropsToRib props) :: ribs
                        | _ => ribs
      in
          case parent of 
              NONE => List.rev ribs'
            | SOME p => extractRuntimeTypeRibs regs p ribs'
      end

fun normalize (regs:Mach.REGS)
              (ty:Ast.TY)
    : Ast.TY = 
    let
        val locals = extractRuntimeTypeRibs regs (#scope regs) []
    in
        Type.normalize (#prog regs) locals ty
    end

fun needGroundTy (regs:Mach.REGS)
                 (ty:Ast.TY)
    : Ast.TY = 
    let
    (* 
     * needGroundTy implements the above assumption: a last-ditch 
     * requirement that we *must* turn this TY into a ground TYPE_EXPR. 
     * We call this in a variety of contexts where the program can't 
     * really sensibly proceed if we can't ground the type.
     *)
        val norm = normalize regs ty
    in
        if Type.isGroundTy norm
        then norm             
        else error regs ["Unable to ground type closure: ", 
                         LogErr.ty (AstQuery.typeExprOf ty)]
    end
    

fun evalTy (regs:Mach.REGS)
           (ty:Ast.TY)
    : Ast.TYPE_EXPR =     
    AstQuery.typeExprOf (needGroundTy regs ty)


(* Exceptions for object-language control transfer. *)
exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> Mach.VAL)
exception ThrowException of Mach.VAL
exception ReturnException of Mach.VAL

exception InternalError

(* Dummy values to permit raising exceptions in non-unit type contexts. *)
val dummyVal = Mach.Null
val dummyObj = Mach.newObj Mach.NoTag Mach.Null NONE
val dummyObjId = 0
val dummyRef = (dummyObj, Name.nons_global)
val dummyTypeExpr = Ast.SpecialType Ast.Any
val dummyNs = Name.noNS


(* Handy operator for subtype checks *)

infix 4 <:;
fun ((tsub:Ast.TYPE_EXPR) <: (tsup:Ast.TYPE_EXPR)) =
    Type.groundIsSubtype tsub tsup
    

fun mathOp (v:Mach.VAL)
           (decimalFn:(Decimal.DEC -> 'a) option)
           (doubleFn:(Real64.real -> 'a) option)
           (intFn:(Int32.int -> 'a) option)
           (uintFn:(Word32.word -> 'a) option)
           (default:'a)
    : 'a =
    let
        fun fnOrDefault fo v = case fo of
                                   NONE => default
                                 | SOME f => f v
    in
        case v of
            Mach.Object (Mach.Obj ob) =>
            (case !(#magic ob) of
                 SOME (Mach.Decimal d) => fnOrDefault decimalFn d
               | SOME (Mach.Double d) => fnOrDefault doubleFn d
               | SOME (Mach.Int i) => fnOrDefault intFn i
               | SOME (Mach.UInt u) => fnOrDefault uintFn u
               | _ => default)
          | _ => default
    end

fun extendScope (p:Mach.SCOPE)
                (ob:Mach.OBJ)
                (kind:Mach.SCOPE_KIND)
    : Mach.SCOPE =
    let
        val Mach.Scope { decimal, ... } = p
    in
        Mach.Scope { parent = (SOME p),
                     object = ob,
                     temps = ref [],
                     kind = kind,
                     decimal = decimal }
    end

fun extendScopeReg (r:Mach.REGS)
                   (ob:Mach.OBJ)
                   (kind:Mach.SCOPE_KIND)
    : Mach.REGS =
    let
        val {scope,this,global,prog,aux} = r
    in
        {scope=extendScope scope ob kind,
         this=this,
         global=global,
         prog=prog,
         aux=aux}
    end

fun withThis (r:Mach.REGS)
             (newThis:Mach.OBJ)
    : Mach.REGS =
    let
        val {scope,this,global,prog,aux} = r
    in
        {scope=scope, 
         this=newThis, 
         global=global, 
         prog=prog,
         aux=aux}
    end

fun withScope (r:Mach.REGS)
              (newScope:Mach.SCOPE)
    : Mach.REGS =
    let
        val {scope,this,global,prog,aux} = r
    in
        {scope=newScope, 
         this=this, 
         global=global, 
         prog=prog,
         aux=aux}
    end

fun withProg (r:Mach.REGS)
             (newProg:Fixture.PROGRAM)
    : Mach.REGS =
    let
        val {scope,this,global,prog,aux} = r
    in
        {scope=scope, 
         this=this, 
         global=global, 
         prog=newProg,
         aux=aux}
    end


fun getObjId (obj:Mach.OBJ)
    : Mach.OBJ_IDENT =
    case obj of
        Mach.Obj { ident, ... } => ident

fun slotObjId (regs:Mach.REGS) 
              (slotFunc:Mach.REGS -> (Mach.OBJ option) ref) 
    : Mach.OBJ_IDENT =
    case !(slotFunc regs) of 
        NONE => ~1
      | SOME obj => getObjId obj



fun getScopeObj (scope:Mach.SCOPE)
    : Mach.OBJ =
    case scope of
        Mach.Scope { object, ... } => object


fun getScopeId (scope:Mach.SCOPE)
    : Mach.OBJ_IDENT = getObjId (getScopeObj scope)


fun getScopeTemps (scope:Mach.SCOPE)
    : Mach.TEMPS =
    case scope of
        Mach.Scope { temps, ... } => temps

fun getTemps (regs:Mach.REGS)
    : Mach.TEMPS =
    getScopeTemps (#scope regs)

(*
 * The global object and scope.
 *)

fun getGlobalScope (regs:Mach.REGS) 
    : Mach.SCOPE = Mach.makeGlobalScopeWith (#global regs)


(*
 * A small number of functions do not fully evaluate to Mach.VAL
 * values, but instead to REFs; these are temporary artifacts of
 * evaluation, not first-class values in the language.
 *)

type REF = (Mach.OBJ * Ast.NAME)

datatype NUMBER_TYPE = ByteNum | IntNum | UIntNum | DoubleNum | DecimalNum 

fun numTypeOf (regs:Mach.REGS) 
              (v:Mach.VAL) 
    : NUMBER_TYPE = 
    case (Mach.needMagic v) of 
        Mach.Byte _ => ByteNum
      | Mach.Int _ => IntNum
      | Mach.UInt _ => UIntNum
      | Mach.Double _ => DoubleNum
      | Mach.Decimal _ => DecimalNum
      | other => error regs ["unexpected magic type in numTypeOf: ", 
                             Ustring.toAscii (Mach.magicToUstring other)]
                          
fun numLE (a:NUMBER_TYPE)
          (b:NUMBER_TYPE) =
    if a = b
    then true
    else 
        case (a,b) of
            (ByteNum, UIntNum) => true
          | (ByteNum, DoubleNum) => true
          | (ByteNum, DecimalNum) => true
          | (UIntNum, DoubleNum) => true
          | (UIntNum, DecimalNum) => true
          | (IntNum, DoubleNum) => true
          | (IntNum, DecimalNum) => true
          | (DoubleNum, DecimalNum) => true
          | _ => false
                 
fun promoteToCommon (regs:Mach.REGS) 
                    (a:NUMBER_TYPE) 
                    (b:NUMBER_TYPE)
    : NUMBER_TYPE =
    if a = b
    then a
    else 
        case if numLE a b then (a,b) else (b,a) of 
            (ByteNum, UIntNum) => UIntNum
          | (ByteNum, DoubleNum) => DoubleNum
          | (ByteNum, DecimalNum) => DecimalNum
          | (UIntNum, DoubleNum) => DoubleNum
          | (UIntNum, DecimalNum) => DecimalNum
          | (IntNum, DoubleNum) => DoubleNum
          | (IntNum, DecimalNum) => DecimalNum
          | (DoubleNum, DecimalNum) => DecimalNum
          (* These two may be a little contentious: they're unordered! *)
          | (IntNum, UIntNum) => DoubleNum
          | (UIntNum, IntNum) => DoubleNum
          | _ => error regs ["unexpected number combination in promoteToCommon"]
                 
(*
 * Ident exprs evaluate to either either explicitly qualified names or implicitly qualified
 * mutinames. We differentiate these cases with a discriminated union.
 *)

datatype NAME_OR_MULTINAME =
         Name of Ast.NAME
       | Multiname of Ast.MULTINAME

fun fmtNomn (nomn:NAME_OR_MULTINAME) =
    case nomn of
        Name name => fmtName name
      | Multiname mname => fmtMultiname mname

fun nomnToStr (nomn:NAME_OR_MULTINAME) =
    case nomn of
        Name name => LogErr.name name
      | Multiname mname => LogErr.multiname mname

fun shouldBeDontEnum (regs:Mach.REGS)
                     (n:Ast.NAME) 
                     (obj:Mach.OBJ) 
    : bool =
    case (#ns n) of
        Ast.Private _ => true
      | _ => (Mach.isBooting regs) 
             andalso (getObjId obj) = (getObjId (#global regs))

(* Fundamental object methods *)

fun allocRib (regs:Mach.REGS)
             (obj:Mach.OBJ)
             (this:Mach.OBJ option)
             (temps:Mach.TEMPS)
             (f:Ast.RIB)
    : unit =
    let
        
        val Mach.Obj { props, ident, ... } = obj
        val _ = trace ["allocating rib on object id #", Int.toString ident]
        val {scope, ...} = regs
        val methodScope = extendScope scope obj Mach.ActivationScope
        fun valAllocState (ty:Ast.TY)
            : Mach.PROP_STATE =
            
            (* Every value fixture has a type, and every type has an
             * associated "allocated state". Note that
             * this is *not* the same as saying that every type
             * has an associated default value; for *some* types
             * the allocated state is a default value; for
             * types that are non-nullable, however, the allocated
             * state is Mach.UninitProp. This property
             * state should never be observable to a user. It is
             * always a hard error to read a property in
             * Mach.UninitProp state, and it is always a hard
             * error to complete the initialization phase of an
             * object with any properties remaining in
             * Mach.UninitProp state. *)

            let
                val Ast.Ty { expr, ribId } = ty 
            in            
                case expr of
                    Ast.SpecialType (Ast.Any) =>
                    Mach.ValProp (Mach.Undef)
                    
                  | Ast.SpecialType (Ast.Null) =>
                    Mach.ValProp (Mach.Null)
                    
                  | Ast.SpecialType (Ast.Undefined) =>
                    Mach.ValProp (Mach.Undef)
                    
                  | Ast.SpecialType (Ast.VoidType) =>
                    error regs ["attempt to allocate void-type property"]
                    
                  (* FIXME: is this correct? Maybe we need to check them all to be nullable? *)
                  | Ast.UnionType _ =>
                    Mach.ValProp (Mach.Null)
                    
                  | Ast.ArrayType _ =>
                    Mach.ValProp (Mach.Null)
                    
                  | Ast.FunctionType _ =>
                    Mach.UninitProp
                    
                  | Ast.ObjectType _ =>
                    Mach.ValProp (Mach.Null)
                    
                  | Ast.AppType {base, ...} =>
                    valAllocState (Ast.Ty { expr=base, ribId=ribId })
                
                  | Ast.NullableType { expr, nullable=true } =>
                    Mach.ValProp (Mach.Null)
                    
                  | Ast.NullableType { expr, nullable=false } =>
                    Mach.UninitProp
                    
                  | Ast.TypeName ident =>
                    error regs ["allocating fixture with unresolved type name: ", LogErr.ty expr]
                    
                  | Ast.ElementTypeRef _ =>
                    error regs ["allocating fixture of unresolved element type reference"]
                
                  | Ast.FieldTypeRef _ =>
                    error regs ["allocating fixture of unresolved field type reference"]
                    
                  (* Note, the definer must have created inits for nonnullable primitive types
                   * where the program does not contain such inits.
                   *)
                  | Ast.InstanceType n =>
                    if (#nonnullable n)
                    then
                        (* It is possible that we're booting and the class n doesn't even exist yet. *)
                        if (not (Mach.isBooting regs)) orelse 
                           Mach.isClass (getValue regs (#global regs) (#name n))
                        then
                            case Type.groundFindConversion (#prog regs) (Ast.SpecialType Ast.Undefined) expr of
                                SOME t => Mach.ValProp (checkAndConvert regs Mach.Undef (makeTy t))
                              | NONE => Mach.UninitProp
                        else
                            Mach.UninitProp
                    else
                        Mach.ValProp Mach.Null
                        
                  | Ast.LamType _ => 
                    Mach.UninitProp
            end

        fun tempPadding n =
            if n = 0
            then []
            else (Ast.SpecialType Ast.Any, Mach.UninitTemp)::(tempPadding (n-1))
                 
        fun allocFixture (n, f) =
            case n of
                Ast.TempName t =>
                let
                    val tmps = !temps
                    val tlen = List.length tmps
                    val emptyTmp = (Ast.SpecialType Ast.Any, Mach.UninitTemp)
                in
                    case f of
                        Ast.ValFixture { ty, ... } =>  
                        (* 
                         * FIXME: temp types are not needed, 
                         * use the value tag for rt typechecking 
                         *)
                        (if t = tlen
                         then (trace ["allocating fixture for temporary ", Int.toString t];
                               temps := emptyTmp::tmps)
                         else 
                             if t < tlen
                             then 
                                 (trace ["ignoring fixture, already allocated ", Int.toString t];
                                  temps := (List.take (tmps, (tlen-t-1))) @ 
                                           (((evalTy regs ty), Mach.UninitTemp) :: 
                                            (List.drop (tmps, (tlen-t)))))
                                 
                             else 
                                 (trace ["allocating rib for temporaries ", 
                                         Int.toString tlen, " to ", Int.toString t];
                                  temps := emptyTmp :: ((tempPadding (t-tlen)) @ tmps)))
                        
                      | _ => error regs ["allocating non-value temporary"]
                end
              | Ast.PropName pn =>
                let
                    fun allocProp state p =
                        if Mach.hasProp props pn
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
                                fun fail _ = error regs ["allocating duplicate property name: ", LogErr.name pn]
                                fun permit _ = (trace ["replacing ", state, " property ", fmtName pn];
                                                Mach.delProp props pn; 
                                                Mach.addProp props pn p)
                                val state = (#state (Mach.getProp props pn))
                            in
                                if Mach.isBooting regs 
                                then fail ()
                                else case state of
                                      Mach.ValProp _ => permit ()
                                    | Mach.MethodProp _ => permit ()
                                    | Mach.NativeFunctionProp _ => permit ()
                                    | Mach.VirtualValProp _ => permit ()
                                    | _ => fail ()
                            end
                        else (trace ["allocating fixture for ", state, " property ", fmtName pn];
                              Mach.addProp props pn p)
                in
                    case f of
                        Ast.TypeFixture ty =>
                            allocProp "type"
                                      { ty = normalize regs ty,
                                        state = Mach.TypeProp,
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = true,
                                                  isFixed = true } }
                            
                      | Ast.MethodFixture { func, ty, readOnly, ... } =>
                        let
                            val Ast.Func { native, ... } = func
                            val p = if native
                                    then Mach.NativeFunctionProp (Mach.getNativeFunction pn)
                                    else Mach.MethodProp (newFunClosure methodScope func this)
                        in
                            allocProp "method"
                                      { ty = normalize regs ty,
                                        state = p,
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = readOnly,
                                                  isFixed = true } }
                        end
                        
                      | Ast.ValFixture { ty, readOnly, ... } =>
                        let
                            val ty = needGroundTy regs ty
                        in
                            allocProp "value"
                                      { ty = ty,
                                        state = valAllocState ty,
                                        attrs = { dontDelete = true,
                                                  dontEnum = true, (* ticket #88 *) (* shouldBeDontEnum regs pn obj, *)
                                                  readOnly = readOnly,
                                                  isFixed = true } }
                        end

                      | Ast.VirtualValFixture { ty, getter, setter, ... } =>
                        let
                            val getFn = case getter of
                                            NONE => NONE
                                          | SOME f => SOME (newFunClosure methodScope f this)
                            val setFn = case setter of
                                            NONE => NONE
                                          | SOME f => SOME (newFunClosure methodScope f this)
                        in
                            allocProp "virtual value"
                                      { ty = needGroundTy regs ty,
                                        state = Mach.VirtualValProp { getter = getFn,
                                                                      setter = setFn },
                                        attrs = { dontDelete = true,
                                                  dontEnum = true, (* ticket #88 *) (* shouldBeDontEnum regs pn obj, *)
                                                  readOnly = true,
                                                  isFixed = true } }
                        end
                        
                      | Ast.ClassFixture cls =>
                        let
                            val Ast.Cls {classRib, ...} = cls
                            val _ = trace ["allocating class object for class ", fmtName pn]
                            val classObj = needObj regs (newClass regs scope cls)
                            val _ = trace ["allocating class rib on class ", fmtName pn]
                            (* FIXME: 'this' binding in class objects might be wrong here. *)
                            val _ = allocObjRib regs classObj NONE classRib
                        in
                            allocProp "class"
                                      { ty = makeTy (Name.typename Name.intrinsic_Class),
                                        state = Mach.ValProp (Mach.Object classObj),
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = true,
                                                  isFixed = true } }
                        end

                      | Ast.NamespaceFixture ns =>
                        allocProp "namespace"
                                  { ty = makeTy (Name.typename Name.ES4_Namespace),
                                    state = Mach.NamespaceProp ns,
                                    attrs = { dontDelete = true,
                                              dontEnum = true,
                                              readOnly = true,
                                              isFixed = true } }

                      | Ast.TypeVarFixture _ =>
                        allocProp "type variable"
                                  { ty = makeTy (Name.typename Name.intrinsic_Type),
                                    state = Mach.TypeVarProp,
                                    attrs = { dontDelete = true,
                                              dontEnum = true,
                                              readOnly = true,
                                              isFixed = true } }

                      | Ast.InterfaceFixture iface =>  (* FIXME *)
                        let
                            val _ = trace ["allocating interface object for interface ", fmtName pn]
                            val ifaceObj = needObj regs (newInterface regs scope iface)
                        in
                            allocProp "interface"
                                      { ty = makeTy (Name.typename Name.intrinsic_Type),
                                        state = Mach.ValProp (Mach.Object ifaceObj),
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = true,
                                                  isFixed = true } }
                        end

                (* | _ => error regs ["Shouldn't happen: failed to match in Eval.allocRib#allocFixture."] *)

                end
    in
        List.app allocFixture f
    end


and allocObjRib (regs:Mach.REGS)
                (obj:Mach.OBJ)
                (this:Mach.OBJ option)
                (f:Ast.RIB)
    : unit =
    let
        val (temps:Mach.TEMPS) = ref []
    in
        allocRib regs obj this temps f;
        if not ((length (!temps)) = 0)
        then error regs ["allocated temporaries in non-scope object"]
        else ()
    end

and allocScopeRib (regs:Mach.REGS)
                  (f:Ast.RIB)
    : unit =
    case (#scope regs) of
        Mach.Scope { object, temps, ... } =>
        allocRib regs object NONE temps f


and asArrayIndex (v:Mach.VAL)
    : Word32.word =
    case v of
        Mach.Object (Mach.Obj ob) =>
        (case !(#magic ob) of
             SOME (Mach.Int i) => if i >= 0 then
                                      Word32.fromInt (Int32.toInt i)
                                  else
                                      0wxFFFFFFFF
           | SOME (Mach.UInt u) => u
           | SOME (Mach.Double d) => if Real64.compare(Real64.realFloor d, d) = EQUAL andalso
                                        d >= 0.0 andalso
                                        d < 4294967295.0
                                     then
                                         Word32.fromLargeInt (Real64.toLargeInt IEEEReal.TO_NEAREST d)
                                     else
                                         0wxFFFFFFFF
           | SOME (Mach.Decimal d) => 0wxFFFFFFFF (* FIXME *)
           | _ => 0wxFFFFFFFF)
      | _ => 0wxFFFFFFFF


and hasOwnValue (obj:Mach.OBJ)
                (n:Ast.NAME)
    : bool =
    case obj of
        Mach.Obj { props, ... } =>
        Mach.hasProp props n


and findValue (obj:Mach.OBJ)
              (n:Ast.NAME)
    : REF option =
    if hasOwnValue obj n
    then SOME (obj, n)
    else (case obj of
              Mach.Obj { proto, ... } =>
              case (!proto) of
                  Mach.Object p => findValue p n
                | _ => NONE)


and hasValue (obj:Mach.OBJ)
             (n:Ast.NAME)
    : bool =
    case findValue obj n of
        NONE => false
      | _ => true

(*
 * *Similar to* ES-262-3 8.7.1 GetValue(V), there's
 * no Reference type in ES4.
 *)
and getValueOrVirtual (regs:Mach.REGS)
                      (obj:Mach.OBJ)
                      (name:Ast.NAME)
                      (doVirtual:bool)
                      (propNotFound:(Mach.OBJ -> Mach.VAL))
    : Mach.VAL =
    let
        val _ = trace ["getting property ", fmtName name, 
                       " on obj #", Int.toString (getObjId obj)]
        val Mach.Obj { props, ... } = obj
        fun upgraded (currProp:Mach.PROP) newVal =
            (Mach.delProp props name;
             Mach.addProp props name { state = Mach.ValProp newVal,
                                       ty = (#ty currProp),
                                       attrs = (#attrs currProp) };
             trace ["upgraded property ", fmtName name ];
             newVal)
    in
        case Mach.findProp props name of
            SOME prop =>
            (case (#state prop) of
                 Mach.TypeProp =>
                 (throwTypeErr regs ["getValue on a type property: ",
                                     LogErr.name name]; 
                  dummyVal)

               | Mach.TypeVarProp =>
                 (throwTypeErr regs ["getValue on a type variable property: ",
                                     LogErr.name name];
                  dummyVal)

               | Mach.UninitProp =>
                 (throwTypeErr regs ["getValue on an uninitialized property: ",
                                     LogErr.name name];
                  dummyVal)

               | Mach.VirtualValProp { getter, ... } =>
                 if doVirtual
                 then
                     case getter of
                         SOME g => invokeFuncClosure (withThis regs obj) g []
                       | NONE => Mach.Undef
                 else
                     (* FIXME: possibly throw here? *)
                     Mach.Undef

               | Mach.NamespaceProp n =>
                 upgraded prop (newNamespace regs n)

               | Mach.NativeFunctionProp nf =>
                 upgraded prop (newNativeFunction regs nf)

               | Mach.MethodProp closure =>
                 upgraded prop (newFunctionFromClosure regs closure)

               | Mach.ValListProp vals =>
                 (* FIXME: The 'arguments' object can't be an array. *)
                 upgraded prop (newArray regs vals)

               | Mach.ValProp v => v)
          | NONE =>
            let
                fun catchAll _ =
                    (* FIXME: need to use builtin Name.es object here, when that file exists. *)
                    (trace ["running meta::get(\"", 
                            (Ustring.toAscii (#id name)), 
                            "\") catchall on obj #", 
                            Int.toString (getObjId obj)];
                     (evalCallMethodByRef (withThis regs obj) 
                                          (obj, Name.meta_get) 
                                          [newString regs (#id name)]))
            in
                if doVirtual
                then 
                    case Mach.findProp props Name.meta_get of
                        SOME { state = Mach.MethodProp _, ... } => catchAll ()
                      | SOME { state = Mach.NativeFunctionProp _, ... } => catchAll ()
                      | _ => propNotFound obj
                else 
                    propNotFound obj
            end
    end


and getValue (regs:Mach.REGS)
             (obj:Mach.OBJ)
             (name:Ast.NAME)
    : Mach.VAL =
    let
        fun propNotFound (curr:Mach.OBJ)
            : Mach.VAL =
            let
                val Mach.Obj { proto, ... } = curr
            in
                case !proto of
                    Mach.Object ob => 
                    getValueOrVirtual regs ob name true propNotFound
                  | _ =>
                    if isDynamic regs obj
                    then Mach.Undef
                    else (throwTypeErr 
                              regs 
                              ["attempting to get nonexistent property ",
                               LogErr.name name,
                               "from non-dynamic object"]; dummyVal)
            end
    in
        getValueOrVirtual regs obj name true propNotFound
    end


and typeOpFailure (regs:Mach.REGS)
                  (prefix:string)
                  (v:Mach.VAL)
                  (tyExpr:Ast.TYPE_EXPR)
    : unit =
    throwTypeErr regs [prefix, ": val=", Mach.approx v,
                       " type=", LogErr.ty (typeOfVal regs v),
                       " wanted=", LogErr.ty tyExpr]
    
and checkAndConvert (regs:Mach.REGS)
                    (v:Mach.VAL)
                    (ty:Ast.TY)
    : Mach.VAL =
    let
        val tyExpr = evalTy regs ty
    in
        if (isCompatible regs v tyExpr)
        then v
        else
            let
                val (classType:Ast.TYPE_EXPR) =
                    case Type.groundFindConversion (#prog regs) (typeOfVal regs v) tyExpr of
                        NONE => (typeOpFailure regs "incompatible types w/o conversion" v tyExpr; 
                                 dummyTypeExpr)
                      | SOME n => n
                val (classTy:Ast.INSTANCE_TYPE) = AstQuery.needInstanceType classType
                val (classObj:Mach.OBJ) = instanceClass regs classTy
                (* FIXME: this will call back on itself! *)
                val converted = evalCallMethodByRef (withThis regs classObj) (classObj, Name.meta_invoke) [v]
            in
                if isCompatible regs converted tyExpr
                then converted
                else (typeOpFailure regs "conversion returned incompatible value" converted tyExpr; dummyVal)
            end
    end

and isDynamic (regs:Mach.REGS)
              (obj:Mach.OBJ)
    : bool =
    let
        val Mach.Obj { tag, ... } = obj
    in
        case tag of
            Mach.ObjectTag _ => true
          | Mach.ArrayTag _ => true
          | Mach.FunctionTag _ => true
          | Mach.NoTag => true
          | Mach.ClassTag ity => (#dynamic ity)
    end

and setValueOrVirtual (regs:Mach.REGS)
                      (obj:Mach.OBJ)
                      (name:Ast.NAME)
                      (v:Mach.VAL)
                      (doVirtual:bool)
    : unit =
    let
        val Mach.Obj { props, ... } = obj
    in
        case Mach.findProp props name of
            SOME existingProp =>
            let
                val existingAttrs = (#attrs existingProp)
                val ty = (#ty existingProp)
                fun newProp _ = { state = Mach.ValProp (checkAndConvert regs v ty),
                                  ty = ty,
                                  attrs = existingAttrs }
                fun write _ =
                    let
                        val np = newProp()
                    in
                        Mach.delProp props name;
                        Mach.addProp props name np
                    end
            in
                case (#state existingProp) of
                    Mach.UninitProp =>
                    throwTypeErr regs ["setValue on uninitialized property",
                                       LogErr.name name]
                

                  | Mach.TypeVarProp =>
                    throwTypeErr regs ["setValue on type variable property:",
                                       LogErr.name name]

                  | Mach.TypeProp =>
                    throwTypeErr regs ["setValue on type property: ",
                                       LogErr.name name]

                  | Mach.NamespaceProp _ =>
                    throwTypeErr regs ["setValue on namespace property: ",
                                       LogErr.name name]

                  | Mach.NativeFunctionProp _ =>
                    throwTypeErr regs ["setValue on native function property: ",
                                       LogErr.name name]

                  | Mach.MethodProp _ =>
                    if (#readOnly existingAttrs)
                    then throwTypeErr regs ["setValue on read-only method property: ",
                                            LogErr.name name]
                    else write ()  (* ES3 style mutable fun props are readOnly *)

                  | Mach.ValListProp _ =>
                    throwTypeErr regs ["setValue on value-list property: ",
                                       LogErr.name name]

                  | Mach.VirtualValProp { setter = SOME s, ... } =>
                    if doVirtual
                    then (invokeFuncClosure (withThis regs obj) s [v]; ())
                    else write ()

                  | Mach.VirtualValProp { setter = NONE, ... } =>
                    if doVirtual
                    then ()  (* ignore it *)
                    else write ()

                  | Mach.ValProp _ =>
                    if (#readOnly existingAttrs)
                    then ()  (* ignore it *)
                    else write ()
            end
          | NONE =>
            let
                fun newProp _ =
                    let
                        val prop = { state = Mach.ValProp v,
                                     ty = makeTy (Ast.SpecialType Ast.Any),
                                     attrs = { dontDelete = false,
                                               dontEnum = shouldBeDontEnum regs name obj,
                                               readOnly = false,
                                               isFixed = false } }
                    in
                        if isDynamic regs obj
                        then Mach.addProp props name prop
                        else throwTypeErr regs ["attempting to add property to non-dynamic object"]
                    end
                fun catchAll _ =
                    (* FIXME: need to use builtin Name.es object here, when that file exists. *)
                    (trace ["running meta::set(\"", 
                            (Ustring.toAscii (#id name)), 
                            "\", ", Mach.approx v,
                            ") catchall on obj #", 
                            Int.toString (getObjId obj)];
                     (evalCallMethodByRef (withThis regs obj) 
                                          (obj, Name.meta_set) 
                                          [newString regs (#id name), v]; 
                      ()))
            in
                if doVirtual
                then
                    case Mach.findProp props Name.meta_set of
                        SOME { state = Mach.MethodProp _, ... } => catchAll ()
                      | SOME { state = Mach.NativeFunctionProp _, ... } => catchAll ()
                      | _ => newProp ()
                else
                    newProp ()
            end
    end


and setValue (regs:Mach.REGS)
             (base:Mach.OBJ)
             (name:Ast.NAME)
             (v:Mach.VAL)
    : unit =
    setValueOrVirtual regs base name v true

(* A "defValue" call occurs when assigning a property definition's
 * initial value, as specified by the user. All other assignments
 * to a property go through "setValue". *)

and defValue (regs:Mach.REGS)
             (base:Mach.OBJ)
             (name:Ast.NAME)
             (v:Mach.VAL)
    : unit =
    case base of
        Mach.Obj { props, ... } =>
        if not (Mach.hasProp props name)
        then error regs ["defValue on missing property: ", LogErr.name name]
        else
            (*
             * defProp is similar to setProp, but follows different rules:
             *
             *   - No adding props, only overwriting allocated ones
             *   - Permitted to write to uninitialized props
             *   - Permitted to write to read-only (const) props
             *)
            let
                val existingProp = Mach.getProp props name
                val ty = (#ty existingProp)
                val newProp = { state = Mach.ValProp (checkAndConvert regs v ty),
                                ty = (#ty existingProp),
                                attrs = (#attrs existingProp) }
                fun writeProp _ =
                    (Mach.delProp props name;
                     Mach.addProp props name newProp)
            in
                case (#state existingProp) of
                    Mach.TypeVarProp =>
                    error regs ["defValue on type variable property: ",
                                LogErr.name name]

                  | Mach.TypeProp =>
                    error regs ["defValue on type property: ",
                                LogErr.name name]

                  | Mach.NamespaceProp _ =>
                    error regs ["defValue on namespace property: ",
                                LogErr.name name]

                  | Mach.NativeFunctionProp _ =>
                    error regs ["defValue on native function property: ",
                                LogErr.name name]

                  | Mach.MethodProp _ =>
                    error regs ["defValue on method property: ",
                                LogErr.name name]

                  | Mach.ValListProp _ =>
                    error regs ["defValue on value-list property: ",
                                LogErr.name name]

                  | Mach.VirtualValProp { setter = SOME s, ... } =>
                    (invokeFuncClosure (withThis regs base) s [v]; ())

                  | Mach.VirtualValProp { setter = NONE, ... } =>
                    error regs ["defValue on virtual property w/o setter: ",
                                LogErr.name name]

                  | Mach.UninitProp => writeProp ()
                  | Mach.ValProp _ => writeProp ()
            end

and instantiateGlobalClass (regs:Mach.REGS)
                           (n:Ast.NAME)
                           (args:Mach.VAL list)
    : Mach.VAL =
    let
        val _ = trace ["instantiating global class ", fmtName n];
        val (cls:Mach.VAL) = getValue regs (#global regs) n
    in
        case cls of
            Mach.Object ob => evalNewExpr regs ob args
          | _ => error regs ["global class name ", LogErr.name n,
                             " did not resolve to object"]
    end

and throwExn (regs:Mach.REGS)
             (name:Ast.NAME) 
             (args:string list)
    : unit =
    if Mach.isBooting regs
    then error regs ["trapped ThrowException during boot: ", 
                     LogErr.name name, "(", LogErr.join ", " args, ")"]
    else raise ThrowException 
                   (instantiateGlobalClass 
                        regs name 
                        [((newString regs) o Ustring.fromString o String.concat) args])

and throwTypeErr (regs:Mach.REGS)
                 (args:string list)
    : unit =
    throwExn regs Name.nons_TypeError args

and throwRefErr (regs:Mach.REGS)
                (args:string list)
    : unit =
    throwExn regs Name.nons_ReferenceError args

and needNamespace (regs:Mach.REGS)
                  (v:Mach.VAL)
    : Ast.NAMESPACE =
    case v of
        Mach.Object (Mach.Obj ob) =>
        (case !(#magic ob) of
             SOME (Mach.Namespace n) => n
           | _ => (throwTypeErr regs ["need namespace"]; dummyNs))
      | _ => (throwTypeErr regs ["need namespace"]; dummyNs)

and needNamespaceOrNull (regs:Mach.REGS)
                        (v:Mach.VAL)
    : Ast.NAMESPACE =
    case v of
        Mach.Object (Mach.Obj ob) =>
        (case !(#magic ob) of
             SOME (Mach.Namespace n) => n
           | _ => (throwTypeErr regs ["need namespace"]; dummyNs))
      | Mach.Null => Name.noNS
      | _ => (throwTypeErr regs ["need namespace"]; dummyNs)

and needNameOrString (regs:Mach.REGS)
                     (v:Mach.VAL)
    : Ast.NAME =
    case v of
        Mach.Object obj => 
        if (typeOfVal regs v) <: (instanceType regs Name.ES4_Name [])
        then
            let
                val nsval = getValue regs obj Name.nons_qualifier
                val idval = getValue regs obj Name.nons_identifier
            in
                Name.make (toUstring regs idval) (needNamespaceOrNull regs nsval)
            end
        else
            Name.nons (toUstring regs v)
      | _ => Name.nons (toUstring regs v)

and needObj (regs:Mach.REGS)
            (v:Mach.VAL)
    : Mach.OBJ =
    case v of
        Mach.Object ob => ob
      | _ => (throwTypeErr regs ["need object"]; dummyObj)

and newObject (regs:Mach.REGS) =
    instantiateGlobalClass 
        regs Name.nons_Object []
    
and newObj (regs:Mach.REGS) =
    needObj regs 
            (instantiateGlobalClass 
                 regs Name.nons_Object [])
    
and newArray (regs:Mach.REGS)
             (vals:Mach.VAL list)
    : Mach.VAL =
    (*
     * NB: Do not reorganize this to call the public Array constructor with the val list
     * directly: that constructor is a bit silly. It interprets a single-argument list as
     * a number, and sets the array to that length. We want to always return an array from
     * this call containing as many values as we were passed, no more no less.
     *)
    let val a = instantiateGlobalClass 
                    regs Name.nons_Array 
                    [newInt regs (Int32.fromInt (List.length vals))]
        fun init a _ [] = ()
          | init a k (x::xs) =
            (setValue regs a (Name.nons (Ustring.fromInt k)) x ;
             init a (k+1) xs)
    in
        init (needObj regs a) 0 vals;
        a
    end

and newRegExp (regs:Mach.REGS)
              (pattern:Ustring.STRING)
              (flags:Ustring.STRING)
    : Mach.VAL =
    instantiateGlobalClass 
        regs Name.nons_RegExp 
        [newString regs pattern, newString regs flags]

and newBuiltin (regs:Mach.REGS)
               (n:Ast.NAME) 
               (m:Mach.MAGIC option)
    : Mach.VAL =
    instantiateGlobalClass 
        regs n 
        [Mach.Object (Mach.setMagic (Mach.newObjNoTag()) m)]

and newDecimal (regs:Mach.REGS) 
               (n:Decimal.DEC)
    : Mach.VAL =
    newBuiltin regs Name.ES4_decimal (SOME (Mach.Decimal n))

and newDouble (regs:Mach.REGS) 
              (n:Real64.real)
    : Mach.VAL =
    if Real64.isNan n
    then newBuiltin regs Name.ES4_double (SOME (Mach.Double n))
    else
	case Mach.findInReal64Cache regs n of
	    SOME obj => Mach.Object obj
	  | NONE => newBuiltin regs Name.ES4_double (SOME (Mach.Double n))

and newInt (regs:Mach.REGS) 
           (n:Int32.int)
    : Mach.VAL =    
    case Mach.findInInt32Cache regs n of
	SOME obj => Mach.Object obj
      | NONE => newBuiltin regs Name.ES4_int (SOME (Mach.Int n))

and newUInt (regs:Mach.REGS) 
            (n:Word32.word)
    : Mach.VAL =
    case Mach.findInWord32Cache regs n of
        SOME obj => Mach.Object obj
      | NONE => newBuiltin regs Name.ES4_uint (SOME (Mach.UInt n))

and newStringWrapper (regs:Mach.REGS)
                    (s:Ustring.STRING)
    : Mach.VAL =
    newBuiltin regs Name.nons_String (SOME (Mach.String s))

and newString (regs:Mach.REGS)
              (s:Ustring.STRING)
    : Mach.VAL =
    case Mach.findInStrCache regs s of
        SOME obj => Mach.Object obj
      | NONE => newBuiltin regs Name.ES4_string (SOME (Mach.String s))

and newByte (regs:Mach.REGS)
            (b:Word8.word)
    : Mach.VAL =
    case Mach.findInWord8Cache regs b of
        SOME obj => Mach.Object obj
      | NONE => newBuiltin regs Name.ES4_byte (SOME (Mach.Byte b))

and newBooleanWrapper (regs:Mach.REGS)
                     (b:bool)
    : Mach.VAL =
    newBuiltin regs Name.nons_Boolean (SOME (Mach.Boolean b))

and newBoolean (regs:Mach.REGS)
               (b:bool)
    : Mach.VAL =
    let
        val cell = 
            if b 
            then Mach.getBooleanTrueSlot regs 
            else Mach.getBooleanFalseSlot regs
    in
        case !cell of
            SOME obj => Mach.Object obj
          | NONE => newBuiltin 
                        regs Name.ES4_boolean 
                        (SOME (Mach.Boolean b))
    end

and newNamespace (regs:Mach.REGS)
                 (n:Ast.NAMESPACE)
    : Mach.VAL =
    case Mach.findInNsCache regs n of
        SOME obj => Mach.Object obj
      | NONE => newBuiltin regs Name.ES4_Namespace (SOME (Mach.Namespace n))

and newName (regs:Mach.REGS)
            (n:Ast.NAME)
    : Mach.VAL =
    case Mach.findInNmCache regs n of
        SOME obj => Mach.Object obj
      | NONE => 
        let 
            val nsmag = Mach.Namespace (#ns n)
            val idmag = Mach.String (#id n)
            val nsval = Mach.Object (Mach.setMagic (Mach.newObjNoTag()) (SOME nsmag))
            val idval = Mach.Object (Mach.setMagic (Mach.newObjNoTag()) (SOME idmag))
            val v = instantiateGlobalClass 
                        regs Name.ES4_Name 
                        [nsval, idval]
        in 
            Mach.Object (Mach.updateNmCache regs (n, needObj regs v))
        end

and newClsClosure (env:Mach.SCOPE)
                  (cls:Ast.CLS)
    : Mach.CLS_CLOSURE =
    { cls = cls, env = env }
    
and newClass (regs:Mach.REGS)
             (e:Mach.SCOPE)
             (cls:Ast.CLS)
    : Mach.VAL =
    let
        val closure = newClsClosure e cls
    in
	newBuiltin regs Name.intrinsic_Class (SOME (Mach.Class closure))
    end

and newIfaceClosure (env:Mach.SCOPE)
                    (iface:Ast.IFACE)
    : Mach.IFACE_CLOSURE =
    { iface = iface, env = env }

and newInterface (regs:Mach.REGS)
                 (e:Mach.SCOPE)
                 (iface:Ast.IFACE)
    : Mach.VAL =
    let
        val closure = newIfaceClosure e iface
    in
	newBuiltin regs Name.intrinsic_Interface (SOME (Mach.Interface closure))
    end

and newFunClosure (e:Mach.SCOPE)
                  (f:Ast.FUNC)
                  (this:Mach.OBJ option)
    : Mach.FUN_CLOSURE =
    { func = f, this = this, env = e }

and newFunctionFromClosure (regs:Mach.REGS)
                           (closure:Mach.FUN_CLOSURE) =
    let
        val { func, ... } = closure
        val Ast.Func { ty, ... } = func
        fun findFuncType e = 
            case e of 
                Ast.LamType { params, body } =>  findFuncType body 
              | Ast.FunctionType fty => fty
              | _ => error regs ["unexpected primary type in function: ", LogErr.ty e]

        val fty = findFuncType (AstQuery.typeExprOf ty)
        val tag = Mach.FunctionTag fty

        val _ = trace ["finding Function.prototype"]
        val funClass = needObj regs (getValue regs (#global regs) 
                                              Name.nons_Function)
        val funProto = getValue regs funClass Name.nons_prototype
        val _ = trace ["building new prototype chained to ",
                       "Function.prototype"]
        val newProtoObj = Mach.setProto (newObj regs) funProto
        val newProto = Mach.Object newProtoObj
        val _ = trace ["built new prototype chained to ",
                       "Function.prototype"]

        val Mach.Obj { magic, ... } = funClass
        val obj = case (!magic) of
                      SOME (Mach.Class funClassClosure) =>
                      constructStandardWithTag 
                          regs
                          funClass funClassClosure [] tag
                    | _ => error regs ["function class lacks class magic"]


    in
        Mach.setMagic obj (SOME (Mach.Function closure));
        setValueOrVirtual regs obj Name.nons_prototype newProto false;
        setValueOrVirtual regs newProtoObj Name.nons_constructor (Mach.Object obj) false;
        Mach.Object obj
    end


and newFunctionFromFunc (regs:Mach.REGS)
                        (scope:Mach.SCOPE)
                        (f:Ast.FUNC)
    : Mach.VAL =
    newFunctionFromClosure regs (newFunClosure scope f NONE)


and newNativeFunction (regs:Mach.REGS)
                      (f:Mach.NATIVE_FUNCTION) =
    let 
	val obj = needObj regs (instantiateGlobalClass 
				    regs 
				    Name.nons_Function 
				    [newString regs Ustring.empty])
    in
	Mach.setMagic obj (SOME (Mach.NativeFunction f));
	Mach.Object obj
    end

(*
 * ES-262-3 9.8 ToString.
 *
 * We do it down here because we have some actual callers who
 * need it inside the implementation of the runtime. Most of the rest
 * is done up in Conversions.es.
 *)

and toUstring (regs:Mach.REGS)
              (v:Mach.VAL)
    : Ustring.STRING =
    case v of
        Mach.Undef => Ustring.undefined_
      | Mach.Null => Ustring.null_
      | Mach.Object obj =>
        let
            val Mach.Obj ob = obj
        in
            case !(#magic ob) of
                SOME magic => Mach.magicToUstring magic
              | NONE => toUstring regs (toPrimitiveWithStringHint regs v)
        end

(*
 * ES-262-3 9.2: The ToBoolean operation
 *)

and toBoolean (v:Mach.VAL) : bool =
    case v of
        Mach.Undef => false
      | Mach.Null => false
      | Mach.Object (Mach.Obj ob) =>
        (case !(#magic ob) of
             SOME (Mach.Boolean b) => b
           | SOME (Mach.Int x) => not (x = (Int32.fromInt 0))
           | SOME (Mach.UInt x) => not (x = (Word32.fromInt 0))
           | SOME (Mach.Double x) => not (Real64.==(x,(Real64.fromInt 0))
                                          orelse
                                          Real64.isNan x)
           | SOME (Mach.Decimal x) => not ((x = Decimal.zero)
                                           orelse
                                           (Decimal.isNaN x))
           | SOME (Mach.String s) => not (Ustring.stringLength s = 0)
           | _ => true)

(*
 * NB: don't move isPrimitive or defaultValue up to Conversions.es: they
 * need to use "v === undefined", but the triple-equals operator is
 * implemented in *terms* of isPrimitive.
 *)

(*
 * ES-262-3 8.6.2.6: The [[DefaultValue]] operation
 *)

and defaultValue (regs:Mach.REGS)
                 (obj:Mach.OBJ)
                 (preferredType:Ustring.STRING)
    : Mach.VAL =
    let
        val (na, nb) = if preferredType = Ustring.String_
                       then (Name.nons_toString, Name.nons_valueOf)
                       else (Name.nons_valueOf, Name.nons_toString)
        val va = if hasValue obj na
                 then evalCallMethodByRef (withThis regs obj) (obj, na) []
                 else Mach.Undef
        val vb = if not (isPrimitive va) andalso hasValue obj nb
                 then evalCallMethodByRef (withThis regs obj) (obj, nb) []
                 else va
    in
        if isPrimitive vb
        then vb
        else (throwTypeErr regs ["defaultValue"]; dummyVal)
    end

and isPrimitive (v:Mach.VAL)
    : bool =
    Mach.isNull v orelse
    Mach.isUndef v orelse
    Mach.isNumeric v orelse
    Mach.isString v orelse
    Mach.isBoolean v

(*
 * ES-262-3 9.1: The ToPrimitive operation
 *)

and toPrimitive (regs:Mach.REGS)
                (v:Mach.VAL) 
                (hint:Ustring.STRING)
    : Mach.VAL =
    if isPrimitive v
    then v
    else defaultValue regs (needObj regs v) hint

and toPrimitiveWithStringHint (regs:Mach.REGS)
                              (v:Mach.VAL)
    : Mach.VAL =
    toPrimitive regs v Ustring.String_

and toPrimitiveWithNumberHint (regs:Mach.REGS)
                              (v:Mach.VAL)
    : Mach.VAL =
    toPrimitive regs v Ustring.Number_

and toPrimitiveWithNoHint (regs:Mach.REGS)
                          (v:Mach.VAL)
    : Mach.VAL =
    toPrimitive regs v Ustring.empty

(*
 * Arithmetic operations.
 *)

and toNumeric (regs:Mach.REGS)
              (v:Mach.VAL)
    : Mach.VAL =
    let
        fun NaN _ = newDouble regs (Real64.posInf / Real64.posInf)
        fun zero _ = newDouble regs (Real64.fromInt 0)
        fun one _ = newDouble regs (Real64.fromInt 1)
    in
        case v of
            Mach.Undef => NaN ()
          | Mach.Null => zero ()
          | Mach.Object (Mach.Obj ob) =>
            (case !(#magic ob) of
                 SOME (Mach.Double _) => v
               | SOME (Mach.Decimal _) => v
               | SOME (Mach.Int _) => v
               | SOME (Mach.UInt _) => v
               | SOME (Mach.Byte _) => v
               | SOME (Mach.Boolean false) => zero ()
               | SOME (Mach.Boolean true) => one ()
               (*
                * FIXME: This is not the correct definition of ToNumber applied to string.
                * See ES-262-3 9.3.1. We need to talk it over.
                *)
               | SOME (Mach.String us) =>
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


and toDecimal (ctxt:Mach.DECIMAL_CONTEXT)
              (v:Mach.VAL)
    : Decimal.DEC =
    case v of
        Mach.Undef => Decimal.NaN
      | Mach.Null => Decimal.zero
      | Mach.Object (Mach.Obj ob) =>
        (case !(#magic ob) of
             SOME (Mach.Double d) =>
             (* NB: Lossy. *)
             (case Decimal.fromString (#precision ctxt) (#mode ctxt) (Real64.toString d) of
                  SOME d' => d'
                | NONE => Decimal.NaN)
           | SOME (Mach.Decimal d) => d
           | SOME (Mach.Int i) => Decimal.fromLargeInt (Int32.toLarge i)
           | SOME (Mach.UInt u) => Decimal.fromLargeInt (Word32.toLargeInt u)
           | SOME (Mach.Byte b) => Decimal.fromLargeInt (Word8.toLargeInt b)
           | SOME (Mach.Boolean false) => Decimal.zero
           | SOME (Mach.Boolean true) => Decimal.one
           (*
            * FIXME: This is not the correct definition either. See toNumeric.
            *)
           | SOME (Mach.String us) =>
             let val s = Ustring.toAscii us
             in
                 case Decimal.fromString (#precision ctxt) (#mode ctxt) s of
                     SOME s' => s'
                   | NONE => Decimal.NaN
             end
           (*
            * FIXME: Possibly wrong here also. See comment in toNumeric.
            *)
           | _ => Decimal.zero)


and toDouble (v:Mach.VAL)
    : Real64.real =
    let
        fun NaN _ = (Real64.posInf / Real64.posInf)
        fun zero _ = (Real64.fromInt 0)
        fun one _ = (Real64.fromInt 1)
    in
        case v of
            Mach.Undef => NaN ()
          | Mach.Null => zero ()
          | Mach.Object (Mach.Obj ob) =>
            (case !(#magic ob) of
                 SOME (Mach.Double d) => d
               | SOME (Mach.Decimal d) =>
                 (* NB: Lossy. *)
                 (case Real64.fromString (Decimal.toString d) of
                      SOME d' => d'
                    | NONE => NaN ())

               | SOME (Mach.Int i) => Real64.fromLargeInt (Int32.toLarge i)
               | SOME (Mach.UInt u) => Real64.fromLargeInt (Word32.toLargeInt u)
               | SOME (Mach.Byte b) => Real64.fromLargeInt (Word8.toLargeInt b)
               | SOME (Mach.Boolean false) => zero ()
               | SOME (Mach.Boolean true) => one ()
               (*
                * FIXME: This is not the correct definition either. See toNumeric.
                *)
               | SOME (Mach.String us) =>
                 let val s = Ustring.toAscii us
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


and isPositiveZero (v:Mach.VAL)
    : bool =
    let
        fun doubleIsPosZero x =
            Real64.class x = IEEEReal.ZERO
            andalso not (Real64.signBit x)
    in
        mathOp v
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsPosZero)
               NONE NONE false
    end


and isNegativeZero (v:Mach.VAL)
    : bool =
    let
        fun doubleIsNegZero x =
            Real64.class x = IEEEReal.ZERO
            andalso Real64.signBit x
    in
        mathOp v
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsNegZero)
               NONE NONE false
    end


and isPositiveInf (v:Mach.VAL)
    : bool =
    let
        fun doubleIsPosInf x =
            Real64.class x = IEEEReal.INF
            andalso not (Real64.signBit x)
    in
        mathOp v
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsPosInf)
               NONE NONE false
    end


and isNegativeInf (v:Mach.VAL)
    : bool =
    let
        fun doubleIsNegInf x =
            Real64.class x = IEEEReal.INF
            andalso Real64.signBit x
    in
        mathOp v
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsNegInf)
               NONE NONE false
    end


and isNaN (v:Mach.VAL)
    : bool =
    mathOp v
           (SOME Decimal.isNaN)
           (SOME Real64.isNan)
           NONE NONE false


and sign (v:Mach.VAL)
    : int =
    let
        (*
         * FIXME: this implemented 'sign' function returns 1, 0, or -1
         * depending on proximity to 0. Some definitions only return 1 or 0,
         * or only return 1 or -1. Don't know which one the ES-262-3 spec means.
         *)

        (* FIXME: should decimal context be used in sign-determination? *)
        fun decimalSign d = case Decimal.compare Decimal.defaultPrecision
                                                 Decimal.defaultRoundingMode
                                                 d Decimal.zero
                             of
                                LESS => ~1
                              | EQUAL => 0
                              | GREATER => 1

        fun uint32Sign u = if u = (Word32.fromInt 0)
                           then 0
                           else 1
    in
        mathOp v
               (SOME decimalSign)
               (SOME Real64.sign)
               (SOME Int32.sign)
               (SOME uint32Sign)
               0
    end


and floor (v:Mach.VAL)
    : LargeInt.int =
    mathOp v
           (SOME Decimal.floorInt)
           (SOME (Real64.toLargeInt IEEEReal.TO_NEGINF))
           (SOME Int32.toLarge)
           (SOME Word32.toLargeInt)
           (LargeInt.fromInt 0)


and signFloorAbs (v:Mach.VAL)
    : LargeInt.int =
    let
        val sign = Int.toLarge (sign v)
        val floor = floor v
    in
        LargeInt.*(sign, (LargeInt.abs floor))
    end


(* ES-262-3 9.5 ToInt32 *)

and toInt32 (regs:Mach.REGS)
            (v:Mach.VAL)
    : Int32.int =
    let
        val v' = toNumeric regs v
    in
        if (isNaN v' orelse
            isPositiveInf v' orelse
            isNegativeInf v' orelse
            isPositiveZero v' orelse
            isNegativeZero v')
        then Int32.fromInt 0
        else
            let
                val l31 = IntInf.pow (2, 31)
                val l32 = IntInf.pow (2, 32)
                val v'' = IntInf.mod (signFloorAbs v', l32)
            in
                Int32.fromLarge (if LargeInt.>= (v'', l31)
                                 then LargeInt.- (v'', l32)
                                 else v'')
            end
    end

and toByte (regs:Mach.REGS)
           (v:Mach.VAL)
    : Word8.word =
    let
        val u32 = toUInt32 regs v
    in
        Word8.fromInt (Word32.toInt (Word32.andb (0wxff, u32)))
    end
    

(* ES-262-3 9.6 ToUInt32 *)

and toUInt32 (regs:Mach.REGS)
             (v:Mach.VAL)
    : Word32.word =
    let
        val v' = toNumeric regs v
    in
        if (isNaN v' orelse
            isPositiveInf v' orelse
            isNegativeInf v' orelse
            isPositiveZero v' orelse
            isNegativeZero v')
        then Word32.fromInt 0
        else
            let
                val l32 = IntInf.pow (2, 32)
            in
                Word32.fromLargeInt (LargeInt.mod (signFloorAbs v', l32))
            end
    end

(* ES-262-3 9.6 ToUInt16 *)

and toUInt16 (regs:Mach.REGS)
             (v:Mach.VAL)
    : Word32.word =
    let
        val v' = toNumeric regs v
    in
        if (isNaN v' orelse
            isPositiveInf v' orelse
            isNegativeInf v' orelse
            isPositiveZero v' orelse
            isNegativeZero v')
        then Word32.fromInt 0
        else
            let
                val l16 = IntInf.pow (2, 16)
            in
                Word32.fromLargeInt (LargeInt.mod (signFloorAbs v', l16))
            end
    end

and getExpectedType (regs:Mach.REGS)
                    (expr:Ast.EXPR)
    : (Ast.TYPE_EXPR * Ast.EXPR) =
    case expr of
        Ast.ExpectedTypeExpr (te, e) => (te, e)
      | _ => (Ast.SpecialType Ast.Any, expr)

and checkCompatible (regs:Mach.REGS)
                    (tyExpr:Ast.TYPE_EXPR)
                    (v:Mach.VAL) =
    if isCompatible regs v tyExpr
    then v
    else error regs ["typecheck failed, val=", Mach.approx v,
                     " type=", LogErr.ty (typeOfVal regs v),
                     " wanted=", LogErr.ty tyExpr]

and evalExpr (regs:Mach.REGS)
             (expr:Ast.EXPR)
    : Mach.VAL =
    case expr of
        Ast.LiteralExpr lit =>
        evalLiteralExpr regs lit

      | Ast.ListExpr es =>
        evalListExpr regs es

      | Ast.LexicalRef { ident, loc } =>
        let
            val _ = LogErr.setLoc loc;
            val (obj, name) = evalRefExpr regs expr true
            val _ = LogErr.setLoc loc;
        in
            getValue regs obj name
        end

      | Ast.ObjectRef { base, ident, loc } =>
        let
            val _ = LogErr.setLoc loc
            val (obj, name) = evalRefExpr regs expr false
            val _ = LogErr.setLoc loc
        in
            getValue regs obj name
        end

      | Ast.LetExpr {defs, body, head} =>
        evalLetExpr regs (valOf head) body

      | Ast.TernaryExpr (aexpr, bexpr, cexpr) =>
        evalCondExpr regs aexpr bexpr cexpr

      | Ast.BinaryExpr (bop, aexpr, bexpr) =>
        evalBinaryOp regs bop aexpr bexpr

      | Ast.UnaryExpr (unop, expr) =>
        evalUnaryOp regs unop expr

      | Ast.TypeExpr ty =>
        evalTypeExpr regs (evalTy regs ty)

      | Ast.ThisExpr =>
        let
            val { this, ... } = regs
        in
            Mach.Object this
        end

      | Ast.SetExpr (aop, pat, expr) =>
        evalSetExpr regs aop pat expr

      | Ast.CallExpr { func, actuals } =>
        let
            fun args _ = map (evalExpr regs) actuals
            val (funcTy, func) = getExpectedType regs func
            val resultTy =
                case funcTy of
                    Ast.FunctionType fty => (#result fty)
                  | _ => Ast.SpecialType Ast.Any
            val result =
                case func of
                    Ast.LexicalRef _ => evalCallMethodByExpr regs func (args ())
                  | Ast.ObjectRef _ => evalCallMethodByExpr regs func (args ())
                  | _ =>
                    let
                        val f = evalExpr regs func
                    in
                        case f of
                            Mach.Object ob => evalCallExpr regs ob (args ())
                          | _ => (throwTypeErr regs ["not a function"]; dummyVal)
                    end
        in
            checkCompatible regs resultTy result
        end

      | Ast.NewExpr { obj, actuals } =>
        let
            fun args _ = map (evalExpr regs) actuals
            val rhs = evalExpr regs obj
        in
            case rhs of
                Mach.Object ob => evalNewExpr regs ob (args())
              | _ => (throwTypeErr regs ["not a constructor"]; dummyVal)
        end

      | Ast.GetTemp n =>
        Mach.getTemp (getScopeTemps (#scope regs)) n

      | Ast.InitExpr (target,temps,inits) =>
        let
            val tempRegs = evalHead regs temps
        in
            evalScopeInits tempRegs target inits;
            Mach.Undef
        end

      | Ast.BinaryTypeExpr (typeOp, expr, tyExpr) =>
        evalBinaryTypeOp regs typeOp expr tyExpr

      | Ast.ExpectedTypeExpr (te, e) =>
        checkCompatible regs te (evalExpr regs e)

      | Ast.GetParam n =>
        LogErr.unimplError ["unhandled GetParam expression"]

      | Ast.SliceExpr _ =>
        LogErr.unimplError ["unhandled Slice expression"]

      | Ast.ApplyTypeExpr { expr, actuals } =>
        evalApplyTypeExpr regs expr (map (evalTy regs) actuals)

      | _ => LogErr.unimplError ["unhandled expression type"]


and applyTypes (regs:Mach.REGS)
               (base:Ast.TY)
               (args:Ast.TYPE_EXPR list)
    : Ast.TYPE_EXPR = 
    let
        val fullTy = 
            case args of 
                [] => base
              | _ => 
                let
                    fun f baseTyExpr = Ast.AppType { base = baseTyExpr,
                                                     args = args }
                in
                    AstQuery.inject f base
                end
    in
        evalTy regs fullTy
    end    


and instanceType (regs:Mach.REGS)
                 (name:Ast.NAME)
                 (args:Ast.TYPE_EXPR list)
    : Ast.TYPE_EXPR = 
    let
        val instanceTy = Type.instanceTy (#prog regs) name
    in
        applyTypes regs instanceTy args
    end

and traceScope (s:Mach.SCOPE)
    : unit =
    if !doTrace
    then 
        let
            val Mach.Scope { object, parent, ... } = s
            val Mach.Obj { ident, props, ... } = getScopeObj s
            val names = map (fn (n,_) => n) (NameMap.listItemsi (!props))
        in    
            trace ["scope: ", Int.toString ident, " = ",
                   (if length names > 5
                    then " ...lots... "
                    else (LogErr.join ", " (map LogErr.name names)))];
            case parent of 
                NONE => ()
              | SOME p => traceScope p
        end
    else
        ()

and bindTypes (regs:Mach.REGS)
              (typeParams:Ast.IDENT list)
              (typeArgs:Ast.TYPE_EXPR list)
              (env:Mach.SCOPE)
    : Mach.SCOPE = 
    let 
        val _ = 
            if not (length typeArgs = length typeParams)
            then error regs ["argument length mismatch when binding type args in env"]
            else ()
        val (scopeObj:Mach.OBJ) = Mach.newObjNoTag ()
        val _ = trace ["binding ", Int.toString (length typeArgs), 
                       " type args to scope #", Int.toString (getObjId scopeObj)]
        val env = extendScope env scopeObj Mach.TypeArgScope
        val paramFixtureNames = map (fn id => Ast.PropName (Name.nons id)) typeParams
        val argFixtures = map (fn te => Ast.TypeFixture (makeTy te)) typeArgs
        val typeRib = ListPair.zip (paramFixtureNames, argFixtures)
        val _ = allocObjRib regs scopeObj NONE typeRib
    in
        env
    end


and applyTypesToClass (regs:Mach.REGS)
                      (classVal:Mach.VAL)
                      (typeArgs:Ast.TYPE_EXPR list)
    : Mach.VAL = 
    let
        val clsClosure = Mach.needClass classVal
        val { cls, env } = clsClosure
        val Ast.Cls { instanceType, ... } = cls
    in
        if Type.isGroundTy instanceType
        then classVal
        else 
            let
                fun applyArgs t = makeTy (applyTypes regs t typeArgs)
                val Ast.Cls c = cls
                val newCls = Ast.Cls { name = (#name c), 
                                       typeParams = (#typeParams c),
                                       nonnullable = (#nonnullable c),
                                       dynamic = (#dynamic c),
                                       (* FIXME: apply to base types when logic for this is present in defn. *)
                                       extends = (#extends c), (* Option.map applyArgs (#extends c), *)
                                       implements = (#implements c), (* map applyArgs (#implements c), *)
                                       classRib = (#classRib c),
                                       instanceRib = (#instanceRib c),
                                       instanceInits = (#instanceInits c),
                                       constructor = (#constructor c),
                                       classType = (#classType c),
                                       instanceType = applyArgs (#instanceType c) }
		val newEnv = bindTypes regs (#typeParams c) typeArgs env
            in
		newClass regs newEnv newCls
            end
    end


and applyTypesToInterface (regs:Mach.REGS)
                          (interfaceVal:Mach.VAL)
                          (typeArgs:Ast.TYPE_EXPR list)
    : Mach.VAL = 
    let
        val ifaceClosure = Mach.needInterface interfaceVal
        val { iface, env } = ifaceClosure
        val Ast.Iface { instanceType, ... } = iface
    in
        if Type.isGroundTy instanceType 
        then interfaceVal
        else 
            let
                fun applyArgs t = makeTy (applyTypes regs t typeArgs)
                val Ast.Iface i = iface
                val newIface = Ast.Iface { name = (#name i), 
                                           typeParams = (#typeParams i),
                                           nonnullable = (#nonnullable i),
                                           extends = map applyArgs (#extends i),
                                           instanceRib = (#instanceRib i),
                                           instanceType = applyArgs (#instanceType i) }
		val newEnv = bindTypes regs (#typeParams i) typeArgs env
            in
		newInterface regs newEnv newIface
            end
    end


and applyTypesToFunction (regs:Mach.REGS)
                         (functionVal:Mach.VAL)
                         (typeArgs:Ast.TYPE_EXPR list)
    : Mach.VAL = 
    let
        val funClosure = Mach.needFunction functionVal
        val { func, this, env } = funClosure
        val Ast.Func { ty, ... } = func
    in
        if Type.isGroundTy ty
        then functionVal
        else 
            let
                fun applyArgs t = makeTy (applyTypes regs t typeArgs)
                val Ast.Func f = func
                val Ast.FunctionSignature { typeParams, ... } = (#fsig f)
                val newFunc = Ast.Func { name = (#name f), 
                                         fsig = (#fsig f),
                                         native = (#native f),
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


and instanceTypeRepresentative (regs:Mach.REGS)
                               (ity:Ast.INSTANCE_TYPE)
                               (applier:Mach.REGS 
                                        -> Mach.VAL 
                                        -> Ast.TYPE_EXPR list 
                                        -> Mach.VAL)
    : Mach.OBJ = 
    let
        val { name, typeArgs, ... } = ity
        val v = getValue regs (#global regs) name
        val v = case typeArgs of 
                    [] => v
                  | _ => applier regs v typeArgs
    in
        needObj regs v
    end


and instanceClass (regs:Mach.REGS)
                  (ity:Ast.INSTANCE_TYPE)
    : Mach.OBJ = 
    instanceTypeRepresentative regs ity applyTypesToClass


and instanceInterface (regs:Mach.REGS)
                      (ity:Ast.INSTANCE_TYPE)
    : Mach.OBJ = 
    instanceTypeRepresentative regs ity applyTypesToClass


and evalApplyTypeExpr (regs:Mach.REGS)
                      (expr:Ast.EXPR)
                      (args:Ast.TYPE_EXPR list)
    : Mach.VAL =
    let
        val v = evalExpr regs expr
    in
        if Mach.isFunction v
        then applyTypesToFunction regs v args
        else 
            if Mach.isClass v
            then applyTypesToClass regs v args
            else
                if Mach.isInterface v
                then applyTypesToInterface regs v args
                else 
                    (throwTypeErr regs ["applying types to unknown base value: ",
                                        Mach.approx v]; dummyVal)
    end


and evalLiteralArrayExpr (regs:Mach.REGS)
                         (exprs:Ast.EXPR list)
                         (ty:Ast.TY option)
    : Mach.VAL =
    let
        val vals = map (evalExpr regs) exprs
        val tyExprs = case Option.map (evalTy regs) ty of
                          NONE => [Ast.SpecialType Ast.Any]
                        | SOME (Ast.ArrayType tys) => tys
                        (* FIXME: hoist this to parsing or defn; don't use
                         * a full TYPE_EXPR in LiteralArray. *)
                        | SOME _ => error regs ["non-array type on array literal"]
        val tag = Mach.ArrayTag tyExprs
        val arrayClass = needObj regs (getValue regs (#global regs) Name.nons_Array)
        val Mach.Obj { magic, ... } = arrayClass
        val obj = case (!magic) of
                      SOME (Mach.Class arrayClassClosure) =>
                      constructStandardWithTag regs arrayClass arrayClassClosure [] tag
                    | _ => error regs ["Error in constructing array literal"]
        val (Mach.Obj {props, ...}) = obj
        fun putVal n [] = n
          | putVal n (v::vs) =
            let
                val name = Name.nons (Ustring.fromInt n)
                (* FIXME: this is probably incorrect wrt. Array typing rules. *)
                val ty = makeTy (if n < (length tyExprs)
                                 then List.nth (tyExprs, n)
                                 else (if (length tyExprs) > 0
                                       then List.last tyExprs
                                       else Ast.SpecialType Ast.Any))
                val prop = { ty = ty,
                             state = Mach.ValProp v,
                             attrs = { dontDelete = false,
                                       dontEnum = false,
                                       readOnly = false,
                                       isFixed = false } }
            in
                Mach.addProp props name prop;
                putVal (n+1) vs
            end
        val numProps = putVal 0 vals
    in
        setValue regs obj Name.nons_length (newUInt regs (Word32.fromInt numProps)) ;
        Mach.Object obj
    end


and evalLiteralObjectExpr (regs:Mach.REGS)
                          (fields:Ast.FIELD list)
                          (ty:Ast.TY option)
    : Mach.VAL =
    let
        fun searchFieldTypes n [] = Ast.SpecialType Ast.Any
          | searchFieldTypes n ({name,ty}::ts) =
            if n = name
            then ty
            else searchFieldTypes n ts
        val tyExprs = case Option.map (evalTy regs) ty of
                          NONE => []
                        | SOME (Ast.ObjectType tys) => tys
                        (* FIXME: hoist this to parsing or defn; don't use
                         * a full TYPE_EXPR in LiteralObject. *)
                        | SOME _ => error regs ["non-object type on object literal"]
        val tag = Mach.ObjectTag tyExprs
        val objectClass = needObj regs (getValue regs (#global regs) Name.nons_Object)
        val Mach.Obj { magic, ... } = objectClass
        val obj = case (!magic) of
                      SOME (Mach.Class objectClassClosure) =>
                      constructStandardWithTag regs objectClass objectClassClosure [] tag
                    | _ => error regs ["Error in constructing array literal"]
        val (Mach.Obj {props, ...}) = obj
        fun processField {kind, name, init} =
            let
                val const = case kind of
                                Ast.Const => true
                              | Ast.LetConst => true
                              | _ => false
                val n = case evalIdentExpr regs name of
                            Name n => n
                          | Multiname n => Name.nons (#id n)
                val v = evalExpr regs init
                val ty = makeTy (searchFieldTypes (#id n) tyExprs)
                val prop = { ty = ty,
                             (* FIXME: handle virtuals *)
                             state = Mach.ValProp v,
                             attrs = { dontDelete = false,
                                       dontEnum = false,
                                       readOnly = const,
                                       isFixed = false } }
            in
                Mach.addProp props n prop
            end
    in
        List.app processField fields;
        Mach.Object obj
    end


and evalLiteralRegExp (regs:Mach.REGS)
                      (re:Ustring.STRING)
    : Mach.VAL =
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


and evalLiteralExpr (regs:Mach.REGS)
                    (lit:Ast.LITERAL)
    : Mach.VAL =
    case lit of
        Ast.LiteralNull => Mach.Null
      | Ast.LiteralUndefined => Mach.Undef
      | Ast.LiteralDouble r => newDouble regs r
      | Ast.LiteralDecimal d => newDecimal regs d
      | Ast.LiteralInt i => newInt regs i
      | Ast.LiteralUInt u => newUInt regs u
      | Ast.LiteralBoolean b => newBoolean regs b
      | Ast.LiteralString s => newString regs s
      | Ast.LiteralArray {exprs, ty} => evalLiteralArrayExpr regs exprs ty
      | Ast.LiteralObject {expr, ty} => evalLiteralObjectExpr regs expr ty
      | Ast.LiteralNamespace n => newNamespace regs n                
      | Ast.LiteralFunction f => newFunctionFromFunc regs (#scope regs) f

      | Ast.LiteralXML _ => LogErr.unimplError ["unhandled literal XML"]
      | Ast.LiteralRegExp re => evalLiteralRegExp regs (#str re)


and evalListExpr (regs:Mach.REGS)
                 (es:Ast.EXPR list)
    : Mach.VAL =
    case es of
        [] => Mach.Undef
      | [e] => evalExpr regs e
      | (e::ez) => ((evalExpr regs e); (evalListExpr regs ez))


and constructObjectViaFunction (regs:Mach.REGS)
                               (ctorObj:Mach.OBJ)
                               (ctor:Mach.FUN_CLOSURE)
                               (args:Mach.VAL list)
    : Mach.VAL =
    case ctorObj of
        Mach.Obj { props, ... } =>
        let
            (* FIXME: the default prototype should be the initial Object prototype,
             * as per ES-262-3 13.2.2, not the current Object prototype. *)
            val (proto:Mach.VAL) =
                if Mach.hasProp props Name.nons_prototype
                then getValue regs ctorObj Name.nons_prototype
                else
                    let
                        val globalObjectObj = 
                            needObj regs (getValue regs 
                                                   (#global regs) 
                                                   Name.nons_Object)
                    in
                        getValue regs globalObjectObj Name.nons_prototype
                    end
            val (newObj:Mach.OBJ) = Mach.setProto (newObj regs) proto
        in
            case invokeFuncClosure (withThis regs newObj) ctor args of
                Mach.Object ob => Mach.Object ob
              | _ => Mach.Object newObj
        end


and evalNewExpr (regs:Mach.REGS)
                (obj:Mach.OBJ)
                (args:Mach.VAL list)
    : Mach.VAL =
    case obj of
        Mach.Obj { magic, ... } =>
        case (!magic) of
            SOME (Mach.Class c) => constructClassInstance regs obj c args
          | SOME (Mach.Function f) => constructObjectViaFunction regs obj f args
          | _ => (throwTypeErr regs ["operator 'new' applied to unknown object"]; dummyVal)


and callGlobal (regs:Mach.REGS)
               (n:Ast.NAME)
               (args:Mach.VAL list)
    : Mach.VAL =
    let
        val _ = trace ["evaluator calling up to global function ", fmtName n]
        val global = (#global regs)
    in
        evalCallMethodByRef regs (global, n) args
    end


and evalCallMethodByExpr (regs:Mach.REGS)
                         (func:Ast.EXPR)
                         (args:Mach.VAL list)
    : Mach.VAL =
    let
        (*
         * If we have a method or native function *property*, we can just
         * call it directly without manufacturing a temporary Function
         * wrapper object.
         *)
        val _ = trace [">>> evalCallMethodByExpr"]
        val (thisObj, r) = evalRefExprFull regs func true
        val _ = trace ["resolved thisObj=#", (Int.toString (getObjId thisObj)), " for call"]
        val result = evalCallMethodByRef (withThis regs thisObj) r args
    in
        trace ["<<< evalCallMethodByExpr"];
        result
    end


and evalCallMethodByRef (regs:Mach.REGS)
                        (r:REF)
                        (args:Mach.VAL list)
    : Mach.VAL =
    let
        val (obj, name) = r
        val _ = trace [">>> evalCallMethodByRef ", fmtName name]
        val Mach.Obj { props, ... } = obj
        val res = 
            case (#state (Mach.getProp props name)) of
                Mach.NativeFunctionProp { func, ...} => func regs args
              | Mach.MethodProp f => 
                invokeFuncClosure (withThis regs obj) f args
              | _ =>
                (trace ["evalCallMethodByRef: non-method property ",
                        "referenced, getting and calling"];
                 evalCallExpr regs (needObj regs (getValue regs obj name)) args)
        val _ = trace ["<<< evalCallMethodByRef ", fmtName name]
    in
        res
    end

and evalCallExpr (regs:Mach.REGS)
                 (fobj:Mach.OBJ)
                 (args:Mach.VAL list)
    : Mach.VAL =
    case fobj of
        Mach.Obj { magic, ... } =>
        case !magic of
            SOME (Mach.NativeFunction { func, ... }) =>
            (trace ["evalCallExpr: entering native function"];
             func regs args)
          | SOME (Mach.Function f) =>
            (trace ["evalCallExpr: entering standard function"];
             invokeFuncClosure regs f args)
          | _ =>
            if hasOwnValue fobj Name.meta_invoke
            then
                (trace ["evalCallExpr: redirecting through meta::invoke"];
                 evalCallMethodByRef regs (fobj, Name.meta_invoke) args)
            else error regs ["evalCallExpr: calling non-callable object"]


and evalSetExpr (regs:Mach.REGS)
                (aop:Ast.ASSIGNOP)
                (lhs:Ast.EXPR)
                (rhs:Ast.EXPR)
    : Mach.VAL =
    let
        val _ = trace ["evalSetExpr"]
        val (lhsType, lhs) = getExpectedType regs lhs
        val (obj, name) = evalRefExpr regs lhs false
        val v =
            let
                fun modifyWith bop =
                    let val v = evalExpr regs rhs
                    in performBinop 
                           regs bop 
                           (checkCompatible 
                                regs lhsType 
                                (getValue regs obj name)) v
                    end
            in
                case aop of
                    Ast.Assign => evalExpr regs rhs
                  | Ast.AssignPlus => modifyWith Ast.Plus
                  | Ast.AssignMinus => modifyWith Ast.Minus
                  | Ast.AssignTimes => modifyWith Ast.Times
                  | Ast.AssignDivide => modifyWith Ast.Divide
                  | Ast.AssignRemainder => modifyWith Ast.Remainder
                  | Ast.AssignLeftShift => modifyWith Ast.LeftShift
                  | Ast.AssignRightShift => modifyWith Ast.RightShift
                  | Ast.AssignRightShiftUnsigned => modifyWith Ast.RightShiftUnsigned
                  | Ast.AssignBitwiseAnd => modifyWith Ast.BitwiseAnd
                  | Ast.AssignBitwiseOr => modifyWith Ast.BitwiseOr
                  | Ast.AssignBitwiseXor => modifyWith Ast.BitwiseXor
                  | Ast.AssignLogicalAnd =>
                    let
                        val a = checkCompatible regs lhsType (getValue regs obj name)
                    in
                        if toBoolean a
                        then evalExpr regs rhs
                        else a
                    end
                  | Ast.AssignLogicalOr =>
                    let
                        val a = checkCompatible regs lhsType (getValue regs obj name)
                    in
                        if toBoolean a
                        then a
                        else evalExpr regs rhs
                    end
            end
    in
        trace ["setExpr assignment to slot ", fmtName name];
        setValue regs obj name v;
        v
    end

and decimalCtxt (regs:Mach.REGS)
    : Mach.DECIMAL_CONTEXT = 
    let
        val Mach.Scope { decimal, ... } = (#scope regs)
    in
        decimal
    end

and evalUnaryOp (regs:Mach.REGS)
                (unop:Ast.UNOP)
                (expr:Ast.EXPR)
    : Mach.VAL =
    let
        fun crement decimalOp doubleOp intOp uintOp byteOp isPre =
            let
                val (exprType, expr) = getExpectedType regs expr
                val (obj, name) = evalRefExpr regs expr false
                val v = checkCompatible regs exprType (getValue regs obj name)
                val (n, n') = 
                    case Mach.needMagic (toNumeric regs v) of
                        Mach.Decimal vd => 
                        let
                            val { precision, mode } = decimalCtxt regs
                            val one = valOf (Decimal.fromStringDefault "1")
                        in
                            (newDecimal regs vd,
                             newDecimal regs (decimalOp precision mode vd one))
                        end

                      | Mach.Double vd =>  
                        let
                            val one = Real64.fromInt 1
                        in
                            (newDouble regs vd, newDouble regs (doubleOp (vd, one)))
                        end

                      | Mach.Int vi => 
                        let
                            val one = Int32.fromInt 1
                        in
                            (newInt regs vi, newInt regs (intOp (vi, one)))
                        end

                      | Mach.UInt vu =>
                        let
                            val one = Word32.fromInt 1
                        in
                            (newUInt regs vu, newUInt regs (uintOp (vu, one)))
                        end

                      | Mach.Byte vb =>
                        let
                            val one = Word8.fromInt 1
                        in
                            (newByte regs vb, newByte regs (byteOp (vb, one)))
                        end

                      | _ => error regs ["non-numeric operand to crement operation"]
            in
                setValue regs obj name n';
                if isPre then n' else n
            end
    in
        case unop of
            Ast.Delete =>
            let
                val (_, expr) = getExpectedType regs expr
                val (Mach.Obj {props, ...}, name) = evalRefExpr regs expr false
            in
                if (#dontDelete (#attrs (Mach.getProp props name)))
                then newBoolean regs false
                else (Mach.delProp props name; newBoolean regs true)
            end

          | Ast.PreIncrement => crement (Decimal.add)
                                        (Real64.+)
                                        (Int32.+)
                                        (Word32.+)
                                        (Word8.+)
                                        true

          | Ast.PreDecrement => crement (Decimal.subtract)
                                        (Real64.-)
                                        (Int32.-)
                                        (Word32.-)
                                        (Word8.-)
                                        true

          | Ast.PostIncrement => crement (Decimal.add)
                                         (Real64.+)
                                         (Int32.+)
                                         (Word32.+)
                                         (Word8.+)
                                         false

          | Ast.PostDecrement => crement (Decimal.subtract)
                                         (Real64.-)
                                         (Int32.-)
                                         (Word32.-)
                                         (Word8.-)
                                         false

          | Ast.BitwiseNot =>
            newInt regs (Int32.fromLarge
                             (Word32.toLargeIntX
                                  (Word32.notb
                                       (toUInt32 regs
                                            (evalExpr regs expr)))))

          | Ast.LogicalNot =>
            newBoolean regs (not (toBoolean (evalExpr regs expr)))

          | Ast.UnaryPlus =>
            toNumeric regs (evalExpr regs expr)

          | Ast.UnaryMinus =>
	    performBinop regs Ast.Minus (newInt regs 0) (evalExpr regs expr)

          | Ast.Void => Mach.Undef

          | Ast.Type =>
            (*
             * FIXME: not clear what this operator does; I thought it just
             * affected parse context.
             *)
            evalExpr regs expr

          | Ast.Typeof =>
            (*
             * ES-262-3 1.4.3 backward-compatibility operation.
             *)
            let
                fun typeNameOfVal (v:Mach.VAL) =
                    case v of
                        Mach.Null => Ustring.null_
                      | Mach.Undef => Ustring.undefined_
                      | Mach.Object (Mach.Obj ob) =>
                        let
                            val n = Mach.nominalBaseOfTag (#tag ob)
                        in
                            if n = Name.ES4_int orelse
                               n = Name.ES4_uint orelse
                               n = Name.ES4_double orelse
                               n = Name.ES4_decimal
                            then Ustring.number_
                            else
                                (if n = Name.ES4_boolean
                                 then Ustring.boolean_
                                 else
                                     (if n = Name.nons_Function
                                      then Ustring.function_
                                      else
                                          (if n = Name.ES4_string
                                           then Ustring.string_
                                           else Ustring.object_)))
                        end
                val (_, expr) = getExpectedType regs expr
            in
                newString regs
                    (case expr of
                         Ast.LexicalRef { ident, loc } =>
                         let
                             val _ = LogErr.setLoc loc
                             val nomn = evalIdentExpr regs ident
                         in
                             case resolveOnScopeChain (#scope regs) nomn of
                                 NONE => Ustring.undefined_
                               | SOME (obj, name) => typeNameOfVal (getValue regs obj name)
                         end
                       | _ => typeNameOfVal (evalExpr regs expr))
            end
    end

and evalTypeExpr (regs:Mach.REGS)
                 (te:Ast.TYPE_EXPR)
    : Mach.VAL =
    case te of 
        Ast.SpecialType st => Mach.Null (* FIXME *)
      | Ast.UnionType ut => Mach.Null (* FIXME *)
      | Ast.ArrayType at => Mach.Null (* FIXME *)
      | Ast.TypeName tn => evalExpr regs (Ast.LexicalRef { ident=tn, loc=NONE })
      | Ast.FunctionType ft => Mach.Null (* FIXME *)
      | Ast.ObjectType ot => Mach.Null (* FIXME *)
      | Ast.NullableType { expr, nullable } => Mach.Null (* FIXME *)
      | Ast.InstanceType { ty, ... } => Mach.Null (* FIXME *)


and performBinop (regs:Mach.REGS)
                 (bop:Ast.BINOP)
                 (va:Mach.VAL)
                 (vb:Mach.VAL)
    : Mach.VAL =

    let
        val ctxt = decimalCtxt regs
        val { precision, mode } = ctxt

        fun simpleNumeric decimalOp doubleOp a b =
            (* 
             * First we perform promotions until we get a common representation.
             * The promotion lattice is a little odd:
             * 
             *    byte -> uint
             *    uint -> double
             *    int -> double
             *    double -> decimal
             * 
             * Then we perform the operation in either double or decimal and 
             * see if the result is integral. If it is -- and the common input 
             * representation was integral -- then we attempt to store the result 
             * in the common integral form but will promote if necessary to 
             * preserve precision. 
             * 
             *)
            let                     
                val commonNumType = promoteToCommon regs 
                                                    (numTypeOf regs a) 
                                                    (numTypeOf regs b)
            in
                case commonNumType of 
                    DecimalNum => 
                    newDecimal regs (decimalOp precision mode
                                               (toDecimal ctxt a) 
                                               (toDecimal ctxt b))

                  | _ => 
                    let 
                        val d = doubleOp ((toDouble a), (toDouble b))
                        val isIntegral = 
                            if Real64.isFinite d
                            then Real64.==(Real64.realTrunc d, d)
                            else false
                    in
                        if isIntegral
                        then 
                            let 
                                val lg = Real64.toLargeInt IEEEReal.TO_NEAREST d
                            in
                                case commonNumType of
                                    ByteNum => if Mach.fitsInByte lg
                                               then newByte regs (Word8.fromLargeInt lg)
                                               else 
                                                   if Mach.fitsInUInt lg
                                                   then newUInt regs (Word32.fromLargeInt lg)
                                                   else newDouble regs d
                                  | UIntNum => if Mach.fitsInUInt lg
                                               then newUInt regs (Word32.fromLargeInt lg)
                                               else newDouble regs d
                                  | IntNum =>  if Mach.fitsInInt lg
                                               then newInt regs (Int32.fromLarge lg)
                                               else newDouble regs d
                                  | _ => newDouble regs d
                            end
                        else 
                            newDouble regs d
                    end
            end

        fun reorder (ord:IEEEReal.real_order) 
            : order =
            case ord of
                IEEEReal.EQUAL => EQUAL
              | IEEEReal.LESS => LESS
              | IEEEReal.GREATER => GREATER

        fun dispatchComparison cmp =            
            let
                val va = toPrimitiveWithNumberHint regs va
                val vb = toPrimitiveWithNumberHint regs vb
            in
                (*
                 * ES-262-3 11.8.5 Abstract Relational Comparison Algorithm
                 *)
                if Mach.isString va andalso Mach.isString vb
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
                                  ByteNum => 
                                  cmp (Word8.compare 
                                           ((toByte regs va),
                                            (toByte regs vb)))

                                | IntNum => 
                                  cmp (Int32.compare 
                                           ((toInt32 regs va),
                                            (toInt32 regs vb)))
                                | UIntNum => 
                                  cmp (Word32.compare 
                                           ((toUInt32 regs va),
                                            (toUInt32 regs vb)))
                                  
                                | DoubleNum => 
                                  cmp (reorder
                                           (Real64.compareReal 
                                                ((toDouble va),
                                                 (toDouble vb))))
                                  
                                | DecimalNum => 
                                  cmp (Decimal.compare 
                                           precision mode 
                                           (toDecimal ctxt va) 
                                           (toDecimal ctxt vb)))
                    end
            end

        fun masku5 (x:Word32.word) : Word.word =
            Word.fromInt (Word32.toInt (Word32.andb (x, (valOf (Word32.fromString "0x1F")))))

        fun i2u (x:Int32.int) : Word32.word =
            Word32.fromLargeInt (Int32.toLarge x)

        fun u2i (x:Word32.word) : Int32.int =
            Int32.fromLarge (Word32.toLargeInt x)

        fun bitwiseWordOp f =
            newUInt regs (f ((toUInt32 regs va),
                             (toUInt32 regs vb)))

        fun pickRepByA (x:Word32.word) =
            if Mach.isUInt va
            then newUInt regs x
            else newInt regs (u2i x)

        (*
         * ES-262-3 11.9.6 Strict Equality Comparison Algorithm
         *)

        fun tripleEquals _ =
            if Mach.isSameType va vb
            then
                if Mach.isUndef va orelse 
                   Mach.isNull va
                then newBoolean regs true
                else
                    if Mach.isNumeric va
                    then if isNaN va orelse 
                            isNaN vb
                         then newBoolean regs false
                         else dispatchComparison (fn x => x = EQUAL)
                    else
                        if Mach.isString va
                        then case Ustring.compare (toUstring regs va) (toUstring regs vb) of
                                 EQUAL => newBoolean regs true
                               | _ => newBoolean regs false
                        else
                            if Mach.isBoolean va
                            then newBoolean regs (toBoolean va = toBoolean vb)
                            else newBoolean regs ((getObjId (needObj regs va)) = (getObjId (needObj regs vb)))
            else
                newBoolean regs false

        (*
         * ES-262-3 11.9.3 Abstract Equality Comparison Algorithm
         *)

        fun doubleEquals' _ =
            if Mach.isSameType va vb
            then tripleEquals ()
            else
                if (Mach.isNull va andalso 
                    Mach.isUndef vb)
                   orelse 
                   (Mach.isUndef va andalso 
                    Mach.isNull vb)
                then newBoolean regs true
                else
                    if (Mach.isNumeric va andalso 
                        Mach.isString vb) 
                       orelse 
                       (Mach.isString va andalso 
                        Mach.isNumeric vb)
                    then
                        performBinop 
                            regs Ast.Equals 
                            (toNumeric regs va) 
                            (toNumeric regs vb)
                    else
                        if Mach.isBoolean va
                        then performBinop 
                                 regs Ast.Equals
                                 (toNumeric regs va) vb
                        else
                            if Mach.isBoolean vb
                            then performBinop 
                                     regs Ast.Equals va 
                                     (toNumeric regs vb)
                            else
                                if (Mach.isString va orelse 
                                    Mach.isNumeric va) andalso 
                                   Mach.isObject vb
                                then performBinop 
                                         regs Ast.Equals va 
                                         (toPrimitiveWithNoHint regs vb)
                                else
                                    if Mach.isObject va andalso 
                                       (Mach.isString vb orelse 
                                        Mach.isNumeric vb)
                                    then performBinop 
                                             regs Ast.Equals
                                             (toPrimitiveWithNoHint regs va) vb
                                    else newBoolean regs false

        val binOpName =
            case bop of
                Ast.Plus => "+"
              | Ast.Minus => "-"
              | Ast.Times => "*"
              | Ast.Divide => "/"
              | Ast.Remainder => "%"
              | Ast.LeftShift => "<<"
              | Ast.RightShift => ">>"
              | Ast.RightShiftUnsigned => ">>>"
              | Ast.BitwiseAnd => "&"
              | Ast.BitwiseOr => "|"
              | Ast.BitwiseXor => "^"
              | Ast.LogicalAnd => "&&"
              | Ast.LogicalOr => "||"
              | Ast.InstanceOf => "instanceof"
              | Ast.In => "in"
              | Ast.Equals => "=="
              | Ast.NotEquals => "!="
              | Ast.StrictEquals => "==="
              | Ast.StrictNotEquals => "!=="
              | Ast.Less => "<"
              | Ast.LessOrEqual => "<="
              | Ast.Greater => ">"
              | Ast.GreaterOrEqual => ">="
              | Ast.Comma => ","
        val res =

            case bop of 

                Ast.Plus => (* ES3 11.6.1 The Addition operator (+) *)
                let
                    val a = toPrimitiveWithNoHint regs va
                    val b = toPrimitiveWithNoHint regs vb
                in
                    if Mach.isString a orelse Mach.isString b
                    then newString regs (Ustring.stringAppend 
                                             (toUstring regs va) 
                                             (toUstring regs vb))
                    else
                        simpleNumeric ( Decimal.add )
                                      ( Real64.+ )
                                      (toNumeric regs a) (toNumeric regs b)
                end
                
              | Ast.Minus => (* ES3 11.6.2 The Subtraction operator (-) *)
                simpleNumeric ( Decimal.subtract )
                              ( Real64.- )
                              (toNumeric regs va) (toNumeric regs vb)
                
              | Ast.Times => 
                simpleNumeric ( Decimal.multiply )
                              ( Real64.* )
                              (toNumeric regs va) (toNumeric regs vb)
                
              | Ast.Divide => 
                simpleNumeric ( Decimal.divide )
                              ( Real64./ )
                              (toNumeric regs va) (toNumeric regs vb)

              | Ast.Remainder =>
                let
                    (*
                     * Something is numerically *very wrong*
                     * with the Real64.rem operation.
                     *)
                    fun realRem (x,y) =
                        let
                            val n = Real.realTrunc(x / y)
                        in
                            x - ( n * y)
                        end
                in
                    simpleNumeric ( Decimal.remainder )
                                  ( realRem )
                                  (toNumeric regs va) (toNumeric regs vb)                    
                end

              | Ast.LeftShift =>
                pickRepByA (Word32.<< ((i2u (toInt32 regs va)),
                                       (masku5 (toUInt32 regs vb))))

              | Ast.RightShift =>
                pickRepByA (Word32.>> ((i2u (toInt32 regs va)),
                                       (masku5 (toUInt32 regs vb))))

              | Ast.RightShiftUnsigned =>
                pickRepByA (Word32.~>> ((toUInt32 regs va),
                                        (masku5 (toUInt32 regs vb))))

              (* FIXME: should we return int if we do int|int or int&int ? *)
              | Ast.BitwiseAnd => bitwiseWordOp (Word32.andb)
              | Ast.BitwiseOr => bitwiseWordOp (Word32.orb)
              | Ast.BitwiseXor => bitwiseWordOp (Word32.xorb)

              | Ast.Equals => doubleEquals' ()

              | Ast.NotEquals =>
                newBoolean regs (not (toBoolean (doubleEquals' ())))

              | Ast.StrictEquals =>
                tripleEquals ()

              | Ast.StrictNotEquals =>
                newBoolean regs (not (toBoolean (tripleEquals ())))

              | Ast.Less =>
                dispatchComparison 
                    (fn x => x = LESS)

              | Ast.LessOrEqual =>
                dispatchComparison 
                    (fn x => (x = LESS) orelse (x = EQUAL))

              | Ast.Greater =>
                dispatchComparison 
                    (fn x => x = GREATER)

              | Ast.GreaterOrEqual =>
                dispatchComparison 
                    (fn x => (x = GREATER) orelse (x = EQUAL))

              | _ => error regs ["unexpected binary operator in performBinOp"]
    in
        trace ["binop: ", 
               Mach.approx va, " ", 
               binOpName, " ", 
               Mach.approx vb, " -> ", 
               Mach.approx res];
        res
    end


and doubleEquals (regs:Mach.REGS)
                 (a:Mach.VAL)
                 (b:Mach.VAL)
    : bool =
    toBoolean (performBinop regs Ast.Equals a b)


and typeOfVal (regs:Mach.REGS)
              (v:Mach.VAL)
    : Ast.TYPE_EXPR =
    let
        val te = case v of
                     Mach.Undef => Ast.SpecialType Ast.Undefined
                   | Mach.Null => Ast.SpecialType Ast.Null
                   | Mach.Object (Mach.Obj {tag, ...}) =>
                     (case tag of
                          (* 
                           * FIXME: Class and Object should be pulling their 
                           * ty from their magic closures, not their tags.
                           * 
                           * Possibly the tags should not carry types at all? Or 
                           * should carry TYs rather than TYPE_EXPR subforms?
                           *)
                          Mach.ClassTag ity => Ast.InstanceType ity
                        | Mach.ObjectTag tys => Ast.ObjectType tys
                        | Mach.ArrayTag tys => Ast.ArrayType tys
                        | Mach.FunctionTag fty => Ast.FunctionType fty
                        | Mach.NoTag =>
                          (* FIXME: this would be a hard error if we didn't use NoTag values
                           * as temporaries. Currently we do, so there are contexts where we
                           * want them to have a type in order to pass a runtime type test.
                           * this is of dubious value to me. -graydon.
                           *
                           * error regs ["typeOfVal on NoTag object"])
                           *)
                          Ast.SpecialType Ast.Any)
    in
        evalTy regs (makeTy te)
    end
                     

and isCompatible (regs:Mach.REGS)
                 (v:Mach.VAL)
                 (tyExpr:Ast.TYPE_EXPR)
    : bool =
    Type.groundIsCompatible (typeOfVal regs v) tyExpr


and evalBinaryTypeOp (regs:Mach.REGS)
                     (bop:Ast.BINTYPEOP)
                     (expr:Ast.EXPR)
                     (ty:Ast.TY)
    : Mach.VAL =
    let
        val v = evalExpr regs expr
    in
        case bop of
            Ast.Cast =>
            if isCompatible regs v (evalTy regs ty)
            then v
            else (typeOpFailure regs "cast failed" v (AstQuery.typeExprOf ty); dummyVal)
          | Ast.To => checkAndConvert regs v ty
          | Ast.Is => newBoolean regs ((typeOfVal regs v) <: (evalTy regs ty))
    end

and hasInstance (regs:Mach.REGS)
                (obj:Mach.OBJ)
                (v:Mach.VAL)
    : bool = 
    let
        val proto = getValue regs obj Name.nons_prototype
        val targId = case proto of 
                         (Mach.Object (Mach.Obj { ident, ... })) => ident
                       | _ => (throwTypeErr regs ["no 'prototype' property found in [[hasInstance]]"]; dummyObjId)
        fun tryVal v' = 
            case v' of 
                Mach.Null => false
              | Mach.Undef => false
              | Mach.Object ob =>
                if getObjId ob = targId
                then true
                else 
                    let
                        val Mach.Obj { proto, ... } = ob
                    in
                        tryVal (!proto)
                    end
    in
        tryVal v
    end

and evalBinaryOp (regs:Mach.REGS)
                 (bop:Ast.BINOP)
                 (aexpr:Ast.EXPR)
                 (bexpr:Ast.EXPR)
    : Mach.VAL =
    case bop of
        Ast.LogicalAnd =>
        let
            val a = evalExpr regs aexpr
        in
            if toBoolean a
            then evalExpr regs bexpr
            else a
        end

      | Ast.LogicalOr =>
        let
            val a = evalExpr regs aexpr
        in
            if toBoolean a
            then a
            else evalExpr regs bexpr
        end

      | Ast.Comma => (evalExpr regs aexpr;
                      evalExpr regs bexpr)

      | Ast.InstanceOf =>
        let
            val a = evalExpr regs aexpr
            val b = evalExpr regs bexpr
        in
            case b of
                Mach.Object ob => 
                newBoolean 
                    regs 
                    (case Mach.getObjMagic ob of 
                         SOME (Mach.Class _) => hasInstance regs ob a
                       | SOME (Mach.Interface _) => hasInstance regs ob a
                       | SOME (Mach.Function _) => hasInstance regs ob a
                       | SOME (Mach.NativeFunction _) => hasInstance regs ob a
                       | _ => (throwTypeErr regs ["operator 'instanceof' applied object with no [[hasInstance]] method"]; false))
              | _ => (throwTypeErr regs ["operator 'instanceof' applied to non-object"]; dummyVal)
        end

      | Ast.In =>
        let
            val a = evalExpr regs aexpr
            val b = evalExpr regs bexpr
            val aname = needNameOrString regs a
        in
            case b of
                Mach.Object obj =>
                newBoolean regs (hasValue obj aname)
              | _ => (throwTypeErr regs ["operator 'in' applied to non-object"]; dummyVal)
        end

      | _ => performBinop regs bop
                          (evalExpr regs aexpr)
                          (evalExpr regs bexpr)


and evalCondExpr (regs:Mach.REGS)
                 (cond:Ast.EXPR)
                 (thn:Ast.EXPR)
                 (els:Ast.EXPR)
    : Mach.VAL =
    let
        val v = evalExpr regs cond
        val b = toBoolean v
    in
        if b
        then evalExpr regs thn
        else evalExpr regs els
    end


and evalExprToNamespace (regs:Mach.REGS)
                        (expr:Ast.EXPR)
    : Ast.NAMESPACE =
    (*
     * If we have a namespace property we can evaluate to an Ast.Namespace
     * directly without manufacturing a temporary Namespace wrapper object.
     *)
    let
        fun evalRefNamespace _ =
            let
                val _ = trace ["evaluating ref to namespace"];
                val (_, expr) = getExpectedType regs expr
                val (obj, name) = evalRefExpr regs expr true
                val Mach.Obj { props, ... } = obj
            in
                case (#state (Mach.getProp props name)) of
                    Mach.NamespaceProp ns => ns
                  | _ => needNamespace regs (getValue regs obj name)
            end
    in
        case expr of
            Ast.LexicalRef _ => evalRefNamespace ()
          | Ast.ObjectRef _ => evalRefNamespace ()
          | _ => needNamespace regs (evalExpr regs expr)
    end


and evalIdentExpr (regs:Mach.REGS)
                  (r:Ast.IDENT_EXPR)
    : NAME_OR_MULTINAME =
    case r of
        Ast.Identifier { ident:Ast.IDENT, openNamespaces:Ast.NAMESPACE list list } =>
        Multiname { nss=openNamespaces, id=ident }

      | Ast.QualifiedIdentifier { qual, ident } =>
        Name { ns = (evalExprToNamespace regs qual), id = ident }

      | Ast.QualifiedExpression { qual, expr } =>
        Name { ns = (evalExprToNamespace regs qual),
               id = toUstring regs (evalExpr regs expr) }

      | Ast.ExpressionIdentifier { expr, openNamespaces } =>
        let
            val v = evalExpr regs expr
        in
            case v of
                Mach.Object obj =>
                if (typeOfVal regs v) <: (instanceType regs Name.ES4_Name [])
                then
                    let
                        val nsval = getValue regs obj Name.nons_qualifier
                        val idval = getValue regs obj Name.nons_identifier
                    in
                        Name { ns = needNamespaceOrNull regs nsval,
                               id = toUstring regs idval }
                    end
                else
                    Multiname { nss = openNamespaces,
                                id = toUstring regs v }
              | _ =>
                Multiname { nss = openNamespaces,
                            id = toUstring regs v }
        end

      | _ => LogErr.unimplError ["unimplemented identifier expression form"]


(*
 * ES-262-3 11.2.1: Resolving member expressions to REFs.
 *)

and evalRefExpr (regs:Mach.REGS)
                (expr:Ast.EXPR)
                (errIfNotFound:bool)
    : REF =
    let
        val (_, r) = evalRefExprFull regs expr errIfNotFound
    in
        r
    end


and evalRefExprFull (regs:Mach.REGS)
                    (expr:Ast.EXPR)
                    (errIfNotFound:bool)
    : (Mach.OBJ * REF) =
    let
        fun defaultRef obj nomn =
            case nomn of
                Multiname mname => (obj, Name.nons (#id mname))
              | Name name => (obj, name)
    in
        case expr of
            Ast.LexicalRef { ident, loc } =>
            let
                val _ = LogErr.setLoc loc
                val nomn = evalIdentExpr regs ident
                val _ = LogErr.setLoc loc
                val refOpt = resolveOnScopeChain (#scope regs) nomn
                val _ = LogErr.setLoc loc
                val r = case refOpt of
                            SOME r => r
                          | NONE => if errIfNotFound
                                    then (throwRefErr regs ["unresolved lexical reference ", nomnToStr nomn]; dummyRef)
                                    else defaultRef (#global regs) nomn
            in
                ((#this regs), r)
            end

          | Ast.ObjectRef { base, ident, loc } =>
            let
                val _ = LogErr.setLoc loc
                val v = evalExpr regs base
                val _ = LogErr.setLoc loc
                val nomn = evalIdentExpr regs ident
                val _ = LogErr.setLoc loc
                val ob = case v of
                             Mach.Object ob => ob
                           | Mach.Null => (throwRefErr regs ["object reference on null value"]; dummyObj)
                           | Mach.Undef => (throwRefErr regs ["object reference on undefined value"]; dummyObj)
                val _ = LogErr.setLoc loc
                val refOpt = resolveName ob nomn
                val _ = LogErr.setLoc loc
                val r = case refOpt of
                            SOME ro => ro
                          | NONE => if errIfNotFound
                                    then (throwRefErr regs ["unresolved object reference ", nomnToStr nomn]; dummyRef)
                                    else defaultRef ob nomn
                val _ = LogErr.setLoc loc
                val (holder, n) = r
                val _ = trace ["resolved object ref to ", fmtName n,
                               " on object #", Int.toString (getObjId holder),
                               " with this=#", Int.toString (getObjId ob)]
            in
                (ob, r)
            end

          | _ => error regs ["need lexical or object-reference expression"]
    end

(*
 EXPR = LetExpr
 *)
and evalLetExpr (regs:Mach.REGS)
                (head:Ast.HEAD)
                (body:Ast.EXPR)
    : Mach.VAL =
    let
        val letRegs = evalHead regs head
    in
        evalExpr letRegs body
    end


and resolveOnScopeChain (scope:Mach.SCOPE)
                        (nomn:NAME_OR_MULTINAME)
    : REF option =
    let
        val _ = case nomn of
                    Name name => trace ["resolving name on scope chain: ",
                                        fmtName name]
                  | Multiname mname => trace ["resolving multiname on scope chain: ",
                                              fmtMultiname mname]

        fun getScopeProps (Mach.Scope {object=Mach.Obj {props, ...}, ... }) = props
        fun getScopeParent (Mach.Scope { parent, ... }) = parent
        fun matchFixedScopeBinding s n nss
          = Mach.matchProps true (getScopeProps s) n nss

        (*
         * Primary lookup is for fixed props along the scope chains, *not* involving
         * prototypes. If that fails, we do a sequence of dynamic-property-permitted
         * lookups on every scope object (and along its prototype chain)
         * in the scope chain.
         *)
        fun tryProtoChain (Mach.Scope {object, parent, ...}) =
            case resolveName object nomn of
                SOME r => SOME r
              | NONE => case parent of
                            NONE => NONE
                          | SOME p => tryProtoChain p
    in
        (*
         * First do a fixed-properties-only lookup along the scope chain alone.
         *)
        case nomn of
            Name name =>
            let
                val props = getScopeProps scope
            in
                if Mach.hasProp props name
                then SOME (getScopeObj scope, name)
                else case getScopeParent scope of
                         NONE => tryProtoChain scope
                       | SOME parent => resolveOnScopeChain parent nomn
            end
          | Multiname mname =>
            case Multiname.resolve
                     mname scope
                     matchFixedScopeBinding
                     getScopeParent
             of
                SOME (Mach.Scope {object, ...}, name) =>
                (trace ["found ",fmtName name]; SOME (object, name))
              | NONE => tryProtoChain scope
    end

(*
 * Scans provided object and prototype chain looking for a slot that
 * matches name (or multiname). Returns a REF to the exact object found.
 *)
and resolveName (obj:Mach.OBJ)
                (nomn:NAME_OR_MULTINAME)
    : REF option =
    let
        fun matchFixedBinding (Mach.Obj {props, ...}) n nss
          = Mach.matchProps true props n nss
        fun matchBinding (Mach.Obj {props, ...}) n nss
          = Mach.matchProps false props n nss
        fun getObjProto (Mach.Obj {proto, ...}) =
            case (!proto) of
                Mach.Object ob => SOME ob
              | _ => NONE
    in
        case nomn of
            Name name => findValue obj name
          | Multiname mname =>
            let
                val fixedRefOpt:(REF option) = Multiname.resolve mname obj matchFixedBinding getObjProto
            in
                case fixedRefOpt of
                    NONE => Multiname.resolve mname obj matchBinding getObjProto
                  | ro => ro
            end
    end


and evalTailCallExpr (regs:Mach.REGS)
                     (e:Ast.EXPR)
    : Mach.VAL =
    raise (TailCallException (fn _ => evalExpr regs e))


and evalNonTailCallExpr (regs:Mach.REGS)
                        (e:Ast.EXPR)
    : Mach.VAL =
    evalExpr regs e
    handle TailCallException thunk => thunk ()
         | ReturnException v => v


and labelMatch (stmtLabels:Ast.IDENT list)
               (exnLabel:Ast.IDENT option)
    : bool =
    case (stmtLabels, exnLabel) of
        (sl::[], SOME el) => sl = el
      | (sl::[], NONE) => sl = Ustring.empty
      | ([], NONE) => false                   (* FIXME: refactor *)
      | ([], SOME el) => false
      | (sl::sls,NONE) =>
        if sl = Ustring.empty
        then true
        else labelMatch sls exnLabel
      | (sl::sls,SOME el) =>
        if sl = el
        then true
        else labelMatch sls exnLabel


and evalStmts (regs:Mach.REGS)
              (stmts:Ast.STMT list)
    : Mach.VAL =
    case stmts of
        [s] => evalStmt regs s
      | (s::ss) => (evalStmt regs s;
                    evalStmts regs ss)
      (* FIXME: need to keep the last non-empty value and fixup abrupt completions here *)
      | [] => Mach.Undef


and evalStmt (regs:Mach.REGS)
             (stmt:Ast.STMT)
    : Mach.VAL =
    case stmt of
        Ast.ExprStmt e => evalExpr regs e
      | Ast.IfStmt {cnd,thn,els} => evalIfStmt regs cnd thn els
      | Ast.WhileStmt w => evalWhileStmt regs w
      | Ast.DoWhileStmt w => evalDoWhileStmt regs w
      | Ast.WithStmt { obj, ty, body } => evalWithStmt regs obj ty body
      | Ast.SwitchStmt { cond, cases, labels } =>
        evalSwitchStmt regs cond cases labels
      | Ast.ForStmt w => evalForStmt regs w
      | Ast.ReturnStmt r => evalReturnStmt regs r
      | Ast.BreakStmt lbl => evalBreakStmt regs lbl
      | Ast.ContinueStmt lbl => evalContinueStmt regs lbl
      | Ast.ThrowStmt t => evalThrowStmt regs t
      | Ast.LabeledStmt (lab, s) => evalLabelStmt regs lab s
      | Ast.BlockStmt b => evalBlock regs b
      | Ast.ClassBlock c => evalClassBlock regs c
      | Ast.LetStmt b => evalBlock regs b
      | Ast.EmptyStmt => Mach.Undef
      | Ast.TryStmt { block, catches, finally } =>
        evalTryStmt regs block catches finally
      | Ast.SwitchTypeStmt { cond, ty, cases } =>
        evalSwitchTypeStmt regs cond ty cases
      | Ast.ForInStmt w => evalForInStmt regs w
      | _ => error regs ["Shouldn't happen: failed to match in Eval.evalStmt."]

and findVal (regs:Mach.REGS)
            (scope:Mach.SCOPE)
            (name:Ast.NAME)
    : Mach.VAL =
    case resolveOnScopeChain scope (Name name) of
        NONE => error regs ["unable to find value: ", LogErr.name name ]
      | SOME (obj, n) => getValue regs obj n


and checkAllPropertiesInitialized (regs:Mach.REGS)
                                  (obj:Mach.OBJ)
    : unit =
    let
        fun checkOne (n:Ast.NAME, p:Mach.PROP) =
            case (#state p) of
                Mach.UninitProp => 
                error regs ["uninitialized property: ",
                            LogErr.name n]
              | _ => ()
    in
        case obj of
            Mach.Obj { props, ... } =>
            NameMap.appi checkOne (!props)
    end


and invokeFuncClosure (regs:Mach.REGS)
                      (closure:Mach.FUN_CLOSURE)
                      (args:Mach.VAL list)
    : Mach.VAL =
    let
        val { func, this, env } = closure
        val _ = trace ["entering func closure in scope #", 
                       Int.toString (getObjId (getScopeObj env))]
        val _ = traceScope env
        val Ast.Func { name, block, param=Ast.Head (paramRib, paramInits), ty, ... } = func
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
                              Ast.Ordinary => idStr
                            | Ast.Operator => "operator " ^ idStr
                            | Ast.Get => "get " ^ idStr
                            | Ast.Set => "set " ^ idStr
                            | Ast.Call => "call " ^ idStr
                            | Ast.Has => "has " ^ idStr

            val _ = Mach.push regs strname args

            val (varObj:Mach.OBJ) = Mach.newObjNoTag ()
            val (varRegs:Mach.REGS) = extendScopeReg regs varObj Mach.ActivationScope
            val (varScope:Mach.SCOPE) = (#scope varRegs)
            val (Mach.Obj {props, ...}) = varObj

            (* FIXME: self-name binding is surely more complex than this! *)
            val selfName = Name.nons (#ident name)
            fun initSelf _ = Mach.addProp props selfName { ty = makeTy (Ast.SpecialType Ast.Any),
                                                           state = Mach.MethodProp closure,
                                                           attrs = { dontDelete = true,
                                                                     dontEnum = true,
                                                                     readOnly = true,
                                                                     isFixed = true } }
        in
            trace ["invokeFuncClosure: allocating scope rib"];
            allocScopeRib varRegs paramRib;
            trace ["invokeFuncClosure: binding args"];
            bindArgs regs varScope func args;
            trace ["invokeFuncClosure: evaluating scope inits on scope obj #",
                   Int.toString (getScopeId varScope)];
            evalScopeInits varRegs Ast.Local paramInits;

            (* NOTE: is this for the binding of a function expression to its optional
             * identifier? If so, we need to extend the scope chain before extending it
             * with the activation object, and add the self-name binding to that new
             * scope, as in sec 13 ed. 3.
             *
             * Changing defValue to setValue for now.
             *)

            initSelf ();
            checkAllPropertiesInitialized regs varObj;
            trace ["invokeFuncClosure: evaluating block"];
            let
                val res = case block of 
                              NONE => Mach.Undef
                            | SOME b => ((evalBlock varRegs b;
                                          Mach.Undef)
                                         handle ReturnException v => v)
            in
                Mach.pop regs;
                res
            end
        end
    end

and catch (regs:Mach.REGS)
          (e:Mach.VAL)
          (clauses:Ast.CATCH_CLAUSE list)
    : Mach.VAL option =
    case clauses of
        [] => NONE
      | {ty, rib, inits, block, ...}::cs =>
        if isCompatible regs e (evalTy regs ty)
        then
            let
                val fixs = valOf rib
                val head = Ast.Head (valOf rib, [])
                val regs = evalHead regs head
                val scope = (#scope regs)
                val obj = (#this regs)
                val temps = getScopeTemps scope
            in
                (if List.null fixs
                 then ()
                 else Mach.defTemp temps 0 e;
                 evalScopeInits regs Ast.Local (valOf inits);
                 SOME (evalBlock regs block))
            end
        else
            catch regs e cs

and evalTryStmt (regs:Mach.REGS)
                (block:Ast.BLOCK)
                (catches:Ast.CATCH_CLAUSE list)
                (finally:Ast.BLOCK option)
    : Mach.VAL =
    let
        fun finishWith (v:Mach.VAL)
            : Mach.VAL =
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
               case catch regs v catches of
                   SOME fix => finishWith fix
                 | NONE => (finishWith v;
                            raise ThrowException v)
    end


and bindArgs (regs:Mach.REGS)
             (argScope:Mach.SCOPE)
             (func:Ast.FUNC)
             (args:Mach.VAL list)
    : unit =
    let
        val Mach.Scope { object = Mach.Obj { props, ... }, ... } = argScope
        val Ast.Func { defaults, ty, ... } = func
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

        val p = length (AstQuery.paramTypesOfFuncTy ty)
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
          | bindArg (n:int) ((arg:Mach.VAL)::args) =
            (trace ["defining temp ", Int.toString n];
             Mach.defTemp argTemps n arg;
             bindArg (n+1) args)

        fun bind (finalArgs:Mach.VAL list) =
            (*
             * FIXME: this is a random guess at the appropriate form
             * of 'arguments'.
             *)
            (Mach.addProp props Name.arguments { state = Mach.ValListProp args,  
                                                 (* args is a better approximation than finalArgs *)
                                                 ty = makeTy (Name.typename Name.nons_Object),
                                                 attrs = { dontDelete = true,
                                                           dontEnum = true,
                                                           readOnly = false,
                                                           isFixed = true } };
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
                              then (List.tabulate (((p-d) - a), (fn _ => Mach.Undef)))
                              else []
                val defExprs = List.drop (defaults, i)
                val defVals = List.map (evalExpr regs) defExprs
                val allArgs = args @ padding @ defVals
            in
                bind (List.take (allArgs, p))
            end
    end


and evalInits (regs:Mach.REGS)
              (obj:Mach.OBJ)
              (temps:Mach.TEMPS)
              (inits:Ast.INITS)
    : unit =
    evalInitsMaybePrototype regs obj temps inits false


and evalInitsMaybePrototype (regs:Mach.REGS)
                            (obj:Mach.OBJ)
                            (temps:Mach.TEMPS)
                            (inits:Ast.INITS)
                            (isPrototypeInit:bool)
    : unit =
    let
        fun evalInit (n,e) =
            let
                val idStr = case n of
                                Ast.PropName { id, ... } => Ustring.toAscii id
                              | Ast.TempName t => ("#" ^ Int.toString t)
                val _ = Mach.push regs ("init " ^ idStr) []
                val v = evalExpr regs e
            in
                case n of
                    Ast.PropName pn =>
                    (trace ["evalInit assigning to prop ", fmtName pn,
                            " on object #", (Int.toString (getObjId obj))];
                     (*                     if isPrototypeInit then
                                                LogErr.log ["Propname: ", fmtName pn]
                                            else
                                                () ; *)
                     if isPrototypeInit
                     then
                         let
                             val Mach.Obj { props, ... } = obj
                         in
                             setValue regs obj pn v;
                             Mach.setPropDontEnum props pn true
                         end
                     else defValue regs obj pn v)
                  | Ast.TempName tn =>
                    (trace ["evalInit assigning to temp ", (Int.toString tn),
                            " on object #", (Int.toString (getObjId obj))];
                     Mach.defTemp temps tn v);
                Mach.pop regs
            end
    in
        List.app evalInit inits
    end

(*
 Evaluate INITs targeting an obj, in scope of temporaries
 *)

and evalObjInits (regs:Mach.REGS)
                 (instanceObj:Mach.OBJ)
                 (head:Ast.HEAD)
    : unit =
    let
        val (Ast.Head (rib,inits)) = head
        val tempRegs = evalHead regs (Ast.Head (rib,[]))
        val temps = getScopeTemps (#scope regs)
    in
        evalInits tempRegs instanceObj temps inits
    end


and findTargetObj (regs:Mach.REGS)
                  (scope:Mach.SCOPE)
                  (target:Ast.INIT_TARGET) =
    let
        val Mach.Scope { object, kind, parent, ...} = scope
        val Mach.Obj {props,...} = object
    in
        trace ["considering init target as object #", Int.toString (getScopeId scope)];
        case target of
            Ast.Local =>
            if (NameMap.numItems (!props)) > 0 orelse 
               not (Option.isSome parent)
            then object
            else findTargetObj regs (valOf parent) target
          (* if there are no props then it is at temp scope *)
                 
          | Ast.Hoisted =>
            if kind = Mach.InstanceScope orelse
               kind = Mach.ActivationScope orelse
               not (Option.isSome parent)
            then object
            else findTargetObj regs (valOf parent) target
                 
          | Ast.Prototype =>
            if kind = Mach.InstanceScope
            then (needObj regs (getValue regs object Name.nons_prototype))
            else findTargetObj regs (valOf parent) target
    end


and evalScopeInits (regs:Mach.REGS)
                   (target:Ast.INIT_TARGET)
                   (inits:Ast.INITS)
    : unit =
    let
        val { scope, ... } = regs
        val Mach.Scope { temps, ...} = scope
        val obj = findTargetObj regs scope target
        val Mach.Obj { ident, ... } = obj
        val _ = trace ["resolved init target to object id #", Int.toString ident]
    in
        evalInitsMaybePrototype regs obj temps inits (target=Ast.Prototype)
    end


and initializeAndConstruct (classRegs:Mach.REGS)
                           (classClosure:Mach.CLS_CLOSURE)
                           (classObj:Mach.OBJ)
                           (args:Mach.VAL list)
                           (instanceObj:Mach.OBJ)
    : unit =
    let
        val { cls, env } = classClosure
        val Ast.Cls { instanceType, ... } = cls
        (* NB: leave this here, it faults if we have a non-ground type, which is the point. *)
        val tyExpr = evalTy classRegs instanceType
    in
        let
            fun idStr ob = Int.toString (getObjId ob)
            val _ = trace ["initializeAndConstruct: this=#", (idStr (#this classRegs)),
                           ", constructee=#", (idStr instanceObj),
                           ", class=#", (idStr classObj)]
            val _ = if getObjId (#this classRegs) = getObjId instanceObj
                    then ()
                    else error classRegs ["constructor running on non-this value"]
            val Ast.Cls { name,
                          extends,
                          instanceInits,
                          constructor,
                          ... } = cls
            fun initializeAndConstructSuper (superArgs:Mach.VAL list) =
                case extends of
                    NONE =>
                    (trace ["checking all properties initialized at root class ", fmtName name];
                     checkAllPropertiesInitialized classRegs instanceObj)
                  | SOME parentTy =>
                    let
                        val parentTy = evalTy classRegs parentTy
                        val _ = trace ["initializing and constructing superclass ", Type.fmtType parentTy]
                        val (superObj:Mach.OBJ) = instanceClass classRegs (AstQuery.needInstanceType parentTy)
                        val (superClsClosure:Mach.CLS_CLOSURE) = Mach.needClass (Mach.Object superObj)
                        val (superRegs:Mach.REGS) = 
                            withThis (withScope classRegs (#env superClsClosure)) instanceObj
                    in
                        initializeAndConstruct
                            superRegs superClsClosure superObj superArgs instanceObj
                    end
        in
            trace ["evaluating instance initializers for ", fmtName name];
            evalObjInits classRegs instanceObj instanceInits;
            case constructor of
                NONE => initializeAndConstructSuper []
              | SOME (Ast.Ctor { settings, superArgs, func }) =>
                let
                    val _ = Mach.push classRegs ("ctor " ^ (Ustring.toAscii (#id name))) args
                    val Ast.Func { block, param=Ast.Head (paramRib,paramInits), ... } = func
                    val (varObj:Mach.OBJ) = Mach.newObjNoTag ()
                    val (varRegs:Mach.REGS) = extendScopeReg classRegs
                                                             varObj
                                                             Mach.ActivationScope
                    val varScope = (#scope varRegs)
                    val (instanceRegs:Mach.REGS) = extendScopeReg classRegs
                                                                  instanceObj
                                                                  Mach.InstanceScope
                    val (ctorRegs:Mach.REGS) = extendScopeReg instanceRegs
                                                              varObj
                                                              Mach.ActivationScope
                in
                    trace ["allocating scope rib for constructor of ", fmtName name];
                    allocScopeRib varRegs paramRib;
                    trace ["binding constructor args of ", fmtName name];
                    bindArgs classRegs varScope func args;
                    trace ["evaluating inits of ", fmtName name,
                           " in scope #", Int.toString (getScopeId varScope)];
                    evalScopeInits varRegs Ast.Local paramInits;
                    trace ["evaluating settings"];
                    evalObjInits varRegs instanceObj settings;
                    trace ["initializing and constructing superclass of ", fmtName name];
                    initializeAndConstructSuper (map (evalExpr varRegs) superArgs);
                    trace ["entering constructor for ", fmtName name];
                    (case block of 
                         NONE => Mach.Undef
                       | SOME b => (evalBlock ctorRegs b
                                    handle ReturnException v => v));
                    Mach.pop classRegs;
                    ()
                end
        end
    end

and constructStandard (regs:Mach.REGS)
                      (classObj:Mach.OBJ)
                      (classClosure:Mach.CLS_CLOSURE)
                      (args:Mach.VAL list)
    : Mach.OBJ =
    let
        val {cls = Ast.Cls { instanceType, ...}, env, ...} = classClosure
        val classRegs = withScope regs env
        val ty = AstQuery.needInstanceType (evalTy classRegs instanceType)
        val (tag:Mach.VAL_TAG) = Mach.ClassTag ty
    in
        constructStandardWithTag classRegs classObj classClosure args tag
    end

and constructStandardWithTag (regs:Mach.REGS)
                             (classObj:Mach.OBJ)
                             (classClosure:Mach.CLS_CLOSURE)
                             (args:Mach.VAL list)
                             (tag:Mach.VAL_TAG)
    : Mach.OBJ =
    let
        val {cls = Ast.Cls { name, instanceRib, ...}, env, ...} = classClosure
        val (proto:Mach.VAL) = if hasOwnValue classObj Name.nons_prototype
                               then getValue regs classObj Name.nons_prototype
                               else Mach.Null
        val (instanceObj:Mach.OBJ) = Mach.newObj tag proto NONE
        (* FIXME: might have 'this' binding wrong in class scope here. *)
        val (classScope:Mach.SCOPE) = extendScope env classObj Mach.InstanceScope
        val classRegs = withThis (withScope regs classScope) instanceObj
    in
        trace ["allocating ", Int.toString (length instanceRib),
               " instance rib for new ", fmtName name];
        allocObjRib classRegs instanceObj (SOME instanceObj) instanceRib;
        trace ["entering most derived constructor for ", fmtName name];
        initializeAndConstruct classRegs classClosure classObj args instanceObj;
        trace ["finished constructing new ", fmtName name];
        instanceObj
    end

and parseFunctionFromArgs (regs:Mach.REGS)
                          (args:Mach.VAL list)
    : (Ustring.STRING * Ast.EXPR) =
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
                                Parser.NoList,
                                Parser.AllowIn)

        val funcExpr = Defn.defExpr (Defn.mkTopEnv (#prog regs)) funcExpr
    in
        (fullStr, funcExpr)
    end


and specialFunctionConstructor (regs:Mach.REGS)
                               (classObj:Mach.OBJ)
                               (classClosure:Mach.CLS_CLOSURE)
                               (args:Mach.VAL list)
    : Mach.OBJ =
    let
        val (source, funcExpr) = parseFunctionFromArgs regs args
        val sname = Name.nons_source
        val sval = newString regs source
        val fv = case funcExpr of
                     Ast.LiteralExpr (Ast.LiteralFunction f) =>
                     newFunctionFromFunc regs (getGlobalScope regs) f
                   | _ => error regs ["function did not parse"];
	val fo = needObj regs fv
    in
        setValue regs fo sname sval;
        fo
    end


and specialArrayConstructor (regs:Mach.REGS)
                            (classObj:Mach.OBJ)
                            (classClosure:Mach.CLS_CLOSURE)
                            (args:Mach.VAL list) :
    Mach.OBJ =
    let
        val instanceObj = constructStandard regs classObj classClosure args
        val Mach.Obj { props, ... } = instanceObj
        fun bindVal _ [] = ()
          | bindVal n (x::xs) =
            (setValue regs instanceObj (Name.nons (Ustring.fromInt n)) x;
             bindVal (n+1) xs)
    in
        case args of
            [] => setValue regs instanceObj Name.nons_length (newUInt regs 0w0)
          | [k] => let val idx = asArrayIndex k
                   in
                       if not (idx = 0wxFFFFFFFF) then
                           setValue regs instanceObj Name.nons_length k
                       else
                           bindVal 0 args
                   end
          | _ => bindVal 0 args;
        Mach.setPropDontEnum props Name.nons_length true;
        Mach.setPropDontEnum props Name.private_Array__length true;
        instanceObj
    end

(*
 * ES-262-3 15.2.2.1 The Object Constructor
 *)

and specialMagicCopyingConstructor (regs:Mach.REGS)
				   (classObj:Mach.OBJ)
				   (classClosure:Mach.CLS_CLOSURE)
				   (args:Mach.VAL list)
    : Mach.OBJ =
    let
	val magic = 
	    case args of 
		[] => error regs ["called special magic-copying constructor with no args"]
	      | v :: _ => Mach.needMagic v
        val obj = constructStandard regs classObj classClosure []
    in
	Mach.setMagic obj (SOME magic)
    end

and specialObjectConstructor (regs:Mach.REGS)
                             (classObj:Mach.OBJ)
                             (classClosure:Mach.CLS_CLOSURE)
                             (args:Mach.VAL list)
    : Mach.OBJ =
    let
        fun instantiate _ = constructStandard regs classObj classClosure args
    in
        case args of
            [] => instantiate ()
          | (Mach.Null :: _) => instantiate ()
          | (Mach.Undef :: _) => instantiate ()
          | (Mach.Object obj :: _) =>
            case Mach.getObjMagic obj of
                NONE => obj
              | SOME m =>
                let
                    (*
                     * FIXME: This part is dubioius. ES-262-3 says to call ToObject
                     * on non-Object primitives. We do not do so here: rather, ToObject
                     * implements its lower half (primitive values) by calling into *here*,
                     * so here is where we do any cloning and magic-copying.
                     *)
                    val nobj = instantiate ()
                in
                    Mach.setMagic nobj (Mach.getObjMagic obj);
                    nobj
                end
    end


and specialBooleanConstructor (regs:Mach.REGS)
                              (classObj:Mach.OBJ)
                              (classClosure:Mach.CLS_CLOSURE)
                              (args:Mach.VAL list)
    : Mach.OBJ =
    let
	val b = case args of 
		    [] => false
		  | v :: _ => toBoolean v
	val cell = 
	    if b 
            then Mach.getBooleanTrueSlot regs 
            else Mach.getBooleanFalseSlot regs
    in
	case !cell of 
	    SOME obj => obj
	  | NONE => 
            let 
		val obj = constructStandard regs classObj classClosure []
            in 
		Mach.setMagic obj (SOME (Mach.Boolean b));
		cell := SOME obj;
		obj
            end
    end


and specialDoubleConstructor (regs:Mach.REGS)
                             (classObj:Mach.OBJ)
                             (classClosure:Mach.CLS_CLOSURE)
                             (args:Mach.VAL list)
    : Mach.OBJ =
    let
	val n = case args of 
		    [] => 0.0
		  | v :: _ => toDouble v
	fun build _ = 
	    let
		val obj = constructStandard regs classObj classClosure []
	    in
		Mach.setMagic obj (SOME (Mach.Double n));
		obj
	    end
    in
	if Real64.isNan n 
	then 
	    let
		val dn = Mach.getDoubleNaNSlot regs
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
	    case Mach.findInReal64Cache regs n of
		SOME obj => obj
	      | NONE => 
		let 
		    val obj = build ()
		in 
		    Mach.updateReal64Cache regs (n, obj);
		    obj
		end
    end


and specialDecimalConstructor (regs:Mach.REGS)
                              (classObj:Mach.OBJ)
                              (classClosure:Mach.CLS_CLOSURE)
                              (args:Mach.VAL list)
    : Mach.OBJ =
    let
	val n = case args of 
		    [] => toDecimal (decimalCtxt regs) (newInt regs 0)
		  | v :: _ => toDecimal (decimalCtxt regs) v
	val obj = constructStandard regs classObj classClosure []
    in
	Mach.setMagic obj (SOME (Mach.Decimal n));
	obj
    end


and specialIntConstructor (regs:Mach.REGS)
                          (classObj:Mach.OBJ)
                          (classClosure:Mach.CLS_CLOSURE)
                          (args:Mach.VAL list)
    : Mach.OBJ =
    let
	val i = case args of 
		    [] => 0
		  | v :: _ => toInt32 regs v
    in
	case Mach.findInInt32Cache regs i of
	    SOME obj => obj
	  | NONE => 
	    let 
		val obj = constructStandard regs classObj classClosure []
	    in 
		Mach.setMagic obj (SOME (Mach.Int i));
		Mach.updateInt32Cache regs (i, obj);
		obj
	    end
    end
    

and specialUIntConstructor (regs:Mach.REGS)
                           (classObj:Mach.OBJ)
                           (classClosure:Mach.CLS_CLOSURE)
                           (args:Mach.VAL list)
    : Mach.OBJ = 
    let 
	val u = case args of 
		    [] => Word32.fromInt 0
		  | v :: _ => toUInt32 regs v
    in
	case Mach.findInWord32Cache regs u of
	    SOME obj => obj
	  | NONE => 
	    let 
		val obj = constructStandard regs classObj classClosure []
	    in 
		Mach.setMagic obj (SOME (Mach.UInt u));
		Mach.updateWord32Cache regs (u, obj);
		obj
	    end
    end


and specialByteConstructor (regs:Mach.REGS)
                           (classObj:Mach.OBJ)
                           (classClosure:Mach.CLS_CLOSURE)
                           (args:Mach.VAL list)
    : Mach.OBJ =
    let 
	val b = case args of 
		    [] => Word8.fromInt 0
		  | v :: _ => toByte regs v
    in
	case Mach.findInWord8Cache regs b of
	    SOME obj => obj
	  | NONE => 
	    let 
		val obj = constructStandard regs classObj classClosure []
	    in 
		Mach.setMagic obj (SOME (Mach.Byte b));
		Mach.updateWord8Cache regs (b, obj);
		obj
	    end
    end


and specialStringConstructor (regs:Mach.REGS)
                             (classObj:Mach.OBJ)
                             (classClosure:Mach.CLS_CLOSURE)
                             (args:Mach.VAL list)
    : Mach.OBJ =
    let 
	val s = case args of 
		    [] => Ustring.empty
		  | v :: _ => toUstring regs v
    in
	case Mach.findInStrCache regs s of
	    SOME obj => obj
	  | NONE => 
	    let 
		val obj = constructStandard regs classObj classClosure []
	    in 
		Mach.setMagic obj (SOME (Mach.String s));
		Mach.updateStrCache regs (s, obj);
		obj
	    end
    end
    

and constructSpecial (regs:Mach.REGS)
                     (id:Mach.OBJ_IDENT)
                     (classObj:Mach.OBJ)
                     (classClosure:Mach.CLS_CLOSURE)
                     (args:Mach.VAL list) :
    Mach.OBJ option =
    let
	val { cls = Ast.Cls { name, ... }, ... } = classClosure
							 
        fun findSpecial [] = NONE
          | findSpecial ((q,f)::rest) = 
            let
                val ident = slotObjId regs q
            in
                if ident = id 
                then 
		    (trace ["running special ctor for instance of class #", 
			    Int.toString id, " = ", fmtName name];
		     SOME (f regs classObj classClosure args))
                else 
		    findSpecial rest
	    end
    in
        findSpecial 
            [
             (Mach.getClassClassSlot, specialMagicCopyingConstructor),
             (Mach.getInterfaceClassSlot, specialMagicCopyingConstructor),
             (Mach.getNamespaceClassSlot, specialMagicCopyingConstructor),

             (Mach.getObjectClassSlot, specialObjectConstructor),
             (Mach.getFunctionClassSlot, specialFunctionConstructor),
             (Mach.getArrayClassSlot, specialArrayConstructor),

             (Mach.getBooleanClassSlot, specialBooleanConstructor),
             (Mach.getDoubleClassSlot, specialDoubleConstructor),
             (Mach.getDecimalClassSlot, specialDecimalConstructor),

             (Mach.getIntClassSlot, specialIntConstructor),
             (Mach.getUintClassSlot, specialUIntConstructor),
             (Mach.getByteClassSlot, specialByteConstructor),
             (Mach.getStringClassSlot, specialStringConstructor)
	    ]
    end


and bindAnySpecialIdentity (regs:Mach.REGS)
                           (obj:Mach.OBJ) =
    if not (Mach.isBooting regs)
    then ()
    else
        let
            val Mach.Obj { ident, magic, ... } = obj
	in
	    case (!magic) of 
		NONE => ()
	      | SOME (Mach.Class { cls = Ast.Cls { name, ... }, ... }) =>
		let
		    val bindings = [
			(Name.intrinsic_Class, Mach.getClassClassSlot),
			(Name.intrinsic_Interface, Mach.getInterfaceClassSlot),
			(Name.ES4_Namespace, Mach.getNamespaceClassSlot),
			
			(Name.nons_Object, Mach.getObjectClassSlot),
			(Name.nons_Array, Mach.getArrayClassSlot),
			(Name.nons_Function, Mach.getFunctionClassSlot),
			
			(Name.nons_String, Mach.getStringWrapperClassSlot),
			(Name.ES4_string, Mach.getStringClassSlot),
			
			(Name.nons_Number, Mach.getNumberClassSlot),
			(Name.ES4_int, Mach.getIntClassSlot),
			(Name.ES4_uint, Mach.getUintClassSlot),
			(Name.ES4_double, Mach.getDoubleClassSlot),
			(Name.ES4_decimal, Mach.getDecimalClassSlot),
			
			(Name.ES4_boolean, Mach.getBooleanClassSlot),
			(Name.nons_Boolean, Mach.getBooleanWrapperClassSlot)
		    ]
		    fun f (n,id) = Mach.nameEq name n
		in
		    case List.find f bindings of
			NONE => ()
		      | SOME (_,func) => 
			let
			    val _ = trace ["binding special identity for class ", fmtName name]
			    val cell = func regs
			in
			    cell := SOME obj
			end
		end
	      | _ => ()
	end
	
and getSpecialPrototype (regs:Mach.REGS)
                        (id:Mach.OBJ_IDENT)
    : (Mach.VAL * bool) option =
    if not (Mach.isBooting regs)
    then NONE
    else 
        let
            fun getExistingProto (q:Mach.REGS -> (Mach.OBJ option) ref)
                : (Mach.VAL * bool) option =
                let
		    val _ = trace ["fetching existing proto"]
                    val objOptRef = q regs 
                    fun propNotFound _ = Mach.Null
                in
                    case !objOptRef of 
                        NONE => NONE
                      | SOME obj => 
                        SOME ((getValueOrVirtual 
                                   regs obj Name.nons_prototype 
                                   false propNotFound), false)
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
                 (Mach.getStringClassSlot, 
                  (fn _ => SOME (newObject regs, true))),
                 (Mach.getStringWrapperClassSlot,
                  (fn _ => getExistingProto Mach.getStringClassSlot)),
                 
                 (Mach.getIntClassSlot, 
                  (fn _ => SOME (newObject regs, true))),
                 (Mach.getUintClassSlot,
                  (fn _ => getExistingProto Mach.getIntClassSlot)),
                 (Mach.getDecimalClassSlot,
                  (fn _ => getExistingProto Mach.getIntClassSlot)),
                 (Mach.getDoubleClassSlot,
                  (fn _ => getExistingProto Mach.getIntClassSlot)),
                 (Mach.getNumberClassSlot, 
                  (fn _ => getExistingProto Mach.getIntClassSlot)),
                 
                 (Mach.getBooleanClassSlot,
                  (fn _ => SOME (newObject regs, true))),
                 (Mach.getBooleanWrapperClassSlot,
                  (fn _ => getExistingProto Mach.getBooleanClassSlot)),
                 
                 (Mach.getArrayClassSlot,
                  (fn _ => SOME (newObject regs, true)))
                ]
        end

and initClassPrototype (regs:Mach.REGS)
                       (obj:Mach.OBJ)
    : unit =
    let
	val Mach.Obj { ident, props, magic, ... } = obj
    in
	case !magic of 
	    SOME (Mach.Class {cls=Ast.Cls {name, extends,...},...}) => 
	    let
		val baseProtoVal =
		    case extends of
			NONE => Mach.Null
		      | SOME baseClassTy =>
			let
			    val ty = AstQuery.needInstanceType (evalTy regs baseClassTy)
			    val ob = instanceClass regs ty
			in                        
			    if hasOwnValue ob Name.nons_prototype
			    then getValue regs ob Name.nons_prototype
			    else Mach.Null
			end
		val _ = trace ["initializing prototype"]
		val (newPrototype, setConstructor) = 
		    case getSpecialPrototype regs ident of
			SOME (p,b) => (needObj regs p, b)
		      | NONE => (newObj regs, true)
		val newPrototype = Mach.setProto newPrototype baseProtoVal
	    in
		trace ["setting proto on ",
		       fmtName name,
		       ": (obj #", Int.toString ident, ").prototype = ", 
		       "(obj #", Int.toString (getObjId newPrototype), ")"];
		defValue regs obj Name.nons_prototype (Mach.Object newPrototype);
		Mach.setPropDontEnum props Name.nons_prototype true;
		if setConstructor
		then 
		    setValueOrVirtual regs newPrototype 
				      Name.nons_constructor 
				      (Mach.Object obj) 
				      false
		else 
		    ();
		trace ["finished initialising class prototype"]
	    end
	  | _ => ()
    end
    
and constructClassInstance (regs:Mach.REGS)
                           (classObj:Mach.OBJ)
                           (classClosure:Mach.CLS_CLOSURE)
                           (args:Mach.VAL list)
    : Mach.VAL =
    let
        val Mach.Obj { ident, ... } = classObj
        val {cls = Ast.Cls { name, ...}, ...} = classClosure
        val _ = Mach.push regs ("new " ^ (Ustring.toAscii (#id name))) args
	val obj = 
	    case constructSpecial regs ident classObj classClosure args of
		SOME ob => ob
              | NONE => constructStandard regs classObj classClosure args			
    in
        bindAnySpecialIdentity regs obj;
	initClassPrototype regs obj;
	Mach.pop regs;
	Mach.Object obj
    end


(*
 * ES-262-3 8.6.2.1 [[Get]](P)
 *
 * FIXME: no idea if this makes the most sense given
 * the ES-262-3 meaning of the operation.
 *)
and get (regs:Mach.REGS)
(obj:Mach.OBJ)
        (n:Ast.NAME)
    : Mach.VAL =
    let
        fun tryObj ob =
            if hasOwnValue ob n
            then getValue regs ob n
            else
                case obj of
                    Mach.Obj { proto, ... } =>
                    (case (!proto) of
                         Mach.Object p => tryObj p
                       | _ => Mach.Undef)
    in
        tryObj obj
    end

(*
 HEAD
 *)

and evalHead (regs:Mach.REGS)
             (head:Ast.HEAD)
    : Mach.REGS =
    let
        val (Ast.Head (rib,inits)) = head
        val obj = Mach.newObjNoTag ()
        val regs = extendScopeReg regs obj Mach.TempScope
        val {scope,...} = regs
        val _ = trace ["built temp scope #",
                       Int.toString (getScopeId (#scope regs)),
                       " for head"]
    in
        allocScopeRib regs rib;
        evalInits regs obj (getScopeTemps scope) inits;
        regs
    end

(*
 BLOCK
 *)

and evalBlock (regs:Mach.REGS)
              (block:Ast.BLOCK)
    : Mach.VAL =
    let
        val Ast.Block {head, body, loc, ...} = block
        val _ = LogErr.setLoc loc
        val blockRegs = evalHead regs (valOf head)
        val _ = LogErr.setLoc loc
        val res = evalStmts blockRegs body
        val _ = LogErr.setLoc loc
    in
        res
    end


and evalClassBlock (regs:Mach.REGS)
                   (classBlock)
    : Mach.VAL =

    (* 
     * The property that holds the class object was allocated when the
     * rib of the outer scope were allocated. Still to do is
     * initialising the properties *of* the class object.
     *)

    let
        val {name, block, ...} = classBlock
        val {scope, ...} = regs
        val name = valOf name

        val _ = trace ["evaluating class stmt for ", fmtName name]

        val classObj = needObj regs (findVal regs scope name)

        (* FIXME: might have 'this' binding wrong in class scope *)
        val _ = trace ["extending scope for class block of ", fmtName name]
        val classRegs = extendScopeReg regs classObj Mach.InstanceScope
    in
        trace ["evaluating class block of ", fmtName name];
        evalBlock classRegs block
    end


and evalIfStmt (regs:Mach.REGS)
               (cnd:Ast.EXPR)
               (thn:Ast.STMT)
               (els:Ast.STMT)
    : Mach.VAL =
    let
        val v = evalExpr regs cnd
        val b = toBoolean v
    in
        if b
        then evalStmt regs thn
        else evalStmt regs els
    end


and evalLabelStmt (regs:Mach.REGS)
                  (lab:Ast.IDENT)
                  (s:Ast.STMT)
    : Mach.VAL =
    evalStmt regs s
    handle BreakException exnLabel =>
           if labelMatch [lab] exnLabel
           then Mach.Undef
           else raise BreakException exnLabel


and evalWhileStmt (regs:Mach.REGS)
                  (whileStmt:Ast.WHILE_STMT)
    : Mach.VAL =
    case whileStmt of
        { cond, body, rib, labels } =>
        let
            val accum = ref Mach.Undef
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


and evalDoWhileStmt (regs:Mach.REGS)
                    (whileStmt:Ast.WHILE_STMT)
    : Mach.VAL =
    case whileStmt of
        { cond, body, rib, labels } =>
        let
            val accum = ref Mach.Undef
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


and evalWithStmt (regs:Mach.REGS)
                 (obj:Ast.EXPR)
                 (ty:Ast.TY)
                 (body:Ast.STMT)
    : Mach.VAL =
    let
        val v = evalExpr regs obj
        val ob = needObj regs v
        val s = extendScope (#scope regs) ob Mach.WithScope
        val regs = withScope regs s
        val regs = withThis regs ob
    in
        evalStmt regs body
    end

and evalSwitchStmt (regs:Mach.REGS)
                   (cond:Ast.EXPR)
                   (cases:Ast.CASE list)
                   (labels:Ast.IDENT list)
    : Mach.VAL =
    let
        fun tryCases (v:Mach.VAL) [] = Mach.Undef
          | tryCases (v:Mach.VAL) ({label, inits, body}::cs) =
            if (case label of
                    NONE => true
                  | SOME e => doubleEquals regs v (evalExpr regs e))
            then
                (* FIXME: This will change when switch stmt bodies are
                 * reorganized and get a proper head. *)
                let
                    val head = Ast.Head ([], case inits of NONE => []
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
               then Mach.Undef
               else raise BreakException exnLabel
    end


and evalSwitchTypeStmt (regs:Mach.REGS)
                       (cond:Ast.EXPR)
                       (ty:Ast.TY)
                       (cases:Ast.CATCH_CLAUSE list)
    : Mach.VAL =
    let
        val v = evalExpr regs cond
    in
        case catch regs v cases of
            NONE => Mach.Undef
          | SOME v => v
    end

(*
 FOR_STMT
     structural subtype test in evalIterable
                                    TODO isEach
 *)

and evalIterable (regs:Mach.REGS)
                 (obj:Ast.EXPR)
    : Mach.OBJ =
    let
        val v = evalExpr regs obj
    in
        (*
         * Implement the IE JScript quirk where for (i in null) and
         * for (i in undefined) do not loop at all by returning an
         * empty object for Undef and Null.
         *)
        case v of
            Mach.Object ob => ob
          | Mach.Undef => newObj regs
          | Mach.Null => newObj regs
    end

and callIteratorGet (regs:Mach.REGS)
                    (iterable:Mach.OBJ)
    : Mach.OBJ =
    let
        val Mach.Obj { props, ... } = iterable
        fun f (name:Ast.NAME, prop:Mach.PROP, (curr:Mach.VAL list)) =
            case prop of
                { state = Mach.ValProp _,
                  attrs = { dontEnum = false, ... },
                  ... } =>
                (case name of
                     { ns = Ast.Public key, id = ident } =>
                     if key = Ustring.empty
                     then
                         (newString regs ident) :: curr
                     else
                         (newName regs name) :: curr
                   | _ => (newName regs name) :: curr)
              | _ => curr
        val iterator = needObj regs (newArray regs (NameMap.foldri f [] (!props)))
    in
        setValue regs iterator Name.nons_cursor (newInt regs 0);
        iterator
    end

and callIteratorNext (regs:Mach.REGS)
                     (iterator:Mach.OBJ)
    : Mach.VAL =
    let
        val lengthValue = getValue regs iterator Name.nons_length
        val length      = toInt32 regs lengthValue
        val cursorValue = getValue regs iterator Name.nons_cursor
        val cursor      = toInt32 regs cursorValue
    in
        if cursor < length
        then
            let
                val nextName       = Name.nons (Ustring.fromInt32 cursor)
                val newCursorValue = newInt regs (cursor + 1)
            in
                setValue regs iterator Name.nons_cursor newCursorValue;
                getValue regs iterator nextName
            end
        else
            raise Mach.StopIterationException
    end

and evalForInStmt (regs:Mach.REGS)
                  (forInStmt:Ast.FOR_ENUM_STMT)
    : Mach.VAL =
    case forInStmt of
        { isEach, defn, obj, rib, next, labels, body, ... } =>
        let
            val iterable = evalIterable regs obj
            val iterator = callIteratorGet regs iterable
            val forInRegs = evalHead regs (Ast.Head (valOf rib, []))

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
                    Ast.ExprStmt e =>
                    let
                        val (ty, e) = getExpectedType regs e
                    in
                        case e of
                            Ast.InitExpr (target, head, inits) =>
                            (target, head, inits, NONE)
                          | Ast.LetExpr {defs, body, head = SOME (Ast.Head (f, i))} =>
                            (Ast.Hoisted, Ast.Head (f, []), i, SOME body)
                          | _ => LogErr.internalError ["evalForInStmt: invalid expr structure"]
                    end
                  | _ => LogErr.internalError ["evalForInStmt: invalid stmt structure"]
                         
            (*
             * A binding init on the lhs of 'in' will be included in nextHead
             * and evaluated once when the tempRegs is created here
             *)

            val tempRegs = evalHead forInRegs nextHead
            val temps = getScopeTemps (#scope tempRegs)

            fun loop (accum:Mach.VAL option) =
                let
                    val v = (SOME (callIteratorNext forInRegs iterator)
                             handle Mach.StopIterationException =>
                                    NONE)
                    val b = (case v of
                                 NONE => false
                               | x => true)
                in
                    if b
                    then
                        let
                            val _ = Mach.defTemp temps 0 (valOf v)   (* def the iteration value *)
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
                NONE => Mach.Undef
              | SOME v => v
        end

and evalForStmt (regs:Mach.REGS)
                (forStmt:Ast.FOR_STMT)
    : Mach.VAL =
    case forStmt of
        { rib, init, cond, update, labels, body, ... } =>
        let
            val forRegs = evalHead regs (Ast.Head (valOf rib, []))

            fun loop (accum:Mach.VAL option) =
                let
                    val (ty, cond) = getExpectedType regs cond
                    val b = case cond of
                                Ast.ListExpr [] => true
                              | _ => toBoolean (checkCompatible regs ty (evalExpr forRegs cond))
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
                NONE => Mach.Undef
              | SOME v => v
        end


and evalReturnStmt (regs:Mach.REGS)
                   (e:Ast.EXPR)
    : Mach.VAL =
    raise (ReturnException (evalExpr regs e))


and evalThrowStmt (regs:Mach.REGS)
                  (e:Ast.EXPR)
    : Mach.VAL =
    raise (ThrowException (evalExpr regs e))


and evalBreakStmt (regs:Mach.REGS)
                  (lbl:Ast.IDENT option)
    : Mach.VAL =
    (trace ["raising BreakException ",
            case lbl of NONE => "empty" 
                      | SOME id => Ustring.toAscii id];
     raise (BreakException lbl))

    
and evalContinueStmt (regs:Mach.REGS)
                     (lbl:Ast.IDENT option)
    : Mach.VAL =
    raise (ContinueException lbl)


and evalAnonFragment (regs:Mach.REGS)
                     (block:Ast.BLOCK)
    : Mach.VAL =
    let
        fun findHoistingScopeObj scope = 
            case scope of 
                Mach.Scope { kind = Mach.InstanceScope, ... } => scope
              | Mach.Scope { kind = Mach.ActivationScope, ... } => scope
              | Mach.Scope { parent = NONE, ... } => scope
              | Mach.Scope { parent = SOME p, ...} => findHoistingScopeObj p
        val scope = findHoistingScopeObj (#scope regs)
        val regs = withScope regs scope
                
        val Ast.Block { pragmas, head, body, loc, ... } = block
        val (rib, inits) = case head of 
                               NONE => ([], [])
                             | SOME (Ast.Head (r,i)) => (r,i)
        val Mach.Scope { object, temps, ... } = scope

        val _ = LogErr.setLoc loc
        val _ = allocScopeRib regs rib

        val _ = LogErr.setLoc loc
        val _ = evalInits regs object temps inits

        val _ = LogErr.setLoc loc
        val res = evalStmts regs body
        val _ = LogErr.setLoc loc
    in
        res
    end

and evalFragment (regs:Mach.REGS)
                 (frag:Ast.FRAGMENT)
    : Mach.VAL =
    let
        fun lastVal [] = Mach.Undef
          | lastVal x = List.last x
    in
        (case frag of 
             Ast.Unit { fragments, ... } => 
             lastVal (map (evalFragment regs ) fragments)
           | Ast.Package { name, fragments } => 
             let
                 val n = LogErr.join "." (map Ustring.toAscii name)
                 val _ = trace ["entering package fragment: ", n]
                 val res = lastVal (map (evalFragment regs ) fragments)
                 val _ = trace ["leaving package fragment: ", n]
             in
                 res
             end
           | Ast.Anon (Ast.Block {head=NONE, ...}) => 
             error regs ["top-level block with no head"]
           | Ast.Anon (Ast.Block {head=SOME (Ast.Head (rib, inits)), body, loc, ...}) => 
             (* 
              * NB: do *not* do evalBlock here. It's not a "normal" block. The ribs 
              * and inits are not intended for a temporary scope, but rather the 
              * scope that you'd search for as a hoisting target (either the activation 
              * scope enclosing an eval, or the global scope)
              *)
             let
                 val _ = trace ["entering anon unit block"]
                 val { scope, ... } = regs
                 val Mach.Scope { temps, ...} = scope
                 val obj = findTargetObj regs scope Ast.Hoisted
                 val Mach.Obj { ident, ... } = obj
             in
                 trace ["resolved anonymous fragment target to obj #", Int.toString ident];
                 LogErr.setLoc loc;
                 allocObjRib regs obj NONE rib;
                 LogErr.setLoc loc;
                 trace ["allocating anonymous fragment inits on obj #", Int.toString ident];
                 evalInits regs obj temps;
                 LogErr.setLoc loc;
                 trace ["running anonymous fragment stmts"];
                 evalStmts regs body
             end)
        handle ThrowException v =>
               let
                   val loc = !LogErr.loc
                   val exnStr = Ustring.toAscii (toUstring regs v)
               in
                   LogErr.setLoc loc;
                   error regs ["uncaught exception: ", Ustring.toAscii (toUstring regs v)]
               end
    end

and evalTopFragment (regs:Mach.REGS)
                    (frag:Ast.FRAGMENT)
    : Mach.VAL =
    let
        val _ = LogErr.setLoc NONE
        val res = evalFragment regs frag
        val _ = Mach.reportProfile regs
    in
        res
    end
end
