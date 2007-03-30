(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Eval = struct 

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[eval] " :: ss) else ()
fun error ss = LogErr.evalError ss

(* Exceptions for object-language control transfer. *)
exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> Mach.VAL)
exception ThrowException of Mach.VAL
exception ReturnException of Mach.VAL

exception InternalError

val thisName = { id = "this", ns = Ast.Internal "" }

fun extendScope (p:Mach.SCOPE) 
                (ob:Mach.OBJ)
                (isVarObject:bool) 
    : Mach.SCOPE = 
    Mach.Scope { parent=(SOME p), object=ob, temps=ref [], isVarObject=isVarObject }


fun getScopeObj (scope:Mach.SCOPE) 
    : Mach.OBJ = 
    case scope of 
        Mach.Scope { object, ...} => object


fun getScopeTemps (scope:Mach.SCOPE) 
    : Mach.TEMPS = 
    case scope of 
        Mach.Scope { temps, ...} => temps


fun needNamespace (v:Mach.VAL) 
    : Ast.NAMESPACE = 
    case v of 
        Mach.Object (Mach.Obj ob) => 
        (case !(#magic ob) of 
             SOME (Mach.Namespace n) => n
           | _ => error ["need namespace"])
      | _ => error ["need namespace"]


fun needObj (v:Mach.VAL) 
    : Mach.OBJ = 
    case v of 
        Mach.Object ob => ob
      | _ => error ["need object"]

(* 
 * A small number of functions do not fully evaluate to Mach.VAL 
 * values, but instead to REFs; these are temporary artifacts of
 * evaluation, not first-class values in the language. 
 *)

type REF = (Mach.OBJ * Ast.NAME)

(* Fundamental object methods *)

(* FIXME: possibly move this to mach.sml *) 

fun allocFixtures (scope:Mach.SCOPE) 
                  (obj:Mach.OBJ) 
                  (temps:Mach.TEMPS)
                  (f:Ast.FIXTURES) 
    : unit =
    case obj of 
        Mach.Obj { props, ...} => 
        let 
            val methodScope = extendScope scope obj false
            fun valAllocState (t:Ast.TYPE_EXPR) 
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

                case t of 
                    Ast.SpecialType (Ast.Any) => 
                    Mach.ValProp (Mach.Undef)

                  | Ast.SpecialType (Ast.Null) => 
                    Mach.ValProp (Mach.Null)

                  | Ast.SpecialType (Ast.Undefined) => 
                    Mach.ValProp (Mach.Undef)

                  | Ast.SpecialType (Ast.VoidType) => 
                    error ["attempt to allocate void-type property"]

                  (* FIXME: is this correct? Maybe we need to check them all to be nullable? *)
                  | Ast.UnionType _ => 
                    Mach.ValProp (Mach.Null)

                  | Ast.ArrayType _ => 
                    Mach.ValProp (Mach.Null)

                  | Ast.TypeName ident => 
                    (* FIXME: resolve nominal type to class or interface, check to see if 
                     * it is nullable, *then* decide whether to set to null or uninit. *)
                    Mach.ValProp (Mach.Null)

                  | Ast.FunctionType _ => 
                    Mach.UninitProp

                  | Ast.ObjectType _ => 
                    Mach.ValProp (Mach.Null)

                  | Ast.AppType {base, ...} => 
                    valAllocState base

                  | Ast.NullableType { expr, nullable=true } => 
                    Mach.ValProp (Mach.Null)
                    
                  | Ast.NullableType { expr, nullable=false } => 
                    Mach.UninitProp

                  | Ast.ElementTypeRef _ => 
                    Mach.ValProp (Mach.Undef)  (* FIXME: should get the type of the element from the array type *)
 
                  | Ast.FieldTypeRef _ => (* FIXME: get type from object type *)
                    Mach.ValProp (Mach.Undef)

            fun tempPadding n =
                if n = 0
                    then []
                    else (Ast.SpecialType Ast.Any, Mach.UninitTemp)::(tempPadding (n-1))

            fun allocFixture (n, f) = 
                case n of 
                    Ast.TempName t => 
                    (case f of 
                         Ast.ValFixture { ty, ... } =>  (* FIXME: temp types are not needed, use the value tag for rt typechecking *)
                         (if t = (List.length (!temps))
                          then (trace ["allocating fixture for temporary ", Int.toString t];
                                temps := (Ast.SpecialType Ast.Any, Mach.UninitTemp)::(!temps)) 
                          else if t < (List.length (!temps))
                                then (trace ["ignoring fixture, already allocated ", Int.toString t];
                                      temps := (List.take (!temps,((length (!temps))-t-1)))@((ty, Mach.UninitTemp)::(List.drop (!temps,(length (!temps)-t))))) 
                          else (trace ["allocating fixtures for temporaries ", Int.toString (length (!temps)), " to ", Int.toString t];
                                temps := (Ast.SpecialType Ast.Any, Mach.UninitTemp)
                                            ::(((tempPadding (t-(length (!temps))))@(!temps)))))  
                       | _ => error ["allocating non-value temporary"])
                  | Ast.PropName pn => 
                    let 
                        fun allocProp state p = 
                            if Mach.hasProp props pn
                            then error ["allocating duplicate property name: ", 
                                        LogErr.name pn]
                            else (trace ["allocating fixture for ", state, " property ", 
                                         LogErr.name pn]; 
                                  Mach.addProp props pn p)                            
                    in 
                        case f of 
                            Ast.TypeFixture te => 
                            allocProp "type" 
                                      { ty = te,
                                        state = Mach.TypeProp,                                   
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = true,
                                                  isFixed = true } }

                          | Ast.MethodFixture { func, ty, readOnly, ... } => 
                            let
                                val Ast.Func { isNative, ... } = func
                                val v = if isNative
                                        then Mach.newNativeFunction (Mach.getNativeFunction pn)
                                        else Mach.newFunc methodScope func
                            in
                                allocProp "method" 
                                          { ty = ty,
                                            state = Mach.ValProp v,
                                            attrs = { dontDelete = true,
                                                      dontEnum = true,
                                                      readOnly = readOnly,
                                                      isFixed = true } }
                            end

                          | Ast.ValFixture { ty, readOnly, ... } => 
                            allocProp "value" 
                                      { ty = ty,
                                        state = valAllocState ty,
                                        attrs = { dontDelete = true,
                                                  dontEnum = false,
                                                  readOnly = readOnly,
                                                  isFixed = true } }
                            
                          | Ast.VirtualValFixture { ty, getter, setter, ... } => 
                            let
                                val getFn = case getter of
                                                NONE => NONE
                                              | SOME f => SOME (Mach.newFunClosure methodScope (#func f))
                                val setFn = case setter of
                                                NONE => NONE
                                              | SOME f => SOME (Mach.newFunClosure methodScope (#func f))
                            in
                                allocProp "virtual value" 
                                          { ty = ty,
                                            state = Mach.VirtualValProp { getter = getFn,
                                                                          setter = setFn },
                                            attrs = { dontDelete = true,
                                                      dontEnum = false,
                                                      readOnly = true,
                                                      isFixed = true } }
                            end
                            
                          | Ast.ClassFixture cls =>
                            let
                                val Ast.Cls {classFixtures,...} = cls
                                val Mach.Object classObj = Mach.newClass scope cls
                                val _ = allocObjFixtures scope classObj classFixtures
                            in 
                            allocProp "class"
                                      { ty = (Name.typename Name.public_Class),
                                        state = Mach.ValProp (Mach.Object classObj),
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = true,
                                                  isFixed = true } }
                            end
                            
                          | Ast.NamespaceFixture ns => 
                            allocProp "namespace" 
                                      { ty = (Name.typename Name.public_Namespace),
                                        state = Mach.ValProp (Mach.newNamespace ns),
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = true,
                                                  isFixed = true } }
                            
                          | Ast.TypeVarFixture =>
                            allocProp "type variable"
                                      { ty = (Name.typename Name.public_Type),
                                        state = Mach.TypeVarProp,
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = true,
                                                  isFixed = true } }
                    end
        in                    
            List.app allocFixture f
        end

    
and  allocObjFixtures (scope:Mach.SCOPE) 
                     (obj:Mach.OBJ) 
                     (f:Ast.FIXTURES) 
    : unit = 
    let 
        val (temps:Mach.TEMPS) = ref [] 
    in
        allocFixtures scope obj temps f;
        if not ((length (!temps)) = 0)
        then error ["allocated temporaries in non-scope object"]
        else ()
    end
    
fun allocScopeFixtures (scope:Mach.SCOPE) 
                       (f:Ast.FIXTURES) 
    : unit = 
    case scope of
        Mach.Scope { object, temps, ...} => 
        allocFixtures scope object temps f


fun hasOwnValue (obj:Mach.OBJ) 
                (n:Ast.NAME) 
    : bool = 
    case obj of 
        Mach.Obj { props, ... } => 
        Mach.hasProp props n


fun hasValue (obj:Mach.OBJ) 
             (n:Ast.NAME) 
    : bool = 
    if hasOwnValue obj n
    then true
    else (case obj of 
              Mach.Obj { proto, ... } => 
              case (!proto) of 
                  Mach.Object p => hasValue p n
                | _ => false)


(* 
 * *Similar to* ES3 8.7.1 GetValue(V), there's 
 * no Reference type in ES4.
 *)
fun getValue (obj:Mach.OBJ, 
              name:Ast.NAME) 
    : Mach.VAL = 
    case obj of 
        Mach.Obj { props, ... } => 
        let 
            val prop = Mach.getProp props name
        in
            case (#state prop) of 
                Mach.TypeProp => 
                error ["getValue on a type property: ",
                       LogErr.name name]

              | Mach.TypeVarProp => 
                error ["getValue on a type variable property: ",
                       LogErr.name name]

              | Mach.UninitProp => 
                error ["getValue on an uninitialized property: ",
                       LogErr.name name]

              | Mach.VirtualValProp { getter = SOME g, ... } => 
                invokeFuncClosure obj g []

              | Mach.VirtualValProp { getter = NONE, ... } => 
                error ["getValue on a virtual property w/o getter: ",
                       LogErr.name name]

              | Mach.ValProp v => v
        end


and setValue (base:Mach.OBJ) 
             (name:Ast.NAME) 
             (v:Mach.VAL) 
    : unit = 
    case base of 
        Mach.Obj { props, ... } => 
        if Mach.hasProp props name
        then 
            let 
                val existingProp = Mach.getProp props name                                   
                val existingAttrs = (#attrs existingProp)
                val newProp = { state = Mach.ValProp v,
                                ty = (#ty existingProp), 
                                attrs = existingAttrs }
            in
                case (#state existingProp) of 
                    Mach.UninitProp => 
                    error ["setValue on uninitialized property", 
                           LogErr.name name]
                    
                  | Mach.TypeVarProp => 
                    error ["setValue on type variable property:", 
                           LogErr.name name]
                    
                  | Mach.TypeProp => 
                    error ["setValue on type property: ", 
                           LogErr.name name]
                    
                  | Mach.VirtualValProp { setter = SOME s, ... } => 
                    (invokeFuncClosure base s [v]; ())
                    
                  | Mach.VirtualValProp { setter = NONE, ... } => 
                    error ["setValue on virtual property w/o setter: ", 
                           LogErr.name name]
                    
                  | Mach.ValProp _ => 
                    if (#readOnly existingAttrs)
                    then error ["setValue on read-only property"]
                    else ((* FIXME: insert typecheck here *)
                          Mach.delProp props name;
                          Mach.addProp props name newProp)
            end
        else
            let 
                val prop = { state = Mach.ValProp v,
                             ty = Ast.SpecialType Ast.Any,
                             attrs = { dontDelete = false,
                                       dontEnum = false,
                                       readOnly = false,
                                       isFixed = false } }
            in
                Mach.addProp props name prop
            end


(* A "defValue" call occurs when assigning a property definition's 
 * initial value, as specified by the user. All other assignments
 * to a property go through "setValue". *)

and defValue (base:Mach.OBJ) 
             (name:Ast.NAME) 
             (v:Mach.VAL) 
    : unit =
    case base of 
        Mach.Obj { props, ... } => 
        if not (Mach.hasProp props name)
        then error ["defValue on missing property: ", LogErr.name name]
        else 
            (* 
             * defProp has relaxed rules: you can write to an 
             * uninitialized property or a read-only property. 
             *)
            let 
                val existingProp = Mach.getProp props name
                val newProp = { state = Mach.ValProp v,
                                ty = (#ty existingProp), 
                                attrs = (#attrs existingProp) }
                fun writeProp _ = 
                    ((* FIXME: insert typecheck here *)
                     Mach.delProp props name;
                     Mach.addProp props name newProp)
            in       
                case (#state existingProp) of                     
                    Mach.TypeVarProp => 
                    error ["defValue on type variable property: ", 
                           LogErr.name name]
                    
                  | Mach.TypeProp => 
                    error ["defValue on type property: ", 
                           LogErr.name name]
                    
                  | Mach.VirtualValProp { setter = SOME s, ... } => 
                    (invokeFuncClosure base s [v]; ())
                    
                  | Mach.VirtualValProp { setter = NONE, ... } => 
                    error ["defValue on virtual property w/o setter: ", 
                           LogErr.name name]
                    
                  | Mach.UninitProp => writeProp ()
                  | Mach.ValProp _ => writeProp ()
            end
            
    
and evalExpr (scope:Mach.SCOPE) 
             (expr:Ast.EXPR)
    : Mach.VAL =
    case expr of
        Ast.LiteralExpr lit => 
        evalLiteralExpr scope lit
        
      | Ast.ListExpr es =>
        evalListExpr scope es
        
      | Ast.LexicalRef { ident, pos } =>
        (LogErr.setPos pos;
         getValue (evalRefExpr scope expr true))

      | Ast.ObjectRef { base, ident, pos } => 
        (LogErr.setPos pos;
         getValue (evalRefExpr scope expr false))
        
      | Ast.LetExpr {defs, body, head} =>
        evalLetExpr scope (valOf head) body

      | Ast.TrinaryExpr (Ast.Cond, aexpr, bexpr, cexpr) => 
        evalCondExpr scope aexpr bexpr cexpr

      | Ast.BinaryExpr (bop, aexpr, bexpr) => 
        evalBinaryOp scope bop aexpr bexpr

      | Ast.UnaryExpr (unop, expr) => 
        evalUnaryOp scope unop expr

      | Ast.ThisExpr => 
        findVal scope (multinameOf {id="this", ns=Ast.Internal ""})

      | Ast.SetExpr (aop, pat, expr) => 
        evalSetExpr scope aop pat (evalExpr scope expr)

      | Ast.CallExpr { func, actuals } => 
        let
            val args = map (evalExpr scope) actuals
            fun withLhs e = 
                case evalRefExpr scope e true of 
                    (obj, name) => evalCallExpr 
                                       (SOME obj) 
                                       (needObj (getValue (obj, name))) 
                                       args
        in
            case func of 
                Ast.LexicalRef _ => withLhs func
              | Ast.ObjectRef _ => withLhs func
              | _ => evalCallExpr NONE (needObj (evalExpr scope func)) args
        end

      | Ast.NewExpr { obj, actuals } => 
        let
            val args = map (evalExpr scope) actuals
        in
            evalNewExpr (needObj (evalExpr scope obj)) args
        end

      | Ast.GetTemp n => 
        Mach.getTemp (getScopeTemps scope) n

      | Ast.InitExpr (target,temps,inits) =>
        let
            val tempScope = evalHead scope temps false
        in
            evalScopeInits tempScope target inits;
            Mach.Undef
        end

      | _ => LogErr.unimplError ["unhandled expression type"]


and evalLiteralArrayExpr (scope:Mach.SCOPE)
                         (exprs:Ast.EXPR list)
                         (ty:Ast.TYPE_EXPR option)
    : Mach.VAL =
    let 
        val vals = map (evalExpr scope) exprs
        val tys = case ty of 
                      NONE => [Ast.SpecialType Ast.Any]
                    | SOME (Ast.ArrayType tys) => tys
                    (* FIXME: hoist this to parsing or defn; don't use
                     * a full TYPE_EXPR in LiteralArray. *)
                    | SOME _ => error ["non-array type on array literal"]
        val tag = Mach.ArrayTag tys
        (* FIXME: hook up to Array.prototype. *)
        val obj = Mach.newObj tag Mach.Undef NONE
        val (Mach.Obj {props, ...}) = obj
        fun putVal n [] = ()
          | putVal n (v::vs) = 
            let 
                val name = { ns = (Ast.Internal ""), id = (Int.toString n) }
                (* FIXME: this is probably incorrect wrt. Array typing rules. *)
                val ty = if n < (length tys) 
                         then List.nth (tys, n)
                         else (if (length tys) > 0
                               then List.last tys
                               else Ast.SpecialType Ast.Any)
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
    in
        putVal 0 vals;
        Mach.Object obj
    end

             
and evalLiteralObjectExpr (scope:Mach.SCOPE)
                          (fields:Ast.FIELD list)
                          (ty:Ast.TYPE_EXPR option)
    : Mach.VAL =
    let 
        fun searchFieldTypes n [] = Ast.SpecialType Ast.Any
          | searchFieldTypes n ({name,ty}::ts) = 
            if n = name 
            then ty
            else searchFieldTypes n ts
        val tys = case ty of 
                      NONE => []
                    | SOME (Ast.ObjectType tys) => tys
                    (* FIXME: hoist this to parsing or defn; don't use
                     * a full TYPE_EXPR in LiteralObject. *)
                    | SOME _ => error ["non-object type on object literal"]
        val tag = Mach.ObjectTag tys
        (* FIXME: hook up to Object.prototype. *)
        val obj = Mach.newObj tag Mach.Undef NONE
        val (Mach.Obj {props, ...}) = obj
        fun processField {kind, name, init} = 
            let 
                val const = case kind of 
                                Ast.Const => true
                              | Ast.LetConst => true
                              | _ => false
                val n = { ns = Ast.Internal "", 
                          id = (#id (evalIdentExpr scope name)) }
                val v = evalExpr scope init
                val ty = searchFieldTypes (#id n) tys
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


and evalLiteralExpr (scope:Mach.SCOPE) 
                    (lit:Ast.LITERAL) 
    : Mach.VAL = 
    case lit of 
        Ast.LiteralNull => Mach.Null
      | Ast.LiteralUndefined => Mach.Undef
      | Ast.LiteralDouble r => Mach.newDouble r
      | Ast.LiteralDecimal d => Mach.newDecimal d
      | Ast.LiteralInt i => Mach.newInt i
      | Ast.LiteralUInt u => Mach.newUInt u
      | Ast.LiteralBoolean b => Mach.newBoolean b
      | Ast.LiteralString s => Mach.newString s
      | Ast.LiteralArray {exprs, ty} => evalLiteralArrayExpr scope exprs ty
      | Ast.LiteralObject {expr, ty} => evalLiteralObjectExpr scope expr ty
      | Ast.LiteralNamespace n => Mach.newNamespace n
      | Ast.LiteralFunction f => Mach.newFunc scope f
      | Ast.LiteralContextualDecimal _ => error ["contextual decimal literal at runtime"]
      | Ast.LiteralContextualDecimalInteger _ => error ["contextual decimal integer literal at runtime"]
      | Ast.LiteralContextualHexInteger _ => error ["contextual hex integer literal at runtime"]
      | _ => LogErr.unimplError ["unhandled literal type"]


and evalListExpr (scope:Mach.SCOPE) 
                 (es:Ast.EXPR list) 
    : Mach.VAL = 
    case es of 
        [] => Mach.Undef
      | [e] => evalExpr scope e
      | (e::ez) => ((evalExpr scope e); (evalListExpr scope ez))

                   
and constructObjectViaFunction (ctorObj:Mach.OBJ) 
                               (ctor:Mach.FUN_CLOSURE) 
                               (args:Mach.VAL list) 
    : Mach.VAL = 
    case ctorObj of 
        Mach.Obj { props, ... } => 
        let
            (* FIXME: the default prototype should be the initial Object prototype, 
             * as per ES3 13.2.2. *)
            val (proto:Mach.VAL) = 
                if Mach.hasProp props Name.public_prototype
                then getValue (ctorObj, Name.public_prototype)
                else Mach.Null
            val (newObj:Mach.OBJ) = 
                Mach.newObj (Mach.ClassTag Name.public_Object) proto NONE
        in
            case invokeFuncClosure newObj ctor args of 
                Mach.Object ob => Mach.Object ob
              | _ => Mach.Object newObj
        end


and evalNewExpr (obj:Mach.OBJ) 
                (args:Mach.VAL list) 
    : Mach.VAL =
    case obj of 
        Mach.Obj { magic, ... } => 
        case (!magic) of 
            SOME (Mach.Class c) => constructClassInstance obj c args
          | SOME (Mach.Function f) => constructObjectViaFunction obj f args
          | _ => error ["operator 'new' applied to unknown object"]

                                                                                 
and evalCallExpr (thisObjOpt:Mach.OBJ option) 
                 (fobj:Mach.OBJ) 
                 (args:Mach.VAL list) 
    : Mach.VAL =
    let 
        val _ = trace ["evalCallExpr"]
        val thisObj = case thisObjOpt of 
                          NONE => Mach.globalObject
                        | SOME t => t
        val thisVal = Mach.Object thisObj
    in
        case fobj of
            Mach.Obj { magic, ... } => 
            case !magic of 
                SOME (Mach.NativeFunction f) => 
                    f args
              | SOME (Mach.Function f) => 
                    (invokeFuncClosure thisObj f args
                        handle ReturnException v => v)
              | _ => 
                    if hasValue fobj Name.meta_invoke
                    then
                        let 
                            val invokeFn = getValue (fobj, Name.meta_invoke)
                        in
                            evalCallExpr NONE (needObj invokeFn) (thisVal :: args)
                        end
                    else error ["calling non-callable object"]
    end


and evalSetExpr (scope:Mach.SCOPE) 
                (aop:Ast.ASSIGNOP) 
                (lhs:Ast.EXPR) 
                (v:Mach.VAL) 
    : Mach.VAL = 
    let
        val (obj, name) = evalRefExpr scope lhs false
        val v =
            let 
                fun modifyWith bop = 
                    performBinop bop (getValue (obj, name)) v
            in
                case aop of 
                    Ast.Assign => v
                  | Ast.AssignPlus mode => modifyWith (Ast.Plus mode)
                  | Ast.AssignMinus mode => modifyWith (Ast.Minus mode)
                  | Ast.AssignTimes mode => modifyWith (Ast.Times mode)
                  | Ast.AssignDivide mode => modifyWith (Ast.Divide mode)
                  | Ast.AssignRemainder mode => modifyWith (Ast.Remainder mode)
                  | Ast.AssignLeftShift => modifyWith Ast.LeftShift
                  | Ast.AssignRightShift => modifyWith Ast.RightShift
                  | Ast.AssignRightShiftUnsigned => modifyWith Ast.RightShiftUnsigned
                  | Ast.AssignBitwiseAnd => modifyWith Ast.BitwiseAnd
                  | Ast.AssignBitwiseOr => modifyWith Ast.BitwiseOr
                  | Ast.AssignBitwiseXor => modifyWith Ast.BitwiseXor
                  | Ast.AssignLogicalAnd => modifyWith Ast.LogicalAnd
                  | Ast.AssignLogicalOr => modifyWith Ast.LogicalOr
            end
    in
        trace ["setExpr assignment to slot ", LogErr.name name];
        setValue obj name v;
        v
    end
    

and evalUnaryOp (scope:Mach.SCOPE) 
                (unop:Ast.UNOP) 
                (expr:Ast.EXPR) 
    : Mach.VAL =
    let
        fun crement (mode:Ast.NUMERIC_MODE) decimalOp doubleOp intOp uintOp isPre = 
            let 
                val (obj, name) = evalRefExpr scope expr false
                val v = getValue (obj, name)

                fun asDecimal _ =                         
                    let 
                        val vd = Mach.toDecimal (#precision mode) (#roundingMode mode) v
                        val one = valOf (Decimal.fromStringDefault "1")
                    in
                        (Mach.newDecimal vd, 
                         Mach.newDecimal (decimalOp 
                                              (#precision mode) 
                                              (#roundingMode mode) 
                                              vd one))
                    end

                fun asDouble _ = 
                    let
                        val vd = Mach.toDouble v
                        val one = Real64.fromInt 1
                    in
                        (Mach.newDouble vd, Mach.newDouble (doubleOp (vd, one)))
                    end

                fun asInt _ = 
                    let
                        val vi = Mach.toInt32 v
                        val one = Int32.fromInt 1
                    in
                        (Mach.newInt vi, Mach.newInt (intOp (vi, one)))
                    end

                fun asUInt _ = 
                    let
                        val vu = Mach.toUInt32 v
                        val one = Word32.fromInt 1
                    in
                        (Mach.newUInt vu, Mach.newUInt (uintOp (vu, one)))
                    end
                    
                val (n, n') = 
                    case (#numberType mode) of 
                        Ast.Decimal => asDecimal ()         
                      | Ast.Double => asDouble ()
                      | Ast.Int => asInt ()
                      | Ast.UInt => asUInt ()
                      | Ast.Number => 
                        (* 
                         * FIXME: not clear in the spec what conversions to perform here. 
                         * Draft spec language says "converted to a number". Defaulting to
                         * "calling toNumeric but otherwise keeping the value in whatever 
                         * type it is before crement".
                         *)                        
                        (case Mach.needMagic (Mach.toNumeric v) of
                             Mach.Decimal _ => asDecimal ()
                           | Mach.Double _ => asDouble ()
                           | Mach.Int _ => asInt ()
                           | Mach.UInt _ => asUInt ()
                           | _ => error ["non-numeric operand to crement operation"])
            in
                setValue obj name n'; 
                if isPre then n' else n
            end
    in
        case unop of 
            Ast.Delete => 
            (case evalRefExpr scope expr false of
                 (Mach.Obj {props, ...}, name) => 
                 (Mach.delProp props name; Mach.newBoolean true))

          | Ast.PreIncrement mode => crement (valOf mode)
                                             (Decimal.add) 
                                             (Real64.+) 
                                             (Int32.+) 
                                             (Word32.+) 
                                             true

          | Ast.PreDecrement mode => crement (valOf mode)
                                             (Decimal.subtract) 
                                             (Real64.-) 
                                             (Int32.-) 
                                             (Word32.-) 
                                             true

          | Ast.PostIncrement mode => crement (valOf mode)
                                              (Decimal.add) 
                                              (Real64.+) 
                                              (Int32.+) 
                                              (Word32.+) 
                                              false

          | Ast.PostDecrement mode => crement (valOf mode)
                                              (Decimal.subtract) 
                                              (Real64.-) 
                                              (Int32.-) 
                                              (Word32.-) 
                                              false

          | Ast.BitwiseNot => 
            Mach.newInt (Int32.fromLarge 
                         (Word32.toLargeInt 
                              (Word32.notb 
                                   (Mach.toUInt32 
                                        (evalExpr scope expr)))))

          | Ast.LogicalNot => 
            Mach.newBoolean (not (Mach.toBoolean (evalExpr scope expr)))

          | Ast.UnaryPlus mode => 
            let 
                val v = evalExpr scope expr
                val mode = valOf mode
            in
                case (#numberType mode) of 
                    Ast.Decimal => Mach.newDecimal (Mach.toDecimal (#precision mode) 
                                                                   (#roundingMode mode) 
                                                                   v)
                  | Ast.Double => Mach.newDouble (Mach.toDouble v)
                  | Ast.Int => Mach.newInt (Mach.toInt32 v)
                  | Ast.UInt => Mach.newUInt (Mach.toUInt32 v)
                  | Ast.Number => Mach.toNumeric v
            end

          | Ast.UnaryMinus mode => 
            let 
                val v = evalExpr scope expr
                val mode = valOf mode
                fun asDecimal _ = Mach.newDecimal (Decimal.minus 
                                                       (#precision mode) 
                                                       (#roundingMode mode) 
                                                       (Mach.toDecimal (#precision mode)
                                                                       (#roundingMode mode)
                                                                       v))
                fun asDouble _ = Mach.newDouble (Real64.~ (Mach.toDouble v))
                fun asInt _ = Mach.newInt (Int32.~ (Mach.toInt32 v))
                fun asUInt _ = Mach.newUInt (Word32.~ (Mach.toUInt32 v))
            in
                case (#numberType mode) of 
                    Ast.Decimal => asDecimal ()
                  | Ast.Double => asDouble ()
                  | Ast.Int => asInt ()
                  | Ast.UInt => asUInt ()
                  | Ast.Number => 
                    (case Mach.needMagic (Mach.toNumeric v) of 
                         Mach.Decimal _ => asDecimal ()
                       | Mach.Double _ => asDouble ()
                       (* 
                        * FIXME: The "numbers" proposal says that when there's no pragma in effect,
                        * unary minus on int and uint stays in the representation if it can be 
                        * performed without loss of precision, else it's converted to double.
                        *
                        * It's not clear to me what "loss of precision" means on unary minus applied
                        * to twos complement numbers, since negation is just "flip bits and add 1". 
                        * Maybe it has to do with overflow? Until this is clarified I'll leave it 
                        * as "preserve the representation" in all cases.
                        *)
                       | Mach.Int _ => asInt ()
                       | Mach.UInt _ => asUInt ()
                       | _ => asDouble ())
            end

          | Ast.Void => Mach.Undef

          | Ast.Type => 
            (* 
             * FIXME: not clear what this operator does; I thought it just 
             * affected parse context. 
             *)
            evalExpr scope expr

          | Ast.Typeof => 
            (* 
             * ES3 1.4.3 backward-compatibility operation.
             *)
            let
                fun typeOfVal (v:Mach.VAL) = 
                    case v of 
                        Mach.Null => "null"
                      | Mach.Undef => "undefined"
                      | Mach.Object (Mach.Obj ob) => 
                        (case !(#magic ob) of 
                             SOME (Mach.UInt _) => "number"
                           | SOME (Mach.Int _) => "number"
                           | SOME (Mach.Double _) => "number"
                           | SOME (Mach.Decimal _) => "number"
                           | SOME (Mach.Bool _) => "boolean"
                           | SOME (Mach.Function _) => "function"
                           | SOME (Mach.NativeFunction _) => "function"
                           | SOME (Mach.String _) => "string"
                           | _ => "object")
            in
                Mach.newString 
                    (case expr of 
                         Ast.LexicalRef { ident, pos } => 
                         let 
                             val _ = LogErr.setPos pos
                             val multiname = evalIdentExpr scope ident
                         in
                             case resolveOnScopeChain scope multiname of 
                                 NONE => "undefined"
                               | SOME (obj, name) => typeOfVal (getValue (obj, name))
                         end
                       | _ => typeOfVal (evalExpr scope expr))
            end
    end


and performBinop (bop:Ast.BINOP) 
                 (a:Mach.VAL) 
                 (b:Mach.VAL) 
    : Mach.VAL = 

    let
        fun stringConcat _ = 
            Mach.newString ((Mach.toString a) ^ (Mach.toString b))

        fun dispatch (mode:Ast.NUMERIC_MODE) decimalOp doubleOp intOp uintOp largeOp =
            case (#numberType mode) of 
                Ast.Decimal => decimalOp (Mach.toDecimal 
                                              (#precision mode) 
                                              (#roundingMode mode) a) 
                                         (Mach.toDecimal 
                                              (#precision mode) 
                                              (#roundingMode mode) b)
              | Ast.Double => doubleOp (Mach.toDouble a) (Mach.toDouble b)
              | Ast.Int => intOp (Mach.toInt32 a) (Mach.toInt32 b)
              | Ast.UInt => uintOp (Mach.toUInt32 a) (Mach.toUInt32 b)
                            
              | Ast.Number => 
                (* 
                 * Ast.Number implies magic operand-based dispatch. 
                 * FIXME: Refactor this if you can figure out how. 
                 *)
                    
                if Mach.isDecimal a orelse Mach.isDecimal b
                then decimalOp (Mach.toDecimal
                                    (#precision mode) 
                                    (#roundingMode mode) a)
                               (Mach.toDecimal 
                                    (#precision mode) 
                                    (#roundingMode mode) b)
                else 
                    (if Mach.isDouble a orelse Mach.isDouble b
                     then (trace ["dynamic dispatch as double op"];
                           doubleOp (Mach.toDouble a) (Mach.toDouble b))
                     else
                         let
                             fun isIntegral x = Mach.isUInt x orelse Mach.isInt x
                             fun enlarge x = if Mach.isUInt x 
                                             then Word32.toLargeInt (Mach.toUInt32 x)
                                             else Int32.toLarge (Mach.toInt32 x)
                         in
                             if isIntegral a andalso isIntegral b
                             then 
                                 (trace ["dynamic dispatch as large op"];
                                  largeOp (enlarge a) (enlarge b))
                             else
                                 (trace ["dynamic dispatch as double op"];
                                  doubleOp (Mach.toDouble a) (Mach.toDouble b))
                         end)
                    
        fun dispatchComparison mode cmp =
            let
                fun decimalOp da db =
                    Mach.newBoolean (cmp (Decimal.compare (#precision mode) 
                                                          (#roundingMode mode) 
                                                          da db))
                fun doubleOp da db = 
                    Mach.newBoolean (cmp (Real64.compare (da, db)))
                fun intOp ia ib = 
                    Mach.newBoolean (cmp (Int32.compare (ia, ib)))
                fun uintOp ua ub = 
                    Mach.newBoolean (cmp (Word32.compare (ua, ub)))
                fun largeOp la lb = 
                    Mach.newBoolean (cmp (LargeInt.compare (la, lb)))
            in
                if Mach.isNumeric a andalso Mach.isNumeric b
                then dispatch mode decimalOp doubleOp intOp uintOp largeOp
                else Mach.newBoolean (cmp (String.compare ((Mach.toString a), 
                                                           (Mach.toString b))))
            end
            
        fun dispatchNumeric mode decimalFn doubleFn intFn uintFn largeFn =
            let
                fun decimalOp da db =
                    Mach.newDecimal (decimalFn (#precision mode) (#roundingMode mode) da db)
                fun doubleOp da db = 
                    Mach.newDouble (doubleFn (da, db))
                fun intOp ia ib = 
                    Mach.newInt (intFn (ia, ib))
                fun uintOp ua ub = 
                    Mach.newUInt (uintFn (ua, ub))
                fun largeOp la lb = 
                    let 
                        val x = largeFn (la, lb)
                    in
                        if Mach.fitsInInt x
                        then Mach.newInt (Int32.fromLarge x)
                        else (if Mach.fitsInUInt x
                              then Mach.newUInt (Word32.fromLargeInt x)
                              else (case Real64.fromString (LargeInt.toString x) of 
                                        SOME d => Mach.newDouble d
                                      | NONE => (case Decimal.fromStringDefault (LargeInt.toString x) of
                                                     SOME d => Mach.newDecimal d
                                                   | NONE => error ["arithmetic overflow"])))
                    end                                 
            in
                dispatch mode decimalOp doubleOp intOp uintOp largeOp
            end            

        fun masku5 (x:Word32.word) : Word.word = 
            Word.fromInt (Word32.toInt (Word32.andb (x, (valOf (Word32.fromString "0x1F")))))

        fun i2u (x:Int32.int) : Word32.word = 
            Word32.fromLargeInt (Int32.toLarge x)

        fun u2i (x:Word32.word) : Int32.int = 
            Int32.fromLarge (Word32.toLargeInt x)

        fun bitwiseWordOp f = 
            Mach.newUInt (f ((Mach.toUInt32 a),
                             (Mach.toUInt32 b)))

        fun pickRepByA (x:Word32.word) = 
            if Mach.isUInt a
            then Mach.newUInt x
            else Mach.newInt (u2i x)
                          
    in
        case bop of
            Ast.Plus mode => 
            if Mach.isString a orelse
               Mach.isString b
            then stringConcat ()
            else dispatchNumeric ( valOf mode ) 
                                 ( Decimal.add )
                                 ( Real64.+ )
                                 ( Int32.+ )
                                 ( Word32.+ )
                                 ( LargeInt.+ )
                                              
          | Ast.Minus mode => 
            dispatchNumeric ( valOf mode ) 
                            ( Decimal.subtract )
                            ( Real64.- )
                            ( Int32.- )
                            ( Word32.- )
                            ( LargeInt.- )

          | Ast.Times mode => 
            dispatchNumeric (valOf mode) 
                            ( Decimal.multiply )
                            ( Real64.* )
                            ( Int32.* )
                            ( Word32.* )
                            ( LargeInt.* )

          | Ast.Divide mode => 
            dispatchNumeric ( valOf mode ) 
                            ( Decimal.divide )
                            ( Real64./ )
                            ( Int32.div )
                            ( Word32.div )
                            ( LargeInt.div )

          | Ast.Remainder mode => 
            dispatchNumeric ( valOf mode ) 
                            ( Decimal.remainder )
                            ( Real64.rem )
                            ( Int32.mod )
                            ( Word32.mod )
                            ( LargeInt.mod )

          | Ast.LeftShift => 
            pickRepByA (Word32.<< ((i2u (Mach.toInt32 a)),
                                   (masku5 (Mach.toUInt32 b))))

          | Ast.RightShift => 
            pickRepByA (Word32.>> ((i2u (Mach.toInt32 a)),
                                   (masku5 (Mach.toUInt32 b))))

          | Ast.RightShiftUnsigned => 
            pickRepByA (Word32.~>> ((Mach.toUInt32 a),
                                    (masku5 (Mach.toUInt32 b))))

          (* FIXME: should we return int if we do int|int or int&int ? *)
          | Ast.BitwiseAnd => bitwiseWordOp (Word32.andb)
          | Ast.BitwiseOr => bitwiseWordOp (Word32.orb)
          | Ast.BitwiseXor => bitwiseWordOp (Word32.xorb)

          | Ast.Equals mode => 
            dispatchComparison (valOf mode) 
                               (fn x => x = EQUAL)
            
          | Ast.NotEquals mode => 
            dispatchComparison (valOf mode) 
                               (fn x => not (x = EQUAL))
            
          | Ast.StrictEquals mode => 
            dispatchComparison (valOf mode) 
                               (fn x => x = EQUAL)

          | Ast.StrictNotEquals mode => 
            dispatchComparison (valOf mode) 
                               (fn x => not (x = EQUAL))

          | Ast.Less mode => 
            dispatchComparison (valOf mode) 
                               (fn x => x = LESS)

          | Ast.LessOrEqual mode => 
            dispatchComparison (valOf mode) 
                               (fn x => (x = LESS) orelse (x = EQUAL))

          | Ast.Greater mode => 
            dispatchComparison (valOf mode) 
                               (fn x => x = GREATER)

          | Ast.GreaterOrEqual mode => 
            dispatchComparison (valOf mode) 
                               (fn x => (x = GREATER) orelse (x = EQUAL))

          | _ => error ["unexpected binary operator in performBinOp"]
    end


and doubleEquals (mode:Ast.NUMERIC_MODE option)
                 (a:Mach.VAL) 
                 (b:Mach.VAL)                  
  : bool = 
    Mach.toBoolean (performBinop (Ast.Equals mode) a b)


(*
fun hasInstance (ob:OBJ)
                (v:VAL)
    : bool = 
    let 
        val Obj { magic, ... } = ob
        fun functionHasInstance _ = 
            case v of 
                Object ob => 
                if hasValue ob Name.public_prototype
                else 
                    let 
                        val proto = getValue ob Name.public_prototype
                    in
                        if Mach.isObject proto
                        then tripleEquals ...
                
            if not isObject v
            then false
            else 
    in
        case !magic of 
            Function => isFunction v orelse isNativeFunction v
          | NativeFunction => isFunction v orelse isNativeFunction v
          | _ => false
    end
*)


and evalBinaryOp (scope:Mach.SCOPE) 
                 (bop:Ast.BINOP) 
                 (aexpr:Ast.EXPR) 
                 (bexpr:Ast.EXPR) 
    : Mach.VAL =
    case bop of 
        Ast.LogicalAnd => 
        let 
            val a = evalExpr scope aexpr
        in
            if Mach.toBoolean a
            then evalExpr scope bexpr
            else a
        end
        
      | Ast.LogicalOr => 
        let 
            val a = evalExpr scope aexpr
        in
            if Mach.toBoolean a
            then a
            else evalExpr scope bexpr
        end

      | Ast.Comma => (evalExpr scope aexpr;
                      evalExpr scope bexpr)

      | Ast.InstanceOf => 
        let 
            val a = evalExpr scope aexpr
            val b = evalExpr scope bexpr
        in
            case b of 
                Mach.Object (ob) =>
                Mach.newBoolean true (* FIXME: (hasInstance ob b) *)
              | _ => raise ThrowException (newByGlobalName Name.public_TypeError)
        end

      | Ast.In =>
        let 
            val a = evalExpr scope aexpr
            val astr = Mach.toString a
            val aname = {id=astr, ns=Ast.Internal ""}
            val b = evalExpr scope bexpr
        in
            case b of 
                Mach.Object (Mach.Obj {props, ...}) =>
                Mach.newBoolean (Mach.hasProp props aname)
              | _ => raise ThrowException (newByGlobalName Name.public_TypeError)
                     
        end
        
      | _ => performBinop bop 
                          (evalExpr scope aexpr) 
                          (evalExpr scope bexpr)


and evalCondExpr (scope:Mach.SCOPE) 
                 (cond:Ast.EXPR) 
                 (thn:Ast.EXPR) 
                 (els:Ast.EXPR) 
    : Mach.VAL = 
    let 
        val v = evalExpr scope cond
        val b = Mach.toBoolean v
    in
        if b 
        then evalExpr scope thn
        else evalExpr scope els
    end
    

and evalIdentExpr (scope:Mach.SCOPE) 
                  (r:Ast.IDENT_EXPR) 
    : Ast.MULTINAME = 
    case r of 
        Ast.Identifier { ident, openNamespaces } => 
        { nss=openNamespaces, id=ident }
        
      | Ast.QualifiedIdentifier { qual, ident } => 
        { nss = [[needNamespace (evalExpr scope qual)]], id = ident }

      | Ast.QualifiedExpression { qual, expr } => 
        { nss = [[needNamespace (evalExpr scope qual)]], 
          id = Mach.toString (evalExpr scope expr) }
        
      | Ast.ExpressionIdentifier expr =>
        { nss = [[Ast.Internal ""]],
          id = Mach.toString (evalExpr scope expr) }

      | _ => LogErr.unimplError ["unimplemented identifier expression form"]


and evalRefExpr (scope:Mach.SCOPE)
                (expr:Ast.EXPR)
                (errIfNotFound:bool)
    : REF =

    let 
        fun makeRefNotFound (b:Mach.VAL option) (mname:Ast.MULTINAME) 
            : REF =
            case (b,mname) of
                (SOME (Mach.Object ob),{id,...}) =>
                (ob,{ns=Ast.Internal "",id=id})  (* FIXME: ns might be user settable default *)
              | (NONE,{id,...}) => 
                (Mach.globalObject,{ns=Ast.Internal "",id=id})
              | _ => error ["ref expression messed up in refOf"]

        val (base,ident) =
            case expr of         
                Ast.LexicalRef { ident, pos } => 
                (LogErr.setPos pos; 
                 (NONE,ident))

              | Ast.ObjectRef { base, ident, pos } => 
                (LogErr.setPos pos; 
                 (SOME (evalExpr scope base), ident))

              | _ => error ["need lexical or object-reference expression"]

        val (multiname:Ast.MULTINAME) = evalIdentExpr scope ident

        val refOpt = 
            case base of 
                SOME (Mach.Object ob) => 
                resolveOnObjAndPrototypes ob multiname
              | NONE => 
                resolveOnScopeChain scope multiname
              | _ => error ["ref expression on non-object value"]
                                                                             
    in
        case refOpt of 
            NONE => if errIfNotFound 
                    then error ["unresolved identifier expression",
                                           LogErr.multiname multiname]
                    else makeRefNotFound base multiname
          | SOME r' => r'
    end

(*
    EXPR = LetExpr
*)
and evalLetExpr (scope:Mach.SCOPE) 
                (head:Ast.HEAD) 
                (body:Ast.EXPR) 
    : Mach.VAL = 
    let 
        val letScope = evalHead scope head false
    in
        evalExpr letScope body
    end


and resolveOnScopeChain (scope:Mach.SCOPE) 
                        (mname:Ast.MULTINAME) 
    : REF option =
    let 
        val _ = trace ["resolving multiname on scope chain: ", 
                              LogErr.multiname mname]
        fun getScopeParent (Mach.Scope { parent, ... }) = parent
        fun hasFixedBinding (Mach.Scope {object=Mach.Obj {props, ...}, ... }, n)
            = Mach.hasFixedProp props n
    in
        (* 
         * First do a fixed-properties-only lookup along the scope chain alone. 
         *)
        case Multiname.resolve 
                 mname scope hasFixedBinding getScopeParent of
            SOME (Mach.Scope {object, ...}, name) => (trace ["found ",LogErr.name name]; SOME (object, name))
          | NONE => 
            (* 
             * If that fails, do a sequence of dynamic-property-permitted
             * lookups on every scope object (and along its prototype chain)
             * in the scope chain. 
             *)
            let
                fun tryProtoChain (Mach.Scope {object, parent, ...}) = 
                    case resolveOnObjAndPrototypes object mname of
                        SOME result => SOME result
                      | NONE => case parent of 
                                    NONE => NONE
                                  | SOME p => tryProtoChain p
            in
                tryProtoChain scope
            end
    end


and resolveOnObjAndPrototypes (obj:Mach.OBJ) 
                              (mname:Ast.MULTINAME) 
    : REF option = 
    let 
        fun hasFixedProp (Mach.Obj {props, ...}, n) = Mach.hasFixedProp props n
        fun hasProp (Mach.Obj {props, ...}, n) = Mach.hasProp props n
        fun getObjProto (Mach.Obj {proto, ...}) = 
            case (!proto) of 
                Mach.Object ob => SOME ob
              | _ => NONE
    in
        trace ["resolveOnObjAndPrototypes: ", LogErr.multiname mname];
        case Multiname.resolve mname obj hasFixedProp getObjProto of
            NONE => Multiname.resolve mname obj hasProp getObjProto
          | refOpt => refOpt
    end

     
and evalTailCallExpr (scope:Mach.SCOPE) 
                     (e:Ast.EXPR) 
    : Mach.VAL = 
    raise (TailCallException (fn _ => evalExpr scope e))


and evalNonTailCallExpr (scope:Mach.SCOPE) 
                        (e:Ast.EXPR) 
    : Mach.VAL = 
    evalExpr scope e
    handle TailCallException thunk => thunk ()
         | ReturnException v => v


and labelEq (stmtLabels:Ast.IDENT list) 
            (exnLabel:Ast.IDENT option) 
    : bool = 
    case (stmtLabels, exnLabel) of
        (sl::[], SOME el) => sl = el
      | (sl::[], NONE) => sl = ""
      | ([], NONE) => false                   (* FIXME: refactor *)
      | ([], SOME el) => false
      | (sl::sls,NONE) => 
            if sl = ""
            then true
            else labelEq sls exnLabel
      | (sl::sls,SOME el) => 
            if sl = el
            then true
            else labelEq sls exnLabel


and evalStmts (scope:Mach.SCOPE) 
              (stmts:Ast.STMT list) 
    : Mach.VAL = 
    case stmts of 
        [s] => evalStmt scope s
      | (s::ss) => (evalStmt scope s; evalStmts scope ss)
      | [] => Mach.Undef


and evalStmt (scope:Mach.SCOPE) 
             (stmt:Ast.STMT) 
    : Mach.VAL = 
    case stmt of 
        Ast.ExprStmt e => evalExpr scope e
      | Ast.IfStmt {cnd,thn,els} => evalIfStmt scope cnd thn els
      | Ast.WhileStmt w => evalWhileStmt scope w
      | Ast.DoWhileStmt w => evalDoWhileStmt scope w
      | Ast.WithStmt { obj, ty, body } => evalWithStmt scope obj ty body
      | Ast.SwitchStmt { mode, cond, cases } => 
        evalSwitchStmt scope mode cond cases
      | Ast.ForStmt w => evalForStmt scope w
      | Ast.ReturnStmt r => evalReturnStmt scope r
      | Ast.BreakStmt lbl => evalBreakStmt scope lbl
      | Ast.ContinueStmt lbl => evalContinueStmt scope lbl
      | Ast.ThrowStmt t => evalThrowStmt scope t
      | Ast.LabeledStmt (lab, s) => evalLabelStmt scope lab s
      | Ast.BlockStmt b => evalBlock scope b
      | Ast.ClassBlock c => evalClassBlock scope c
      | Ast.LetStmt b => evalBlock scope b
      | Ast.EmptyStmt => Mach.Undef
      | Ast.TryStmt { block, catches, finally } => 
        evalTryStmt scope block catches finally
      | Ast.SwitchTypeStmt { cond, ty, cases } => 
        LogErr.unimplError ["switch-type statements not handled"]
      | Ast.ForInStmt w => 
        LogErr.unimplError ["for-in loops not handled"]


and multinameOf (n:Ast.NAME) = 
    { nss = [[(#ns n)]], id = (#id n) }


and findVal (scope:Mach.SCOPE) 
            (mn:Ast.MULTINAME) 
    : Mach.VAL = 
    case resolveOnScopeChain scope mn of 
        NONE => error ["unable to resolve multiname: ", 
                                  LogErr.multiname mn ]
      | SOME (obj, name) => getValue (obj, name) 


and checkAllPropertiesInitialized (obj:Mach.OBJ)
    : unit = 
    let 
        fun checkOne (n:Ast.NAME, p:Mach.PROP) = 
            case (#state p) of 
                Mach.UninitProp => error ["uninitialized property: ", 
                                                     LogErr.name n]
              | _ => ()
    in
        case obj of 
            Mach.Obj { props, ... } => 
            List.app checkOne (!props)
    end


and invokeFuncClosure (this:Mach.OBJ) 
                      (closure:Mach.FUN_CLOSURE) 
                      (args:Mach.VAL list) 
    : Mach.VAL =
    let
        val { func, env, allTypesBound } = closure
        val Ast.Func { name, fsig, block, param=(paramFixtures, paramInits), ... } = func
    in
        if not allTypesBound
        then error ["invoking function with unbound type variables"]
        else
            let 
                val (varObj:Mach.OBJ) = Mach.newSimpleObj NONE
                val (varScope:Mach.SCOPE) = extendScope env varObj true
                                            
                (* FIXME: infer a fixture for "this" so that it's properly typed, dontDelete, etc.
                 * Also this will mean changing to defVar rather than setVar, for 'this'. *)
                val thisVal = Mach.Object this
                fun initThis v = setValue varObj thisName thisVal
                                 
                (* FIXME: self-name binding is surely more complex than this! *)
                val selfName = { id = (#ident name), ns = Ast.Internal "" }
                val selfTag = Mach.FunctionTag fsig
                val selfVal = Mach.newObject selfTag Mach.Null (SOME (Mach.Function closure))
            in
                allocScopeFixtures varScope paramFixtures;
                initThis thisVal;
                bindArgs env varScope func args;
                evalScopeInits varScope Ast.Local paramInits;

                (* NOTE: is this for the binding of a function expression to its optional
                 * identifier? If so, we need to extend the scope chain before extending it
                 * with the activation object, and add the self-name binding to that new 
                 * scope, as in sec 13 ed. 3.
                 * 
                 * Changing defValue to setValue for now.
                 *)

                setValue varObj selfName selfVal;                
                checkAllPropertiesInitialized varObj;
                evalBlock varScope block
            end
    end


and evalTryStmt (scope:Mach.SCOPE) 
                (block:Ast.BLOCK) 
                (catches:Ast.CATCH_CLAUSE list)
                (finally:Ast.BLOCK option) 
    : Mach.VAL = 
    let
        fun typesCompatible a b = true (* FIXME: do a real type test here! *)

        fun catch (e:Mach.VAL) 
                  (clauses:Ast.CATCH_CLAUSE list) 
            : Mach.VAL option = 
            case clauses of
                [] => NONE
              | {ty, fixtures, block, ...}::cs => 
                if typesCompatible ty e
                then 
                    let 
                        (* FIXME: doesn't this need inits? *)
                        val head = (valOf fixtures, [])
                        val scope = evalHead scope head false
                    in
                        SOME (evalBlock scope block)
                    end
                else
                    catch e cs

        fun finishWith (v:Mach.VAL) 
            : Mach.VAL = 
            case finally of 
                NONE => v
              (* 
               * FIXME: Not sure how to read the spec wrt. labels; 
               * are we supposed to return the finally-block's value
               * or the main block's value? 
               *)
              | SOME f => evalBlock scope f
    in
        evalBlock scope block
        handle ThrowException v => 
               case catch v catches of 
                   SOME fix => finishWith fix
                 | NONE => (finishWith v; 
                            raise ThrowException v)
    end
           

(*    
    Here are the structures we have to work with to instantiate objects:

     and CLS =
         Cls of
           { extends: NAME option,
             implements: NAME list,
             classFixtures: FIXTURES,
             instanceFixtures: FIXTURES,
             instanceInits: INITS,
             constructor: CTOR option,
             classType: TYPE_EXPR,
             instanceType: TYPE_EXPR }

     and CTOR =
         Ctor of
           { settings: INITS,
             func: FUNC }

     and FUNC =
         Func of 
           { name: FUNC_NAME,
             fsig: FUNC_SIG,                   
             fixtures: FIXTURES option,
             inits: STMT list,
             body: BLOCK }

    Here's how it works:

    val scope = [globalObj,classObj]
    val thisObj = newObj 
    evalFixtures scope thisObj instanceFixtures
    evalInits scope thisObj instanceInits (* step 4 *)

    val paramsObj = newObj
    val paramsFixtures = (#fixtures (#func (#constructor cls)))
	evalFixtures scope paramsObj paramsFixtures
    val paramsInits = (#inits (#func (#constructor class)))
    evalInits scope paramsObj paramsInits

	val settingsInits = (#settings (#constructor cls))
    evalInits paramsObj::scope thisObj settingsInits
        (* settings calls super constructor jumping to step 4 *)
    
    checkAllPropertiesInitialized obj

	val ctorBody = (#body (#func (#constructor cls)))
    evalBlock paramsObj::(thisObj::scope) thisObj ctorBody

*)

(* 

On the whiteboard today (feb 22):

  - get class closure for RHS of operator "new"
  - make an object
  - allocate instance fixtures
  CTOR(x) - eval inits in class scope [..., class]
          - allocate param obj
          - allocate param fixtures
          - eval param inits in class scope [..., class]
          - eval instance settings [...,class,params]
          - call CTOR(parent), via super() call at end of settings
          - check all allocated fixtures are initialized
          - execute ctor body (if native, run native fn) [..,class,instance,params]

*)

and bindArgs (outerScope:Mach.SCOPE)
             (argScope:Mach.SCOPE)
             (func:Ast.FUNC)
             (args:Mach.VAL list)
    : unit =
    let
        val Ast.Func { defaults, ty, ... } = func

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
         *)

        val p = length (#params ty)
        val d = length defaults
        val a = length args
        val i = (a+d)-p
        val argTemps = getScopeTemps argScope
        fun bindArg _ [] = ()
          | bindArg (n:int) ((arg:Mach.VAL)::args) = 
            (Mach.defTemp argTemps n arg; 
             bindArg (n+1) args)
    in
        if a + d < p
        then error ["not enough args to function ", 
                    Int.toString a, " given ", 
                    Int.toString (p-d), " expected"]
        else 
            let
                val defExprs = List.drop (defaults, i)
                val defVals = List.map (evalExpr outerScope) defExprs
                val allArgs = args @ defVals
            in
                bindArg 0 allArgs
            end
    end


and evalInits (scope:Mach.SCOPE)
              (obj:Mach.OBJ)
              (temps:Mach.TEMPS)
              (inits:Ast.INITS)
    : unit = evalInitsMaybePrototype scope obj temps inits false


and evalInitsMaybePrototype (scope:Mach.SCOPE)
              (obj:Mach.OBJ)   
              (temps:Mach.TEMPS)
              (inits:Ast.INITS)
              (isPrototypeInit:bool)
    : unit =
    let 
        fun evalInit (n,e) =
            let
                val _ = trace [">> evalInit"]
                val v = evalExpr scope e
                val _ = trace ["<< evalInit"]
            in
                case n of 
                    Ast.PropName pn => 
                    (trace ["evalInit assigning to prop ", LogErr.name pn];
                     if isPrototypeInit 
                     then setValue obj pn v
                     else defValue obj pn v)
                  | Ast.TempName tn => 
                    (trace ["evalInit assigning to temp ", (Int.toString tn)];
                     Mach.defTemp temps tn v)
            end
    in 
        trace [">> evalInits"];
        List.app evalInit inits;
        trace ["<< evalInits"]
    end

(*
    Evaluate INITs targeting an obj, in scope of temporaries
*)

and evalObjInits (scope:Mach.SCOPE)                   
                 (instanceObj:Mach.OBJ)
                 (head:Ast.HEAD)
    : unit = 
        let
            val (fixtures,inits) = head
            val tempScope = evalHead scope (fixtures,[]) false
            val temps = getScopeTemps tempScope
        in
            evalInits tempScope instanceObj temps inits
        end


and evalScopeInits (scope:Mach.SCOPE)
                   (target:Ast.INIT_TARGET)
                   (inits:Ast.INITS)
    : unit =
        let
            fun targetOf (Mach.Scope { object, temps, isVarObject, parent}) =
                case (target,parent,object) of
                     (Ast.Local,parent,Mach.Obj {props,...}) => 
                             if (length (!props)) > 0 
                                then object
                                else targetOf (valOf parent) 
                                     (* if there are no props then it is at temp scope *)
                   | (Ast.Hoisted,parent,obj) => 
                             if isVarObject=true 
                                then obj
                                else targetOf (valOf parent)
                   | (Ast.Prototype,parent,obj) => 
                             if isVarObject=true 
                                then ((fn (Mach.Object ob) => ob) (* FIXME: there must be a builtin way to do this *)
                                          (getValue (obj, Name.public_prototype)))
                                else targetOf (valOf parent)
            val obj = targetOf scope
        in case scope of
            Mach.Scope { temps, ...} =>
                evalInitsMaybePrototype scope obj temps inits (target=Ast.Prototype)
        end


and initializeAndConstruct (classClosure:Mach.CLS_CLOSURE)
                           (classObj:Mach.OBJ)
                           (classScope:Mach.SCOPE)                           
                           (args:Mach.VAL list) 
                           (instanceObj:Mach.OBJ)
    : unit =
    case classClosure of 
        { cls, env, allTypesBound } => 
        if not allTypesBound 
        then error ["constructing instance of class with unbound type variables"]
        else
            let
                val Ast.Cls { name, 
                              extends,
                              instanceInits,
                              constructor, 
                              ... } = cls
                fun initializeAndConstructSuper (superArgs:Mach.VAL list) = 
                    case extends of 
                        NONE => 
                        (trace ["checking all properties initialized at root class", 
                                       LogErr.name name];
                         checkAllPropertiesInitialized instanceObj)
                      | SOME superName => 
                        let 
                            val (superObj:Mach.OBJ) = needObj (findVal env (multinameOf superName))
                            val (superClsClosure:Mach.CLS_CLOSURE) = 
                                case Mach.getObjMagic superObj of
                                    SOME (Mach.Class cc) => cc
                                  | _ => error ["Superclass object ", 
                                                           LogErr.name superName, 
                                                           "is not a class closure"]
                            val (superEnv:Mach.SCOPE) = (#env superClsClosure)
                        in
                            initializeAndConstruct 
                                superClsClosure superObj superEnv superArgs instanceObj
                        end
            in
                trace ["evaluating instance initializers for ", LogErr.name name];
                evalObjInits classScope instanceObj instanceInits;   
                case constructor of 
                    NONE => initializeAndConstructSuper []
                  | SOME (Ast.Ctor { settings, superArgs, func }) => 
                    let 
                        val Ast.Func { block, param=(paramFixtures,paramInits), ... } = func
                        val (varObj:Mach.OBJ) = Mach.newSimpleObj NONE
                        val (varScope:Mach.SCOPE) = extendScope classScope varObj false
                        val (ctorScope:Mach.SCOPE) = extendScope varScope instanceObj true
                    in
                        trace ["allocating scope fixtures for constructor of ", LogErr.name name];
                        allocScopeFixtures varScope paramFixtures;
                        trace ["binding constructor args of ", LogErr.name name];
                        bindArgs classScope varScope func args;
                        trace ["evaluating inits of ", LogErr.name name];
                        evalScopeInits varScope Ast.Local paramInits;
                        trace ["evaluating settings"];                        
                        evalObjInits varScope instanceObj settings;
                        trace ["initializing and constructing superclass of ", LogErr.name name];
                        initializeAndConstructSuper (map (evalExpr varScope) superArgs);  
                        trace ["entering constructor for ", LogErr.name name];
                        evalBlock ctorScope block;
                        ()
                    end
            end


and constructClassInstance (classObj:Mach.OBJ)
                           (classClosure:Mach.CLS_CLOSURE) 
                           (args:Mach.VAL list) 
    : Mach.VAL =
    let
        val {cls = Ast.Cls { name, instanceFixtures, ...}, env, ...} = classClosure

        val (tag:Mach.VAL_TAG) = Mach.ClassTag name
        val (proto:Mach.VAL) = if hasOwnValue classObj Name.public_prototype
                               then getValue (classObj, Name.public_prototype)
                               else Mach.Null
                                    
        val (classScope:Mach.SCOPE) = extendScope env classObj false
        val (instanceObj:Mach.OBJ) = Mach.newObj tag proto NONE
    in
        trace ["allocating ", 
               Int.toString (length instanceFixtures), 
               " instance fixtures for new ", LogErr.name name];
        allocObjFixtures classScope instanceObj instanceFixtures;
        trace ["entering most derived constructor for ", LogErr.name name];
        initializeAndConstruct classClosure classObj classScope args instanceObj;
        trace ["finished constructing new ", LogErr.name name];
        Mach.Object instanceObj
    end


and newByGlobalName (n:Ast.NAME) 
    : Mach.VAL = 
    let
        val (cls:Mach.VAL) = getValue (Mach.globalObject, n)
    in
        case cls of 
            Mach.Object ob => evalNewExpr ob []
          | _ => error ["trying to 'new' non-object global value"]
    end


(* 
 * ES3 9.9 ToObject 
 * 
 * FIXME: no idea if this makes the most sense given 
 * the ES3 meaning of the operation. 
 *)

and toObject (v:Mach.VAL) 
    : Mach.OBJ = 
    case v of 
        Mach.Undef => raise ThrowException (newByGlobalName Name.public_TypeError)
      | Mach.Null => raise ThrowException (newByGlobalName Name.public_TypeError)
      | Mach.Object ob => ob


(* 
 * ES3 8.6.2.1 [[Get]](P)
 * 
 * FIXME: no idea if this makes the most sense given 
 * the ES3 meaning of the operation. 
 *)
and get (obj:Mach.OBJ) 
        (n:Ast.NAME) 
    : Mach.VAL =
    let
        fun tryObj ob = 
            if hasOwnValue ob n
            then getValue (ob, n)
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
 * ES3 8.6.2.6 [[DefaultValue]](hint)
 * 
 * FIXME: no idea if this makes the most sense given 
 * the ES3 meaning of the operation. 
 *)
and defaultValue (obj:Mach.OBJ) 
                 (hint:string option)
    : Mach.VAL = 
    let 
        fun tryProps [] = raise ThrowException 
                                    (newByGlobalName 
                                         Name.public_TypeError)
          | tryProps (n::ns) =
            let 
                val f = get obj n
            in 
                if Mach.isObject f
                then 
                    let 
                        val v = evalCallExpr (SOME obj) (needObj f) []
                    in
                        if isPrimitive v
                        then v
                        else tryProps ns
                    end
                else
                    tryProps ns
            end
    in
        (* FIXME: Date objects are supposed to default to "String" hint. *)
        if hint = (SOME "String")
        then tryProps [Name.public_toString, Name.public_valueOf]
        else tryProps [Name.public_valueOf, Name.public_toString]
    end


and isPrimitive (v:Mach.VAL) 
    : bool = 
    case v of 
        Mach.Null => true
      | Mach.Undef => true
      | Mach.Object (Mach.Obj ob) => 
        (case !(#magic ob) of
             SOME (Mach.UInt _) => true
           | SOME (Mach.Int _) => true
           | SOME (Mach.Double _) => true
           | SOME (Mach.Decimal _) => true
           | SOME (Mach.String _) => true
           | SOME (Mach.Bool _) => true
           | _ => false)

(* 
 * ES3 1.9 ToPrimitive 
 * 
 * FIXME: no idea if this makes the most sense given 
 * the ES3 meaning of the operation. 
 *)
and toPrimitive (v:Mach.VAL) 
                (preferredType:string option)
    : Mach.VAL = 
    if isPrimitive v 
    then v
    else defaultValue (needObj v) preferredType
         
(*
    HEAD
*)

and evalHead (scope:Mach.SCOPE)
             (head:Ast.HEAD)
             (isVarObject:bool)
    : Mach.SCOPE =
        let
            val (fixtures,inits) = head
            val obj = Mach.newObj (Mach.ClassTag Name.public_Object) Mach.Null NONE
            val scope = extendScope scope obj isVarObject
            val temps = getScopeTemps scope
        in
            allocFixtures scope obj temps fixtures;
            evalInits scope obj temps inits;
            scope
        end
            
(*
    BLOCK
*)

and evalBlock (scope:Mach.SCOPE) 
              (block:Ast.BLOCK) 
    : Mach.VAL = 
    let 
        val Ast.Block {head, body, pos, ...} = block
        val _ = LogErr.setPos pos
        val blockScope = evalHead scope (valOf head) false
    in
        evalStmts blockScope body
    end



(*
    Initialise Class Prototype

    Initialise a class object. A class object has been allocated and stored
    in a global property. All that remains is to initialise it by executing
    the class sttatement.

    The class statement sets static and prototype properties while executing
    its body.
*)

and initClassPrototype (scope)
                       (classObj:Mach.OBJ) 
    : unit =
    let 
        val Mach.Obj { props, magic, ... } = classObj
        val SOME (Mach.Class {cls=Ast.Cls {extends,...},...}) = !magic
        val baseProtoVal = 
            case extends of 
                NONE => Mach.Null
              | SOME baseClassName => 
                let
                in
                    case findVal scope (multinameOf baseClassName) of 
                        Mach.Object ob => 
                        if hasOwnValue ob Name.public_prototype
                        then getValue (ob, Name.public_prototype)
                        else Mach.Null
                      | _ => error ["base class resolved to non-object: ", 
                                    LogErr.name baseClassName]
                end
                
        val _ = trace ["constructing prototype"]
        val newPrototype = Mach.newObj (Mach.ClassTag Name.public_Object) baseProtoVal NONE
    in
        defValue classObj Name.public_prototype (Mach.Object newPrototype);
        trace ["finished initialising class prototype"]
    end

(*
    ClassBlock classBlock

*)

and evalClassBlock (scope:Mach.SCOPE) 
                   (classBlock)
    : Mach.VAL =

    (* 
        The property that holds the class object was allocated when the 
        fixtures of the outer scope were allocated. Still to do is
        initialising the class object, including creating its prototype
    *)

    let 
        val {name, block, ...} = classBlock

        val _ = trace ["evaluating class stmt for ", LogErr.name (valOf name)]
        
        val classObj = needObj (findVal scope (multinameOf (valOf name)))

        val _ = initClassPrototype scope classObj
        val classScope = extendScope scope classObj true
    in
        evalBlock classScope block
    end

(*

*)

and evalIfStmt (scope:Mach.SCOPE) 
               (cnd:Ast.EXPR) 
               (thn:Ast.STMT)
               (els:Ast.STMT) 
    : Mach.VAL = 
    let 
        val v = evalExpr scope cnd 
        val b = Mach.toBoolean v
    in
        if b 
        then evalStmt scope thn
        else evalStmt scope els
    end


and evalLabelStmt (scope:Mach.SCOPE) 
                  (lab:Ast.IDENT) 
                  (s:Ast.STMT) 
    : Mach.VAL =
    evalStmt scope s
        handle BreakException exnLabel => 
            if labelEq [lab] exnLabel
                then Mach.Undef
                else raise BreakException exnLabel


and evalWhileStmt (scope:Mach.SCOPE)
                  (whileStmt:Ast.WHILE_STMT)
    : Mach.VAL =
    case whileStmt of
        { cond, body, fixtures, labels } =>
        let
            val accum = ref Mach.Undef
            fun loop _ =
                if Mach.toBoolean (evalExpr scope cond)
                then 
                    (accum := evalStmt scope body;
                     loop ())
                    handle ContinueException exnLabel => 
                           if labelEq labels exnLabel
                           then loop ()
                           else raise ContinueException exnLabel
                                      
                         | BreakException exnLabel => 
                           if labelEq labels exnLabel
                           then (!accum)
                           else raise BreakException exnLabel
                else 
                    (!accum)
        in
            loop ()
        end


and evalDoWhileStmt (scope:Mach.SCOPE)
                    (whileStmt:Ast.WHILE_STMT)
    : Mach.VAL =
    case whileStmt of
        { cond, body, fixtures, labels } =>
        let
            val accum = ref Mach.Undef
            fun loop _ =
                let 
                    fun bottom _ = 
                        if Mach.toBoolean (evalExpr scope cond)
                        then loop ()
                        else (!accum)
                in                
                    (accum := evalStmt scope body;
                     bottom ())
                    handle ContinueException exnLabel => 
                           if labelEq labels exnLabel
                           then bottom ()
                           else raise ContinueException exnLabel
                                      
                         | BreakException exnLabel => 
                           if labelEq labels exnLabel
                           then (!accum)
                           else raise BreakException exnLabel
                end
        in
            loop ()
        end


and evalWithStmt (scope:Mach.SCOPE) 
                 (obj:Ast.EXPR) 
                 (ty:Ast.TYPE_EXPR) 
                 (body:Ast.STMT) 
    : Mach.VAL = 
    let 
        val v = evalExpr scope obj
        val s = extendScope scope (toObject v) false
    in
        evalStmt s body
    end


and evalSwitchStmt (scope:Mach.SCOPE)
                   (mode:Ast.NUMERIC_MODE option)
                   (cond:Ast.EXPR)
                   (cases:Ast.CASE list) 
    : Mach.VAL = 
    let
        fun tryCases (v:Mach.VAL) [] = Mach.Undef
          | tryCases (v:Mach.VAL) ({label, inits, body}::cs) =
            if (case label of 
                    NONE => true
                  | SOME e => doubleEquals mode v (evalExpr scope e))
            then 
                (* FIXME: This will change when switch stmt bodies are
                 * reorganized and get a proper head. *)
                let
                    val head = ([], case inits of NONE => [] 
                                                | SOME i => i)
                    val caseScope = evalHead scope head false
                in
                    evalBlock caseScope body
                end
            else
                tryCases v cs
    in
        tryCases (evalExpr scope cond) cases
    end
    

(*
    FOR_STMT

*)

and evalForInStmt (scope:Mach.SCOPE)
                  (forStmt:Ast.FOR_ENUM_STMT)
    : Mach.VAL = 
    Mach.Undef

and evalForStmt (scope:Mach.SCOPE)
                (forStmt:Ast.FOR_STMT)
    : Mach.VAL =
    case forStmt of
        { fixtures, init, cond, update, labels, body, ...} =>
        let
            val forScope = evalHead scope (valOf fixtures, []) false

            fun loop (accum:Mach.VAL option) =
                let
                    val v = evalExpr forScope cond
                    val b = Mach.toBoolean v
                in
                    if b
                    then
                        let
                            val curr = (SOME (evalStmt forScope body)
                                        handle ContinueException exnLabel => 
                                               if labelEq labels exnLabel
                                               then NONE
                                               else raise ContinueException exnLabel)
                            val next = (case curr 
                                         of NONE => accum
                                          | x => x)
                        in
                            evalExpr forScope update;
                            loop next handle BreakException exnLabel => 
                                          if labelEq labels exnLabel  
                                          then accum
                                          else raise BreakException exnLabel
                        end
                    else
                        accum
                end
        in
            evalStmts forScope init;
            case 
                loop NONE handle BreakException exnLabel => 
                              if labelEq labels exnLabel  
                              then NONE
                              else raise BreakException exnLabel
            of
                NONE => Mach.Undef
              | SOME v => v
        end


and evalReturnStmt (scope:Mach.SCOPE) 
                   (e:Ast.EXPR) 
    : Mach.VAL =
    raise (ReturnException (evalExpr scope e))


and evalThrowStmt (scope:Mach.SCOPE) 
                  (e:Ast.EXPR) 
    : Mach.VAL =
    raise (ThrowException (evalExpr scope e))


and evalBreakStmt (scope:Mach.SCOPE) 
                  (lbl:Ast.IDENT option) 
    : Mach.VAL =
    (trace ["raising BreakException ",case lbl of NONE => "empty" | SOME id => id];
    raise (BreakException lbl))


and evalContinueStmt (scope:Mach.SCOPE) 
                     (lbl:Ast.IDENT option) 
    : Mach.VAL =
    raise (ContinueException lbl)


and evalPackage (scope:Mach.SCOPE) 
                (package:Ast.PACKAGE) 
    : Mach.VAL = 
    evalBlock scope (#block package)


and evalProgram (prog:Ast.PROGRAM) 
    : Mach.VAL = 
    (LogErr.setPos NONE;
     setValue Mach.globalObject thisName (Mach.Object Mach.globalObject);
     allocScopeFixtures Mach.globalScope (valOf (#fixtures prog));
     map (evalPackage Mach.globalScope) (#packages prog);
     evalBlock Mach.globalScope (#block prog))

end
