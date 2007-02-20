(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Eval = struct 

(* Exceptions for object-language control transfer. *)
exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> Mach.VAL)
exception ThrowException of Mach.VAL
exception ReturnException of Mach.VAL

exception InternalError
    
fun newName (i:Ast.IDENT) 
            (n:Mach.NS) 
  = { ns=n, id=i }


fun getType (tyOpt:Ast.TYPE_EXPR option) 
    : Ast.TYPE_EXPR = 
    case tyOpt of 
        SOME t => t
      | NONE => Ast.SpecialType Ast.Any
                
fun getScopeObj (scope:Mach.SCOPE) 
    : Mach.OBJ = 
    case scope of 
        Mach.Scope { object, ...} => object


fun needNamespace (v:Mach.VAL) 
    : Ast.NAMESPACE = 
    case v of 
        Mach.Object (Mach.Obj ob) => 
        (case !(#magic ob) of 
             SOME (Mach.Namespace n) => n
           | _ => LogErr.evalError ["need namespace"])
      | _ => LogErr.evalError ["need namespace"]


fun needObj (v:Mach.VAL) 
    : Mach.OBJ = 
    case v of 
        Mach.Object ob => ob
      | _ => LogErr.evalError ["need object"]

(* 
 * A small number of functions do not fully evaluate to Mach.VAL 
 * values, but instead to REFs; these are temporary artifacts of
 * evaluation, not first-class values in the language. 
 *)

type REF = (Mach.OBJ * Mach.NAME)

(* Fundamental object methods *)

(* FIXME: possibly move this to mach.sml *) 

fun allocObjFixtures (scope:Mach.SCOPE) 
                     (obj:Mach.OBJ) 
                     (f:Ast.FIXTURES) 
    : unit =
    case obj of 
        Mach.Obj { props, ...} => 
        let 
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
                    LogErr.evalError ["attempt to allocate void-type property"]

                  (* FIXME: is this correct? Maybe we need to check them all to be nullable? *)
                  | Ast.UnionType _ => 
                    Mach.ValProp (Mach.Null)

                  | Ast.ArrayType _ => 
                    Mach.ValProp (Mach.Null)

                  | Ast.NominalType { ident, ... } => 
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

            fun allocFixture (n, f) = 
                let 
                    fun allocProp state p = 
                        if Mach.hasProp props n
                        then LogErr.defnError 
                                 ["allocating duplicate property name: ", 
                                  LogErr.name n]
                        else (LogErr.trace ["allocating ", state, " property ", 
                                            LogErr.name n]; 
                              Mach.addProp props n p)
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
                        
                      | Ast.ValFixture { ty, readOnly, ... } => 
                        allocProp "value" 
                                  { ty = ty,
                                    state = valAllocState ty,
                                    attrs = { dontDelete = true,
                                              dontEnum = false,
                                              readOnly = readOnly,
                                              isFixed = true } }
                        
                      | Ast.VirtualValFixture { ty, setter, ... } => 
                        allocProp "virtual value" 
                                  { ty = ty,
                                    state = Mach.UninitProp,
                                    attrs = { dontDelete = true,
                                              dontEnum = false,
                                              readOnly = (case setter of NONE => true | _ => false),
                                              isFixed = true } }
                        
                      | Ast.ClassFixture cls => 
                        allocProp "class"
                                  { ty = Mach.classType,
                                    state = Mach.ValProp (Mach.newClass scope cls),
                                    attrs = { dontDelete = true,
                                              dontEnum = true,
                                              readOnly = true,
                                              isFixed = true } }

                      | Ast.NamespaceFixture ns => 
                        allocProp "namespace" 
                                  { ty = Mach.namespaceType,
                                    state = Mach.ValProp (Mach.newNamespace ns),
                                    attrs = { dontDelete = true,
                                              dontEnum = true,
                                              readOnly = true,
                                              isFixed = true } }
                        
                      | Ast.TypeVarFixture =>
                        allocProp "type variable"
                                  { ty = Mach.typeType,
                                    state = Mach.TypeVarProp,
                                    attrs = { dontDelete = true,
                                              dontEnum = true,
                                              readOnly = true,
                                              isFixed = true } }
                end
        in                    
            List.app allocFixture f
        end

fun extendScope (p:Mach.SCOPE) 
                (t:Mach.SCOPE_TAG) 
                (ob:Mach.OBJ) 
    : Mach.SCOPE = 
    Mach.Scope { parent=(SOME p), tag=t, object=ob, temps=ref [] }

    
fun allocScopeFixtures (scope:Mach.SCOPE) 
                       (f:Ast.FIXTURES) 
    : unit = 
        allocObjFixtures scope (getScopeObj scope) f

    
fun evalExpr (scope:Mach.SCOPE) 
             (expr:Ast.EXPR)
    : Mach.VAL =
    case expr of
        Ast.LiteralExpr lit => 
        evalLiteralExpr lit
        
      | Ast.ListExpr es =>
        evalListExpr scope es
        
      | Ast.LexicalRef { ident } =>
            Mach.getValue (evalRefExpr scope expr true)

      | Ast.ObjectRef { base, ident } => 
            Mach.getValue (evalRefExpr scope expr false)
        
      | Ast.LetExpr {defs, body, fixtures} =>
        evalLetExpr scope (valOf fixtures) defs body

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
                                       (needObj (Mach.getValue (obj, name))) 
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
        
      | Ast.FunExpr func => 
        let
        in
            Mach.newFunc scope func
        end
      | Ast.AllocTemp (n,e) => 
        let
            val Mach.Scope frame = scope
            val v = evalExpr scope e
            val temps = !(#temps frame)
        in
            if n = (List.length temps)
                then (#temps frame) := v::(!(#temps frame)) 
                else LogErr.evalError ["temp count out of sync"];
            v
        end
      | Ast.KillTemp n => 
        let
            val Mach.Scope frame = scope
            val temps = !(#temps frame)
        in
            if n < (List.length temps)
                then (#temps frame) := tl (!(#temps frame))
                else LogErr.evalError ["temp count out of sync"];
            Mach.Undef
        end
      | Ast.GetTemp n =>
        let
            val Mach.Scope frame = scope
        in
            List.nth (!(#temps frame),n)
        end
      | _ => LogErr.unimplError ["unhandled expression type"]


and evalLiteralExpr (lit:Ast.LITERAL) 
    : Mach.VAL = 
    case lit of 
        Ast.LiteralNull => Mach.Null
      | Ast.LiteralUndefined => Mach.Undef
      | Ast.LiteralNumber n => Mach.newNumber n
      | Ast.LiteralBoolean b => Mach.newBoolean b
      | Ast.LiteralString s => Mach.newString s
      | Ast.LiteralNamespace n => Mach.newNamespace n
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
                if Mach.hasProp props Mach.publicPrototypeName
                then Mach.getValue (ctorObj, Mach.publicPrototypeName)
                else Mach.Null
            val (newObj:Mach.OBJ) = 
                Mach.newObj Mach.intrinsicObjectBaseTag proto NONE
        in
            Mach.defValue newObj Mach.internalConstructorName (Mach.Object ctorObj);
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
            SOME (Mach.Class c) => constructClassInstance c args
          | SOME (Mach.Function f) => constructObjectViaFunction obj f args
          | _ => LogErr.evalError ["operator 'new' applied to unknown object"]

                                                                                 
and evalCallExpr (thisObjOpt:Mach.OBJ option) 
                 (fobj:Mach.OBJ) 
                 (args:Mach.VAL list) 
    : Mach.VAL =
    let 
        val _ = LogErr.trace ["evalCallExpr"]
        val thisObj = case thisObjOpt of 
                          NONE => Mach.globalObject
                        | SOME t => t
        val thisVal = Mach.Object thisObj
    in
        case fobj of
            Mach.Obj { magic, ... } => 
            case !magic of 
                SOME (Mach.HostFunction f) => 
                    f args
              | SOME (Mach.Function f) => 
                    (invokeFuncClosure thisObj f args
                        handle ReturnException v => v)
              | _ => 
                    if Mach.hasValue fobj Mach.intrinsicInvokeName
                    then
                        let 
                            val invokeFn = Mach.getValue (fobj, Mach.intrinsicInvokeName)
                        in
                            evalCallExpr NONE (needObj invokeFn) (thisVal :: args)
                        end
                    else LogErr.evalError ["calling non-callable object"]
    end

and evalSetExpr (scope:Mach.SCOPE) 
                (aop:Ast.ASSIGNOP) 
                (pat:Ast.PATTERN) 
                (v:Mach.VAL) 
    : Mach.VAL = 
    let
        fun modified obj name = 
            let 
                fun modifyWith bop = 
                    performBinop bop (Mach.getValue (obj, name)) v
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
    in case pat of 
        Ast.SimplePattern expr => 
            let
                val r = evalRefExpr scope expr false
            in
                case r of (obj, name) => 
                      let 
                          val v = modified obj name
                      in 
                          Mach.setValue obj name v;
                          v
                      end
            end
      | _ => LogErr.unimplError ["unexpected pattern form in assignment"]
    end


and evalUnaryOp (scope:Mach.SCOPE) 
                (unop:Ast.UNOP) 
                (expr:Ast.EXPR) 
    : Mach.VAL =
    let
        fun crement f isPre = 
            let 
                val r = evalRefExpr scope expr false
                val n = case r of 
                            (obj, name) => 
                            Mach.toNum (Mach.getValue (obj, name))
                val n' = Mach.newNumber (f (n, 1.0))
                val n'' = if isPre
                          then n'
                          else Mach.newNumber n
            in
                case r of 
                    (obj, name) => 
                    (Mach.setValue obj name n'; n'')
            end
    in            
        case unop of 
            Ast.Delete => 
            (case evalRefExpr scope expr false of
                 (Mach.Obj {props, ...}, name) => 
                 (Mach.delProp props name; Mach.newBoolean true))
            
          | Ast.PreIncrement => crement Real.+ true
          | Ast.PreDecrement => crement Real.- true
          | Ast.PostIncrement => crement Real.+ false
          | Ast.PostDecrement => crement Real.- false
          | _ => LogErr.unimplError ["unhandled unary operator"]
    end


and performBinop (bop:Ast.BINOP) 
                 (a:Mach.VAL) 
                 (b:Mach.VAL) 
    : Mach.VAL = 
    let 
        fun wordOp wop = 
            let 
                val wa = Word.fromInt (trunc (Mach.toNum a))
                val wb = Word.fromInt (trunc (Mach.toNum b))
            in
                Mach.newNumber (real (Word.toInt (wop (wa, wb))))
            end
        fun isNum n = 
            case n of 
                Mach.Object (Mach.Obj {magic, ... }) => 
                             (case !magic of 
                                  SOME (Mach.Number _) => true
                                | _ => false)
              | _ => false
    in
        case bop of 
            Ast.Plus _ => 
            if isNum a andalso isNum b
            then Mach.newNumber ((Mach.toNum a) + (Mach.toNum b))
            else Mach.newString ((Mach.toString a) ^ (Mach.toString b))
          | Ast.Minus _ => Mach.newNumber ((Mach.toNum a) - (Mach.toNum b))
          | Ast.Times _ => Mach.newNumber ((Mach.toNum a) * (Mach.toNum b))
          | Ast.Divide _ => Mach.newNumber ((Mach.toNum a) / (Mach.toNum b))
          | Ast.Remainder _ => Mach.newNumber (real (Int.rem ((trunc (Mach.toNum a)), 
                                                              (trunc (Mach.toNum b)))))
                             
          | Ast.LeftShift => wordOp Word.<<
          | Ast.RightShift => wordOp Word.>>
          | Ast.RightShiftUnsigned => wordOp Word.~>>
          | Ast.BitwiseAnd => wordOp Word.andb
          | Ast.BitwiseOr => wordOp Word.orb
          | Ast.BitwiseXor => wordOp Word.xorb
                              
          | Ast.Equals _ => Mach.newBoolean (Mach.equals a b)
          | Ast.NotEquals _ => Mach.newBoolean (not (Mach.equals a b))
          | Ast.StrictEquals _ => Mach.newBoolean (Mach.equals a b)
          | Ast.StrictNotEquals _ => Mach.newBoolean (not (Mach.equals a b))
          | Ast.Less _ => Mach.newBoolean (Mach.less a b)
          | Ast.LessOrEqual _ => Mach.newBoolean ((Mach.less a b) orelse (Mach.equals a b))
          | Ast.Greater _ => Mach.newBoolean (not ((Mach.less a b) orelse (Mach.equals a b)))
          | Ast.GreaterOrEqual _ => Mach.newBoolean (not (Mach.less a b))
                                  
          | _ => LogErr.unimplError ["unhandled binary operator type"]
    end


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
        
      | _ => performBinop bop (evalExpr scope aexpr) (evalExpr scope bexpr)


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
    : Mach.MULTINAME = 
    case r of 
        Ast.Identifier { ident, openNamespaces } => 
        { nss=openNamespaces, id=ident }
        
      | Ast.QualifiedIdentifier { qual, ident } => 
        { nss = [[needNamespace (evalExpr scope qual)]], id = ident }

      | Ast.QualifiedExpression { qual, expr } => 
        { nss = [[needNamespace (evalExpr scope qual)]], 
          id = Mach.toString (evalExpr scope expr) }

      | _ => LogErr.unimplError ["unimplemented identifier expression form"]


and evalRefExpr (scope:Mach.SCOPE)
                (expr:Ast.EXPR)
                (errIfNotFound:bool)
    : REF =

    let 
        fun makeRefNotFound (b:Mach.VAL option) (mname:Mach.MULTINAME) 
            : REF =
            let
            in case (b,mname) of
                (SOME (Mach.Object ob),{id,...}) =>
                    (ob,{ns=Ast.Internal "",id=id})  (* FIXME: ns might be user settable default *)
              | (NONE,{id,...}) => 
                    (Mach.globalObject,{ns=Ast.Internal "",id=id})
              | _ => LogErr.evalError ["ref expression messed up in refOf"]
            end

        val (base,ident) =
            case expr of         
                Ast.LexicalRef { ident } => (NONE,ident)
              | Ast.ObjectRef { base, ident } => (SOME (evalExpr scope base), ident)
              | _ => LogErr.evalError ["need lexical or object-reference expression"]

        val (multiname:Mach.MULTINAME) = evalIdentExpr scope ident

        val refOpt = (case base of 
                          SOME (Mach.Object ob) => 
                              resolveOnObjAndPrototypes ob multiname
                        | NONE => 
                              resolveOnScopeChain scope multiname
                        | _ => LogErr.evalError ["ref expression on non-object value"])

    in
        case refOpt of 
            NONE => if errIfNotFound 
                        then LogErr.evalError ["unresolved identifier expression",LogErr.multiname multiname]
                        else makeRefNotFound base multiname
          | SOME r' => r'
    end


and evalLetExpr (scope:Mach.SCOPE) 
                (fixtures:Ast.FIXTURES) 
                (defs:Ast.VAR_BINDING list) 
                (body:Ast.EXPR list) 
    : Mach.VAL = 
    let 
        val obj = Mach.newObj Mach.intrinsicObjectBaseTag Mach.Null NONE
        val newScope = extendScope scope Mach.Let obj        
    in
        allocScopeFixtures scope fixtures;
        (* todo: what do we use for a namespace here *)
        evalVarBindings scope (Ast.Internal "") defs; 
        evalListExpr newScope body
    end


and processVarDefn (scope:Mach.SCOPE)
                   (vd:Ast.VAR_DEFN)
                   (procOneName:Mach.NAME -> Mach.VAL -> unit)
    : unit = 
    let
        val ns = needNamespace (evalExpr scope (#ns vd))
        fun procOneBinding (vb:Ast.VAR_BINDING) = 
            processVarBinding scope NONE ns vb procOneName
    in
        List.app procOneBinding (#bindings vd)
    end


and processVarBinding (scope:Mach.SCOPE) 
                      (v:Mach.VAL option)
                      (ns:Ast.NAMESPACE)
                      (binding:Ast.VAR_BINDING)
                      (procOneName:Mach.NAME -> Mach.VAL -> unit)
    : unit =
    case binding of 
        Ast.Binding { init, pattern, ty } =>
        let 
        fun procWithValue v' = 
                case pattern of 
                    Ast.IdentifierPattern id => 
                         let 
                             val n = { ns = ns, id = id}
                         in
                 LogErr.trace ["binding variable ", LogErr.name n];
                 procOneName n v'
                         end
                  | _ => LogErr.unimplError ["unhandled pattern form in binding"]                     
        in
            case v of 
                SOME v' => procWithValue v'
              | NONE => 
                (case init of 
                     SOME e => procWithValue (evalExpr scope e)
                   | NONE => ())
        end
        

and evalVarBinding (scope:Mach.SCOPE)
                   (v:Mach.VAL option)
                   (ns:Ast.NAMESPACE)
                   (defn:Ast.VAR_BINDING)
    : unit = 
    (* Here we are evaluating only the *definition* affect of the
     * binding, as the binding produced a fixture and we've already 
     * allocated and possibly initialized a property for the fixture.
     *)
    processVarBinding scope v ns defn (Mach.defValue (getScopeObj scope))


and evalVarBindings (scope:Mach.SCOPE)
                    (ns:Ast.NAMESPACE)
                    (defns:Ast.VAR_BINDING list)
    : unit =
    List.app (evalVarBinding scope NONE ns) defns


and resolveOnScopeChain (scope:Mach.SCOPE) 
                        (mname:Mach.MULTINAME) 
    : REF option =
    (LogErr.trace ["resolving multiname on scope chain: ", LogErr.multiname mname];     
     case scope of 
         Mach.Scope { parent, object, ... } => 
         case resolveOnObjAndPrototypes object mname of
             NONE => (case parent of 
                          SOME p => resolveOnScopeChain p mname
                        | NONE => NONE)
           | result => result)


and resolveOnObjAndPrototypes (obj:Mach.OBJ) 
                              (mname:Mach.MULTINAME) 
    : REF option = 
    (LogErr.trace ["resolveOnObjAndPrototypes: ", LogErr.multiname mname];
    case obj of 
        Mach.Obj ob => 
        case resolveOnObj obj mname of 
            NONE => 
            let 
                val proto = !(#proto ob)
            in
                case proto of 
                    Mach.Object ob => resolveOnObjAndPrototypes ob mname
                  | _ => NONE
            end
          | result => result)


and resolveOnObj (obj:Mach.OBJ) 
                 (mname:Mach.MULTINAME) 
    : REF option =
    case obj of 
        Mach.Obj ob => 
        let     
            val id = (#id mname)                     
            val _ = LogErr.trace ["candidate object props: "]
            val _ = List.app (fn (k,_) => LogErr.trace ["prop: ", LogErr.name k]) (!(#props ob))

            fun tryName [] = NONE
              | tryName (x::xs) =
                let 
                    val n = { ns=x, id=id } 
                    val _ = LogErr.trace(["trying ",LogErr.name n])
                in
                    if Mach.hasProp (#props ob) n
                    then (LogErr.trace ["found property with name ", LogErr.name n]; 
                          SOME (obj, n))
                    else tryName xs
                end

            (* try each of the nested namespace sets in turn to see
               if there is a match. raise an exception if there is
               more than one match. continue down the scope stack
               if there are none *)

            fun tryMultiname [] = NONE  
              | tryMultiname (x::xs:Ast.NAMESPACE list list) = 
                let 
                    val rf = tryName x
                in case rf of
                    SOME rf => SOME rf
                  | NONE => tryMultiname xs
                end

        in
            tryMultiname (#nss mname)  (* todo: check for only one match *)
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


and labelEq (stmtLabel:Ast.IDENT option) 
            (exnLabel:Ast.IDENT option) 
    : bool = 
    case (stmtLabel, exnLabel) of
        (SOME sl, SOME el) => sl = el
      | (NONE, SOME _) => false
      | (_, NONE) => true


and evalStmts (scope:Mach.SCOPE) 
              (stmts:Ast.STMT list) 
    : Mach.VAL = 
    (map (evalStmt scope) stmts; Mach.Undef)


and evalStmt (scope:Mach.SCOPE) 
             (stmt:Ast.STMT) 
    : Mach.VAL = 
    case stmt of 
        Ast.ExprStmt e => evalListExpr scope e
      | Ast.IfStmt {cnd,thn,els} => evalIfStmt scope cnd thn els
      | Ast.WhileStmt w => evalWhileStmt scope w
      | Ast.ReturnStmt r => evalReturnStmt scope r
      | Ast.BreakStmt lbl => evalBreakStmt scope lbl
      | Ast.ContinueStmt lbl => evalContinueStmt scope lbl
      | Ast.ThrowStmt t => evalThrowStmt scope t
      | Ast.LabeledStmt (lab, s) => evalLabelStmt scope lab s
      | Ast.BlockStmt b => evalBlock scope b
      | Ast.ClassBlock c => evalClassBlock scope c
      | Ast.EmptyStmt => Mach.Undef
      | _ => LogErr.unimplError ["unimplemented statement type"]


and multinameOf (n:Ast.NAME) = 
    { nss = [[(#ns n)]], id = (#id n) }

and findVal (scope:Mach.SCOPE) 
            (mn:Ast.MULTINAME) 
    : Mach.VAL = 
    case resolveOnScopeChain scope mn of 
        NONE => LogErr.evalError ["unable to resolve multiname: ", 
                                  LogErr.multiname mn ]
      | SOME (obj, name) => Mach.getValue (obj, name) 


and evalDefn (scope:Mach.SCOPE) 
             (d:Ast.DEFN) 
    : unit = 
    case d of 
        Ast.FunctionDefn f => evalFuncDefn scope f
      | Ast.VariableDefn {bindings,ns,...} => 
        evalVarBindings scope (needNamespace (evalExpr scope ns)) bindings
      | Ast.NamespaceDefn ns => () (* handled during allocation *)
(*      | Ast.ClassDefn cd => evalClassDefn scope cd  *)
      | _ => LogErr.unimplError ["unimplemented definition type"]


and evalDefns (scope:Mach.SCOPE) 
              (ds:Ast.DEFN list) 
    : unit = 
    List.app (evalDefn scope) ds


and checkAllPropertiesInitialized (obj:Mach.OBJ)
    : unit = 
    let 
        fun checkOne (n:Mach.NAME, p:Mach.PROP) = 
            case (#state p) of 
                Mach.UninitProp => LogErr.evalError ["uninitialized property: ", 
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
    case closure of 
        { func=(Ast.Func f), allTypesBound, env } => 
        if not allTypesBound
        then LogErr.evalError ["invoking function with unbound type variables"]
        else 
            case (#fsig f) of 
                Ast.FunctionSignature { params, ... } => 
                let
                    val (varObj:Mach.OBJ) = Mach.newSimpleObj NONE
                    val (varScope:Mach.SCOPE) = extendScope env Mach.VarActivation varObj

                    fun initArg v = 
                        let
                            val Mach.Scope frame = varScope
                        in
                            (#temps frame) := v::(!(#temps frame)) 
                        end

                    (* FIXME: infer a fixture for "this" so that it's properly typed, dontDelete, etc.
                     * Also this will mean changing to defVar rather than setVar, for 'this'. *)
                    val thisName = { id = "this", ns = Ast.Internal "" }
                    val thisVal = Mach.Object this
                    fun initThis v = 
                        let
                        in
                            Mach.defValue varObj thisName thisVal
                        end

                    (* FIXME: self-name binding is surely more complex than this! *)
                    val selfName = { id = (#ident (#name f)), ns = Ast.Internal "" }
                    val selfTag = Mach.FunctionTag (#fsig f)
                    val selfVal = Mach.newObject selfTag Mach.Null (SOME (Mach.Function closure))

                in
                    allocScopeFixtures varScope (valOf (#fixtures f));
                    (* FIXME: handle arg-list length mismatch correctly. *)
                    initThis thisVal;
                    List.app initArg (List.rev args);
                    evalStmts varScope (#inits f);

                    (* NOTE: is this for the binding of a function expression to its optional
                       identifier? If so, we need to extend the scope chain before extending it
                       with the activation object, and add the self-name binding to that new 
                       scope, as in sec 13 ed. 3.

                       Changing defValue to setValue for now.
                    *)
                    Mach.setValue varObj selfName selfVal;

                    checkAllPropertiesInitialized varObj;
                    evalBlock varScope (#body f)
                end


and constructClassInstance (closure:Mach.CLS_CLOSURE) 
                           (args:Mach.VAL list) 
    : Mach.VAL = 
    case closure of 
        { cls, allTypesBound, env } => 
        if not allTypesBound
        then LogErr.evalError ["constructing instance of class with unbound type variables"]
        else 
            case cls of 
                Ast.Cls { instanceFixtures, ... } =>
                let
                    val obj = Mach.newObj Mach.intrinsicObjectBaseTag Mach.Null NONE
(*
                    val ns = (#ns definition)
                    val n = { ns = needNamespace (evalExpr env ns), 
                              id = (#ident definition) }
                    val _ = LogErr.trace ["constructing instance of ", LogErr.name n]
                    val classTag = Mach.ClassTag n
                    val proto = if Mach.hasOwnValue obj Mach.publicPrototypeName
                                then Mach.getValue (obj, Mach.publicPrototypeName)
                                else Mach.Null
                    val (obj:Mach.OBJ) = Mach.newObj classTag proto NONE
                    val (objScope:Mach.SCOPE) = extendScope env Mach.VarInstance obj
                    val (instance:Mach.VAL) = Mach.Object obj
(* FIXME: need to get ctor from instance block
                    val ctor = (#constructor definition)
*)
                    val ctor:Ast.FUNC_DEFN option = NONE

                    (* FIXME: infer a fixture for "this" so that it's properly typed, dontDelete, etc.
                     * Also this will mean changing to defVar rather than setVar, for 'this'. *)
                    val thisName = { id = "this", ns = Ast.Internal "" }

                    (* FIXME: self-name binding is surely more complex than this! *)
                    val selfTag = Mach.ClassTag n
                    val selfVal = Mach.newObject selfTag Mach.Null (SOME (Mach.Class closure))
(* FIXME: need to get instance block from fixture that is passed into this function
                    val Ast.Block iblk = (valOf (#instanceBlock definition))
*)
                    val iblk = NONE
*)
                in
                    allocObjFixtures env obj instanceFixtures;
(*
                    case ctor of 
                        NONE => (checkAllPropertiesInitialized obj; instance)
                      | SOME ({native, ns,
                               func = Ast.Func 
                                          { fsig=Ast.FunctionSignature { params, inits, ... }, 
                                            body, fixtures, ... }, ... }) => 
                        let 
                            val ns = needNamespace (evalExpr env ns)
                            val (varObj:Mach.OBJ) = Mach.newSimpleObj NONE
                            val (varScope:Mach.SCOPE) = extendScope env Mach.VarActivation varObj
                            fun bindArg (a, b) = evalVarBinding varScope (SOME a) (Ast.Internal "") b
                            fun initInstanceVar (vd:Ast.VAR_DEFN) = 
                                processVarDefn env vd (Mach.defValue obj)

                        in
                            allocScopeFixtures varScope (valOf fixtures);
                            (* FIXME: is this correct? we currently bind the self name on obj as well.. *)
                            Mach.defValue obj n selfVal;
                            LogErr.trace ["initialializing instance methods of ", LogErr.name n];
                            List.app (evalFuncDefnFull env obj) (#instanceMethods definition);
                            List.app initInstanceVar (#instanceVars definition);

                            (* FIXME: evaluate instance-var initializers declared in class as well. *)

                            if (native)
                            then 
                                (* Native constructors take over here. *)
                                let 
                                    val nativeCtor = Native.getNativeMethod n n
                                    val _ = LogErr.trace ["running native constructor for ", LogErr.name n]
                                    val result = nativeCtor env obj args
                                in
                                    LogErr.trace ["checking native initialization of ", LogErr.name n];
                                    checkAllPropertiesInitialized obj;
                                    result
                                end
                            else 
                                (* Otherwise run any initializers and constructor. *)
                                ((* FIXME: handle arg-list length mismatch correctly. *)
                                 LogErr.trace ["binding constructor args of ", LogErr.name n];
                                 List.app bindArg (ListPair.zip (args, params));
                                 Mach.setValue varObj thisName instance;
                                 Mach.defValue varObj n selfVal;
                                 LogErr.trace ["running initializers of ", LogErr.name n];

                                 (case inits of 
                                      NONE => ()
                                    | SOME inits => 
                                      let
                                          val (initVals:Mach.VAL list) = List.map (evalExpr varScope) inits
                                          fun bindInit (a, b) = evalVarBinding objScope (SOME a) ns b
                                      in
                                          List.app bindInit (ListPair.zip (initVals, bindings))
                                      end);
                                 LogErr.trace ["checking initialization of ", LogErr.name n];
                                 checkAllPropertiesInitialized obj; 
                                 let 
                                     (* Now the strange part: we re-parent the arguments var object
                                      * to the instance object, before running the constructor body. *)
                                     val _ = LogErr.trace ["running constructor of ", LogErr.name n]
                                     val (newVarScope:Mach.SCOPE) = extendScope objScope Mach.VarActivation varObj
                                     val _ = evalBlock newVarScope body 
                                 in 
                                     instance
                                 end)
                        end
*)
                    Mach.Object obj
                end

(* 
 * This is the dynamic phase of function definition; it assumes that the 
 * function has already had its fixtures constructed during the
 * definition phase and a property for it has been allocated to the ininitialized
 * state.
 *) 

and evalFuncDefn (scope:Mach.SCOPE) 
                 (f:Ast.FUNC_DEFN) =
    evalFuncDefnFull scope (getScopeObj scope) f

and evalFuncDefnFull (scope:Mach.SCOPE) 
                     (target:Mach.OBJ)
                     (f:Ast.FUNC_DEFN)
    : unit = 
    let 
        val func = (#func f)
        val name = case func of Ast.Func { name={ident, kind}, ...} => 
                                case kind of 
                                    Ast.Ordinary => ident
                                  | Ast.Call => "call" (* FIXME: hack until parser fixed *)
                                  | _ => LogErr.unimplError ["evaluating unhandled type of function name"]

        val fval = Mach.newFunc scope func
        val fname = {id = name, 
                     ns = (needNamespace (evalExpr scope (#ns f)))}
    in
        LogErr.trace ["defining function ", LogErr.name fname];
        Mach.defValue target fname fval
    end

(*
    BLOCK
*)

and evalBlock (scope:Mach.SCOPE) 
              (block:Ast.BLOCK) 
    : Mach.VAL = 
    case block of 
        Ast.Block {defns, stmts, fixtures, inits, ...} => 
        let 
            val blockObj = Mach.newObj Mach.intrinsicObjectBaseTag Mach.Null NONE
            val blockScope = extendScope scope Mach.Let blockObj
        in
            LogErr.trace ["initializing block scope"];
            allocScopeFixtures blockScope (valOf fixtures);
            LogErr.trace ["evaluating block scope statements"];
            let 
                val v = evalStmts blockScope stmts 
            in 
                LogErr.trace ["exiting block"];
                v
            end
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
                       (extends:Ast.NAME option) 
    : unit =
    let 
        val baseProtoVal = 
            case extends of 
                NONE => Mach.Null
              | SOME baseClassName => 
                let
                in
                    case findVal scope (multinameOf baseClassName) of 
                        Mach.Object ob => 
                        if Mach.hasOwnValue ob Mach.publicPrototypeName
                        then Mach.getValue (ob, Mach.publicPrototypeName)
                        else Mach.Null
                      | _ => LogErr.evalError ["base class resolved to non-object: ", 
                                               LogErr.name baseClassName]
                end
                
        val _ = LogErr.trace ["constructing prototype"]
        val newPrototype = Mach.newObj Mach.intrinsicObjectBaseTag baseProtoVal NONE

    in
        Mach.defValue classObj Mach.publicPrototypeName (Mach.Object newPrototype);
        LogErr.trace ["finished initialising class prototype"]
    end

and evalClassBlock (scope:Mach.SCOPE) 
                   ({name:Ast.NAME option,fixtures:Ast.FIXTURES option,block:Ast.BLOCK,extends,...})
    : Mach.VAL =

    (* 
        The property that holds the class object was allocated when the 
        fixtures of the outer scope were allocated. Still to do is
        allocating and initialising the class object

        Steps:
        - allocate the class object
        - allocate the class prototype object (an instance of the class)
        - set the outer class property
        - push the class object on to the scope chain
        - execute the current block
    *)

    let 
        val _ = LogErr.trace ["evaluating class stmt for ", LogErr.name (valOf name)]

        (* get the class object allocated when the property was instantiated *)
        val classObj = needObj (findVal scope (multinameOf (valOf name)))

        (* allocate the fixed properties for the class object *)
        val _ = allocObjFixtures scope classObj (valOf fixtures)

        (* init the class prototype if not going to be set by the user *)
        val _ = initClassPrototype scope classObj extends

        (* extend the scope chain with the class object *)
        val classScope = extendScope scope Mach.VarClass classObj

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
    handle BreakException exnLabel 
           => if labelEq (SOME lab) exnLabel
              then Mach.Undef
              else raise BreakException exnLabel


and evalWhileStmt (scope:Mach.SCOPE) 
                  (whileStmt:Ast.WHILE_STMT) 
    : Mach.VAL = 
    case whileStmt of 
        { cond, body, contLabel } => 
        let
            fun loop (accum:Mach.VAL option) = 
                let 
                    val v = evalExpr scope cond
                    val b = Mach.toBoolean v
                in
                    if b 
                    then 
                        let
                            val curr = (SOME (evalStmt scope body)
                                        handle ContinueException exnLabel => 
                                               if labelEq contLabel exnLabel
                                               then NONE
                                               else raise ContinueException exnLabel)
                            val next = (case curr 
                                         of NONE => accum
                                          | x => x)
                        in
                            loop next
                        end
                    else
                        accum
                end
        in
            case loop NONE of
                NONE => Mach.Undef
              | SOME v => v
        end


and evalReturnStmt (scope:Mach.SCOPE) 
                   (e:Ast.EXPR list) 
    : Mach.VAL =
    raise (ReturnException (evalListExpr scope e))


and evalThrowStmt (scope:Mach.SCOPE) 
                  (e:Ast.EXPR list) 
    : Mach.VAL =
    raise (ThrowException (evalListExpr scope e))


and evalBreakStmt (scope:Mach.SCOPE) 
                  (lbl:Ast.IDENT option) 
    : Mach.VAL =
    raise (BreakException lbl)


and evalContinueStmt (scope:Mach.SCOPE) 
                     (lbl:Ast.IDENT option) 
    : Mach.VAL =
    raise (ContinueException lbl)


and evalPackage (scope:Mach.SCOPE) 
                (package:Ast.PACKAGE) 
    : Mach.VAL = 
    evalBlock scope (#body package)


and evalProgram (prog:Ast.PROGRAM) 
    : Mach.VAL = 
    (Mach.populateIntrinsics Mach.globalObject;
     allocScopeFixtures Mach.globalScope (valOf (#fixtures prog));
     map (evalPackage Mach.globalScope) (#packages prog);
     evalBlock Mach.globalScope (#body prog))

end
