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

fun allocFixtures (scope:Mach.SCOPE) 
                  (obj:Mach.OBJ) 
                  (temps:Mach.TEMPS)
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

            fun allocFixture (n, f) = 
                case n of 
                    Ast.TempName t => 
                    (case f of 
                         Ast.ValFixture { ty, ... } => 
                         (if t = (List.length (!temps))
                          then temps := (ty, Mach.UninitTemp)::(!temps) 
                          else LogErr.evalError ["temp count out of sync"])
                       | _ => LogErr.evalError ["allocating non-value temporary"])
                  | Ast.PropName pn => 
                    let 
                        fun allocProp state p = 
                            if Mach.hasProp props pn
                            then LogErr.evalError 
                                     ["allocating duplicate property name: ", 
                                      LogErr.name pn]
                            else (LogErr.trace ["allocating ", state, " property ", 
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
                (ob:Mach.OBJ) 
    : Mach.SCOPE = 
    Mach.Scope { parent=(SOME p), object=ob, temps=ref [] }

    
fun allocObjFixtures (scope:Mach.SCOPE) 
                     (obj:Mach.OBJ) 
                     (f:Ast.FIXTURES) 
    : unit = 
    let 
        val (temps:Mach.TEMPS) = ref [] 
    in
        allocFixtures scope obj temps;
        if not ((length (!temps)) = 0)
        then LogErr.evalError ["allocated temporaries in non-scope object"]
        else ()
    end
    
fun allocScopeFixtures (scope:Mach.SCOPE) 
                       (f:Ast.FIXTURES) 
    : unit = 
    case scope of
        Mach.Scope { object, temps, ...} => 
        allocFixtures scope object temps f

    
fun evalExpr (scope:Mach.SCOPE) 
             (expr:Ast.EXPR)
    : Mach.VAL =
    case expr of
        Ast.LiteralExpr lit => 
        evalLiteralExpr scope lit
        
      | Ast.ListExpr es =>
        evalListExpr scope es
        
      | Ast.LexicalRef { ident } =>
            Mach.getValue (evalRefExpr scope expr true)

      | Ast.ObjectRef { base, ident } => 
            Mach.getValue (evalRefExpr scope expr false)
        
      | Ast.LetExpr {defs, body, fixtures, inits} =>
        evalLetExpr scope (valOf fixtures) (valOf inits) body

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

      | Ast.GetTemp n => 
        Mach.getTemp (getScopeTemps scope) n

      | Ast.DefTemp (n, e) => 
        (Mach.defTemp (getScopeTemps scope) n (evalExpr scope e);
         Mach.Undef)
        
      | _ => LogErr.unimplError ["unhandled expression type"]


and evalLiteralExpr (scope:Mach.SCOPE) 
                    (lit:Ast.LITERAL) 
    : Mach.VAL = 
    case lit of 
        Ast.LiteralNull => Mach.Null
      | Ast.LiteralUndefined => Mach.Undef
      | Ast.LiteralNumber n => Mach.newNumber n
      | Ast.LiteralBoolean b => Mach.newBoolean b
      | Ast.LiteralString s => Mach.newString s
      | Ast.LiteralNamespace n => Mach.newNamespace n
      | Ast.LiteralFunction { func, ... } => Mach.newFunc scope func
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
            SOME (Mach.Class c) => constructClassInstance obj c args
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
                (lhs:Ast.EXPR) 
                (v:Mach.VAL) 
    : Mach.VAL = 
    let
        val (obj, name) = evalRefExpr scope lhs false
        val v =
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
    in
        Mach.setValue obj name v;
        v
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
            case (b,mname) of
                (SOME (Mach.Object ob),{id,...}) =>
                (ob,{ns=Ast.Internal "",id=id})  (* FIXME: ns might be user settable default *)
              | (NONE,{id,...}) => 
                (Mach.globalObject,{ns=Ast.Internal "",id=id})
              | _ => LogErr.evalError ["ref expression messed up in refOf"]

        val (base,ident) =
            case expr of         
                Ast.LexicalRef { ident } => (NONE,ident)
              | Ast.ObjectRef { base, ident } => (SOME (evalExpr scope base), ident)
              | _ => LogErr.evalError ["need lexical or object-reference expression"]

        val (multiname:Mach.MULTINAME) = evalIdentExpr scope ident

        val refOpt = 
            case base of 
                SOME (Mach.Object ob) => 
                resolveOnObjAndPrototypes ob multiname
              | NONE => 
                resolveOnScopeChain scope multiname
              | _ => LogErr.evalError ["ref expression on non-object value"]
                                                                             
    in
        case refOpt of 
            NONE => if errIfNotFound 
                    then LogErr.evalError ["unresolved identifier expression",
                                           LogErr.multiname multiname]
                    else makeRefNotFound base multiname
          | SOME r' => r'
    end

and evalLetExpr (scope:Mach.SCOPE) 
                (fixtures:Ast.FIXTURES) 
                (inits:Ast.INITS) 
                (body:Ast.EXPR) 
    : Mach.VAL = 
    let 
        val obj = Mach.newObj Mach.intrinsicObjectBaseTag Mach.Null NONE
        val newScope = extendScope scope obj        
    in
        allocScopeFixtures scope fixtures;
        evalInits scope obj (getScopeTemps scope) inits;
        evalExpr newScope body
    end

and resolveOnScopeChain (scope:Mach.SCOPE) 
                        (mname:Mach.MULTINAME) 
    : REF option =
    let 
        val _ = LogErr.trace ["resolving multiname on scope chain: ", 
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
            SOME (Mach.Scope {object, ...}, name) => SOME (object, name)
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
                              (mname:Mach.MULTINAME) 
    : REF option = 
    let 
        val _ = LogErr.trace ["resolveOnObjAndPrototypes: ", LogErr.multiname mname];
        fun hasFixedProp (Mach.Obj {props, ...}, n) = Mach.hasFixedProp props n
        fun hasProp (Mach.Obj {props, ...}, n) = Mach.hasProp props n
        fun getObjProto (Mach.Obj {proto, ...}) = 
            case (!proto) of 
                Mach.Object ob => SOME ob
              | _ => NONE
    in
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
        Ast.ExprStmt e => evalExpr scope e
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
        { func=(Ast.Func f), env, allTypesBound } => 
        if not allTypesBound
        then LogErr.evalError ["invoking function with unbound type variables"]
        else 
            case (#fsig f) of 
                Ast.FunctionSignature { params, ... } => 
                let
                    val (varObj:Mach.OBJ) = Mach.newSimpleObj NONE
                    val (varScope:Mach.SCOPE) = extendScope env varObj

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
                    fun initThis v = Mach.defValue varObj thisName thisVal

                    (* FIXME: self-name binding is surely more complex than this! *)
                    val selfName = { id = (#ident (#name f)), ns = Ast.Internal "" }
                    val selfTag = Mach.FunctionTag (#fsig f)
                    val selfVal = Mach.newObject selfTag Mach.Null (SOME (Mach.Function closure))

                    (* If we have:
                     * 
                     * P parameter fixtures
                     * D parameter defaults (at the end)
                     * A args 
                     *
                     * Then we must have A >= P-D
                     *
                     * We allocate P fixtures, assign the args to [0, P-A),
                     * and assign the defaults for [P-A, P).
                     *)
(*
(****
                    fun hasDefault (Ast.Binding { init = NONE, ... }) = false
                      | hasDefault (Ast.Binding { init = _, ... }) = true
****)
                    val (b,i) = params
                    val p = length b
                    val d = 0  (* FIXME: get this from the parser/definer, length (List.filter hasDefault params) *)
                    val a = length args
                    val (fixtures,inits) = #param f
                in
                    allocScopeFixtures varScope fixtures;
                    (* FIXME: handle arg-list length mismatch correctly. *)
                    initThis thisVal;

                    (* List.app initArg (List.rev args); *)
                    evalScopeInits varScope inits;

                    (* NOTE: is this for the binding of a function expression to its optional
                       identifier? If so, we need to extend the scope chain before extending it
                       with the activation object, and add the self-name binding to that new 
                       scope, as in sec 13 ed. 3.

                       Changing defValue to setValue for now.
                    *)
                    Mach.setValue varObj selfName selfVal;

                    checkAllPropertiesInitialized varObj;
                    evalBlock varScope (#block f)
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

    Here how it works:

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

and bindArgs (argScope:Mach.SCOPE)
             (bindings:Ast.BINDING list)
             (args:Mach.VAL list)
    : unit =
    let
        val argObj = getScopeObj argScope
        val argTemps = getScopeTemps argScope
        fun bindArg (arg, Ast.Binding {ident, ...}) = 
            case ident of 
                Ast.TempIdent ti => Mach.defTemp argTemps ti arg
              | Ast.PropIdent pi => Mach.defValue argObj {ns=Ast.Internal "", id=pi} arg
    in
        List.app bindArg (ListPair.zip (args, bindings))
    end



and evalInits (scope:Mach.SCOPE)
              (obj:Mach.OBJ)
              (temps:Mach.TEMPS)
              (inits:Ast.INITS)
    : unit =
    let val _ = LogErr.trace ["evalInits"]
    in case inits of
        ((n,e)::rest) =>
            let
                val v = evalExpr scope e
            in
                (case n of 
                     Ast.PropName pn => Mach.defValue obj pn v
                   | Ast.TempName tn => Mach.defTemp temps tn v);
                evalInits scope obj temps rest
            end
      | _ => ()
    end

and evalObjInits (scope:Mach.SCOPE)                   
                 (obj:Mach.OBJ)
                 (inits:Ast.INITS)
    : unit = 
    let
        val (temps:Mach.TEMPS) = ref []
    in
        evalInits scope obj temps inits;
        if not ((length (!temps)) = 0)
        then LogErr.evalError ["initialized temporaries in non-scope object"]
        else ()
    end

and evalScopeInits (scope:Mach.SCOPE)                   
                   (inits:Ast.INITS)
    : unit =
    case scope of
        Mach.Scope { object, temps, ...} => 
        evalInits scope object temps inits

and initializeAndConstruct (classClosure:Mach.CLS_CLOSURE)
                           (classObj:Mach.OBJ)
                           (classScope:Mach.SCOPE)                           
                           (args:Mach.VAL list) 
                           (instanceObj:Mach.OBJ)
    : unit =
    case classClosure of 
        { cls, env, allTypesBound } => 
        if not allTypesBound 
        then LogErr.evalError ["constructing instance of class with unbound type variables"]
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
                        (LogErr.trace ["checking all properties initialized at root class", 
                                       LogErr.name name];
                         checkAllPropertiesInitialized instanceObj)
                      | SOME superName => 
                        let 
                            val (superObj:Mach.OBJ) = needObj (findVal env (multinameOf superName))
                            val (superClsClosure:Mach.CLS_CLOSURE) = 
                                case Mach.getObjMagic superObj of
                                    SOME (Mach.Class cc) => cc
                                  | _ => LogErr.evalError ["Superclass object ", 
                                                           LogErr.name superName, 
                                                           "is not a class closure"]
                            val (superEnv:Mach.SCOPE) = (#env superClsClosure)
                        in
                            initializeAndConstruct 
                                superClsClosure superObj superEnv superArgs instanceObj
                        end
            in 
                LogErr.trace ["evaluating instance initializers for ", LogErr.name name];
                evalObjInits classScope instanceObj instanceInits;
                case constructor of 
                    NONE => initializeAndConstructSuper []
                  | SOME (Ast.Ctor 
                              { settings, 
                                func = Ast.Func 
                                           { fsig = Ast.FunctionSignature 
                                                        { params, 
                                                          ... }, 
                                             body, 
                                             ...}}) => 
                    let 
                        val (varObj:Mach.OBJ) = Mach.newSimpleObj NONE
                        val (varScope:Mach.SCOPE) = extendScope classScope varObj
                        val (ctorScope:Mach.SCOPE) = extendScope varScope instanceObj
                        val ((paramBindings:Ast.BINDING list), _) = params
                    in
                        LogErr.trace ["binding constructor args of ", LogErr.name name];
                        bindArgs varScope paramBindings args;
                        LogErr.trace ["evaluating settings for ", LogErr.name name];
                        evalObjInits varScope instanceObj settings;
                        LogErr.trace ["initializing and constructing superclass of ", LogErr.name name];
                        (* FIXME: evaluate superArgs from super(...) call. *)
                        initializeAndConstructSuper ([(*superArgs*)]);                        
                        LogErr.trace ["entering constructo for ", LogErr.name name];
                        evalBlock ctorScope body;
                        ()
                    end
            end
                                     

and constructClassInstance (classObj:Mach.OBJ)
                           (classClosure:Mach.CLS_CLOSURE) 
                           (args:Mach.VAL list) 
    : Mach.VAL =
    Mach.Undef
(*
        let
            val {cls, allTypesBound, env} = classClosure
        in
            if not allTypesBound
            then LogErr.evalError ["constructing instance of class with unbound type variables"]
            else
                let
                    val classScope = extendScope env classObj

                    val Ast.Cls { name, 
                                  extends,
                                  instanceFixtures, 
                                  instanceInits, 
                                  constructor, 
                                  ... } = cls
                    val tag = Mach.ClassTag name
                    val proto = if Mach.hasOwnValue classObj Mach.publicPrototypeName
                                then Mach.getValue (classObj, Mach.publicPrototypeName)
                                else Mach.Null

                    val constructBase = 
                    val (baseObj, baseCls) = case extends of 
                                      NONE => NONE
                                      SOME baseClassName => 
                                      case findVal scope (multinameOf baseClassName) of 
                                          

                    val (instanceObj:Mach.OBJ) = Mach.newObj tag proto NONE
                    val (instanceScope:Mach.SCOPE) = extendScope classScope thisObj
                    val (instanceVal:Mach.VAL) = Mach.Object thisObj

                    (* FIXME: infer a fixture for "this" so that it's properly typed, dontDelete, etc.
                     * Also this will mean changing to defVar rather than setVar, for 'this'. *)
                    val thisName = { id = "this", ns = Ast.Internal "" }                                   

                in
                    allocObjFixtures classScope instanceObj instanceFixtures;
                    evalObjInits classScope instanceObj instanceInits;
                    case constructor of 
                        NONE => (checkAllPropertiesInitialized instanceObj; 
                                 instanceVal)  
(* TODO: run base class inits *)

 FIXME: needs resuscitation
                      | SOME ({native, ns,
                               func = Ast.Func 
                                          { fsig=Ast.FunctionSignature { params, inits, ... }, 
                                            body, paramFixtures, bodyFixtures, ... }, ... }) => 
                        let 
                            val ns = needNamespace (evalExpr env ns)
                            val (varObj:Mach.OBJ) = Mach.newSimpleObj NONE
                            val (varScope:Mach.SCOPE) = extendScope env varObj
                            fun bindArg (a, b) = evalVarBinding varScope (SOME a) (Ast.Internal "") b
                            fun initInstanceVar (vd:Ast.VAR_DEFN) = 
                                processVarDefn env vd (Mach.defValue obj)

                        in
                            allocScopeFixtures varScope (valOf paramFixtures);
                            allocScopeFixtures varScope (valOf bodyFixtures);
                            (* FIXME: is this correct? We also bind this name on the ctor var obj, below. *)
                            Mach.defValue obj n (Mach.Object classObj);
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
                                 Mach.defValue varObj n (Mach.Object classObj);
                                 Mach.setValue varObj thisName instance;
                                 LogErr.trace ["running initializers of ", LogErr.name n];
                                 (case inits of 
                                      NONE => ()
                                    | SOME { b=bindings, i=inits } => 
                                      let
                                          val (initVals:Mach.VAL list) = List.map (evalExpr varScope) inits
                                          fun bindInit (a, b) = evalVarBinding objScope (SOME a) ns b
                                      in
                                          List.app bindInit (ListPair.zip (initVals, bindings))
                                      end);
                                 LogErr.trace ["checking initialization of ", LogErr.name n];
                                 checkAllPropertiesInitialized obj; 
                                 let 
                                     (* Build a scope containing both the args and the obj. *)
                                     val _ = LogErr.trace ["running constructor of ", LogErr.name n]
                                     val (newVarScope:Mach.SCOPE) = extendScope objScope varObj
                                     val _ = evalBlock newVarScope body 
                                 in 
                                     instance
                                 end)
                        end
                    Mach.Object thisObj
                end
        end
*)

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
    HEAD
*)

and evalHead (scope:Mach.SCOPE)
             (head:Ast.HEAD)
    : Mach.OBJ =
        let
            val obj = Mach.newObj Mach.intrinsicObjectBaseTag Mach.Null NONE
            val (fixtures,inits) = head
        in
            allocObjFixtures scope obj fixtures;
            evalObjInits scope obj inits;
            obj
        end
            
(*
    BLOCK
*)

and evalBlock (scope:Mach.SCOPE) 
              (block:Ast.BLOCK) 
    : Mach.VAL = 
    case block of 
        Ast.Block {head=SOME head, body, ...} => 
        let 
            val blockFrame = evalHead scope head
            val blockScope = extendScope scope blockFrame
        in
            evalStmts blockScope body
        end
      | _ => LogErr.internalError ["in evalBlock"]



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
        val classScope = extendScope scope classObj

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
        { cond, body, fixtures, contLabel } => 
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
    raise (BreakException lbl)


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
    (Mach.populateIntrinsics Mach.globalObject;
     allocScopeFixtures Mach.globalScope (valOf (#fixtures prog));
     map (evalPackage Mach.globalScope) (#packages prog);
     evalBlock Mach.globalScope (#block prog))

end
