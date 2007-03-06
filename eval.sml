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

            fun allocFixture (n, f) = 
                case n of 
                    Ast.TempName t => 
                    (case f of 
                         Ast.ValFixture { ty, ... } => 
                         (if t = (List.length (!temps))
                          then (trace ["allocating fixture for temporary ", Int.toString t];
                                temps := (ty, Mach.UninitTemp)::(!temps)) 
                          else (error ["temp count out of sync ", Int.toString t]);())
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
                (isVarObject:bool) 
    : Mach.SCOPE = 
    Mach.Scope { parent=(SOME p), object=ob, temps=ref [], isVarObject=isVarObject }

    
fun allocObjFixtures (scope:Mach.SCOPE) 
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
        
      | Ast.InitExpr (target,temps,inits) =>
        let
            val tempScope = evalHead scope temps false
        in
            evalScopeInits tempScope target inits;
            Mach.Undef
        end

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
      | Ast.LiteralFunction f => Mach.newFunc scope f
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
        
      | Ast.ExpressionIdentifier expr =>
        { nss = [[Ast.Internal ""]],
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
              | _ => error ["ref expression messed up in refOf"]

        val (base,ident) =
            case expr of         
                Ast.LexicalRef { ident } => (NONE,ident)
              | Ast.ObjectRef { base, ident } => (SOME (evalExpr scope base), ident)
              | _ => error ["need lexical or object-reference expression"]

        val (multiname:Mach.MULTINAME) = evalIdentExpr scope ident

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
                        (mname:Mach.MULTINAME) 
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
        val _ = trace ["resolveOnObjAndPrototypes: ", LogErr.multiname mname];
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
      | Ast.LetStmt b => evalBlock scope b
      | Ast.EmptyStmt => Mach.Undef
      | _ => LogErr.unimplError ["unimplemented statement type"]


and multinameOf (n:Ast.NAME) = 
    { nss = [[(#ns n)]], id = (#id n) }

and findVal (scope:Mach.SCOPE) 
            (mn:Ast.MULTINAME) 
    : Mach.VAL = 
    case resolveOnScopeChain scope mn of 
        NONE => error ["unable to resolve multiname: ", 
                                  LogErr.multiname mn ]
      | SOME (obj, name) => Mach.getValue (obj, name) 


and checkAllPropertiesInitialized (obj:Mach.OBJ)
    : unit = 
    let 
        fun checkOne (n:Mach.NAME, p:Mach.PROP) = 
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
        val Ast.Func { name, fsig, block, param=(fixtures, inits), ... } = func
    in
        if not allTypesBound
        then error ["invoking function with unbound type variables"]
        else
            let 
                val (varObj:Mach.OBJ) = Mach.newSimpleObj NONE
                val (varScope:Mach.SCOPE) = extendScope env varObj true
                                            
                (* FIXME: infer a fixture for "this" so that it's properly typed, dontDelete, etc.
                 * Also this will mean changing to defVar rather than setVar, for 'this'. *)
                val thisName = { id = "this", ns = Ast.Internal "" }
                val thisVal = Mach.Object this
                fun initThis v = Mach.setValue varObj thisName thisVal
                                 
                (* FIXME: self-name binding is surely more complex than this! *)
                val selfName = { id = (#ident name), ns = Ast.Internal "" }
                val selfTag = Mach.FunctionTag fsig
                val selfVal = Mach.newObject selfTag Mach.Null (SOME (Mach.Function closure))
            in
                allocScopeFixtures varScope fixtures;
                initThis thisVal;
                bindArgs env varScope func args;
                evalScopeInits varScope Ast.Local inits;

                (* NOTE: is this for the binding of a function expression to its optional
                 * identifier? If so, we need to extend the scope chain before extending it
                 * with the activation object, and add the self-name binding to that new 
                 * scope, as in sec 13 ed. 3.
                 * 
                 * Changing defValue to setValue for now.
                 *)

                Mach.setValue varObj selfName selfVal;                
                checkAllPropertiesInitialized varObj;
                evalBlock varScope block
            end
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
    : unit =
    let val _ = trace ["evalInits"]
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
        then error ["initialized temporaries in non-scope object"]
        else ()
    end

and evalInstanceInits (scope:Mach.SCOPE)                   
                      (instanceObj:Mach.OBJ)
                      (head:Ast.HEAD)
    : unit = 
        let
            val (fixtures,inits) = head
            val tempScope = evalHead scope (fixtures,[]) false
        in
            evalObjInits tempScope instanceObj inits
        end

and evalScopeInits (scope:Mach.SCOPE)
                   (target:Ast.INIT_TARGET)
                   (inits:Ast.INITS)
    : unit =
            let
                fun targetOf (Mach.Scope { object, temps, isVarObject, parent}) =
                    case (target,parent) of
                         (Ast.Local,_) => object
                       | (Ast.Hoisted,parent) => 
                                 if isVarObject=true 
                                    then object
                                    else targetOf (valOf parent)
                       | _ => LogErr.unimplError ["unhandled target kind"]
                val obj = targetOf scope
            in case scope of
                Mach.Scope { temps, ...} =>
                    evalInits scope obj temps inits
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
                evalInstanceInits classScope instanceObj instanceInits;
(* was                evalObjInits classScope instanceObj instanceInits; *)
                case constructor of 
                    NONE => initializeAndConstructSuper []
                  | SOME (Ast.Ctor { settings, func }) => 
                    let 
                        val Ast.Func { block, param=(fixtures,inits), ... } = func
                        val (varObj:Mach.OBJ) = Mach.newSimpleObj NONE
                        val (varScope:Mach.SCOPE) = extendScope classScope varObj false
                        val (ctorScope:Mach.SCOPE) = extendScope varScope instanceObj true
                        val (settings_fixtures,settings_inits) = settings
                    in
                        trace ["allocating scope fixtures for constructor of ", LogErr.name name];
                        allocScopeFixtures varScope fixtures;                
                        trace ["binding constructor args of ", LogErr.name name];
                        bindArgs classScope varScope func args;
                        trace ["evaluating inits of ", LogErr.name name];
                        evalScopeInits varScope Ast.Local inits;
                        trace ["allocating scope fixtures for settings temps"];                        
                        allocScopeFixtures varScope settings_fixtures;                
                        trace ["evaluating settings for ", LogErr.name name];
                        evalObjInits varScope instanceObj settings_inits;
                        trace ["initializing and constructing superclass of ", LogErr.name name];
                        (* FIXME: evaluate superArgs from super(...) call. *)
                        initializeAndConstructSuper ([(*superArgs*)]);                        
                        trace ["entering constructo for ", LogErr.name name];
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
        val (proto:Mach.VAL) = if Mach.hasOwnValue classObj Mach.publicPrototypeName
                               then Mach.getValue (classObj, Mach.publicPrototypeName)
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
        trace ["defining function ", LogErr.name fname];
        Mach.defValue target fname fval
    end

(*
    HEAD
*)

and evalHead (scope:Mach.SCOPE)
             (head:Ast.HEAD)
             (isVarObject:bool)
    : Mach.SCOPE =
        let
            val (fixtures,inits) = head
            val obj = Mach.newObj Mach.intrinsicObjectBaseTag Mach.Null NONE
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
    case block of 
        Ast.Block {head=SOME head, body, ...} => 
        let 
            val blockScope = evalHead scope head false
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
        val Mach.Obj { props, ... } = classObj
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
                      | _ => error ["base class resolved to non-object: ", 
                                               LogErr.name baseClassName]
                end
                
        val _ = trace ["constructing prototype"]
        val newPrototype = Mach.newObj Mach.intrinsicObjectBaseTag baseProtoVal NONE
    in
        Mach.defValue classObj
                      Mach.publicPrototypeName 
                      (Mach.Object newPrototype);
        trace ["finished initialising class prototype"]
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
        val _ = trace ["evaluating class stmt for ", LogErr.name (valOf name)]

        (* get the class object allocated when the property was instantiated *)
        val classObj = needObj (findVal scope (multinameOf (valOf name)))

        (* allocate the fixed properties for the class object *)
        val _ = allocObjFixtures scope classObj (valOf fixtures)

        (* init the class prototype if not going to be set by the user *)
        val _ = initClassPrototype scope classObj extends

        (* extend the scope chain with the class object *)
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
