structure Eval = struct 

(* Exceptions for object-language control transfer. *)
exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> Mach.VAL)
exception ThrowException of Mach.VAL
exception ReturnException of Mach.VAL
    
fun newName (i:Ast.IDENT) (n:Mach.NS) = { ns=n, id=i }

fun getType (tyOpt:Ast.TYPE_EXPR option) : Ast.TYPE_EXPR = 
    case tyOpt of 
	SOME t => t
      | NONE => Ast.SpecialType Ast.Any

fun getAttrs (attrs:Ast.ATTRIBUTES) = 
    case attrs of Ast.Attributes a => a

fun getAttrNs (attrs:Ast.ATTRIBUTES) = 
    case attrs of Ast.Attributes {ns, ...} => ns

fun getScopeObj (scope:Mach.SCOPE) : Mach.OBJ = 
    case scope of Mach.Scope { object, ...} => object

(* 
 * A small number of functions do not fully evaluate to Mach.VAL 
 * values, but instead to REFs; these are temporary artifacts of
 * evaluation, not first-class values in the language. 
 *)

type REF = (Mach.OBJ * Mach.NAME)

(* Fundamental object methods *)

(* FIXME: possibly move this to mach.sml *) 

fun initObj (scope:Mach.SCOPE) (obj:Mach.OBJ) (f:Ast.FIXTURES) : unit = 
    case (obj, f) of 
	(Mach.Obj { props, ...}, Ast.Fixtures { bindings, ... }) => 
	let 
	    fun valInitPropState (t:Ast.TYPE_EXPR) : Mach.PROP_STATE = 

		(* Every value fixture has a type, and every type has an
		 * associated "initial property state". Note that
		 * this is *not* the same as saying that every type
		 * has an associated default value; for *some* types
		 * the initial property state is a default value; for
		 * types that are non-nullable, however, the initial
		 * property state is Mach.UninitProp. This property
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
		    LogErr.evalError ["attempt to initialize void-type property"]

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
		    valInitPropState base

		  | Ast.NullableType { expr, nullable=true } => 
		    Mach.ValProp (Mach.Null)
		    
		  | Ast.NullableType { expr, nullable=false } => 
		    Mach.UninitProp

	    fun initFixture (n, f) = 
		let 
		    fun initProp p = 
			if Mach.hasProp props n
			then LogErr.defnError 
				 ["initializing object with ",
				  "duplicate property name: ", 
				  LogErr.name n]
			else (LogErr.trace ["initializing property ", 
					    LogErr.name n]; 
			      Mach.addProp props n p)
		in 
		    case f of 
			Ast.TypeFixture te => 
			initProp { ty = te,
				   state = Mach.TypeProp,				   
				   attrs = { dontDelete = true,
					     dontEnum = true,
					     readOnly = true,
					     isFixed = true } }
								
		      | Ast.ValFixture { ty, readOnly, ... } => 
			initProp { ty = ty,
				   state = valInitPropState ty,
				   attrs = { dontDelete = true,
					     dontEnum = false,
					     readOnly = readOnly,
					     isFixed = true } }

		      | Ast.ClassFixture cd => 
			initProp { ty = Mach.classType,
				   state = Mach.ValProp (Mach.newClass scope cd),
				   attrs = { dontDelete = true,
					     dontEnum = true,
					     readOnly = true,
					     isFixed = true } }

		      | Ast.NamespaceFixture ns => 
			initProp { ty = Mach.namespaceType,
				   state = Mach.ValProp (Mach.newNamespace ns),
				   attrs = { dontDelete = true,
					     dontEnum = true,
					     readOnly = true,
					     isFixed = true } }

		      | Ast.TypeVarFixture =>
			initProp { ty = Mach.typeType,
				   state = Mach.TypeVarProp,
				   attrs = { dontDelete = true,
					     dontEnum = true,
					     readOnly = true,
					     isFixed = true } }
		end
	in		    
	    List.app initFixture bindings
	end

fun extendScope (p:Mach.SCOPE) (t:Mach.SCOPE_TAG) (ob:Mach.OBJ) : Mach.SCOPE = 
    Mach.Scope { parent=(SOME p), tag=t, object=ob }

fun initScope (scope:Mach.SCOPE) (f:Ast.FIXTURES) : unit = 
    initObj scope (getScopeObj scope) f

fun evalExpr (scope:Mach.SCOPE) (expr:Ast.EXPR) =
    case expr of
	Ast.LiteralExpr lit => 
	evalLiteralExpr lit

      | Ast.ListExpr es =>
	evalListExpr scope es

      | Ast.LexicalRef { ident } =>
	(case evalLhsExpr scope expr of
	     (obj, name) => Mach.getValue obj name)
	
      | Ast.ObjectRef { base, ident } =>
	(case evalLhsExpr scope expr of
	     (obj, name) => Mach.getValue obj name)
	
      | Ast.LetExpr {defs, body, fixtures} => 
	evalLetExpr scope (getFixtures fixtures) defs body

      | Ast.TrinaryExpr (Ast.Cond, aexpr, bexpr, cexpr) => 
	evalCondExpr scope aexpr bexpr cexpr

      | Ast.BinaryExpr (bop, aexpr, bexpr) => 
	evalBinaryOp scope bop aexpr bexpr

      | Ast.UnaryExpr (unop, expr) => 
	evalUnaryOp scope unop expr

      | Ast.SetExpr (aop, pat, expr) => 
	evalSetExpr scope aop pat (evalExpr scope expr)

      | Ast.CallExpr {func, actuals} => 
	let
	    val args = map (evalExpr scope) actuals
	    fun withLhs e = 
		case evalLhsExpr scope e of 
		    (obj, name) => evalCallExpr 
				       (SOME obj) 
				       (needObj (Mach.getValue obj name)) 
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
	

      | _ => LogErr.unimplError ["unhandled expression type"]

and evalLiteralExpr (lit:Ast.LITERAL) : Mach.VAL = 
    case lit of 
        Ast.LiteralNull => Mach.Null
      | Ast.LiteralUndefined => Mach.Undef
      | Ast.LiteralNumber n => Mach.newNumber n
      | Ast.LiteralBoolean b => Mach.newBoolean b
      | Ast.LiteralString s => Mach.newString s
      | Ast.LiteralNamespace n => Mach.newNamespace n
      | _ => LogErr.unimplError ["unhandled literal type"]

and evalListExpr (scope:Mach.SCOPE) (es:Ast.EXPR list) : Mach.VAL = 
    case es of 
	[] => Mach.Undef
      | [e] => evalExpr scope e
      | (e::ez) => ((evalExpr scope e); (evalListExpr scope ez))

and needObj (v:Mach.VAL) : Mach.OBJ = 
    case v of 
	Mach.Object ob => ob
      | _ => LogErr.evalError ["need object"]

and constructObjectViaFunction (ctorObj:Mach.OBJ) (ctor:Mach.FUN_CLOSURE) (args:Mach.VAL list) : Mach.VAL = 
    case ctorObj of 
	Mach.Obj { props, ... } => 
	let
	    (* FIXME: the default prototype should be the initial Object prototype, 
	     * as per ES3 13.2.2. *)
	    val (proto:Mach.VAL) = if Mach.hasProp props Mach.internalPrototypeName
				   then Mach.getValue ctorObj Mach.internalPrototypeName
				   else Mach.Null
	    val (newObj:Mach.OBJ) = Mach.newObj Mach.intrinsicObjectBaseTag proto NONE
	in
	    Mach.defValue newObj Mach.internalConstructorName (Mach.Object ctorObj);
	    case invokeFuncClosure newObj ctor args of 
		Mach.Object ob => Mach.Object ob
	      | _ => Mach.Object newObj
	end

and evalNewExpr  (obj:Mach.OBJ) (args:Mach.VAL list) : Mach.VAL =
    case obj of 
	Mach.Obj { magic, ... } => 
	case (!magic) of 
	    SOME (Mach.Class c) => constructClassInstance c args
	  | SOME (Mach.Function f) => constructObjectViaFunction obj f args
	  | _ => LogErr.evalError ["operator 'new' applied to unknown object"]
										 
and evalCallExpr (thisObj:Mach.OBJ option) (fobj:Mach.OBJ) (args:Mach.VAL list) : Mach.VAL =
    case fobj of
	Mach.Obj { magic, ... } => 
	case !magic of 
	    SOME (Mach.HostFunction f) => f args
	  | SOME (Mach.Function f) => 
	    ((case thisObj of 
		  NONE => invokeFuncClosure Mach.globalObject f args
		| SOME this => invokeFuncClosure this f args)
	     handle ReturnException v => v)
	  | _ => LogErr.evalError ["calling non-callable object"]
						       

(* 
 * FIXME: possibly try to factor and merge this with evalVarBinding,
 * but not sure if that's easy. 
 *)

and evalSetExpr (scope:Mach.SCOPE) (aop:Ast.ASSIGNOP) (pat:Ast.PATTERN) (v:Mach.VAL) : Mach.VAL = 
    let
	fun modified obj name = 
	    let 
		fun modifyWith bop = performBinop bop (Mach.getValue obj name) v
	    in
		case aop of 
		    Ast.Assign => v
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
		  | Ast.AssignLogicalAnd => modifyWith Ast.LogicalAnd
		  | Ast.AssignLogicalOr => modifyWith Ast.LogicalOr
	    end
    in
	case pat of 
	    Ast.IdentifierPattern id => 
	    let
		val multiname = evalIdentExpr scope id
		val refOpt = resolveOnScopeChain scope multiname
	    in
		case refOpt of 
		    SOME (obj, name) => 
		    let 
			val v = modified obj name
		    in 
			Mach.setValue obj name v;
			v
		    end
		  | NONE => LogErr.evalError ["unresolved identifier pattern"]
	end
      | Ast.SimplePattern expr => 
	let
	    val r = evalLhsExpr scope expr
	in
	    case r of (obj, name) => 
		      let 
			  val v = modified obj name
		      in 
			  Mach.setValue obj name v;
			  v
		      end
	end
      |	_ => LogErr.unimplError ["unhandled pattern form in assignment"]
    end

and evalLhsExpr (scope:Mach.SCOPE) (expr:Ast.EXPR) : REF = 
    case expr of 	
	Ast.LexicalRef { ident } => 
	evalRefExpr scope NONE ident
      | Ast.ObjectRef { base, ident } => 
	evalRefExpr scope (SOME (evalExpr scope base)) ident
      | _ => LogErr.evalError ["need lexical or object-reference expression"]

and evalUnaryOp (scope:Mach.SCOPE) (unop:Ast.UNOP) (expr:Ast.EXPR) : Mach.VAL =
    let
	fun crement f isPre = 
	    let 
		val r = evalLhsExpr scope expr
		val n = case r of 
			    (obj, name) => 
			    Mach.toNum (Mach.getValue obj name)
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
	    (case evalLhsExpr scope expr of
		 (Mach.Obj {props, ...}, name) => 
		 (Mach.delProp props name; Mach.newBoolean true))
	    
	  | Ast.PreIncrement => crement Real.+ true
	  | Ast.PreDecrement => crement Real.- true
	  | Ast.PostIncrement => crement Real.+ false
	  | Ast.PostDecrement => crement Real.- false
	  | _ => LogErr.unimplError ["unhandled unary operator"]
    end

and performBinop (bop:Ast.BINOP) (a:Mach.VAL) (b:Mach.VAL) : Mach.VAL = 
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
	    Ast.Plus => 
	    if isNum a andalso isNum b
	    then Mach.newNumber ((Mach.toNum a) + (Mach.toNum b))
	    else Mach.newString ((Mach.toString a) ^ (Mach.toString b))
	  | Ast.Minus => Mach.newNumber ((Mach.toNum a) - (Mach.toNum b))
	  | Ast.Times => Mach.newNumber ((Mach.toNum a) * (Mach.toNum b))
	  | Ast.Divide => Mach.newNumber ((Mach.toNum a) / (Mach.toNum b))
	  | Ast.Remainder => Mach.newNumber (real (Int.rem ((trunc (Mach.toNum a)), 
							    (trunc (Mach.toNum b)))))
			     
	  | Ast.LeftShift => wordOp Word.<<
	  | Ast.RightShift => wordOp Word.>>
	  | Ast.RightShiftUnsigned => wordOp Word.~>>
	  | Ast.BitwiseAnd => wordOp Word.andb
	  | Ast.BitwiseOr => wordOp Word.orb
	  | Ast.BitwiseXor => wordOp Word.xorb
			      
	  | Ast.Equals => Mach.newBoolean (Mach.equals a b)
	  | Ast.NotEquals => Mach.newBoolean (not (Mach.equals a b))
	  | Ast.StrictEquals => Mach.newBoolean (Mach.equals a b)
	  | Ast.StrictNotEquals => Mach.newBoolean (not (Mach.equals a b))
	  | Ast.Less => Mach.newBoolean (Mach.less a b)
	  | Ast.LessOrEqual => Mach.newBoolean ((Mach.less a b) orelse (Mach.equals a b))
	  | Ast.Greater => Mach.newBoolean (not ((Mach.less a b) orelse (Mach.equals a b)))
	  | Ast.GreaterOrEqual => Mach.newBoolean (not (Mach.less a b))
				  
	  | _ => LogErr.unimplError ["unhandled binary operator type"]
    end

and evalBinaryOp (scope:Mach.SCOPE) (bop:Ast.BINOP) (aexpr:Ast.EXPR) (bexpr:Ast.EXPR) : Mach.VAL =
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
	     

and evalCondExpr (scope:Mach.SCOPE) (cond:Ast.EXPR) (thn:Ast.EXPR) (els:Ast.EXPR) : Mach.VAL = 
    let 
        val v = evalExpr scope cond
        val b = Mach.toBoolean v
    in
	if b 
	then evalExpr scope thn
	else evalExpr scope els
    end
    

and needNamespace (v:Mach.VAL) : Ast.NAMESPACE = 
    case v of 
	Mach.Object (Mach.Obj ob) => 
	(case !(#magic ob) of 
	     SOME (Mach.Namespace n) => n
	   | _ => LogErr.evalError ["need namespace"])
      | _ => LogErr.evalError ["need namespace"]

and evalIdentExpr (scope:Mach.SCOPE) (r:Ast.IDENT_EXPR) : Mach.MULTINAME = 
    case r of 
	Ast.Identifier { ident, openNamespaces } => 
	{ nss=openNamespaces, id=ident }
	
      | Ast.QualifiedIdentifier { qual, ident } => 
	{ nss = [needNamespace (evalExpr scope qual)], id = ident }

      | Ast.QualifiedExpression { qual, expr } => 
	{ nss = [needNamespace (evalExpr scope qual)], 
	  id = Mach.toString (evalExpr scope expr) }

      | _ => LogErr.unimplError ["unimplemented identifier expression form"]
		   

and evalRefExpr (scope:Mach.SCOPE) (b:Mach.VAL option) (r:Ast.IDENT_EXPR) : REF =
    let 
	val (multiname:Mach.MULTINAME) = evalIdentExpr scope r
	val refOpt = (case b of 
			  SOME (Mach.Object ob) => 
			  resolveOnObjAndPrototypes ob multiname
			| NONE => 
			  resolveOnScopeChain scope multiname
			| _ => LogErr.evalError ["ref expression on non-object value"])
    in
	case refOpt of 
	    NONE => LogErr.evalError ["unresolved identifier expression"]
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
        initScope scope fixtures;
	evalVarBindings scope defs;
        evalListExpr newScope body
    end

and evalVarBinding (scope:Mach.SCOPE)
		   (v:Mach.VAL option)
		   (defn:Ast.VAR_BINDING)
    : unit =
    (* Here we are evaluating only the *definition* affect of the
     * binding, as the binding produced a fixture and we've already 
     * allocated and possibly initialized a property for the fixture.
     *)
    case defn of 
	Ast.Binding { kind, init, attrs, pattern, ty } =>
	let 
	    fun defToValue v' = 
		case pattern of 
		    Ast.IdentifierPattern id => 
		    (case id of 
			 Ast.Identifier {ident, ...} => 
			 let 
			     val ns = getAttrNs attrs
			     val n = { ns = needNamespace (evalExpr scope ns), 
				       id = ident}
			 in
			     LogErr.trace ["defining variable ", LogErr.name n];
			     Mach.defValue (getScopeObj scope) n v'
			 end
		       | _ => LogErr.unimplError ["unhandled identifier form in ",
						  "identifier binding pattern"])
		  | _ => LogErr.unimplError ["unhandled pattern form in binding"]		     
	in
	    case v of 
		SOME v' => defToValue v'
	      | NONE => 
		(case init of 
		     SOME e => defToValue (evalExpr scope e)
		   | NONE => ())
	end
    
and evalVarBindings (scope:Mach.SCOPE)
    (defns:Ast.VAR_BINDING list)
    : unit =
    List.app (evalVarBinding scope NONE) defns
          
and resolveOnScopeChain (scope:Mach.SCOPE) (mname:Mach.MULTINAME) : REF option =
    (LogErr.trace ["resolving multiname on scope chain: ", LogErr.multiname mname];     
    case scope of 
	Mach.Scope { parent, object, ... } => 
	case resolveOnObjAndPrototypes object mname of
	    NONE => (case parent of 
			 SOME p => resolveOnScopeChain p mname
		       | NONE => NONE)
	  | result => result)
    
and resolveOnObjAndPrototypes (obj:Mach.OBJ) (mname:Mach.MULTINAME) : REF option = 
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
	  | result => result

and resolveOnObj (obj:Mach.OBJ) (mname:Mach.MULTINAME) : REF option =
    case obj of 
	Mach.Obj ob => 
	let     
	    val id = (#id mname)		     
	    val _ = LogErr.trace ["candidate object props: "]
	    val _ = List.app (fn (k,_) => LogErr.trace ["prop: ", LogErr.name k]) (!(#props ob))
	    fun tryName [] = (LogErr.trace ["no matches found on candidate object"]; 
			      NONE)
	      | tryName (x::xs) = 
		let 
		    val n = {ns=x, id=id} 
		    val _ = LogErr.trace ["resolving candidate name ", LogErr.name n]
		in
		    if Mach.hasProp (#props ob) n
		    then (LogErr.trace ["found property with name ", LogErr.name n]; 
			  SOME (obj, n))
		    else tryName xs
		end
	in
	    tryName (#nss mname)
	end

and evalTailCallExpr (scope:Mach.SCOPE) (e:Ast.EXPR) : Mach.VAL = 
    raise (TailCallException (fn _ => evalExpr scope e))

and evalNonTailCallExpr (scope:Mach.SCOPE) (e:Ast.EXPR) : Mach.VAL = 
    evalExpr scope e
    handle TailCallException thunk => thunk ()
         | ReturnException v => v

and labelEq (stmtLabel:Ast.IDENT option) (exnLabel:Ast.IDENT option) : bool = 
    case (stmtLabel, exnLabel) of
	(SOME sl, SOME el) => sl = el
      | (NONE, SOME _) => false
      | (_, NONE) => true
		     
and evalStmts (scope:Mach.SCOPE) (stmts:Ast.STMT list) : Mach.VAL = 
    (map (evalStmt scope) stmts; Mach.Undef)
		       
and evalStmt (scope:Mach.SCOPE) (stmt:Ast.STMT) : Mach.VAL = 
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
      | Ast.EmptyStmt => Mach.Undef
      | _ => LogErr.unimplError ["unimplemented statement type"]

and evalClassDefn (scope:Mach.SCOPE) (cd:Ast.CLASS_DEFN) : unit =
    (* The class objects were installed into the scope during initialization,
     * all we do here is build a prototype for this class and link up
     * the implicit prototype chains.
     *)
    let 
	val ns = getAttrNs (#attrs cd)
	val currClassMname = { nss = [needNamespace (evalExpr scope ns)],
			       id = (#name cd) }
	val currClassObj = 
	    case resolveOnScopeChain scope currClassMname of 
		NONE => LogErr.evalError ["unable to resolve class multiname: ", 
					  LogErr.multiname currClassMname ]
	      | SOME (obj, name) => 
		case Mach.getValue obj name of 
		    Mach.Object ob => ob
		  | _ => LogErr.evalError ["class name resolved to non-object: ",
					   LogErr.multiname currClassMname ]
			 
	val baseProtoVal = 
	    case (#extends cd) of 
		NONE => Mach.Null
	      | SOME ie => 
		let
		    val baseClassMname = evalIdentExpr scope ie
		in
		    case resolveOnScopeChain scope baseClassMname of 
			NONE => LogErr.evalError ["unable to resolve base class multiname: ", 
						  LogErr.multiname baseClassMname]
		      | SOME (obj, name) => 
			case Mach.getValue obj name of 
			    Mach.Object ob => 
			    if Mach.hasOwnValue ob Mach.internalPrototypeName
			    then Mach.getValue ob Mach.internalPrototypeName
			    else Mach.Null
			  | _ => LogErr.evalError ["base class resolved to non-object: ", 
						   LogErr.multiname baseClassMname]
		end
			     
	val newPrototype = Mach.newObj Mach.intrinsicObjectBaseTag baseProtoVal NONE
	fun addProtoMethod (f:Ast.FUNC_DEFN) = 
	    let 
 		val ns = getAttrNs (#attrs f)
		val n = case f of 
			    {func = Ast.Func { name = { ident, ... }, ... }, ... } => 
			    { ns = needNamespace (evalExpr scope ns), 
			      id = ident }

		(* FIXME: 'scope' is not the correct environment for the method; we actually want to use
		 * the scope containing the class statics, which should really be stored in 
		 * the class closure, but currently isn't. *)
		val fval = Mach.newFunc scope (#func f)
	    in
		Mach.defValue newPrototype n fval
	    end
	(* FIXME: something to do with proto fixtures? Or do we have yet another structural 
	 * induction on the form of a VAR_BINDING? *)
	fun addProtoVar (vb:Ast.VAR_BINDING) = ()
	    
    in
	List.app addProtoMethod (#protoMethods cd);
	List.app addProtoVar (#protoVars cd);
	(* FIXME: install the protoMethods and protoVars into the prototype. *)
	Mach.setValue currClassObj Mach.internalPrototypeName baseProtoVal
    end
	    

and evalDefn (scope:Mach.SCOPE) (d:Ast.DEFN) : unit = 
    case d of 
	Ast.FunctionDefn f => evalFuncDefn scope f
      | Ast.VariableDefn bs => evalVarBindings scope bs
      | Ast.NamespaceDefn ns => () (* handled during initialization *)
      | Ast.ClassDefn cd => evalClassDefn scope cd
      | _ => LogErr.unimplError ["unimplemented definition type"]
	     
and evalDefns (scope:Mach.SCOPE) (ds:Ast.DEFN list) : unit = 
    List.app (evalDefn scope) ds
	     
and getFixtures (f:Ast.FIXTURES option) : Ast.FIXTURES = 
    case f of 
	SOME f' => f'
      | NONE => LogErr.evalError ["missing expected fixtures"]

and checkAllPropertiesInitialized (obj:Mach.OBJ) = 
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

and invokeFuncClosure (this:Mach.OBJ) (closure:Mach.FUN_CLOSURE) (args:Mach.VAL list) : Mach.VAL =
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
		    fun bindArg (a, b) = evalVarBinding varScope (SOME a) b

		    (* FIXME: infer a fixture for "this" so that it's properly typed, dontDelete, etc.
		     * Also this will mean changing to defVar rather than setVar, for 'this'. *)
		    val thisName = { id = "this", ns = Ast.Internal "" }
		    val thisVal = Mach.Object this

		    (* FIXME: self-name binding is surely more complex than this! *)
		    val selfName = { id=(#ident (#name f)), ns = Ast.Internal "" }
		    val selfTag = Mach.FunctionTag (#fsig f)
		    val selfVal = Mach.newObject selfTag Mach.Null (SOME (Mach.Function closure))
		in
		    initScope varScope (getFixtures (#fixtures f));
		    (* FIXME: handle arg-list length mismatch correctly. *)
		    List.app bindArg (ListPair.zip (args, params));
		    Mach.setValue varObj thisName thisVal;
		    Mach.defValue varObj selfName selfVal;
		    checkAllPropertiesInitialized varObj;
		    evalBlock varScope (#body f)
		end

and constructClassInstance (closure:Mach.CLS_CLOSURE) (args:Mach.VAL list) : Mach.VAL = 
    case closure of 
	{ cls, allTypesBound, env } => 
	if not allTypesBound
	then LogErr.evalError ["constructing instance of class with unbound type variables"]
	else 
	    case cls of 
		Mach.Cls { definition, ... } =>
		let
		    val ns = getAttrNs (#attrs definition)
		    val n = { ns = needNamespace (evalExpr env ns), 
			      id = (#name definition) }
		    val _ = LogErr.trace ["constructing instance of ", LogErr.name n]
		    val classTag = Mach.ClassTag n
		    (* FIXME: copy prototype from CLS_CLOSURE here *)
		    val (obj:Mach.OBJ) = Mach.newObj classTag Mach.Null NONE
		    val (objScope:Mach.SCOPE) = extendScope env Mach.VarInstance obj
		    val (instance:Mach.VAL) = Mach.Object obj
		    val ctor = (#constructor definition)

		    (* FIXME: infer a fixture for "this" so that it's properly typed, dontDelete, etc.
		     * Also this will mean changing to defVar rather than setVar, for 'this'. *)
		    val thisName = { id = "this", ns = Ast.Internal "" }

		    (* FIXME: self-name binding is surely more complex than this! *)
		    val selfTag = Mach.ClassTag n
		    val selfVal = Mach.newObject selfTag Mach.Null (SOME (Mach.Class closure))
		in
		    initObj env obj (getFixtures (#instanceFixtures definition));
		    case ctor of 
			NONE => (checkAllPropertiesInitialized obj; instance)
		      | SOME ({func = Ast.Func { fsig=Ast.FunctionSignature { params, inits, ... }, 
						 body, fixtures, ... }, ... }) => 
			let 
			    val (varObj:Mach.OBJ) = Mach.newSimpleObj NONE
			    val (varScope:Mach.SCOPE) = extendScope env Mach.VarActivation varObj
			    fun bindArg (a, b) = evalVarBinding varScope (SOME a) b
			in
			    initScope varScope (getFixtures fixtures);
			    (* FIXME: handle arg-list length mismatch correctly. *)
			    LogErr.trace ["binding constructor args of ", LogErr.name n];
			    List.app bindArg (ListPair.zip (args, params));
			    Mach.setValue varObj thisName instance;
			    Mach.defValue varObj n selfVal;
			    (* FIXME: is this correct? we currently bind the self name on obj as well.. *)
			    Mach.defValue obj n selfVal;
			    (* Run any initializers if they exist. *)
			    LogErr.trace ["running initializers of ", LogErr.name n];
			    (case inits of 
				 NONE => ()
			       | SOME { defns, inits } => 
				 let
				     val (initVals:Mach.VAL list) = List.map (evalExpr varScope) inits
				     fun bindInit (a, b) = evalVarBinding objScope (SOME a) b
				 in
				     List.app bindInit (ListPair.zip (initVals, defns))
				 end);
			    LogErr.trace ["checking initializers completed properly for ", LogErr.name n];
			    checkAllPropertiesInitialized obj; 
			    let 
				(* Now the strange part: we re-parent the arguments var object
				 * to the instance object, before running the constructor body. *)
				val _ = LogErr.trace ["running constructor of ", LogErr.name n]
				val (newVarScope:Mach.SCOPE) = extendScope objScope Mach.VarActivation varObj
				val _ = evalBlock newVarScope body 
			    in 
				instance
			    end
			end
		end
		
(* 
 * This is the dynamic phase of function definition; it assumes that the 
 * function has already had its fixtures constructed during the
 * definition phase and a property for it has been allocated to the ininitialized
 * state.
 *) 
and evalFuncDefn (scope:Mach.SCOPE) (f:Ast.FUNC_DEFN) : unit = 
    let 
	val func = (#func f)
	val funcAttrs = getAttrs (#attrs f)
	val name = case func of Ast.Func { name={ident, ...}, ...} => ident
	val fval = Mach.newFunc scope func
	val fname = {id = name, 
		     ns = (needNamespace (evalExpr scope (#ns funcAttrs)))}
    in
	LogErr.trace ["defining function ", LogErr.name fname];
	Mach.defValue (getScopeObj scope) fname fval
    end

and evalBlock (scope:Mach.SCOPE) (block:Ast.BLOCK) : Mach.VAL = 
    case block of 
	Ast.Block {defns, stmts, fixtures, ...} => 
	let 
	    val blockObj = Mach.newObj Mach.intrinsicObjectBaseTag Mach.Null NONE
	    val blockScope = extendScope scope Mach.Let blockObj
	in
	    LogErr.trace ["initializing block scope"];
	    initScope blockScope (getFixtures fixtures); 
	    LogErr.trace ["evaluating block scope definitions"];
	    evalDefns blockScope defns;
	    LogErr.trace ["evaluating block scope statements"];
	    let 
		val v = evalStmts blockScope stmts 
	    in 
		LogErr.trace ["exiting block"];
		v
	    end
	end

and evalIfStmt (scope:Mach.SCOPE) (cnd:Ast.EXPR) (thn:Ast.STMT) (els:Ast.STMT) : Mach.VAL = 
    let 
        val v = evalExpr scope cnd 
        val b = Mach.toBoolean v
    in
        if b 
        then evalStmt scope thn
        else evalStmt scope els
    end
    
and evalLabelStmt (scope:Mach.SCOPE) (lab:Ast.IDENT) (s:Ast.STMT) : Mach.VAL = 
    evalStmt scope s
    handle BreakException exnLabel 
	   => if labelEq (SOME lab) exnLabel
              then Mach.Undef
              else raise BreakException exnLabel
			 
and evalWhileStmt (scope:Mach.SCOPE) (whileStmt:Ast.WHILE_STMT) : Mach.VAL = 
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
    
and evalReturnStmt (scope:Mach.SCOPE) (e:Ast.EXPR list) : Mach.VAL
  = raise (ReturnException (evalListExpr scope e))
	  
and evalThrowStmt (scope:Mach.SCOPE) (e:Ast.EXPR list) : Mach.VAL
  = raise (ThrowException (evalListExpr scope e))
	  
and evalBreakStmt (scope:Mach.SCOPE) (lbl:Ast.IDENT option) : Mach.VAL
  = raise (BreakException lbl)
	  
and evalContinueStmt (scope:Mach.SCOPE) (lbl:Ast.IDENT option) : Mach.VAL
  = raise (ContinueException lbl)

and evalPackage (scope:Mach.SCOPE) (package:Ast.PACKAGE) : Mach.VAL = 
    evalBlock scope (#body package)

and evalProgram (prog:Ast.PROGRAM) : Mach.VAL = 
    (Mach.populateIntrinsics Mach.globalObject;
     map (evalPackage Mach.globalScope) (#packages prog);
     evalBlock Mach.globalScope (#body prog))


end
