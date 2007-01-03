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

fun getAttrs (attrs:Ast.ATTRIBUTES) = case attrs of Ast.Attributes a => a

fun getScopeObj (scope:Mach.SCOPE) : Mach.OBJ = case scope of Mach.Scope { object, ...} => object

(* 
 * A small number of functions do not fully evaluate to Mach.VAL 
 * values, but instead to REFs; these are temporary artifacts of
 * evaluation, not first-class values in the language. 
 *)

type REF = (Mach.OBJ * Mach.NAME)

(* 
 * A small number of functions return an INIT_LIST, a 
 * type of binding list that represents a request to allocate 
 * a certain set of fixtures and initialize them to given
 * values. 
 *)

type INIT_LIST = (Mach.NAME * Mach.VAL) list

(* Fundamental object methods *)

fun getValue (obj:Mach.OBJ) (name:Mach.NAME) : Mach.VAL = 
    case obj of 
	Mach.Obj {props, ...} => 
	let 
	    val prop = Mach.getProp props name
	in
	    if (#kind prop) = Mach.TypeProp
	    then LogErr.evalError ["property is a type, wanted a value"]
	    else (#value prop)
	end

fun setProp (obj:Mach.OBJ) (n:Mach.NAME) (p:Mach.PROP) : unit =
    case obj of 
	Mach.Obj ob => Mach.addProp (#props ob) n p

fun setValue (base:Mach.OBJ) (name:Mach.NAME) (v:Mach.VAL) : unit =     
    case base of 
	Mach.Obj {props,...} => 
	if Mach.hasProp props name
	then 
	    let 
		val existingProp = Mach.getProp props name
		val _ = if (#kind existingProp) = Mach.TypeProp 
			then LogErr.evalError ["assigning a value to a type property"]
			else ()
		val existingAttrs = (#attrs existingProp)
		val newProp = { kind = Mach.ValProp,
				ty = (#ty existingProp), 
				value = v, 
				attrs = { dontDelete = (#dontDelete existingAttrs), 
					  dontEnum = (#dontEnum existingAttrs),
					  readOnly = (#readOnly existingAttrs),
					  isFixed = (#isFixed existingAttrs) } }
	    in
		if (#readOnly existingAttrs)
		then LogErr.evalError ["assigning to read-only property"]
		else ();
		(* FIXME: insert typecheck here *)
		Mach.delProp props name;
		Mach.addProp props name newProp
	    end
	else
	    let 
		val prop = { kind = Mach.ValProp,
			     ty = Ast.SpecialType Ast.Any,
			     value = v,
			     attrs = { dontDelete = false,
				       dontEnum = false,
				       readOnly = false,
				       isFixed = false } }
	    in
		Mach.addProp props name prop
	    end


fun extendScope (p:Mach.SCOPE) (t:Mach.SCOPE_TAG) (ob:Mach.OBJ) : Mach.SCOPE = 
    Mach.Scope { parent=(SOME p), tag=t, object=ob }

fun initScope (scope:Mach.SCOPE) (f:Ast.FIXTURES) (il:INIT_LIST) : unit = 
    case f of 
	Ast.Fixtures { bindings, ... } => 
	let 
	    fun findInit n = 
		let 
		    fun search [] = NONE
		      | search ((k,v)::bs) = 
			if k = n 
			then SOME v
			else search bs
		in
		    search il
		end

	    fun initFixture (n, f) = 
		let 
		    fun initProp p = 
			case getScopeObj scope of 
			    Mach.Obj {props,...} => 
			    if Mach.hasProp props n
			    then LogErr.defnError ["initializing scope with duplicate property name"]
			    else (LogErr.trace ["initializing property ", LogErr.name n]; 
				  Mach.addProp props n p)
		in 
		    case f of 
			Ast.TypeFixture te => 
			(case findInit n of
			     SOME _ => LogErr.defnError 
					   ["initializing fixed type property with value"]
			   | NONE => 
			     initProp { kind = Mach.TypeProp,
					ty = Mach.typeType,
					value = Mach.newSimpleObject (SOME (Mach.Type te)),
					attrs = { dontDelete = true,
						  dontEnum = true,
						  readOnly = true,
						  isFixed = true } })
		      | Ast.ValFixture { ty, readOnly, ... } => 
			let 
			    val v = case findInit n of 
					(* FIXME: Undef or Null? *)
					NONE => Mach.Undef
				      | SOME i => i
			in
			    (* FIXME: type-check v against ty here *)
			    initProp { kind = Mach.ValProp,
				       ty = ty,
				       value = v,
				       attrs = { dontDelete = true,
						 dontEnum = false,
						 readOnly = readOnly,
						 isFixed = true } }
			end
		      | Ast.NamespaceFixture ns => 
			(case findInit n of
			     SOME _ => LogErr.defnError ["initializing namespace fixture"]
			   | NONE => 					
			     initProp { kind = Mach.ValProp,
					ty = Mach.namespaceType,
					value = Mach.newSimpleObject (SOME (Mach.Namespace ns)),
					attrs = { dontDelete = true,
						  dontEnum = true,
						  readOnly = true,
						  isFixed = true } })
		      | Ast.TypeVarFixture =>
			()
		end
	in		    
	    List.app initFixture bindings
	end

fun evalExpr (scope:Mach.SCOPE) (expr:Ast.EXPR) =
    case expr of
	Ast.LiteralExpr lit => 
	evalLiteralExpr lit

      | Ast.ListExpr es =>
	evalListExpr scope es

      | Ast.LexicalRef { ident } =>
	(case evalLhsExpr scope expr of
	     (obj, name) => getValue obj name)
	
      | Ast.ObjectRef { base, ident } =>
	(case evalLhsExpr scope expr of
	     (obj, name) => getValue obj name)
	
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
				       (needObj (getValue obj name)) 
				       args
	in
	    case func of 
		Ast.LexicalRef _ => withLhs func
	      | Ast.ObjectRef _ => withLhs func
	      | _ => evalCallExpr NONE (needObj (evalExpr scope func)) args
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
		fun modifyWith bop = performBinop bop (getValue obj name) v
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
			setValue obj name v;
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
			  setValue obj name v;
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
			    Mach.toNum (getValue obj name)
		val n' = Mach.newNumber (f (n, 1.0))
		val n'' = if isPre
			  then n'
			  else Mach.newNumber n
	    in
		case r of 
		    (obj, name) => 
		    (setValue obj name n'; n'')
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
	val inits = evalVarBindings scope defs		    
    in
        initScope scope fixtures inits;
        evalListExpr newScope body
    end

and evalVarBinding (scope:Mach.SCOPE)
		   (v:Mach.VAL option)
		   (defn:Ast.VAR_BINDING)
    : INIT_LIST =
    case defn of 
	(Ast.Binding { kind, init, attrs, pattern, ty }) =>
	case pattern of 
	    Ast.IdentifierPattern id => 
	    (case id of 
		 Ast.Identifier {ident, ...} => 
		 let 
		     val Ast.Attributes {ns,...} = attrs
		     val v' = case v of 
				  SOME v'' => v''
				| NONE => (case init of 
					       NONE => Mach.Undef
					     | SOME e => evalExpr scope e)
		     val n = {ns=needNamespace (evalExpr scope ns), id=ident}
		     val _ = LogErr.trace ["allocating variable binding ", LogErr.name n]
		 in
		     [(n, v')]
		 end
	       | _ => LogErr.unimplError ["unhandled identifier form in ",
					  "identifier binding pattern"])
	  | _ => LogErr.unimplError ["unhandled pattern form in binding"]

and evalVarBindings (scope:Mach.SCOPE)
    (defns:Ast.VAR_BINDING list)
    : INIT_LIST =
    List.concat (map (evalVarBinding scope NONE) defns)
          
and resolveOnScopeChain (scope:Mach.SCOPE) (mname:Mach.MULTINAME) : REF option =
    (LogErr.trace ["resolving name on scope chain:", (#id mname)];     
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
	    fun tryName [] = NONE
	      | tryName (x::xs) = 
		let 
		    val n = {ns=x, id=id} 
		    val _ = LogErr.trace ["resolving candidate name ", LogErr.name n]
		in
		    if Mach.hasProp (#props ob) n
		    then SOME (obj, n)
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

and evalDefn (scope:Mach.SCOPE) (d:Ast.DEFN) : INIT_LIST = 
    case d of 
	Ast.FunctionDefn f => evalFuncDefn scope f
      | Ast.VariableDefn bs => evalVarBindings scope bs 
      | _ => LogErr.unimplError ["unimplemented definition type"]
	     
and evalDefns (scope:Mach.SCOPE) (ds:Ast.DEFN list) : INIT_LIST = 
    List.concat (map (evalDefn scope) ds)
	     
and getFixtures (f:Ast.FIXTURES option) : Ast.FIXTURES = 
    case f of 
	SOME f' => f'
      | NONE => LogErr.evalError ["missing expected fixtures"]

and invokeFuncClosure (this:Mach.OBJ) (closure:Mach.FUN_CLOSURE) (args:Mach.VAL list) : Mach.VAL =
    case closure of 
	{ func=(Ast.Func f), allTypesBound, env } => 
	if not allTypesBound
	then LogErr.evalError ["invoking function with unbound type variables"]
	else 
	    case (#fsig f) of 
		Ast.FunctionSignature { params, ... } => 
		let
		    val fixtures = getFixtures (#fixtures f)
				       
		    val (varObj:Mach.OBJ) = Mach.newObj Mach.intrinsicObjectBaseTag Mach.Null NONE
		    val (varScope:Mach.SCOPE) = extendScope env Mach.VarActivation varObj

		    fun bindArg (a, b) = evalVarBinding env (SOME a) b
		    val inits = List.concat (map bindArg (ListPair.zip (args, params)))

		    (* FIXME: bind 'this' type from func *)
		    val thisProp = { kind = Mach.ValProp,
				     ty = Ast.SpecialType Ast.Any,
				     value = Mach.Object this,
				     attrs = { dontDelete = true,
					       dontEnum = true,
					       readOnly = true,
					       isFixed = false } }

		in
		    initScope varScope fixtures inits;
		    setProp varObj {id="this", ns=Ast.Internal ""} thisProp;
		    evalBlock varScope (#body f)
		end
		
(* 
 * This is the dynamic phase of function definition; it assumes that the 
 * function has already had its fixtures constructed during the
 * definition phase. 
 *) 
and evalFuncDefn (scope:Mach.SCOPE) (f:Ast.FUNC_DEFN) : INIT_LIST = 
    let 
	val func = (#func f)
	val funcAttrs = getAttrs (#attrs f)
	val name = case func of Ast.Func { name={ident, ...}, ...} => ident
	val fval = Mach.newFunc scope func
	val fname = {id = name, 
		     ns = (needNamespace (evalExpr scope (#ns funcAttrs)))}
    in
	LogErr.trace ["allocating function binding ", LogErr.name fname];
	[(fname, fval)]
    end

and evalBlock (scope:Mach.SCOPE) (block:Ast.BLOCK) : Mach.VAL = 
    case block of 
	Ast.Block {defns, stmts, fixtures, ...} => 
	let 
	    val blockObj = Mach.newObj Mach.intrinsicObjectBaseTag Mach.Null NONE
	    val blockScope = extendScope scope Mach.Let blockObj
	    val inits = evalDefns scope defns
	in
	    LogErr.trace ["initializing block scope"];
	    initScope blockScope (getFixtures fixtures) inits; 
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
