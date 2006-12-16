structure Eval = struct 

(* Exceptions for object-language control transfer. *)
exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> Mach.VAL)
exception ThrowException of Mach.VAL
exception ReturnException of Mach.VAL
exception SemanticException of Ast.USTRING

fun unimpl s =
    (List.app TextIO.print ["*unimplemented: ", s, "\n"];
     raise Mach.UnimplementedException s)

fun semant s = 
    (List.app TextIO.print ["*semantic error: ", s, "\n"];
     raise SemanticException s)
    
fun newName (i:Ast.IDENT) (n:Mach.NS) = {ns=n, id=i}

(* A small number of functions do not fully evaluate to Mach.VAL 
 * values, but instead to REFs; these are temporary artifacts of
 * evaluation, not first-class values in the language. *)

type REF = (Mach.OBJ * Mach.NAME)

(* Fundamental object methods *)

fun getValue (obj:Mach.OBJ) (name:Mach.NAME) : Mach.VAL = 
    case obj of 
	Mach.Obj {props, ...} => 
	let 
	    val prop = Mach.getBinding props name
	in
	    if (#kind prop) = Mach.TypeProp
	    then semant "property is a type, wanted a value"
	    else (#value prop)
	end

fun setProp (obj:Mach.OBJ) (n:Mach.NAME) (p:Mach.PROP) : unit =
    case obj of 
	Mach.Obj ob => Mach.addBinding (#props ob) n p

fun setValue (base:Mach.OBJ) (name:Mach.NAME) (v:Mach.VAL) : unit =     
    case base of 
	Mach.Obj {props,...} => 
	if Mach.hasBinding props name
	then 
	    let 
		val existingProp = Mach.getBinding props name
		val _ = if (#kind existingProp) = Mach.TypeProp 
			then semant "assigning a value to a type property"
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
		then semant "assigning to read-only property"
		else ();
		(* FIXME: insert typecheck here *)
		Mach.delBinding props name;
		Mach.addBinding props name newProp
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
		Mach.addBinding props name prop
	    end


fun extendScope (p:Mach.SCOPE) (t:Mach.SCOPE_TAG) (ob:Mach.OBJ) : Mach.SCOPE = 
    Mach.Scope { parent=(SOME p), tag=t, object=ob }

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
	
      | Ast.LetExpr {defs, body} => 
	evalLetExpr scope defs body

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
	    

      | _ => 
	raise unimpl "unhandled expression type"

and evalLiteralExpr (lit:Ast.LITERAL) : Mach.VAL = 
    case lit of 
        Ast.LiteralNull => Mach.Null
      | Ast.LiteralUndefined => Mach.Undef
      | Ast.LiteralNumber n => Mach.newNumber n
      | Ast.LiteralBoolean b => Mach.newBoolean b
      | Ast.LiteralString s => Mach.newString s
      | Ast.LiteralNamespace n => Mach.newNamespace n
      | _ => unimpl "unhandled literal type"

and evalListExpr (scope:Mach.SCOPE) (es:Ast.EXPR list) : Mach.VAL = 
    case es of 
	[] => Mach.Undef
      | [e] => evalExpr scope e
      | (e::ez) => ((evalExpr scope e); (evalListExpr scope ez))

and needObj (v:Mach.VAL) : Mach.OBJ = 
    case v of 
	Mach.Object ob => ob
      | _ => semant "need object"

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
	  | _ => semant "calling non-callable object"
						       

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
		  | NONE => raise Mach.MultiReferenceException multiname
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
      |	_ => unimpl "unhandled pattern form in assignment"
    end

and evalLhsExpr (scope:Mach.SCOPE) (expr:Ast.EXPR) : REF = 
    case expr of 	
	Ast.LexicalRef { ident } => 
	evalRefExpr scope NONE ident
      | Ast.ObjectRef { base, ident } => 
	evalRefExpr scope (SOME (evalExpr scope base)) ident
      | _ => semant "need lexical or object-reference expression"

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
		 (Mach.delBinding props name; Mach.newBoolean true))
	    
	  | Ast.PreIncrement => crement Real.+ true
	  | Ast.PreDecrement => crement Real.- true
	  | Ast.PostIncrement => crement Real.+ false
	  | Ast.PostDecrement => crement Real.- false
	  | _ => unimpl "unhandled unary operator"
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
				  
	  | _ => unimpl "unhandled binary operator type"
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
	   | _ => semant "need namespace")
      | _ => semant "need namespace"

and evalIdentExpr (scope:Mach.SCOPE) (r:Ast.IDENT_EXPR) : Mach.MULTINAME = 
    case r of 
	Ast.Identifier { ident, openNamespaces } => 
	{ nss=(!openNamespaces), id=ident }
	
      | Ast.QualifiedIdentifier { qual, ident } => 
	{ nss = [needNamespace (evalExpr scope qual)], id = ident }

      | Ast.QualifiedExpression { qual, expr } => 
	{ nss = [needNamespace (evalExpr scope qual)], 
	  id = Mach.toString (evalExpr scope expr) }

      | _ => unimpl "unimplemented identifier expression form"
		   

and evalRefExpr (scope:Mach.SCOPE) (b:Mach.VAL option) (r:Ast.IDENT_EXPR) : REF =
    let 
	val (multiname:Mach.MULTINAME) = evalIdentExpr scope r
	val refOpt = (case b of 
			  SOME (Mach.Object ob) => 
			  resolveOnObjAndPrototypes ob multiname
			| NONE => 
			  resolveOnScopeChain scope multiname
			| _ => semant "ref expression on non-object value")
    in
	case refOpt of 
	    NONE => raise Mach.MultiReferenceException multiname
	  | SOME r' => r'
    end
    

and evalLetExpr (scope:Mach.SCOPE) (defs:Ast.VAR_BINDING list) (body:Ast.EXPR list) : Mach.VAL = 
    let 
	val obj = Mach.newObj Mach.intrinsicObjectBaseTag NONE NONE
        val newScope = extendScope scope Mach.Let obj
    in
        List.app (evalVarBinding scope (SOME obj) NONE) defs;
        evalListExpr newScope body
    end
    
and getType (tyOpt:Ast.TYPE_EXPR option) : Ast.TYPE_EXPR = 
    case tyOpt of 
	SOME t => t
      | NONE => Ast.SpecialType Ast.Any

and getAttrs (attrs:Ast.ATTRIBUTES) = case attrs of Ast.Attributes a => a

and getScopeObj (scope:Mach.SCOPE) : Mach.OBJ = case scope of Mach.Scope { object, ...} => object

and evalVarBinding 
	(scope:Mach.SCOPE) 
	(obj:Mach.OBJ option) 
	(v:Mach.VAL option) 
	(defn:Ast.VAR_BINDING) : unit =
    let 
	val ob = case obj of 
		     SOME ob' => ob'
		   | NONE => getScopeObj scope
    in
	case defn of 
	    (Ast.Binding { kind, init, attrs, pattern, ty }) =>
	    case pattern of 
		Ast.IdentifierPattern id => 
		(case id of 
		     Ast.Identifier {ident, ...} => 
		     let 
			 val Ast.Attributes {ns,...} = attrs
			 val n = newName ident (needNamespace (evalExpr scope ns))
			 val ro = case kind of 
				      Ast.Const => true
				    | Ast.LetConst => true
				    | _ => false
			 val t = getType ty
			 val v' = case v of 
				      SOME v'' => v''
				    | NONE => (case init of 
						  NONE => Mach.Undef
						| SOME e => evalExpr scope e)
			 (* FIXME: Perform typecheck here and handle different binding kinds. *)
			 val prop = { kind = Mach.ValProp,
				      ty = t, 
				      value = v', 
				      attrs = { dontDelete = true,
						dontEnum = true,
						readOnly = ro,
						isFixed = false } }
		     in
			 setProp ob n prop
		     end
		   | _ => unimpl "unhandled identifier form in identifier binding pattern"
		)
	      | _ => unimpl "unhandled pattern form in binding"
    end
          
and resolveOnScopeChain (scope:Mach.SCOPE) (mname:Mach.MULTINAME) : REF option =
    case scope of 
	Mach.Scope { parent, object, ... } => 
	case resolveOnObjAndPrototypes object mname of
	    NONE => (case parent of 
			 SOME p => resolveOnScopeChain p mname
		       | NONE => NONE)
	  | result => result
    
and resolveOnObjAndPrototypes (obj:Mach.OBJ) (mname:Mach.MULTINAME) : REF option = 
    case obj of 
	Mach.Obj ob => 
	case resolveOnObj obj mname of 
	    NONE => 
	    let 
		val proto = !(#proto ob)
	    in
		case proto of 
                    SOME (Mach.Object ob) => resolveOnObjAndPrototypes ob mname
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
		in
		    if Mach.hasBinding (#props ob) n
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
      | _ => unimpl "unimplemented statement type"

and evalDefns (scope:Mach.SCOPE) (ds:Ast.DEFN list) : unit = 
    List.app (evalDefn scope) ds

and evalDefn (scope:Mach.SCOPE) (d:Ast.DEFN) : unit = 
    case d of 
	Ast.FunctionDefn f => evalFuncDefn scope f
      | Ast.VariableDefn bs => List.app (evalVarBinding scope NONE NONE) bs
      | _ => unimpl "unimplemented definition type"

and invokeFuncClosure (this:Mach.OBJ) (closure:Mach.FUNC_CLOSURE) (args:Mach.VAL list) : Mach.VAL =
    case closure of 
	{ func=(Ast.Func f), allTypesBound, env } => 
	if not allTypesBound
	then semant "invoking function with unbound type variables"
	else 
	    case (#fsig f) of 
		Ast.FunctionSignature { params, ... } => 
		let
		    val (varObj:Mach.OBJ) = Mach.newObj Mach.intrinsicObjectBaseTag NONE NONE
		    val (varScope:Mach.SCOPE) = extendScope env Mach.VarActivation varObj
		    fun bindArgs [] [] = ()
		      | bindArgs [] (b::bz) = semant "Too few actuals"
		      | bindArgs (a::az) [] = semant "Too many actuals"
		      | bindArgs (a::az) (b::bz) = (evalVarBinding env (SOME varObj) (SOME a) b; 
						    bindArgs az bz)
		    (* FIXME: bind 'this' type from func *)
		    val thisProp = { kind = Mach.ValProp,
				     ty = Ast.SpecialType Ast.Any,
				     value = Mach.Object this,
				     attrs = { dontDelete = true,
					       dontEnum = true,
					       readOnly = true,
					       isFixed = false } }
		in
		    bindArgs args params;
		    setProp varObj {id="this", ns=Ast.Private} thisProp;
		    evalBlock varScope (#body f)
		end

and evalFuncDefn (scope:Mach.SCOPE) (f:Ast.FUNC_DEFN) : unit = 
    let 
	val func = (#func f)
	val fsig = case func of Ast.Func { fsig, ...} => fsig
	val name = case func of Ast.Func { name={ident, ...}, ...} => ident
	val fval = Mach.newFunc scope func
	val funcAttrs = getAttrs (#attrs f)
	val funcProp = { kind = Mach.ValProp,
			 ty = Ast.FunctionType fsig,
			 value = fval,
			 attrs = { dontDelete = true,
				   dontEnum = false,
				   readOnly = false,
				   isFixed = false } }
	val funcName = {id = name, 
			ns = (needNamespace (evalExpr scope (#ns funcAttrs)))}
    in
	setProp (getScopeObj scope) funcName funcProp
    end

and evalBlock (scope:Mach.SCOPE) (block:Ast.BLOCK) : Mach.VAL = 
    case block of 
	Ast.Block {defns, stmts, ...} => 
	(evalDefns scope defns;
	 evalStmts scope stmts)

and evalIfStmt (scope:Mach.SCOPE) (cnd:Ast.EXPR) (thn:Ast.STMT) (els:Ast.STMT) = 
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
