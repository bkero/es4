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

(* Fundamental object methods *)

fun setProp (obj:Mach.OBJ) (n:Mach.NAME) (p:Mach.PROP) =
    case obj of 
	Mach.Obj ob => Mach.addBinding (#bindings ob) n p

fun setValue (base:Mach.OBJ) (name:Mach.NAME) (v:Mach.VAL) =     
    (case base of 
	 Mach.Obj {bindings,...} => 
	 if Mach.hasBinding bindings name
	 then 
	     let 
		 val existingProp = Mach.getBinding bindings name
		 val existingAttrs = (#attrs existingProp)
		 val newProp = { ty = (#ty existingProp), 
				 value = v, 
				 attrs = { dontDelete = (#dontDelete existingAttrs), 
					   dontEnum = (#dontEnum existingAttrs),
					   readOnly = (#readOnly existingAttrs) } }
	     in
		 if (#readOnly existingAttrs)
		 then semant "assigning to read-only property"
		 else ();
		 (* FIXME: insert typecheck here *)
		 Mach.delBinding bindings name;
		 Mach.addBinding bindings name newProp
	     end
	 else
	     let 
		 val prop = { ty = Ast.SpecialType Ast.Any,
			      value = v,
			      attrs = { dontDelete = false,
					dontEnum = false,
					readOnly = false } }
	     in
		 Mach.addBinding bindings name prop
	     end; v)


fun extendScope (p:Mach.SCOPE) (t:Mach.SCOPE_TAG) (ob:Mach.OBJ) = 
    Mach.Scope { parent=(SOME p), tag=t, obj=ob }


fun evalExpr (scope:Mach.SCOPE) (expr:Ast.EXPR) =
    case expr of
	Ast.LiteralExpr lit => 
	evalLiteralExpr lit

      | Ast.ListExpr es =>
	evalListExpr scope es

      | Ast.LexicalRef { ident } =>
        evalRefExpr scope NONE ident

      | Ast.ObjectRef { base, ident } =>
        evalRefExpr scope (SOME (evalExpr scope base)) ident

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
	evalCallExpr 
	    (evalExpr scope func) 
	    (map (evalExpr scope) actuals)

      | _ => 
	raise unimpl "unhandled expression type"

and evalLiteralExpr (lit:Ast.LITERAL) = 
    case lit of 
        Ast.LiteralNull => Mach.Null
      | Ast.LiteralUndefined => Mach.Undef
      | Ast.LiteralNumber r => Mach.Num r
      | Ast.LiteralBoolean b => Mach.Bool b
      | Ast.LiteralString s => Mach.Str s
      | Ast.LiteralNamespace n => Mach.Namespace n
      | _ => unimpl "unhandled literal type"

and evalListExpr (scope:Mach.SCOPE) (es:Ast.EXPR list) = 
    case es of 
	[] => Mach.Undef
      | [e] => evalExpr scope e
      | (e::ez) => ((evalExpr scope e); (evalListExpr scope ez))

and evalCallExpr (v:Mach.VAL) (args:Mach.VAL list) =
    let 
	val func = 
	    case v of
		Mach.Reference r => Mach.deref r
	      | _ => v
	val thisObj = 
	    case v of
		Mach.Reference (Mach.Ref {base, ...}) => base
	      | _ => Mach.globalObject
    in
	case func of 
	    Mach.Function (Mach.Fun f) => (#code f) thisObj args
	  | _ => semant "calling non-function type"
    end


(* 
 * FIXME: possibly try to factor and merge this with evalVarBinding,
 * but not sure if that's easy. 
 *)

and evalSetExpr (scope:Mach.SCOPE) (aop:Ast.ASSIGNOP) (pat:Ast.PATTERN) (v:Mach.VAL) = 
    let
	fun modified r = 
	    let 
		fun modifyWith bop = performBinop bop (Mach.deref r) v
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
		    SOME (Mach.Reference r) => 
		    (case r of Mach.Ref { base, name } => setValue base name (modified r))
		  | SOME _ => semant "assignment to non-reference"
		  | NONE => raise Mach.MultiReferenceException multiname
	end
      | Ast.SimplePattern expr => 
	let
	    val r = evalExpr scope expr
	in
	    case r of 
		Mach.Reference r => 
		(case r of Mach.Ref { base, name } => setValue base name (modified r))
	      | _ => semant "assigning to non-reference"
	end
      |	_ => unimpl "unhandled pattern form in assignment"
    end


and rval (x:Mach.VAL) = 
    case x of 
        (Mach.Reference (Mach.Ref {base=Mach.Obj {bindings,...}, name})) => 
        (#value (Mach.getBinding bindings name))
      | _ => x
	     
and lval (x:Mach.VAL) = 
    case x of 
	Mach.Reference r => r
      | _ => semant "assigning to non-reference"

and evalUnaryOp (scope:Mach.SCOPE) (unop:Ast.UNOP) (expr:Ast.EXPR) =
    let
	fun crement f isPre = 
	    let 
		val r = lval (evalExpr scope expr)
		val n = Mach.toNum (Mach.deref r)
		val n' = Mach.Num (f (n, 1.0))
		val n'' = if isPre
			  then n'
			  else Mach.Num n
	    in
		case r of 
		    Mach.Ref {base, name} => (setValue base name n'; n'')
	    end
    in	    
	case unop of 
	    Ast.Delete => 
	    (case evalExpr scope expr of
		 Mach.Reference (Mach.Ref {base=Mach.Obj {bindings, ...}, name}) => 
		 (Mach.delBinding bindings name; Mach.Bool true)
	       | _ => Mach.Bool true)
	    
	  | Ast.PreIncrement => crement Real.+ true
	  | Ast.PreDecrement => crement Real.- true
	  | Ast.PostIncrement => crement Real.+ false
	  | Ast.PostDecrement => crement Real.- false
	  | _ => unimpl "unhandled unary operator"
    end

and performBinop (bop:Ast.BINOP) (a:Mach.VAL) (b:Mach.VAL) = 
    let 
	fun toNum x = Mach.toNum (rval x)
	fun toString x = Mach.toString (rval x)
	fun wordOp wop = 
	    let 
		val wa = Word.fromInt (trunc (toNum a))
		val wb = Word.fromInt (trunc (toNum b))
	    in
		Mach.Num (real (Word.toInt (wop (wa, wb))))
	    end
    in
	case bop of 
	    Ast.Plus => 
	    (case (rval a, rval b) of 
		 (Mach.Num na, Mach.Num nb) => Mach.Num (na + nb)
	       | (_,_) => Mach.Str ((toString a) ^ (toString b)))
	  | Ast.Minus => Mach.Num ((toNum a) - (toNum b))
	  | Ast.Times => Mach.Num ((toNum a) * (toNum b))
	  | Ast.Divide => Mach.Num ((toNum a) / (toNum b))
	  | Ast.Remainder => Mach.Num (real (Int.rem ((trunc (toNum a)), (trunc (toNum b)))))
			     
	  | Ast.LeftShift => wordOp Word.<<
	  | Ast.RightShift => wordOp Word.>>
	  | Ast.RightShiftUnsigned => wordOp Word.~>>
	  | Ast.BitwiseAnd => wordOp Word.andb
	  | Ast.BitwiseOr => wordOp Word.orb
	  | Ast.BitwiseXor => wordOp Word.xorb
			      
	  | Ast.Equals => Mach.Bool (Mach.equals a b)
	  | Ast.NotEquals => Mach.Bool (not (Mach.equals a b))
	  | Ast.StrictEquals => Mach.Bool (Mach.equals a b)
	  | Ast.StrictNotEquals => Mach.Bool (not (Mach.equals a b))
	  | Ast.Less => Mach.Bool (Mach.less a b)
	  | Ast.LessOrEqual => Mach.Bool ((Mach.less a b) orelse (Mach.equals a b))
	  | Ast.Greater => Mach.Bool (not ((Mach.less a b) orelse (Mach.equals a b)))
	  | Ast.GreaterOrEqual => Mach.Bool (not (Mach.less a b))
				  
	  | _ => unimpl "unhandled binary operator type"
    end

and evalBinaryOp (scope:Mach.SCOPE) (bop:Ast.BINOP) (aexpr:Ast.EXPR) (bexpr:Ast.EXPR) =
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
	     

and evalCondExpr (scope:Mach.SCOPE) (cond:Ast.EXPR) (consequent:Ast.EXPR) (alternative:Ast.EXPR) = 
    let 
        val v = evalExpr scope cond
        val b = Mach.toBoolean v
    in
	if b 
	then evalExpr scope consequent
	else evalExpr scope alternative
    end
    

and needNamespace v = 
    case v of 
	Mach.Namespace n => n
      | _ => semant "need namespace"

and evalIdentExpr (scope:Mach.SCOPE) (r:Ast.IDENT_EXPR) = 
    case r of 
	Ast.Identifier { ident, openNamespaces } => 
	{ nss=(!openNamespaces), id=ident }
	
      | Ast.QualifiedIdentifier { qual, ident } => 
	{ nss = [needNamespace (evalExpr scope qual)], id = ident }

      | Ast.QualifiedExpression { qual, expr } => 
	{ nss = [needNamespace (evalExpr scope qual)], 
	  id = Mach.toString (evalExpr scope expr) }

      | _ => unimpl "unimplemented identifier expression form"
		   

and evalRefExpr (scope:Mach.SCOPE) (b:Mach.VAL option) (r:Ast.IDENT_EXPR) =
    let 
	val (multiname:Mach.MULTINAME) = evalIdentExpr scope r
	val refOpt = (case b of 
			  SOME (Mach.Object ob) => resolveOnObjAndPrototypes ob multiname
			| NONE => resolveOnScopeChain scope multiname
			| _ => semant "ref expression on non-object value")
    in
	case refOpt of 
	    NONE => raise Mach.MultiReferenceException multiname
	  | SOME r' => r'
    end
    

and evalLetExpr (scope:Mach.SCOPE) (defs:Ast.VAR_BINDING list) (body:Ast.EXPR list) = 
    let 
	val obj = Mach.newObject NONE
        val newScope = extendScope scope Mach.Let obj
    in
        List.app (evalVarBinding scope (SOME obj) NONE) defs;
        evalListExpr newScope body
    end
    
and getType (tyOpt:Ast.TYPE_EXPR option) = 
    case tyOpt of 
	SOME t => t
      | NONE => Ast.SpecialType Ast.Any

and getAttrs (attrs:Ast.ATTRIBUTES) = case attrs of Ast.Attributes a => a

and getScopeObj (scope:Mach.SCOPE) = case scope of Mach.Scope {obj, ...} => obj

and evalVarBinding (scope:Mach.SCOPE) (obj:Mach.OBJ option) (v:Mach.VAL option) (defn:Ast.VAR_BINDING) =
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
			 val prop = { ty = t, 
				      value = v', 
				      attrs = { dontDelete = true,
						dontEnum = true,
						readOnly = ro } }
		     in
			 setProp ob n prop
		     end
		   | _ => unimpl "unhandled identifier form in identifier binding pattern"
		)
	      | _ => unimpl "unhandled pattern form in binding"
    end
          
and resolveOnScopeChain (scope:Mach.SCOPE) (mname:Mach.MULTINAME) =     
    case scope of 
	Mach.Scope { parent, obj, ... } => 
	case resolveOnObjAndPrototypes obj mname of
	    NONE => (case parent of 
			 SOME p => resolveOnScopeChain p mname
		       | NONE => NONE)
	  | result => result
    
and resolveOnObjAndPrototypes (obj:Mach.OBJ) (mname:Mach.MULTINAME) = 
    case obj of 
	Mach.Obj ob => 
	case resolveOnObj obj mname of 
	    NONE => 
	    let 
		val proto = !(#prototype ob)
	    in
		case proto of 
		    NONE => NONE
                  | SOME p => resolveOnObjAndPrototypes p mname
	    end
	  | result => result

and resolveOnObj (obj:Mach.OBJ) (mname:Mach.MULTINAME) =    
    case obj of 
	Mach.Obj ob => 
	let     
	    val id = (#id mname)
	    fun tryName [] = NONE
	      | tryName (x::xs) = 
		let 
		    val n = {ns=x, id=id} 
		in
		    if Mach.hasBinding (#bindings ob) n
		    then SOME (Mach.Reference (Mach.Ref { base = obj, name = n }))
		    else tryName xs
		end
	in
	    tryName (#nss mname)
	end

and evalTailCallExpr scope e = 
    raise (TailCallException (fn _ => evalExpr scope e))

and evalNonTailCallExpr scope e = 
    evalExpr scope e
    handle TailCallException thunk => thunk ()
         | ReturnException v => v

fun labelEq stmtLabel exnLabel = 
    case (stmtLabel, exnLabel) of
	(SOME sl, SOME el) => sl = el
      | (NONE, SOME _) => false
      | (_, NONE) => true
		     
fun evalStmts (scope:Mach.SCOPE) (stmts:Ast.STMT list) = 
    (map (evalStmt scope) stmts; Mach.Undef)
		       
and evalStmt scope (stmt:Ast.STMT) = 
    case stmt of 
	Ast.ExprStmt e => evalListExpr scope e
      | Ast.IfStmt i => evalIfStmt scope i
      | Ast.WhileStmt w => evalWhileStmt scope w
      | Ast.ReturnStmt r => evalReturnStmt scope r
      | Ast.BreakStmt lbl => evalBreakStmt scope lbl
      | Ast.ContinueStmt lbl => evalContinueStmt scope lbl
      | Ast.ThrowStmt t => evalThrowStmt scope t
      | Ast.LabeledStmt (lab, s) => evalLabelStmt scope lab s
      | Ast.BlockStmt b => evalBlock scope b
      | Ast.EmptyStmt => Mach.Undef
      | _ => unimpl "unimplemented statement type"

and evalDefns (scope:Mach.SCOPE) (ds:Ast.DEFN list) = 
    (List.app (evalDefn scope) ds; Mach.Undef)

and evalDefn (scope:Mach.SCOPE) (d:Ast.DEFN) = 
    case d of 
	Ast.FunctionDefn f => evalFuncDefn scope f
      | Ast.VariableDefn bs => List.app (evalVarBinding scope NONE NONE) bs
      | _ => unimpl "unimplemented definition type"

and evalFunc (scope:Mach.SCOPE) (f:Ast.FUNC) : Mach.FUN = 
    (* Our goal here is to convert the definition AST of the function
     * into an SML function that invokes the interpreter on the
     * function's body with a new scope. The new scope should be the
     * lexical scope of the definition augmented with bindings connecting
     * all the formals to their corresponding actuals, and a final binding
     * for the name 'this', bound to the dynamic value provided from the
     * function call site. 
     *)
    let 
	val func = case f of Ast.Func f' => f'
	val funcSign = case (#sign func) of Ast.FunctionSignature s => s
	fun funcClosure (thisObj:Mach.OBJ) (args:Mach.VAL list) = 
	    let
		val (varObj:Mach.OBJ) = Mach.newObject NONE 
		val (varScope:Mach.SCOPE) = extendScope scope Mach.VarActivation varObj
		fun bindArgs [] [] = ()
		  | bindArgs [] (b::bz) = semant "Too few actuals"
		  | bindArgs (a::az) [] = semant "Too many actuals"
		  | bindArgs (a::az) (b::bz) = (evalVarBinding scope (SOME varObj) (SOME a) b; 
						bindArgs az bz)		    
		val thisProp = { ty = Ast.SpecialType Ast.Any,
				 value = Mach.Object thisObj,
				 attrs = { dontDelete = true,
					   dontEnum = true,
					   readOnly = true } }
	    in
		bindArgs args (#params funcSign);
		setProp varObj {id="this", ns=Ast.Private} thisProp;
		evalBlock varScope (#body func)
	    end
    in
	Mach.Fun { code = funcClosure,
		   ty = signToType (#sign func),
		   definition = NONE,
		   layout = Mach.emptyFuncLayout }
    end

and signToType (sign:Ast.FUNC_SIGN) = 
    let
	val paramTypes = ref []
	val hasRest = ref false
	fun processFormal formal = 
	    case formal of 
		Ast.Binding { kind, ty, ... } => 
		if kind = Ast.Rest
		then hasRest := true
		else paramTypes := ty :: !paramTypes
    in
	case sign of 
	    Ast.FunctionSignature { params, resulttype, ... } => 
	    (List.app processFormal params; 
	     Ast.FunctionType { paramTypes = List.rev (!paramTypes),
				returnType = resulttype,
				boundThisType = SOME (Ast.SpecialType Ast.Any) (* FIXME: calculate a 'this' type! *),
				hasRest = !hasRest })
    end


and evalFuncDefn (scope:Mach.SCOPE) (f:Ast.FUNC_DEFN) = 
    let 
	val func = case (#func f) of Ast.Func f' => f'
	(* Fixme: maybe some sort of different binding form for different name kinds? *)
	val name = case (#kind (#name func)) of
		       _ => (#ident (#name func))
	val (funcClosure:Mach.FUN) = evalFunc scope (#func f)
	val funcAttrs = getAttrs (#attrs f)
	val funcProp = { ty = signToType (#sign func),
			 value = Mach.Function funcClosure,
			 attrs = { dontDelete = true,
				   dontEnum = false,
				   readOnly = false } }
	val funcName = {id = name, 
			ns = (needNamespace (evalExpr scope (#ns funcAttrs)))}
    in
	setProp (getScopeObj scope) funcName funcProp
    end

and evalBlock scope block = 
    case block of 
	Ast.Block {defns, stmts, ...} => 
	(evalDefns scope defns;
	 evalStmts scope stmts)

and evalIfStmt scope ifStmt = 
    case ifStmt of 
	{ cnd, thn, els } => 
	let 
            val v = evalExpr scope cnd 
            val b = Mach.toBoolean v
	in
            if b 
            then evalStmt scope thn
            else evalStmt scope els
	end
    
and evalLabelStmt scope lab s = 
    evalStmt scope s
    handle BreakException exnLabel 
	   => if labelEq (SOME lab) exnLabel
              then Mach.Undef 
              else raise BreakException exnLabel
			 
and evalWhileStmt scope whileStmt = 
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
    
and evalReturnStmt scope e
  = raise (ReturnException (evalListExpr scope e))
	  
and evalThrowStmt scope e
  = raise (ThrowException (evalListExpr scope e))
	  
and evalBreakStmt scope lbl
  = raise (BreakException lbl)
	  
and evalContinueStmt scope lbl
  = raise (ContinueException lbl)

and evalPackage scope (package:Ast.PACKAGE) = 
    evalBlock scope (#body package)

and evalProgram (prog:Ast.PROGRAM) = 
    (Mach.populateIntrinsics Mach.globalObject;
     map (evalPackage Mach.globalScope) (#packages prog);
     evalBlock Mach.globalScope (#body prog))


end
