structure Defn = struct

(* 
 * The goal of the definition phase is to put together the fixtures
 * of the program, as well as insert class, function and interface
 * objects into the global object.
 *)

fun inr f (a, b) = (a, f b)


fun resolveFixture (fixs:Ast.FIXTURES list) (mname:Mach.MULTINAME) : Ast.FIXTURE =
    case fixs of 
	[] => LogErr.defnError ["unresolved fixture"]
      |	(Ast.Fixtures { bindings, ... }) :: parents => 
	let     
	    val id = (#id mname)
	    fun tryName [] = NONE
	      | tryName (x::xs) = 
		let 
		    val n = {ns=x, id=id} 
		in
		    if Mach.hasFixture bindings n
		    then SOME n
		    else tryName xs
		end
	in
	    case tryName (#nss mname) of 
		SOME n => Mach.getFixture bindings n 
	      | NONE => resolveFixture parents mname
	end


fun resolveExprToNamespace (fixs:Ast.FIXTURES list) (expr:Ast.EXPR) : Mach.NS = 
    case expr of 
	Ast.LiteralExpr (Ast.LiteralNamespace ns) => ns
      | Ast.LexicalRef {ident = Ast.Identifier {ident, openNamespaces} } => 
	let 
	    val mname = {nss = openNamespaces, id = ident}
	in
	    case resolveFixture fixs mname of 
		Ast.NamespaceFixture ns => ns
	      | _ => LogErr.defnError ["namespace expression resolved ",
				       "to non-namespace fixture"]
	end
      | _ => LogErr.defnError ["unexpected expression type ",
			       "in namespace context"]


fun newFrame (parentFixtures:Ast.FIXTURES list) 
	     (newBindings:Ast.FIXTURE_BINDINGS) 
    : Ast.FIXTURES = 
    case parentFixtures of 
	[] => 
	Ast.Fixtures { tag = Ast.FrameFixtures,
		       bindings = newBindings,
		       isExtensible = false,
		       openNamespaces = [Ast.Internal ""],
		       numberType = Ast.Number,
		       roundingMode = Ast.HalfEven } 
      | (Ast.Fixtures { numberType, roundingMode, openNamespaces, ... } :: _) =>
	Ast.Fixtures { tag = Ast.FrameFixtures,
		       bindings = newBindings,
		       isExtensible = false,
		       openNamespaces = openNamespaces, 
		       numberType = numberType,
		       roundingMode = roundingMode } 


fun defClass (parentFixtures:Ast.FIXTURES list) 
	     (cdef:Ast.CLASS_DEFN) 
    : (Ast.FIXTURE_BINDINGS * Ast.CLASS_DEFN) = 
    (* FIXME *)
    case cdef of 
	{ name, 
	  nonnullable,
	  attrs, 
	  params,
	  extends,
	  implements, 
	  classFixtures, 
	  instanceFixtures,
	  body,
	  instanceVars,
	  instanceMethods,
	  vars,
	  methods, 
	  constructor, 
	  initializer } => ([], cdef)


and defVar (parentFixtures:Ast.FIXTURES list) 
	    (var:Ast.VAR_BINDING) 
    : (Ast.FIXTURE_BINDINGS * Ast.VAR_BINDING) = 
    (* FIXME *)
    ([], var)


and defVars (parentFixtures:Ast.FIXTURES list) 
	    (vars:Ast.VAR_BINDING list) 
    : (Ast.FIXTURE_BINDINGS * (Ast.VAR_BINDING list)) = 
    let
	val (fbl, vbl) = ListPair.unzip (map (defVar parentFixtures) vars)
    in
	(List.concat fbl, vbl)
    end


and defFuncSig (parentFixtures:Ast.FIXTURES list) 
    (name:Ast.NAME option)
    (fsig:Ast.FUNC_SIG)
    : (Ast.FUNC_SIG * Ast.FIXTURE_BINDINGS * Ast.FIXTURE_BINDINGS) =
    case fsig of 
	Ast.FunctionSignature { typeParams, params, inits, 
				returnType, thisType, 
				hasBoundThis, hasRest } =>
	let 
	    fun mkTypeVarBinding x = ({ns=Ast.Internal "", id=x}, Ast.TypeVarFixture)
	    val typeParamBindings = map mkTypeVarBinding typeParams
	    val boundTypeFixtures = newFrame parentFixtures typeParamBindings
	    val typeEnv = (boundTypeFixtures :: parentFixtures)
	    val (paramBindings, newParams) = defVars typeEnv params
	    val (initBindings, newInits) = 
		case inits of NONE => ([], NONE)
			    | SOME i => 
			      let 
				  val (bindings, newDefns) = defVars typeEnv (#defns i)
			      in
				  (bindings, SOME { defns = newDefns, inits = (#inits i) })
			      end
	    val selfBindings = 
		case name of 
		    NONE => []
		  | SOME n => [(n, Ast.ValFixture { ty = Ast.FunctionType fsig,
						    readOnly = true,
						    isOverride = false })]
	    val newFsig = Ast.FunctionSignature { typeParams = typeParams,
						  params = newParams,
						  inits = newInits,
						  returnType = defTyExpr typeEnv returnType,
						  thisType = case thisType of 
								 NONE => NONE
							       | SOME t => SOME (defTyExpr typeEnv t),
						  hasBoundThis = hasBoundThis,
						  hasRest = hasRest }
	    val allParamBindings = paramBindings @ initBindings
	    val funcBindings = (allParamBindings @ typeParamBindings @ selfBindings)
	in
	    (newFsig, selfBindings, funcBindings)
	end


and defFunc (parentFixtures:Ast.FIXTURES list) 
	    (f:Ast.FUNC_DEFN) 
    : (Ast.FIXTURE_BINDINGS * Ast.FUNC_DEFN) = 
    case (#func f) of 
	Ast.Func { name, fsig, body, ... } =>
	let 
	    val attrs = (#attrs f)
	    val qualNs = case attrs of 
			     Ast.Attributes { ns, ... } => 
			     resolveExprToNamespace parentFixtures ns
	    val ident = case (#kind name) of 
			    Ast.Ordinary => (#ident name)
			  | _ => LogErr.unimplError ["unhandled type of function name"]
				 
	    val (newFsig, outerBindings, innerBindings) = 
		defFuncSig parentFixtures (SOME { ns=qualNs, id=ident }) fsig
	    val funcFixtures = newFrame parentFixtures innerBindings
	    val newFunc = Ast.Func { name = name, 
				     fsig = newFsig, 
				     body = defBlock (funcFixtures :: parentFixtures) body,
				     fixtures = SOME funcFixtures }
	in
	    (outerBindings, {func = newFunc, kind = (#kind f), attrs = attrs })
	end

	
and defPragma (parentFixtures:Ast.FIXTURES list) 
	      (pragma:Ast.PRAGMA) 
    : Ast.FIXTURE_BINDINGS = 
    (* FIXME *)
    []


and defIdentExpr (parentFixtures:Ast.FIXTURES list) (ie:Ast.IDENT_EXPR) : Ast.IDENT_EXPR = 
    ie


and defExpr (parentFixtures:Ast.FIXTURES list) (expr:Ast.EXPR) : Ast.EXPR = 
    let 
	fun sub e = defExpr parentFixtures e
	fun subs e = defExprs parentFixtures e
    in
	case expr of 
	    Ast.TrinaryExpr (t, e1, e2, e3) => 
	    Ast.TrinaryExpr (t, sub e1, sub e2, sub e3)
	    
	  | Ast.BinaryExpr (b, e1, e2) => 
	    Ast.BinaryExpr (b, sub e1, sub e2) 
	    
	  | Ast.BinaryTypeExpr (b, e, te) => 
	    Ast.BinaryTypeExpr (b, sub e, defTyExpr parentFixtures te)

	  | Ast.UnaryExpr (u, e) => 
	    Ast.UnaryExpr (u, sub e)

	  | Ast.TypeExpr t => 
	    Ast.TypeExpr (defTyExpr parentFixtures t)

	  | Ast.NullaryExpr n => 
	    Ast.NullaryExpr n

	  | Ast.YieldExpr eso => 
	    (case eso of 
		 NONE => Ast.YieldExpr NONE
	       | SOME es => Ast.YieldExpr (SOME (subs es)))

	  | Ast.SuperExpr eo => 
	    (case eo of
		 NONE => Ast.SuperExpr NONE
	       | SOME e => Ast.SuperExpr (SOME (sub e)))
	    
	  (* FIXME: possibly need to reinterpret literals given arithmetic modes. *)
	  | Ast.LiteralExpr le => 
	    Ast.LiteralExpr le
	    
	  | Ast.CallExpr {func, actuals} => 
	    Ast.CallExpr {func = sub func,
			  actuals = map sub actuals }

	  | Ast.ApplyTypeExpr { expr, actuals } =>
	    Ast.ApplyTypeExpr { expr = sub expr,
				actuals = map (defTyExpr parentFixtures) actuals }

	  | Ast.LetExpr { defs, body, fixtures } => 
	    let
		val (b0, newDefs) = defVars parentFixtures defs
		val f0 = newFrame parentFixtures b0
		val newBody = defExprs (f0 :: parentFixtures) body
	    in
		Ast.LetExpr { defs = newDefs,
			      body = newBody,
			      fixtures = SOME f0 }
	    end

	  | Ast.NewExpr { obj, actuals } => 
	    Ast.NewExpr { obj = sub obj,
			  actuals = subs actuals }

	  | Ast.FunExpr { ident, fsig, body, fixtures } => 
	    let
		val (newFsig, _, innerBindings) = 
		    case ident of 
			SOME id => defFuncSig parentFixtures (SOME { ns=(Ast.Internal ""), id=id }) fsig
		      | NONE => defFuncSig parentFixtures NONE fsig
		val funcFixtures = newFrame parentFixtures innerBindings
	    in
		Ast.FunExpr { ident = ident,
			      fsig = newFsig,
			      body = defBlock (funcFixtures :: parentFixtures) body,
			      fixtures = SOME funcFixtures }
	    end

	  | Ast.ObjectRef { base, ident } =>
	    Ast.ObjectRef { base = sub base,
			    ident = defIdentExpr parentFixtures ident }

	  | Ast.LexicalRef { ident } => 
	    Ast.LexicalRef { ident = defIdentExpr parentFixtures ident } 

	  | Ast.SetExpr (a, p, e) => 
	    (* FIXME: probably need to do something complicated with temporary bindings here. *)
	    let 
		val (_, newPattern) = defPattern parentFixtures p
	    in
		Ast.SetExpr (a, newPattern, sub e)
	    end

	  | Ast.ListExpr es => 
	    Ast.ListExpr (subs es) 

	  | Ast.SliceExpr (a, b, c) => 
	    Ast.SliceExpr (subs a, subs b, subs c) 
    end


and defExprs (parentFixtures:Ast.FIXTURES list) 
	     (expr:Ast.EXPR list) 
    : Ast.EXPR list = 
    map (defExpr parentFixtures) expr


and defTyExpr (parentFixtures:Ast.FIXTURES list)
	      (ty:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR = 
    (* FIXME *)
    ty

    
and defPattern (parentFixtures:Ast.FIXTURES list)
	       (pat:Ast.PATTERN) 
    : (Ast.FIXTURE_BINDINGS * Ast.PATTERN) = 
    (* FIXME *)
    ([], pat)


and defStmt (parentFixtures:Ast.FIXTURES list) (stmt:Ast.STMT) : (Ast.STMT) = 
    let
	fun reconstructForEnumStmt (fe:Ast.FOR_ENUM_STMT) = 
	    case fe of 
		{ ptrn, obj, defns, contLabel, body } => 
		let
		    val (b0, newPtrn) = 
			case ptrn of 
			    NONE => ([], NONE)
			  | SOME p => inr (SOME) (defPattern parentFixtures p)
		    val f0 = newFrame parentFixtures b0
		    val newObj =  defExprs parentFixtures obj
		    val (b1, newDefns) = defVars (f0 :: parentFixtures) defns
		    val f1 = newFrame (f0 :: parentFixtures) (b1 @ b0)
		    val newBody = defStmt (f1 :: parentFixtures) body
		in
		    { ptrn = newPtrn,
		      obj = newObj,
		      defns = newDefns,
		      contLabel = contLabel,
		      body = newBody }
		end
	fun reconstructWhileStmt (w:Ast.WHILE_STMT) = 
	    case w of 
		{ cond, body, contLabel} => 
		let 
		    val newCond = defExpr parentFixtures cond
		    val newBody = defStmt parentFixtures body
		in
		    { cond=newCond, 
		      body=newBody, 
		      contLabel=contLabel}
		end

	fun reconstructForStmt { defns, init, cond, update, contLabel, body } =
	    let
		val (b0, newDefns) = defVars parentFixtures defns
		val f0 = newFrame parentFixtures b0
		val newFix = f0 :: parentFixtures
		val newInit = defExprs newFix init
		val newCond = defExprs newFix cond
		val newUpdate = defExprs newFix update
		val newBody = defStmt newFix body
	    in
		Ast.ForStmt { defns = newDefns,
			      init = newInit,
			      cond = newCond,
			      update = newUpdate,
			      contLabel = contLabel,
			      body = newBody }
	    end

	fun reconstructCatch { bind, body } =
	    let 
		val (b0, newBind) = defVar parentFixtures bind
		val f0 = newFrame parentFixtures b0
	    in		     
		{ bind = newBind, 
		  body = defBlock (f0 :: parentFixtures) body }
	    end	    

	fun reconstructCase { label, body } =
	    { label = (case label of 
			   NONE => NONE 
			 | SOME e => SOME (defExprs parentFixtures e)),
	      body = defBlock parentFixtures body }


	fun reconstructTyCase { ptrn, body } =
	    let 
		val (b0, newPtrn) = 
		    case ptrn of NONE => 
				 ([], NONE)
			       | SOME b => 
				 inr (SOME) (defVar parentFixtures b)
		val f0 = newFrame parentFixtures b0
	    in
		{ ptrn = newPtrn,
		  body = defBlock (f0 :: parentFixtures) body }
	    end	    
    in
	case stmt of
	    Ast.EmptyStmt => 
	    Ast.EmptyStmt

	  | Ast.ExprStmt es => 
	    Ast.ExprStmt (defExprs parentFixtures es)
	    
	  | Ast.ForEachStmt fe => 
	    Ast.ForEachStmt (reconstructForEnumStmt fe)
	    
	  | Ast.ForInStmt fe => 
	    Ast.ForInStmt (reconstructForEnumStmt fe)
	    
	  | Ast.ThrowStmt es => 
	    Ast.ThrowStmt (defExprs parentFixtures es)
	    
	  | Ast.ReturnStmt es => 
	    Ast.ReturnStmt (defExprs parentFixtures es)
	    
	  | Ast.BreakStmt i => 
	    Ast.BreakStmt i

	  | Ast.ContinueStmt i => 
	    Ast.ContinueStmt i
				  
	  | Ast.BlockStmt b =>
	    Ast.BlockStmt (defBlock parentFixtures b)
	    
	  | Ast.LabeledStmt (id, s) => 
	    Ast.LabeledStmt (id, (defStmt parentFixtures s))
	    
	  | Ast.LetStmt (vbs, stmt) => 
	    let
		val (b0, newVbs) = defVars parentFixtures vbs
		val f0 = newFrame parentFixtures b0
		val newStmt = defStmt (f0 :: parentFixtures) stmt
	    in
		Ast.LetStmt (newVbs, newStmt)
	    end
	    
	  | Ast.SuperStmt es => 
	    Ast.SuperStmt (defExprs parentFixtures es)
	    
	  | Ast.WhileStmt w => 
	    Ast.WhileStmt (reconstructWhileStmt w)
	    
	  | Ast.DoWhileStmt w => 
	    Ast.DoWhileStmt (reconstructWhileStmt w)
	    
	  | Ast.ForStmt f => 
	    reconstructForStmt f
	    
	  | Ast.IfStmt { cnd, thn, els } => 
	    Ast.IfStmt { cnd = defExpr parentFixtures cnd,
			 thn = defStmt parentFixtures thn,
			 els = defStmt parentFixtures els }
	    
	  | Ast.WithStmt { obj, ty, body } => 
	    Ast.WithStmt { obj = (defExprs parentFixtures obj),
			   ty = (defTyExpr parentFixtures ty),
			   body = defStmt parentFixtures body }
	    
	  | Ast.TryStmt { body, catches, finally } => 
	    Ast.TryStmt { body = defBlock parentFixtures body,
			  catches = map reconstructCatch catches,
			  finally = case finally of 
					NONE => 
					NONE
				      | SOME b => 
					SOME (defBlock parentFixtures b) }
	    
	  | Ast.SwitchStmt { cond, cases } => 
	    Ast.SwitchStmt { cond = defExprs parentFixtures cond,
			     cases = map reconstructCase cases }
	    
	  | Ast.SwitchTypeStmt { cond, ty, cases } =>
	    Ast.SwitchTypeStmt { cond = defExprs parentFixtures cond,
				 ty = defTyExpr parentFixtures ty,
				 cases = map reconstructTyCase cases }
	    
	  | Ast.Dxns { expr } => 
	    Ast.Dxns { expr = defExpr parentFixtures expr }
    end	    

	    
and defDefn (parentFixtures:Ast.FIXTURES list) 
	    (defn:Ast.DEFN) 
    : (Ast.FIXTURE_BINDINGS * Ast.DEFN) = 
    case defn of 
	Ast.VariableDefn vbs => 
	inr (Ast.VariableDefn) (defVars parentFixtures vbs)

      | Ast.FunctionDefn fd => 
	inr (Ast.FunctionDefn) (defFunc parentFixtures fd)

      | Ast.NamespaceDefn { attrs=(Ast.Attributes {ns, ...}), ident, init } => 
	let
	    val qualNs = resolveExprToNamespace parentFixtures ns
	    val newNs = Ast.UserDefined ident
	    val fixtureName = { ns = qualNs, id = ident } 
	in
	    ([(fixtureName, Ast.NamespaceFixture newNs)], defn)
	end

      | _ => ([], defn)


and defDefns (parentFixtures:Ast.FIXTURES list) 
	     (defns:Ast.DEFN list) 
    : (Ast.FIXTURE_BINDINGS * (Ast.DEFN list)) = 
    let
	val (fbl, newDefns) = ListPair.unzip (map (defDefn parentFixtures) defns)
    in
	(List.concat fbl, newDefns)
    end


and defBlock (parentFixtures:Ast.FIXTURES list) (block:Ast.BLOCK) : Ast.BLOCK = 
    case block of 
	Ast.Block { pragmas, defns, stmts, ... } => 
	let 
	    val b0 = List.concat (List.map (defPragma parentFixtures) pragmas)
	    val f0 = newFrame parentFixtures b0
	    val (b1, newDefns) = defDefns (f0 :: parentFixtures) defns
	    val f1 = newFrame (f0 :: parentFixtures) (b1 @ b0)
	    val newStmts = map (defStmt (f1 :: parentFixtures)) stmts
	in
	    Ast.Block { pragmas=pragmas,
			defns=newDefns,
			stmts=newStmts,
			fixtures=SOME f1 }
	end

	
and defTopBlock (parentFixtures:Ast.FIXTURES list) (b:Ast.BLOCK) : Ast.BLOCK =
    (* This is a special case of block: we permit "top-blocks" to contain class definitions,
     * that support forward-declaration and must resolve to a proper
     * inheritence tree. *)
    
    (* FIXME: actually implement class resolution! *)
	defBlock parentFixtures b

    
and defPackage (parentFixtures:Ast.FIXTURES list) (package:Ast.PACKAGE) : Ast.PACKAGE =
    { name = (#name package),
      body = defTopBlock parentFixtures (#body package) }

    
and defProgram (prog:Ast.PROGRAM) : Ast.PROGRAM = 
    let 
	fun mkNamespaceFixtureBinding pkg = 
	    case (pkg:Ast.PACKAGE) of 
		{name, ...} => ({ns=Ast.Internal "", id=name}, 
				Ast.NamespaceFixture (Ast.Public name))
	val topFixtures = 
	    [Ast.Fixtures 
		 { tag = Ast.GlobalFixtures,
		   bindings = map mkNamespaceFixtureBinding (#packages prog),
		   isExtensible = false,
		   openNamespaces = [Ast.Internal ""],
		   numberType = Ast.Number,
		   roundingMode = Ast.HalfEven }]
    in
	{ packages = map (defPackage topFixtures) (#packages prog),
	  body = defTopBlock topFixtures (#body prog) }
	
    end
end
