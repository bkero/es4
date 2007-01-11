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

fun newFixtures (parentFixtures:Ast.FIXTURES list) 
		(newBindings:Ast.FIXTURE_BINDINGS) 
    : Ast.FIXTURES = 
    case parentFixtures of 
	[] => Ast.Fixtures { bindings = newBindings,
			     openNamespaces = [Ast.Internal ""],
			     numberType = Ast.Number,
			     roundingMode = Ast.HalfEven } 
      | (Ast.Fixtures { numberType, roundingMode, openNamespaces, ... } :: _) =>
	Ast.Fixtures { bindings = newBindings,
		       openNamespaces = openNamespaces, 
		       numberType = numberType,
		       roundingMode = roundingMode } 


type classBlockAnalysis = 
     { instanceVars: Ast.VAR_BINDING list,
       instanceMethods: Ast.FUNC list,
       vars: Ast.VAR_BINDING list,
       methods: Ast.FUNC list,
       constructor: Ast.FUNC option,
       initializer: Ast.STMT list,
       ifixtures: Ast.FIXTURES, 
       fixtures: Ast.FIXTURES }

fun analyzeClassBlock 
	(parentFixtures:Ast.FIXTURES list) 
	(n:Ast.NAME) 
	(b:Ast.BLOCK) 
    : classBlockAnalysis = 
    case b of 
	Ast.Block { pragmas, defns, stmts, ... } => 
	let
	    fun legalDefn (d:Ast.DEFN) : Ast.DEFN = 
		case d of 
		    Ast.ClassDefn _ => LogErr.defnError ["illegal nested class definition"]
		  | Ast.InterfaceDefn _ => LogErr.defnError ["illegal interface definition in class definition"]
		  | Ast.TypeDefn _ => LogErr.defnError ["illegal type definition in class definition"]
		  | x => x

	    fun partitionVarBindings (vbs:Ast.VAR_BINDING list) 
		: ((Ast.VAR_BINDING list) * (Ast.VAR_BINDING list)) = 
		List.partition 
		    (fn Ast.Binding { attrs = Ast.Attributes { static, ... }, ... } => static)
		vbs

	    fun classifyDefn (d:Ast.DEFN) : ((Ast.DEFN option) * (Ast.DEFN option)) = 
		case (legalDefn d) of 
		    Ast.VariableDefn vbs => 
		    let 
			fun proj x = case x of [] => NONE | y => SOME (Ast.VariableDefn y)
			val (svbs, ivbs) = partitionVarBindings vbs
		    in
			(proj svbs, proj ivbs)
		    end

		  | Ast.NamespaceDefn nd => (SOME (Ast.NamespaceDefn nd), NONE)
		  | Ast.FunctionDefn fd => 
		    (case fd of 
			 { attrs = Ast.Attributes { static, ... }, ... } => 
			 if static 
			 then (SOME (Ast.FunctionDefn fd), NONE)
			 else (NONE, SOME (Ast.FunctionDefn fd)))

		  | _ => LogErr.defnError ["unexpected definition"]

	    fun classifyDefns (ds:Ast.DEFN list) : ((Ast.DEFN list) * (Ast.DEFN list)) = 
		let 
		    val (staticOpts, instanceOpts) = ListPair.unzip (map classifyDefn ds)
		    val lower = (map valOf) o (List.filter isSome)
		in
		    (lower staticOpts, lower instanceOpts)
		end

	    fun isCtor (d:Ast.DEFN) : bool = 
		case d of 
		    (* FIXME: this might be an incorrect algorithm for
		     * determining ctor-ness *)
		    Ast.FunctionDefn { attrs = Ast.Attributes { ns, ... }, 
				       func = Ast.Func { name = { ident, ... }, ... },
				       ... } => 
		    let 
			val fname = { id = ident, 
				      ns = resolveExprToNamespace parentFixtures ns }
		    in
			fname = n
		    end
		  | _ => false

	    fun getFunc (d:Ast.DEFN) : (Ast.FUNC option) = 
		case d of 
		    Ast.FunctionDefn { func, ... } => SOME func
		  | _ => NONE

	    fun getVarBindings (d:Ast.DEFN) : Ast.VAR_BINDING list = 
		case d of 
		    Ast.VariableDefn vbs => vbs
		  | _ => []

	    val (staticDefns, instanceDefns) = classifyDefns defns

	    val b0 = List.concat (List.map (defPragma parentFixtures) pragmas)
	    val f0 = newFixtures parentFixtures b0

	    val (b1, newStaticDefns) = defDefns (f0 :: parentFixtures) staticDefns
	    val f1 = newFixtures (f0 :: parentFixtures) b1

	    val newStmts = map (defStmt (f1 :: parentFixtures)) stmts

	    val (b2, newInstanceDefns) = defDefns (f1 :: f0 :: parentFixtures) instanceDefns
	    val f2 = newFixtures (f1 :: f0 :: parentFixtures) b2

	    val (ctorDefns, nonCtorInstanceDefns) = List.partition isCtor newInstanceDefns
	    val ctorDefn = case ctorDefns of 
			       [Ast.FunctionDefn { func, ... }] => SOME func
			     | [] => NONE
			     | _ => LogErr.defnError ["illegal constructor definition(s)"]
	in 
	    { instanceVars = List.concat (List.map getVarBindings nonCtorInstanceDefns),
	      instanceMethods = List.mapPartial getFunc nonCtorInstanceDefns,
	      vars = List.concat (List.map getVarBindings newStaticDefns),
	      methods = List.mapPartial getFunc newStaticDefns,
	      constructor = ctorDefn,
	      initializer = newStmts,	       
	      fixtures = f1,
	      ifixtures = f2 }
    end

and mergeClasses (base:Ast.CLASS_DEFN) (curr:Ast.CLASS_DEFN) : Ast.CLASS_DEFN = 
    let 
	fun mergeFixtures a b = 
	    case (a, b) of 
		(SOME (Ast.Fixtures fa), (SOME (Ast.Fixtures fb))) => 
		SOME (Ast.Fixtures { bindings = (#bindings fa) @ (#bindings fb),
				     openNamespaces = (#openNamespaces fb),
				     numberType = (#numberType fb),
				     roundingMode = (#roundingMode fb) })
	      | (SOME x, NONE) => SOME x
	      | (NONE, SOME x) => SOME x
	      | (NONE, NONE) => NONE
    in
	{ name = (#name curr),
	  nonnullable = (#nonnullable curr),
	  attrs = (#attrs curr),
	  params = (#params curr),
	  extends = (#extends curr),
	  implements = (#implements curr) @ (#implements base),
	  classFixtures = (#classFixtures curr),
	  instanceFixtures = mergeFixtures (#instanceFixtures base) (#instanceFixtures curr),
	  body = (#body curr),
	  instanceVars = (#instanceVars curr) @ (#instanceVars base),
	  instanceMethods = (#instanceMethods curr) @ (#instanceMethods base),
	  vars = (#vars curr),
	  methods = (#methods curr),
	  constructor = (#constructor curr),
	  initializer = (#initializer curr) }
    end
      

and resolveOneClass (parentFixtures:Ast.FIXTURES list)
		    (unresolved:Ast.CLASS_DEFN list)
		    (resolved:(Ast.CLASS_DEFN list) ref)    
		    (children:Ast.NAME list)
		    (curr:Ast.CLASS_DEFN) : (Ast.NAME * Ast.CLASS_DEFN) = 
    let 
	fun qualName (cd:Ast.CLASS_DEFN) = { ns = Ast.Internal "", id = (#name cd) }
	val currName = qualName curr
	fun seenAsChild (n:Ast.NAME) = List.exists (fn ch => ch = n) children
	fun findResolved (n:Ast.NAME) = List.find (fn c => (qualName c) = n) (!resolved)
	fun isFinal (cd:Ast.CLASS_DEFN) : bool =
	    case (#attrs cd) of 
		Ast.Attributes { final, ... } => final
	fun findBaseClassDef (n:Ast.NAME) (cds:Ast.CLASS_DEFN list) = 
	    case cds of 
		[] => LogErr.defnError ["unable to find class definition ", LogErr.name n]
	      | x::xs => if n = (qualName x) 
			 then (if isFinal x
			       then LogErr.defnError ["attempting to extend final class ", LogErr.name n]
			       else x)
			 else findBaseClassDef n xs
	fun identExprToName ie = 
	    case ie of 
		Ast.Identifier {ident, ...} => {ns = Ast.Internal "", 
						id = ident}
	      | _ => LogErr.defnError ["unhandled form of identifier expresison in class defn"]
    in
	case findResolved currName of 
	    SOME existingDefn => (currName, existingDefn)
	  | NONE => 
	    let 
		val _ = LogErr.trace ["analyzing class block for ", LogErr.name currName]
		val cba = analyzeClassBlock parentFixtures currName (#body curr)
		val analyzedCurrClassDef = 
		    { name = (#name curr),
		      nonnullable = (#nonnullable curr),
		      attrs = (#attrs curr),
		      params = (#params curr),
		      extends = (#extends curr),
		      implements = (#implements curr),
		      
		      classFixtures = SOME (#fixtures cba),
		      instanceFixtures = SOME (#ifixtures cba),
		      body = (#body curr),
		      
		      instanceVars = (#instanceVars cba),
		      instanceMethods = (#instanceMethods cba),
		      vars = (#vars cba),
		      methods = (#methods cba),
		      constructor = (#constructor cba),
		      initializer = (#initializer cba) }
	    in
		case (#extends curr) of 
		    SOME baseIdentExpr => 
		    let 
			val baseName = identExprToName baseIdentExpr
			val unresolvedBaseClassDef = 
			    if seenAsChild baseName
			    then LogErr.defnError ["cyclical class inheritence detected at ", LogErr.name baseName]
			    else findBaseClassDef baseName unresolved 
			val (_, resolvedBaseClassDef) = 
			    resolveOneClass 
				parentFixtures 
				unresolved 
				resolved
				(currName :: children) 
				unresolvedBaseClassDef
		    in
			(currName, mergeClasses resolvedBaseClassDef analyzedCurrClassDef)
		    end
		  | NONE => ((qualName curr), analyzedCurrClassDef)
	    end
    end


and defVar (parentFixtures:Ast.FIXTURES list) 
	    (var:Ast.VAR_BINDING) 
    : (Ast.FIXTURE_BINDINGS * Ast.VAR_BINDING) = 
    case var of 
	Ast.Binding { kind, init, attrs, pattern, ty } => 
	let
	    val newInit = case init of 
			      NONE => NONE
			    | SOME e => SOME (defExpr parentFixtures e)
	    val newTy = case ty of 
			    NONE => NONE
			  | SOME t => SOME (defTyExpr parentFixtures t)
	    val fixtureTy = case newTy of
				NONE => Ast.SpecialType Ast.Any
			      | SOME t => t
	    val newPattern = defPattern parentFixtures pattern
	    val isReadOnly = case kind of 
				 Ast.Const => true
			       | Ast.LetConst => true
			       | _ => false		
	    val fixtureBindings = 
		case newPattern of 
		    Ast.IdentifierPattern (Ast.Identifier { ident, ... }) => 
		    [({ns=Ast.Internal "", id=ident}, Ast.ValFixture { ty = fixtureTy, 
								       readOnly = isReadOnly,
								       isOverride = false })]
		  (* FIXME: do other pattern forms introduce fixtures? *)
		  | _ => []
	in
	    (fixtureBindings, 
	     Ast.Binding { kind = kind,
			   init = newInit,
			   attrs = attrs,
			   pattern = newPattern,
			   ty = newTy })
	end


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
	    val boundTypeFixtures = newFixtures parentFixtures typeParamBindings
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
	    val funcFixtures = newFixtures parentFixtures innerBindings
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
    let 
	val openNamespaces = case parentFixtures of 
				 [] => []
			       | (Ast.Fixtures { openNamespaces, ... }) :: _ => openNamespaces
    in
	case ie of 
	    Ast.Identifier { ident, ... } => 
	    Ast.Identifier { ident=ident, openNamespaces=openNamespaces } 

	  | Ast.AttributeIdentifier ai => 
	    Ast.AttributeIdentifier (defIdentExpr parentFixtures ai)

	  | Ast.TypeIdentifier {ident, typeParams} => 
	    Ast.TypeIdentifier {ident=(defIdentExpr parentFixtures ident), typeParams=typeParams}

	  | _ => ie
    end

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
		val f0 = newFixtures parentFixtures b0
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
		val funcFixtures = newFixtures parentFixtures innerBindings
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
		val newPattern = defPattern parentFixtures p
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
    : Ast.PATTERN = 

    case pat of 
	Ast.ObjectPattern fields => 
	Ast.ObjectPattern (map (fn { name, ptrn } => 
				   { name = defIdentExpr parentFixtures name, 
				     ptrn = defPattern parentFixtures ptrn }) fields)

      | Ast.ArrayPattern ptrns => 
	Ast.ArrayPattern (map (defPattern parentFixtures) ptrns)

      | Ast.SimplePattern e => 
	Ast.SimplePattern (defExpr parentFixtures e)

      | Ast.IdentifierPattern ie => 
	Ast.IdentifierPattern (defIdentExpr parentFixtures ie)

and defStmt (parentFixtures:Ast.FIXTURES list) (stmt:Ast.STMT) : (Ast.STMT) = 
    let
	fun reconstructForEnumStmt (fe:Ast.FOR_ENUM_STMT) = 
	    case fe of 
		{ ptrn, obj, defns, contLabel, body } => 
		let
		    val newPtrn = 
			case ptrn of 
			    NONE => NONE
			  | SOME p => SOME (defPattern parentFixtures p)
		    val newObj =  defExprs parentFixtures obj
		    val (b0, newDefns) = defVars parentFixtures defns
		    val f0 = newFixtures parentFixtures b0
		    val newBody = defStmt (f0 :: parentFixtures) body
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
		val f0 = newFixtures parentFixtures b0
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
		val f0 = newFixtures parentFixtures b0
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
		val f0 = newFixtures parentFixtures b0
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
		val f0 = newFixtures parentFixtures b0
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

and defNamespace (parentFixtures:Ast.FIXTURES list) (nd:Ast.NAMESPACE_DEFN) : 
    (Ast.FIXTURE_BINDINGS * Ast.NAMESPACE_DEFN) = 
    case nd of 
	{ attrs=(Ast.Attributes {ns, ...}), ident, init } => 
	let
	    val qualNs = resolveExprToNamespace parentFixtures ns
	    val newNs = Ast.UserDefined ident
	    val fixtureName = { ns = qualNs, id = ident } 
	in
	    ([(fixtureName, Ast.NamespaceFixture newNs)], nd)
	end

	    
and defDefn (parentFixtures:Ast.FIXTURES list) 
	    (defn:Ast.DEFN) 
    : (Ast.FIXTURE_BINDINGS * Ast.DEFN) = 
    case defn of 
	Ast.VariableDefn vbs => 
	inr (Ast.VariableDefn) (defVars parentFixtures vbs)

      | Ast.FunctionDefn fd => 
	inr (Ast.FunctionDefn) (defFunc parentFixtures fd)

      | Ast.NamespaceDefn nd => 
	inr (Ast.NamespaceDefn) (defNamespace parentFixtures nd)

      | Ast.ClassDefn cd => 
	LogErr.defnError ["class definition at non-top block"]

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
	    val f0 = newFixtures parentFixtures b0
	    val (b1, newDefns) = defDefns (f0 :: parentFixtures) defns
	    val f1 = newFixtures (f0 :: parentFixtures) b1
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

    case b of 
	Ast.Block { pragmas, defns, stmts, ... } => 
	let 
	    val pragmaBindings = List.concat (List.map (defPragma parentFixtures) pragmas)
	    val pragmaFixtures = newFixtures parentFixtures pragmaBindings
	    val f0 = pragmaFixtures :: parentFixtures

	    fun isClassDefn d = case d of Ast.ClassDefn _ => true | _ => false
	    fun getClassDefn d = case d of Ast.ClassDefn c => SOME c | _ => NONE
	    val (classDefns, otherDefns) = List.partition isClassDefn defns
	    val tmp:(Ast.CLASS_DEFN list) ref = ref []
	    val rawClassDefns = List.mapPartial getClassDefn defns
	    val resolve = resolveOneClass parentFixtures rawClassDefns tmp []
	    val newRawClassDefns = map resolve rawClassDefns
	    val newClassDefns = map (fn (_, cd) => Ast.ClassDefn cd) newRawClassDefns
	    val classBindings = map (inr (Ast.ClassFixture)) newRawClassDefns
	    val classFixtures = newFixtures parentFixtures classBindings
	    val f1 = classFixtures :: f0

	    val (defnBindings, newOtherDefns) = defDefns f0 otherDefns
	    val defnFixtures = newFixtures f0 (classBindings @ defnBindings)
	    val f2 = defnFixtures :: f0

	    val newStmts = map (defStmt f2) stmts
	in
	    Ast.Block { pragmas = pragmas,
			defns = (newClassDefns @ newOtherDefns),
			stmts = newStmts,
			fixtures = SOME defnFixtures }
	end

    
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
		 { bindings = map mkNamespaceFixtureBinding (#packages prog),
		   openNamespaces = [Ast.Internal ""],
		   numberType = Ast.Number,
		   roundingMode = Ast.HalfEven }]
    in
	{ packages = map (defPackage topFixtures) (#packages prog),
	  body = defTopBlock topFixtures (#body prog) }
	
    end
end
