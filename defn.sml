structure Defn = struct

(* 
 * The goal of the definition phase is to put together the fixtures
 * of the program, as well as insert class, function and interface
 * objects into the global object.
 *)

fun inr f (a, b) = (a, f b)

(* fun inr (f:'b->'c) (x:'a, y:'b) : ('a * 'c) = (x, f y) *)

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


fun newFrame (bs:Ast.FIXTURE_BINDINGS) : Ast.FIXTURES = 
    Ast.Fixtures { tag = Ast.FrameFixtures,
		   bindings = bs,
		   isExtensible = false } 


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

and defFunc (parentFixtures:Ast.FIXTURES list) 
	    (f:Ast.FUNC_DEFN) 
    : (Ast.FIXTURE_BINDINGS * Ast.FUNC_DEFN) = 
    (* FIXME *)
    ([], f)
    (* LogErr.unimplError ["function definitions temporarily disabled "] *)
(*     case fixs of 
	Mach.Fixtures { fixtures, ... } =>
	let
	    val func = (#func f)
	    val fsig = case func of Ast.Func { fsig, ...} => fsig
	    val body = case func of Ast.Func { body, ...} => body
	    val name = case func of Ast.Func { name={ident, ...}, ...} => ident
	    val funcAttrs = getAttrs (#attrs f)
	    val subFixtures = Mach.Fixtures { tag = Mach.FunctionFixtures,
					      parent = fixtures, 
					      fixtures = newFixtureBindings (),
					      isExtensible = false }
	    val funcFixture = Mach.PropFixture { ty = Ast.FunctionType fsig,
						 readOnly = true,
						 isOverride = false,
						 subFixtures = SOME subFixtures } 
	    val ns = resolveExprToNamespace fixs (#ns funcAttrs)
	    val funcName = {id = name, ns = ns}
	in
	    if Mach.hasBinding fixtures name 
	    then LogErr.defnError ["duplicate fixture: ", name ]
	    else (defBlock subFixtures body;
		  Mach.addBinding fixtures name funcFixture)
    end
*)

and defPragma (parentFixtures:Ast.FIXTURES list) 
	      (pragma:Ast.PRAGMA) 
    : Ast.FIXTURE_BINDINGS = 
    (* FIXME *)
    []

and defExpr (parentFixtures:Ast.FIXTURES list) (expr:Ast.EXPR) : Ast.EXPR = 
    expr

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
		    val f0 = newFrame b0
		    val newObj =  defExprs parentFixtures obj
		    val (b1, newDefns) = defVars (f0 :: parentFixtures) defns
		    val f1 = newFrame (b1 @ b0)
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
		val f0 = newFrame b0
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
		val f0 = newFrame b0
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
		val f0 = newFrame b0
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
		val f0 = newFrame b0
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
	    val fixtureName = {ns = qualNs, id = ident } 
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
	    val f0 = newFrame b0
	    val (b1, newDefns) = defDefns (f0 :: parentFixtures) defns
	    val f1 = newFrame (b1 @ b0)
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
		   isExtensible = false }]
    in
	{ packages = map (defPackage topFixtures) (#packages prog),
	  body = defTopBlock topFixtures (#body prog) }
	
    end
end
