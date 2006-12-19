structure Defn = struct

(* 
 * The goal of the definition phase is to put together the fixtures
 * of the program, as well as insert class, function and interface
 * objects into the global object.
 *)

fun resolveFixture (fixs:Mach.FIXTURES) (mname:Mach.MULTINAME) : Ast.FIXTURE =
    case fixs of 
	Ast.Fixtures { parent, bindings, ... } => 
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
	      | NONE => (case parent of 
			     NONE => LogErr.defnError ["unresolved fixture"]
			   | SOME p => resolveFixture p mname)
	end

fun resolveExprToNamespace (fixs:Mach.FIXTURES) (expr:Ast.EXPR) : Mach.NS = 
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

and defClass (firstPassFixtures:Ast.FIXTURES option) (parentFixtures:Ast.FIXTURES option) (cdef:Ast.CLASS_DEFN) : Ast.FIXTURE_BINDINGS = 
    []

and defVars (parentFixtures:Ast.FIXTURES option) (vars:Ast.VAR_BINDING list) : Ast.FIXTURE_BINDINGS = 
    []


and defFunc (parentFixtures:Ast.FIXTURES option) (f:Ast.FUNC_DEFN) : Ast.FIXTURE_BINDINGS = 
    []
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

and defPragma (firstPassFixtures:Ast.FIXTURES option) (parentFixtures:Ast.FIXTURES option) (pragma:Ast.PRAGMA) : Ast.FIXTURE_BINDINGS = 
    []

and defStmt  (parentFixtures:Ast.FIXTURES option) (cls:Ast.STMT) : Ast.FIXTURE_BINDINGS = 
    []

and defDefn (firstPassFixtures:Ast.FIXTURES option) (parentFixtures:Ast.FIXTURES option) (defn:Ast.DEFN) : Ast.FIXTURE_BINDINGS = 
    case firstPassFixtures of 
	SOME _ => 
	(case defn of 
	     (* Do a partial resolution -- class defns only -- the first time *)
	     Ast.ClassDefn cd => defClass firstPassFixtures parentFixtures cd
	   | _ => [])
      | NONE => 
	(case defn of 
	     (* Do the full resolution the second time *) 
	     Ast.ClassDefn cd => defClass firstPassFixtures parentFixtures cd
	   | Ast.VariableDefn vbs => defVars parentFixtures vbs
	   | Ast.FunctionDefn fd => defFunc parentFixtures fd
	   | _ => [])

and defBlock (firstPassFixtures:Ast.FIXTURES option) (parentFixtures:Ast.FIXTURES option) (block:Ast.BLOCK) : Ast.BLOCK = 
    case block of 
	Ast.Block { pragmas, defns, stmts, ... } => 
	let 
	    val newFixtures = case firstPassFixtures of 
				  SOME f => SOME f
				| NONE => SOME (Ast.Fixtures 
						    { tag = Ast.BlockFixtures,
						      parent = parentFixtures,
						      bindings = [],
						      isExtensible = false })
	    val newBindings = case firstPassFixtures of 
				  NONE => 
				  (* Do a partial resolution -- pragmas and defns only -- the first time *)
				  ((List.concat (List.map (defPragma firstPassFixtures newFixtures) pragmas)) @ 
				   (List.concat (List.map (defDefn firstPassFixtures newFixtures) defns)))
				| SOME _ => 
				  (* Do the full resolution the second time *) 
				  ((List.concat (List.map (defPragma firstPassFixtures newFixtures) pragmas)) @ 
				   (List.concat (List.map (defDefn firstPassFixtures newFixtures) defns)) @ 
				   (List.concat (List.map (defStmt newFixtures) stmts)))
				  
	in
	    Ast.Block { pragmas=pragmas,
			defns=defns,
			stmts=stmts,
			fixtures=SOME (Ast.Fixtures 
					   { tag = Ast.BlockFixtures,
					     parent = parentFixtures,
					     bindings = newBindings,
					     isExtensible = false } ) }
	end
	
and defMutuallyRecursiveTopLevelBlock (parentFixtures:Ast.FIXTURES option)(b:Ast.BLOCK) : Ast.BLOCK =
    let
	val body0 = defBlock NONE parentFixtures b
	val fixtures0 = case body0 of Ast.Block b' => (#fixtures b')
    in	
	defBlock fixtures0 parentFixtures b
    end
    
and defPackage (parentFixtures:Ast.FIXTURES option) (package:Ast.PACKAGE) : Ast.PACKAGE =
    { name = (#name package),
      body = defMutuallyRecursiveTopLevelBlock parentFixtures (#body package) }
    
and defProgram (prog:Ast.PROGRAM) : Ast.PROGRAM = 
    let 
	fun mkNamespaceFixtureBinding pkg = 
	    case (pkg:Ast.PACKAGE) of 
		{name, ...} => ({ns=Ast.Internal "", id=name}, 
				Ast.NamespaceFixture (Ast.Public name))
	val topFixtures = 
	    SOME (Ast.Fixtures 
		      { tag = Ast.GlobalFixtures,
			parent = NONE,
			bindings = map mkNamespaceFixtureBinding (#packages prog),
			isExtensible = false } )
    in
	{ packages = map (defPackage topFixtures) (#packages prog),
	  body = defMutuallyRecursiveTopLevelBlock topFixtures (#body prog) }
	
    end
end
