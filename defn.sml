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

and defClass (fixs:Ast.FIXTURES option) (cdef:Ast.CLASS_DEFN) : Ast.FIXTURE_BINDINGS = 
    []

and defVars (fixs:Ast.FIXTURES option) (vars:Ast.VAR_BINDING list) : Ast.FIXTURE_BINDINGS = 
    case fixs of 
	NONE => []
      | SOME fxs => []


and defFunc (fixs:Ast.FIXTURES option) (f:Ast.FUNC_DEFN) : Ast.FIXTURE_BINDINGS = 
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

and defPragma (cls:Ast.PRAGMA) : Ast.FIXTURE_BINDINGS = 
    []

and defStmt (fxs:Ast.FIXTURES option) (cls:Ast.STMT) : Ast.FIXTURE_BINDINGS = 
    []

and defDefn (fxs:Ast.FIXTURES option) (defn:Ast.DEFN) : Ast.FIXTURE_BINDINGS = 
    case defn of 
	Ast.ClassDefn cd => defClass fxs cd
      | Ast.VariableDefn vbs => defVars fxs vbs
      | Ast.FunctionDefn fd => defFunc fxs fd
      | _ => []

and defBlock (fixs:Ast.FIXTURES option) (block:Ast.BLOCK) : Ast.BLOCK = 
    case block of 
	Ast.Block { pragmas, defns, stmts, ... } => 
	let 
	    val fixtures0 = Ast.Fixtures 
				{ tag = Ast.BlockFixtures,
				  parent = NONE,
				  bindings = ((List.concat (List.map defPragma pragmas)) @ 
					      (List.concat (List.map (defDefn NONE) defns)) @ 
					      (List.concat (List.map (defStmt NONE) stmts))),
				  isExtensible = false }
			    
	    val fixtures1 = Ast.Fixtures 
				{ tag = Ast.BlockFixtures,
				  parent = NONE,
				  bindings = ((List.concat (List.map defPragma pragmas)) @ 
					      (List.concat (List.map (defDefn (SOME fixtures0)) defns)) @ 
					      (List.concat (List.map (defStmt (SOME fixtures0)) stmts))),
				  isExtensible = false }
	in
	    Ast.Block { pragmas=pragmas,
			defns=defns,
			stmts=stmts,
			fixtures=SOME fixtures1 }
	end

and defPackage (package:Ast.PACKAGE) : Ast.PACKAGE =     
    { name = (#name package),
      body = defBlock NONE (#body package) }
    
and defProgram (prog:Ast.PROGRAM) : Ast.PROGRAM = 
    { packages = map defPackage (#packages prog),
      body = defBlock NONE (#body prog) }

end
