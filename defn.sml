structure Defn = struct

(* 
 * The goal of the definition phase is to put together the fixtures
 * of the program, as well as insert class, function and interface
 * objects into the global object.
 *)


fun resolveFixture (fixs:Mach.FIXTURES) (mname:Mach.MULTINAME) : Ast.FIXTURE =
    case fixs of 
	Ast.Fixtures { parent, fixtures, ... } => 
	let     
	    val id = (#id mname)
	    fun tryName [] = NONE
	      | tryName (x::xs) = 
		let 
		    val n = {ns=x, id=id} 
		in
		    if Mach.hasFixture fixtures n
		    then SOME n
		    else tryName xs
		end
	in
	    case tryName (#nss mname) of 
		SOME n => Mach.getFixture fixtures n 
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


fun addNominalTypeObjects _ : unit = 
    ()

and defClass (fixs:Mach.FIXTURES) (cdef:Ast.CLASS_DEFN) : unit = 
    ()

and defVars (fixs:Mach.FIXTURES) (vars:Ast.VAR_BINDING list) : unit = 
    ()

and defFunc (fixs:Mach.FIXTURES) (f:Ast.FUNC_DEFN) : unit = 
    LogErr.unimplError ["function definitions temporarily disabled "]
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

and defInterface (fixs:Mach.FIXTURES) (idef:Ast.INTERFACE_DEFN) : unit = 
    ()

and defDefn (fixs:Mach.FIXTURES) (defn:Ast.DEFN) : unit = 
    case defn of 
	Ast.ClassDefn cd => defClass fixs cd
      | Ast.VariableDefn vbs => defVars fixs vbs
      | Ast.FunctionDefn fd => defFunc fixs fd
      | Ast.InterfaceDefn id => defInterface fixs id
      | _ => LogErr.unimplError ["unhandled definition form"]

and defBlock (fixs:Mach.FIXTURES) (block:Ast.BLOCK) : unit = 
    case block of 
	Ast.Block { defns, ... } => List.app (defDefn fixs) defns
	

and defPackage (fixs:Mach.FIXTURES) (package:Ast.PACKAGE) : unit = 
    defBlock fixs (#body package)

and defProgram (prog:Ast.PROGRAM) : unit = 
    (List.app (defPackage Mach.globalFixtures) (#packages prog);
     defBlock Mach.globalFixtures (#body prog);
     addNominalTypeObjects ();
     Mach.addFixturesToObject Mach.globalFixtures Mach.globalObject)

end
