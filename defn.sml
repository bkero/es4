structure Defn = struct

(* 
 * The goal of the definition phase is to put together the fixtures
 * of the program, as well as insert class, function and interface
 * objects into the global object.
 *)

fun addNominalTypeObjects _ : unit = 
    ()

fun defClass (fixs:Mach.FIXTURES) (cdef:Ast.CLASS_DEFN) : unit = 
    ()

fun defVars (fixs:Mach.FIXTURES) (vars:Ast.VAR_BINDING list) : unit = 
    ()

fun defFunc (fixs:Mach.FIXTURES) (fdef:Ast.FUNC_DEFN) : unit = 
    ()

fun defInterface (fixs:Mach.FIXTURES) (idef:Ast.INTERFACE_DEFN) : unit = 
    ()

fun defDefn (fixs:Mach.FIXTURES) (defn:Ast.DEFN) : unit = 
    case defn of 
	Ast.ClassDefn cd => defClass fixs cd
      | Ast.VariableDefn vbs => defVars fixs vbs
      | Ast.FunctionDefn fd => defFunc fixs fd
      | Ast.InterfaceDefn id => defInterface fixs id
      | _ => LogErr.unimplError ["unhandled definition form"]


fun defBlock (fixs:Mach.FIXTURES) (block:Ast.BLOCK) : unit = 
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
