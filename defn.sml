structure Defn = struct

(* 
 * The goal of the definition phase is to put together the fixtures
 * of the program, as well as insert class, function and interface
 * objects into the global object.
 *)

fun addNominalTypeObjects _ : unit = 
    ()

fun defBlock (fix:Mach.FIXTURES) (block:Ast.BLOCK) : unit = 
    ()

and defPackage (fix:Mach.FIXTURES) (package:Ast.PACKAGE) : unit = 
    defBlock fix (#body package)

and defProgram (prog:Ast.PROGRAM) : unit = 
    (List.app (defPackage Mach.globalFixtures) (#packages prog);
     defBlock Mach.globalFixtures (#body prog);
     addNominalTypeObjects ();
     Mach.addFixturesToObject Mach.globalFixtures Mach.globalObject)

end
