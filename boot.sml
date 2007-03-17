(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* The ES4 "boot environment". *)
structure Boot = struct 

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[boot] " :: ss) else ()
fun error ss = LogErr.hostError ss

fun load f = 
    (trace ["loading boot file ", f];
     Eval.evalProgram (Defn.defProgram (Parser.parseFile f)))

fun printProp ((n:Ast.NAME), (p:Mach.PROP)) = 
    let 
	val ps = case (#state p) of 
		     Mach.TypeVarProp => "[typeVar]"
		   | Mach.TypeProp => "[type]"
		   | Mach.UninitProp => "[uninit]"
		   | Mach.ValProp _ => "[val]"
		   | Mach.VirtualValProp _ => "[virtualProp]"
    in
	trace [LogErr.name n, " -> ", ps]
    end
	
fun printFixture ((n:Ast.FIXTURE_NAME), (f:Ast.FIXTURE)) = 
    let
	val fs = case f of 
		     Ast.NamespaceFixture _ => "[namespace]"
		   | Ast.ClassFixture _ => "[class]"
		   | Ast.TypeVarFixture => "[typeVar]"
		   | Ast.TypeFixture _ => "[type]"
		   | Ast.MethodFixture _ => "[method]"
		   | Ast.ValFixture _ => "[val]"
		   | Ast.VirtualValFixture _ => "[virtualVal]"
    in
	case n of
	    Ast.TempName n => trace ["temp #", Int.toString n, " -> ", fs]
          | Ast.PropName n => trace [LogErr.name n, " -> ", fs]
    end

fun describeGlobal _ = 
     if !doTrace
     then 
	 (trace ["global object contents:"];
	  case Mach.globalObject of 
	      Mach.Obj {props, ...} => 
	      List.app printProp (!props);
	  trace ["top fixture contents:"];
	  List.app printFixture (!Defn.topFixtures))    
     else ()
    
    
fun boot _ = 
    (Defn.resetTopFixtures ();
     Mach.resetGlobalObject ();
     Native.registerNatives ();
     load "builtins/Object.es";
     load "builtins/Global.es";
     load "builtins/Error.es";
     load "builtins/Conversions.es";
     load "builtins/Function.es";

     load "builtins/Boolean.es";
(*
     load "builtins/uint.es";
     load "builtins/int.es";
     load "builtins/double.es";
     load "builtins/decimal.es";
     load "builtins/Number.es";
     load "builtins/Numeric.es";
     load "builtins/Math.es";

     load "builtins/string_primitive.es";
     load "builtins/String.es";
     load "builtins/Unicode.es";

     load "builtins/Array.es";
     load "builtins/ByteArray.es";
     load "builtins/Function.es";
     load "builtins/Date.es";
     load "builtins/JSON.es";

     load "builtins/RegExpCompiler.es";
     load "builtins/RegExpEvaluator.es";
     load "builtins/RegExp.es";
*)
     describeGlobal ())
end
