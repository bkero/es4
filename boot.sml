(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* The ES4 "boot environment". *)
structure Boot = struct 

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[boot] " :: ss) else ()
fun error ss = LogErr.hostError ss

fun loadFiles fs = 
    let
        fun parse f = 
            (trace ["parsing boot file ", f]; 
             (f, Parser.parseFile f))
        fun def (f,p) = 
            (trace ["defining boot file ", f]; 
             (f, Defn.defProgram p))
        fun eval (f, p) = 
            (trace ["evaluating boot file ", f]; 
             Eval.evalProgram p)
    in
        map eval (map def (map parse fs))
    end

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

     loadFiles 
         [
          "builtins/Object.es",
          "builtins/Error.es",      
          "builtins/Conversions.es",
          "builtins/Global.es",
          "builtins/Function.es",
          
          "builtins/Boolean.es",
          "builtins/boolean_primitive.es",
          
          "builtins/Number.es",
          "builtins/double.es",
          "builtins/int.es",
          "builtins/uint.es",
          "builtins/decimal.es",
          "builtins/Numeric.es",
          
          "builtins/String.es",
          "builtins/string_primitive.es",
          
          "builtins/ByteArray.es",
          "builtins/Date.es",
          
          "builtins/JSON.es"
         ];
(*
     "builtins/Array.es",
     "builtins/Math.es",
     "builtins/Unicode.es",
     "builtins/RegExpCompiler.es",
     "builtins/RegExpEvaluator.es",
     "builtins/RegExp.es",
*)
     describeGlobal ())
end
