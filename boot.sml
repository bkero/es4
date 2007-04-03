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
		   | Mach.VirtualValProp _ => "[virtual val]"
           | Mach.MethodProp _ => "[method]"
           | Mach.NativeFunctionProp _ => "[native function]"
           | Mach.NamespaceProp _ => "[namespace]"
    in
	trace [LogErr.name n, " -> ", ps]
    end
	
fun printFixture ((n:Ast.FIXTURE_NAME), (f:Ast.FIXTURE)) = 
    let
	val fs = case f of 
		     Ast.NamespaceFixture _ => "[namespace]"
		   | Ast.ClassFixture _ => "[class]"
		   | Ast.InterfaceFixture => "[interface]"
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
	  case Eval.getGlobalObject () of 
	      Mach.Obj {props, ...} => 
	      List.app printProp (!props);
	  trace ["top fixture contents:"];
	  List.app printFixture (!Defn.topFixtures))    
     else ()
          
    
fun boot _ = 
    let
        val _ = trace ["resetting top fixtures"];
        val _ = Defn.resetTopFixtures ()

        val _ = trace ["allocating global object"];
        val globalObj = Mach.newObj (Mach.ClassTag Name.public_Object) Mach.Null NONE
        val _ = trace ["installing global object"];
        val _ = Eval.resetGlobal globalObj
        val globalScope = Eval.getGlobalScope ()

        (* 
         * We have to do a small bit of delicate work here because the global object
         * needs to get installed as the root scope *inbetween* the moment of its
         * allocation and the execution of its (class Object) constructor body. 
         * 
         * There is no provision for this in the standard object-construction 
         * protocol Eval.constructClassInstance, so we inline it here. 
         * 
         * There are also a few other "root" classes that require special processing
         * during startup to avoid feedback loops in their definition.
         *)

        fun loadRootClass (name:Ast.IDENT) 
          =
          let 
              val fullName = Name.public name
              val _ = trace ["loading fundamental ", name, " class from builtin/", name ,".es"];
              val prog = Defn.defProgram (Parser.parseFile ("builtins/" ^ name ^ ".es"))
              val _ = trace ["fetching ", LogErr.name fullName, " class definition"];
              val fix = Defn.getFixture (valOf (#fixtures prog)) (Ast.PropName fullName)
              val cls = case fix of 
                            Ast.ClassFixture cls => cls
                          | _ => error [LogErr.name fullName, " did not resolve to a class fixture"]
              val _ = trace ["allocating class ", LogErr.name fullName];
              val closure = Eval.newClsClosure globalScope cls
              val obj = Mach.newObj (Mach.ClassTag Name.public_Class) Mach.Null (SOME (Mach.Class closure))
              val _ = trace ["binding class ", LogErr.name fullName];
              val _ = Eval.setValue globalObj fullName (Mach.Object obj)
          in
              (cls, closure, obj)
          end           

        val nonBootTopFixtures = !Defn.topFixtures
        val (objClass, objClassClosure, objClassObj) = loadRootClass "Object"

        val _ = trace ["running Object constructor on global object"];
        val Ast.Cls { instanceFixtures, ...} = objClass
        val objClassScope = Eval.extendScope globalScope objClassObj false
        val _ = Eval.allocObjFixtures objClassScope globalObj instanceFixtures
        val _ = Eval.initializeAndConstruct objClassClosure objClassObj objClassScope [] globalObj

        val _ = loadRootClass "Namespace"
        val _ = loadRootClass "Class"
        val _ = loadRootClass "Function"

        val _ = Eval.allocScopeFixtures (Eval.getGlobalScope()) nonBootTopFixtures
    in
        Native.registerNatives ();
        loadFiles 
            [
             "builtins/Magic.es",
             "builtins/Conversions.es",

             "builtins/String.es",
             "builtins/string_primitive.es",
             
             "builtins/Boolean.es",
             "builtins/boolean_primitive.es",
             
             "builtins/Number.es",
             "builtins/double.es",
             "builtins/int.es",
             "builtins/uint.es",
             "builtins/decimal.es",
             "builtins/Numeric.es",
             
             "builtins/Global.es",
             
             "builtins/ByteArray.es",
             "builtins/Date.es",
             
             "builtins/JSON.es",
             "builtins/Array.es",

             "builtins/Error.es"             
            ];
         
        (*
         "builtins/Math.es",
         "builtins/Unicode.es",
         "builtins/RegExpCompiler.es",
         "builtins/RegExpEvaluator.es",
         "builtins/RegExp.es",
         *)
        describeGlobal ()
    end
end

