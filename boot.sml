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
           | Mach.ValListProp _ => "[val list]"
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
        val globalRegs = Eval.getInitialRegs ()

        (* Allocate any standard anonymous user namespaces like magic and meta. *)
        val _ = Eval.allocScopeFixtures (Eval.getInitialRegs()) (!Defn.topFixtures)

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

        fun loadRootClass (fullName:Ast.NAME) 
          =
          let 
              val id = (#id fullName)
              val filename = ("builtins/" ^ (Ustring.toFilename id) ^ ".es")

              val _ = trace ["loading fundamental ", LogErr.name fullName, " class from ", filename];
              val prog = Defn.defProgram (Parser.parseFile filename)

              val _ = trace ["fetching ", LogErr.name fullName, " class definition"];
              val fix = Defn.getFixture (valOf (#fixtures prog)) (Ast.PropName fullName)
              val cls = case fix of 
                            Ast.ClassFixture cls => cls
                          | _ => error [LogErr.name fullName, " did not resolve to a class fixture"]

              val _ = trace ["allocating class ", LogErr.name fullName];
              val closure = Eval.newClsClosure (#scope globalRegs) cls
              val obj = Mach.newObj (Mach.ClassTag Name.intrinsic_Class) Mach.Null (SOME (Mach.Class closure))
              val classRegs = Eval.extendScopeReg globalRegs obj Mach.InstanceScope

              val _ = trace ["allocating class fixtures for ", LogErr.name fullName];
              val Ast.Cls { classFixtures, ... } = cls
              val _ = Eval.allocObjFixtures classRegs obj NONE classFixtures

              val _ = trace ["binding class ", LogErr.name fullName];
              val Mach.Obj { props, ... } = globalObj
              val _ = Mach.addProp props fullName
                                   { ty = Name.typename Name.intrinsic_Class,
                                     state = Mach.ValProp (Mach.Object obj),
                                     attrs = { dontDelete = true,
                                               dontEnum = false,
                                               readOnly = true,
                                               isFixed = true } }
              (* 
               * We return a "residual" program for each root class, to evaluate after 
               * all other classes load, typically because the packages & block of the 
               * root prog involves init statements that hit classes like "string" 
               * or "boolean" that are not yet defined. 
               *)
              val residualProg = { packages = (#packages prog),
                                   fixtures = SOME [],
                                   block = (#block prog) }
          in
              (cls, closure, obj, residualProg)
          end           

        fun completeClassFixtures classObj = 
            let 
              (* 
               * Now the weird / feedbacky part: we go find the class "Class" and allocate
               * its instance fixtures on the object we just built. For non-root classes
               * this happens automatically because they're *instances* of class "Class",
               * but the object we build only *says* it's an instance of class "Class"; it
               * hasn't actually run through any sort of normal construction protocol for
               * class "Class".
               *
               * Note that we do this *after* we bound the object to a position in the 
               * global object, because we want this tying-the-knot trick to work when
               * we're defining class "Class" itself, and we won't be able to find it 
               * by name until just now.
               *)
                val classRegs = Eval.extendScopeReg globalRegs classObj Mach.InstanceScope
                val classClass = Eval.findVal (#scope globalRegs) (Eval.multinameOf Name.intrinsic_Class)
                val Ast.Cls { instanceFixtures, ... } = (#cls (Mach.needClass classClass))
            in
                Eval.allocObjFixtures classRegs classObj (SOME classObj) instanceFixtures
            end

        val (objClass, objClassClosure, objClassObj, residualObjectProg) = loadRootClass Name.public_Object
        val _ = trace ["running Object constructor on global object"];
        val Ast.Cls { instanceFixtures, ...} = objClass
        val objClassRegs = Eval.extendScopeReg globalRegs objClassObj Mach.InstanceScope
        val _ = Eval.allocObjFixtures objClassRegs globalObj (SOME globalObj) instanceFixtures
        val _ = Eval.initializeAndConstruct objClassClosure objClassObj objClassRegs [] globalObj

        val (_, _, classClassObj, residualClassProg) = loadRootClass Name.intrinsic_Class
        val (_, _, functionClassObj, residualFunctionProg) = loadRootClass Name.public_Function

        val _ = completeClassFixtures objClassObj
        val _ = completeClassFixtures classClassObj
        val _ = completeClassFixtures functionClassObj
    in
        Native.registerNatives ();
        loadFiles 
            [
             "builtins/Namespace.es",
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
             
             "builtins/Math.es",
             "builtins/Global.es",
             
             "builtins/Array.es",  (* before Date *)

             "builtins/ByteArray.es",
             "builtins/Date.es",

          
             "builtins/Unicode.es",
             "builtins/RegExpCompiler.es",
             "builtins/RegExpEvaluator.es",
             "builtins/RegExp.es",

             "builtins/JSON.es",
             "builtins/Error.es",           
             "builtins/Shell.es"
            ];
        trace ["running residual programs"];
        Eval.evalProgram residualObjectProg;
        Eval.evalProgram residualClassProg;
        Eval.evalProgram residualFunctionProg;
        Eval.bindSpecialIdentities ();
        describeGlobal ()
    end
end

