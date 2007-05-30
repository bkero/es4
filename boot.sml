(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* The ES4 "boot environment". *)
structure Boot = struct 

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[boot] " :: ss) else ()
fun error ss = LogErr.hostError ss


fun instantiateRootClass (fullName:Ast.NAME) (prog:Ast.PROGRAM) : 
    (Ast.CLS * Mach.CLS_CLOSURE * Mach.OBJ * Ast.PROGRAM)
  =
  let 
      val globalObj = Eval.getGlobalObject ()
      val globalRegs = Eval.getInitialRegs ()

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

fun completeClassFixtures name classObj = 
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
        val globalRegs = Eval.getInitialRegs ()
        val classRegs = Eval.extendScopeReg globalRegs classObj Mach.InstanceScope
        val classClass = Eval.findVal (#scope globalRegs) Name.intrinsic_Class
        val Ast.Cls { instanceFixtures, ... } = (#cls (Mach.needClass classClass))
    in
        Eval.allocObjFixtures classRegs classObj (SOME classObj) instanceFixtures
    end

fun runObjectConstructorOnGlobalObject objClass objClassObj objClassClosure = 
    let
        val _ = trace ["running Object constructor on global object"];
        val globalObj = Eval.getGlobalObject ()
        val globalRegs = Eval.getInitialRegs ()
        val Ast.Cls { instanceFixtures, ...} = objClass
        val objClassRegs = Eval.extendScopeReg globalRegs objClassObj Mach.InstanceScope
        val _ = Eval.allocObjFixtures objClassRegs globalObj (SOME globalObj) instanceFixtures
    in
        Eval.initializeAndConstruct objClassClosure objClassObj objClassRegs [] globalObj
    end

fun loadFile f = 
    let
        val _ = trace ["parsing boot file ", f]
        val p = Parser.parseFile f
        val _ = trace ["defining boot file ", f]
    in
        Defn.defProgram p
    end

fun loadFiles fs = 
    let
        fun parse f = 
            (trace ["parsing boot file ", f]; 
             (f, Parser.parseFile f))
        fun def (f,p) = 
            (trace ["defining boot file ", f]; 
             (f, Defn.defProgram p))
    in
        map def (map parse fs)
    end

fun verifyFiles fs = 
    let 
        fun ver (f, p) = 
            (trace ["verifying boot file ", f]; 
             (f, Verify.verifyProgram p))
    in
        map ver fs
    end


fun evalFiles fs = 
    let 
        fun eval (f, p) = 
            (trace ["evaluating boot file ", f]; 
             Eval.evalProgram p)
    in
        map eval fs
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
	      NameMap.appi printProp (!props);
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
        val _ = Eval.booting := true

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
         * There are also other 3 "root" classes that require special processing
         * during startup to avoid feedback loops in their definition.
         *)

        val _ = Native.registerNatives ();

        val objProg = loadFile "builtins/Object.es"
        val clsProg = loadFile "builtins/Class.es"
        val funProg = loadFile "builtins/Function.es"

        val otherProgs = loadFiles ["builtins/Namespace.es",
                                    "builtins/Magic.es",
                                    "builtins/Conversions.es",
                                    "builtins/String.es",
                                    "builtins/string_primitive.es",

                                    "builtins/Name.es",
                                    
                                    "builtins/Error.es",
                                    
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
                                    
                                    "builtins/Unicode.es",
                                    "builtins/RegExpCompiler.es",
                                    "builtins/RegExpEvaluator.es",
                                    "builtins/RegExp.es",
                                    
                                    "builtins/Date.es",
                                    "builtins/JSON.es",
                                    "builtins/Shell.es"
                                   ]

        val objProg = Verify.verifyProgram objProg
        val clsProg = Verify.verifyProgram clsProg
        val funProg = Verify.verifyProgram funProg

        val otherProgs = verifyFiles otherProgs

        val (objClass, objClassClosure, objClassObj, residualObjectProg) = instantiateRootClass Name.public_Object objProg
        val _ = runObjectConstructorOnGlobalObject objClass objClassObj objClassClosure 

        val (_, _, classClassObj, residualClassProg) = instantiateRootClass Name.intrinsic_Class clsProg
        val (_, _, funClassObj, residualFunProg) = instantiateRootClass Name.public_Function funProg

        val globalRegs = Eval.getInitialRegs ()
    in
        completeClassFixtures Name.public_Object objClassObj;
        completeClassFixtures Name.intrinsic_Class classClassObj;
        completeClassFixtures Name.public_Function funClassObj;

        Eval.initClassPrototype globalRegs objClassObj;
        Eval.initClassPrototype globalRegs classClassObj;
        Eval.initClassPrototype globalRegs funClassObj;

        evalFiles otherProgs;

        Eval.evalProgram residualObjectProg;
        Eval.evalProgram residualClassProg;
        Eval.evalProgram residualFunProg;

        Eval.booting := false;
        Eval.resetStack ();
        describeGlobal ()
    end
end

