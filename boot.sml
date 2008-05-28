(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 *
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 *
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 *
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * End of Terms and Conditions
 *
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *)
(* The ES4 "boot environment". *)
structure Boot = struct

(* Local tracing machinery *)

val doTrace = ref false
fun log ss = LogErr.log ("[boot] " :: ss)
fun trace ss = if (!doTrace) then log ss else ()
fun error ss = LogErr.hostError ss
val langEd = 4


fun lookupRoot (rootRib:Ast.RIB)
               (n:Ast.NAME)
    : Ast.CLASS =
    let
        val fix = Fixture.getFixture rootRib (Ast.PropName n)
    in
        case fix of
            Ast.ClassFixture cls => cls
          | _ => error [LogErr.name n, " did not resolve to a class fixture"]
    end
    

fun instantiateRootClass (regs:Mach.REGS) 
                         (fullName:Ast.NAME) 
                         (proto:Mach.OBJ)
    : (Ast.CLASS * Mach.OBJ) =
  let
      val rootRib = (#rootRib regs)
      val cls = lookupRoot rootRib fullName
      val Ast.Class { classRib, ... } = cls
      val clsClass = lookupRoot rootRib Name.intrinsic_Class
                                        
      val _ = trace ["allocating class ", LogErr.name fullName];
      val obj = Mach.newObject (Mach.PrimitiveTag (Mach.ClassPrimitive cls)) (Mach.Object proto) classRib

      val classRegs = Eval.extendScopeReg regs obj Mach.InstanceScope

      val _ = trace ["allocating ", Int.toString (length classRib), 
                     " class fixtures on class ", LogErr.name fullName,
                     ", object #", Int.toString (Eval.getObjId (obj))];          
      val _ = if (!doTrace) 
              then Fixture.printRib classRib
              else ()
      val _ = Eval.allocObjRib classRegs obj NONE classRib

      val _ = trace ["binding class ", LogErr.name fullName];
      val Mach.Obj { props, ... } = (#global regs)
      val _ = if Mach.hasProp props fullName
              then error ["global object already has a binding for ", LogErr.name fullName]
              else ()
      val _ = Mach.addProp props fullName
                           { ty = Ast.ClassType clsClass,
                             state = Mach.ValProp (Mach.Object obj),
                             attrs = { removable = false,
                                       enumerable = false,
                                       writable = Mach.ReadOnly,
                                       fixed = true } }
      val _ = Eval.bindAnySpecialIdentity regs obj
  in
      (cls, obj)
  end

fun completeClassFixtures (regs:Mach.REGS)
                          (name:Ast.NAME)
                          (classObj:Mach.OBJ) 
    : unit =
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
        val classRegs = Eval.extendScopeReg regs classObj Mach.InstanceScope
        val Ast.Class { instanceRib, ... } = lookupRoot (#rootRib regs) Name.intrinsic_Class
    in
        Eval.allocObjRib classRegs classObj (SOME classObj) instanceRib
    end

fun runConstructorOnObject (regs:Mach.REGS)
                           (class:Ast.CLASS) 
                           (classObj:Mach.OBJ) 
                           (obj:Mach.OBJ)
    : unit =
    let
        val _ = trace ["allocating deferred ribs and running deferred constructor"];
        val Ast.Class { instanceRib, ...} = class
        val classRegs = Eval.extendScopeReg regs classObj Mach.InstanceScope
        val classRegs = Eval.withThis regs obj
        val _ = Eval.allocObjRib classRegs obj (SOME obj) instanceRib
    in
        Eval.initializeAndConstruct classRegs class classObj [] obj
    end

fun loadFile (rootRib:Ast.RIB)
             (f:string) 
    : (Ast.RIB * Ast.FRAGMENT) =
    let
        val _ = trace ["parsing boot file ", f]
        val frag = Parser.parseFile f
        val _ = trace ["defining boot file ", f]
    in
        Defn.defTopFragment rootRib frag langEd
    end

fun loadFiles (rootRib:Ast.RIB) 
              (fs:string list)
    : (Ast.RIB * ((string * Ast.FRAGMENT) list)) =
    let
        fun f rootRib accum (file::files) = 
            let 
                val _ = trace ["parsing and defining boot file ", file]
                val frag = Parser.parseFile file
                val (rootRib', frag') = Defn.defTopFragment rootRib frag langEd
            in
                f rootRib' ((file, frag')::accum) files
            end
          | f rootRib accum _ = (rootRib, List.rev accum)
    in
        f rootRib [] fs
    end

val verifyBuiltins = ref false (* FIXME *)

fun verifyFiles rootRib fs =
    let
        fun ver (file, frag) =
            (trace ["verifying boot file ", file];
             (file,
              Verify.verifyTopFragment rootRib (!verifyBuiltins) frag))
    in
        map ver fs
    end

fun evalFiles (regs:Mach.REGS)
              (fs:(string * Ast.FRAGMENT) list)
    : Mach.VALUE list =
    let
        fun eval (file, frag) =
            (trace ["evaluating boot file ", file];
             Eval.evalTopFragment regs frag)
    in
        map eval fs
    end


fun printProp ((n:Ast.NAME), (p:Mach.PROPERTY)) =
    let
	val ps = case (#state p) of
		     Mach.TypeVarProp => "[typeVar]"
		   | Mach.TypeProp => "[type]"
		   | Mach.UninitProp => "[uninit]"
		   | Mach.ValProp _ => "[val]"
		   | Mach.VirtualValProp _ => "[virtual val]"
           | Mach.NativeFunctionProp _ => "[native function]"
           | Mach.ValListProp _ => "[val list]"
    in
	trace [LogErr.name n, " -> ", ps]
    end


fun describeGlobal (regs:Mach.REGS) = 
    if !doTrace
    then
        (trace ["contents of global object:"];
         Mach.inspect (Mach.Object (#global regs)) 1;
         trace ["contents of top rib:"];
         Fixture.printRib (#rootRib regs))
    else 
        ()

fun filterOutRootClasses (frag:Ast.FRAGMENT) : Ast.FRAGMENT =
    let
        fun nonRootClassFixture ((Ast.PropName n), _) = if n = Name.public_Object orelse
                                                           n = Name.intrinsic_Class orelse
                                                           n = Name.public_Function orelse
                                                           n = Name.intrinsic_Interface
                                                        then false
                                                        else true
          | nonRootClassFixture _ = true
        fun filterRib rib = 
            List.filter nonRootClassFixture rib
        fun filterHeadOpt (SOME (Ast.Head (rib, inits))) = SOME (Ast.Head (filterRib rib, inits))
          | filterHeadOpt NONE = NONE
    in
        case frag of 
            Ast.Anon (Ast.Block { pragmas, defns, head, body, loc }) => 
            Ast.Anon (Ast.Block { pragmas = pragmas, 
                                  defns = defns, 
                                  head = filterHeadOpt head, 
                                  body = body, 
                                  loc = loc })
    end

fun boot (baseDir:string) : Mach.REGS =
    let
        val dir = OS.Path.joinDirFile {dir = baseDir, file = "builtins"}
        fun builtin file = OS.Path.joinDirFile {dir = dir, file = file}
        val _ = Native.registerNatives ();
        val rootRib = Defn.initRib

        (*
         * We have to do a small bit of delicate work here because we have to 
         * construct the regs that *uses* the global object inbetween allocating
         * it and running its constructor. 
         *
         * There is no provision for this in the standard object-construction
         * protocol Eval.constructClassInstance, so we inline it here.
         *
         * There are also 4 "root" classes that require special processing
         * during startup to avoid feedback loops in their definition: Object, 
         * Class and Function.
         *)

        val (rootRib, objFrag) = loadFile rootRib (builtin "Object.es")
        val (rootRib, clsFrag) = loadFile rootRib (builtin "Class.es")
        val (rootRib, funFrag) = loadFile rootRib (builtin "Function.es")
        val (rootRib, ifaceFrag) = loadFile rootRib (builtin "Interface.es")

        val (rootRib, otherFrags) = 
            loadFiles rootRib 
                      [builtin "Namespace.es",
                       builtin "Helper.es",
                       builtin "Conversions.es",


                       builtin "string_primitive.es",
                       builtin "String.es",

                       (* 
                        * boolean before Boolean because the latter
                        * takes the prototype object set up by the
                        * former.
                        *)
                       builtin "boolean_primitive.es",
                       builtin "Boolean.es",                                              
                       
                       (* 
                        * Likewise the number primitive types and the
                        * Number type all use int's prototype
                        * (which should be made first), and the String
                        * type uses string's prototype.
                        *)

                       builtin "double.es",
                       builtin "decimal.es",
                       builtin "Number.es",

                       builtin "Math.es",
                       builtin "Global.es",
                                              
                       builtin "Name.es",

                       builtin "Error.es",
                       builtin "EncodingError.es",
                       builtin "EvalError.es",
                       builtin "RangeError.es",
                       builtin "ReferenceError.es",
                       builtin "SyntaxError.es",
                       builtin "TypeError.es",
                       builtin "URIError.es",

                       builtin "Array.es",  (* before Date *)

                       builtin "Shell.es",   (* before RegExp, for debugging *)
                       builtin "UnicodeClasses.es",
                       builtin "UnicodeCasemapping.es",
                       builtin "UnicodeTbl.es",
                       builtin "Unicode.es",
                       builtin "RegExpCompiler.es",
                       builtin "RegExpEvaluator.es",
                       builtin "RegExp.es",
                       builtin "Date.es",
                       builtin "MetaObjects.es"

                       (* builtin "Vector.es", *)
                       (* builtin "Map.es" *)
                 ]

        val glob = 
            let
                val cls = lookupRoot rootRib Name.public_Object
            in
                Mach.newObject (Mach.InstanceTag cls) Mach.Null rootRib
            end

        val objFrag = Verify.verifyTopFragment rootRib (!verifyBuiltins) objFrag 
        val clsFrag = Verify.verifyTopFragment rootRib (!verifyBuiltins) clsFrag
        val funFrag = Verify.verifyTopFragment rootRib (!verifyBuiltins) funFrag
        val ifaceFrag = Verify.verifyTopFragment rootRib (!verifyBuiltins) ifaceFrag
        val _ = verifyFiles rootRib otherFrags

        val regs = Mach.makeInitialRegs rootRib glob
        val _ = Mach.setBooting regs true

        (* BEGIN SPEED HACK *)
        (* 
         * This wins a factor of 60 in performance. It's worth it.
         * Just don't copy this nonsense to the spec. 
         *)
        val _ = Type.cacheLoad := SOME (fn i => Mach.findInTyCache regs i)
        val _ = Type.cacheSave := SOME (fn i => fn t => (Mach.updateTyCache regs (i, t); ()))
        (* END SPEED HACK *)                

        (* Build the Object and Function prototypes as instances of public::Object first. *)
        val objClass = lookupRoot rootRib Name.public_Object
        val Ast.Class { instanceRib, ... } = objClass
        val objPrototype = Mach.newObject (Mach.InstanceTag objClass) Mach.Null instanceRib
        val funPrototype = Mach.newObject (Mach.InstanceTag objClass) (Mach.Object objPrototype) instanceRib

        val (objClass, objClassObj) = instantiateRootClass regs Name.public_Object funPrototype
        val (_, classClassObj) = instantiateRootClass regs Name.intrinsic_Class funPrototype
        val (_, funClassObj) = instantiateRootClass regs Name.public_Function funPrototype
        val (_, ifaceClassObj) = instantiateRootClass regs Name.intrinsic_Interface funPrototype            

        (* Allocate runtime representations of anything in the initRib. *)
        val _ = trace ["allocating ribs for all builtins"]
                                    
        val _ = Eval.allocScopeRib regs Defn.initRib
        val _ = trace ["allocated ribs for initial rib"]

        val _ = describeGlobal regs;
    in

        trace ["completing class fixtures"];
        completeClassFixtures regs Name.public_Object objClassObj;
        completeClassFixtures regs Name.intrinsic_Class classClassObj;
        completeClassFixtures regs Name.public_Function funClassObj;
        completeClassFixtures regs Name.intrinsic_Interface ifaceClassObj;

        (* NB: order matters here. *)
        trace ["initializing class prototypes"];
        Eval.initClassPrototype regs funClassObj;
        Eval.initClassPrototype regs objClassObj;
        Eval.initClassPrototype regs classClassObj;
        Eval.initClassPrototype regs ifaceClassObj;

        trace ["evaluating other files"];
        evalFiles regs otherFrags;

        runConstructorOnObject regs objClass objClassObj glob;
        runConstructorOnObject regs objClass objClassObj objPrototype;        
        runConstructorOnObject regs objClass objClassObj funPrototype;

        Eval.evalTopFragment regs (filterOutRootClasses objFrag);
        Eval.evalTopFragment regs (filterOutRootClasses clsFrag);
        Eval.evalTopFragment regs (filterOutRootClasses funFrag);
        Eval.evalTopFragment regs (filterOutRootClasses ifaceFrag);

        Mach.setBooting regs false;
        Mach.resetProfile regs;
        describeGlobal regs;

        regs
    end
end

