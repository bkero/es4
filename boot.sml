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
fun trace ss = if (!doTrace) then LogErr.log ("[boot] " :: ss) else ()
fun error ss = LogErr.hostError ss


fun lookupRoot (prog:Fixture.PROGRAM)
               (n:Ast.NAME)
    : (Ast.CLS * Ast.INSTANCE_TYPE) = 
    let
        val _ = trace ["fetching ", LogErr.name n, " class definition"];
        val rib = Fixture.getTopRib prog
        val fix = Fixture.getFixture rib (Ast.PropName n)
        val cls = case fix of
                      Ast.ClassFixture cls => cls
                    | _ => error [LogErr.name n, " did not resolve to a class fixture"]
        val Ast.Cls { instanceType, ... } = cls
        val ty = case instanceType of 
                     Ast.Ty { expr = Ast.InstanceType ity, ... } => ity
                   | _ => error [LogErr.name n, " does not have an instance type"]
    in
        (cls, ty)
    end
    

fun instantiateRootClass (regs:Mach.REGS) 
                         (fullName:Ast.NAME) 
    : (Ast.CLS * Mach.CLS_CLOSURE * Mach.OBJ) =
  let
      val prog = (#prog regs)
      val (cls, _) = lookupRoot prog fullName
      val (_, cty) = lookupRoot prog Name.intrinsic_Class
                                        
      val _ = trace ["allocating class ", LogErr.name fullName];
      val closure = Eval.newClsClosure (#scope regs) cls
      val obj = Mach.newObj (Mach.ClassTag cty) Mach.Null (SOME (Mach.Class closure))
      val classRegs = Eval.extendScopeReg regs obj Mach.InstanceScope

      val _ = trace ["allocating class fixtures for ", LogErr.name fullName];
      val Ast.Cls { classRib, ... } = cls
      val _ = Eval.allocObjRib classRegs obj NONE classRib

      val _ = trace ["binding class ", LogErr.name fullName];
      val Mach.Obj { props, ... } = (#global regs)
      val _ = Mach.addProp props fullName
                           { ty = Ast.InstanceType cty,
                             state = Mach.ValProp (Mach.Object obj),
                             attrs = { dontDelete = true,
                                       dontEnum = true,
                                       readOnly = true,
                                       isFixed = true } }
  in
      (cls, closure, obj)
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
        val (Ast.Cls { instanceRib, ... }, _) = lookupRoot (#prog regs) Name.intrinsic_Class
    in
        Eval.allocObjRib classRegs classObj (SOME classObj) instanceRib
    end

fun runObjectConstructorOnGlobalObject (regs:Mach.REGS)
                                       (objClass:Ast.CLS) 
                                       (objClassObj:Mach.OBJ) 
                                       (objClassClosure:Mach.CLS_CLOSURE)
    : unit =
    let
        val _ = trace ["running Object constructor on global object"];
        val Ast.Cls { instanceRib, ...} = objClass
        val objClassRegs = Eval.extendScopeReg regs objClassObj Mach.InstanceScope
        val glob = (#global regs)
        val _ = Eval.allocObjRib objClassRegs glob (SOME glob) instanceRib
    in
        Eval.initializeAndConstruct objClassRegs objClassClosure objClassObj [] glob
    end

fun loadFile (prog:Fixture.PROGRAM)
             (f:string) 
    : (Fixture.PROGRAM * Ast.FRAGMENT) =
    let
        val _ = trace ["parsing boot file ", f]
        val frag = Parser.parseFile f
        val _ = trace ["defining boot file ", f]
    in
        Defn.defTopFragment prog frag
    end

fun loadFiles (prog:Fixture.PROGRAM) 
              (fs:string list)
    : (Fixture.PROGRAM * ((string * Ast.FRAGMENT) list)) =
    let
        fun f prog accum (file::files) = 
            let 
                val _ = trace ["parsing and defining boot file ", file]
                val frag = Parser.parseFile file
                val (prog', frag') = Defn.defTopFragment prog frag
            in
                f prog' ((file, frag')::accum) files
            end
          | f prog accum _ = (prog, List.rev accum)
    in
        f prog [] fs
    end

(*
fun verifyFiles prog fs =
    let
        fun ver (file, frag) =
            (trace ["verifying boot file ", file];
             (file, Verify.verifyFragment frag))
    in
        map ver fs
    end
*)

fun evalFiles (regs:Mach.REGS)
              (fs:(string * Ast.FRAGMENT) list)
    : Mach.VAL list =
    let
        fun eval (file, frag) =
            (trace ["evaluating boot file ", file];
             Eval.evalTopFragment regs frag)
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

(*
fun printFixture ((n:Ast.FIXTURE_NAME), (f:Ast.FIXTURE)) =
    let
	val fs = case f of
		     Ast.NamespaceFixture _ => "[namespace]"
		   | Ast.ClassFixture _ => "[class]"
		   | Ast.InterfaceFixture _ => "[interface]"
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
*)

fun describeGlobal (regs:Mach.REGS) = 
    if !doTrace
    then
	    (trace ["global object contents:"];
	     let 
             val Mach.Obj { props, ... } = (#global regs)
         in
	         NameMap.appi printProp (!props);
             trace ["top fixture contents:"];
	         Fixture.printRib (Fixture.getTopRib (#prog regs))
         end)
    else 
        ()

fun boot _ : Mach.REGS =
    let
        val _ = Native.registerNatives ();
        val prog = Fixture.mkProgram Defn.initRib

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

        val (prog, objFrag) = loadFile prog "builtins/Object.es"
        val (prog, clsFrag) = loadFile prog "builtins/Class.es"
        val (prog, funFrag) = loadFile prog "builtins/Function.es"
        val (prog, ifaceFrag) = loadFile prog "builtins/Interface.es"

        (* FIXME: val objFrag = Verify.verifyProgram objProg *)

        val (prog, otherFrags) = 
            loadFiles prog 
                      ["builtins/Namespace.es",
                       "builtins/Magic.es",
                       "builtins/Internal.es",
                       "builtins/Conversions.es",
                       "builtins/String.es",
                       "builtins/string_primitive.es",
                       
                       "builtins/Name.es",
                                              
                       "builtins/Error.es",
                       "builtins/EvalError.es",
                       "builtins/RangeError.es",
                       "builtins/ReferenceError.es",
                       "builtins/SyntaxError.es",
                       "builtins/TypeError.es",
                       "builtins/URIError.es",
                       
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
                       
                       "builtins/Shell.es",   (* before RegExp, for debugging *)
                       "builtins/UnicodeClasses.es",
                       "builtins/UnicodeCasemapping.es",
                       "builtins/UnicodeTbl.es",
                       "builtins/Unicode.es",
                       "builtins/RegExpCompiler.es",
                       "builtins/RegExpEvaluator.es",
                       "builtins/RegExp.es",
                       "builtins/Date.es",
                       "builtins/JSON.es"
                 ]

        val glob = 
            let
                val (_, objIty) = lookupRoot prog Name.nons_Object
                val objTag = Mach.ClassTag objIty
            in
                Mach.newObj objTag Mach.Null NONE
            end

        val regs = Mach.makeInitialRegs prog glob
        val _ = Mach.setBooting regs true

        val (objClass, objClassClosure, objClassObj) = 
            instantiateRootClass regs Name.nons_Object

        val (_, _, classClassObj) = instantiateRootClass regs Name.intrinsic_Class
        val (_, _, funClassObj) = instantiateRootClass regs Name.nons_Function
        val (_, _, ifaceClassObj) = instantiateRootClass regs Name.intrinsic_Interface

        (* Allocate runtime representations of anything in the initRib. *)
        val _ = trace ["allocating ribs for all builtins"]
        val _ = Eval.allocScopeRib regs (Defn.initRib @ (Fixture.getTopRib prog))
        val _ = trace ["allocated ribs for all builtins"]

        val _ = describeGlobal regs;

       (* FIXME: 

        val clsFrag = Verify.verifyProgram clsFrag
        val funFrag = Verify.verifyProgram funFrag                      
        val ifaceFrag = Verify.verifyProgram ifaceFrag
        val otherProgs = verifyFiles otherProgs
                         
        *)
    in
        completeClassFixtures regs Name.nons_Object objClassObj;
        completeClassFixtures regs Name.intrinsic_Class classClassObj;
        completeClassFixtures regs Name.nons_Function funClassObj;
        completeClassFixtures regs Name.intrinsic_Interface ifaceClassObj;

        Eval.initClassPrototype regs objClassObj;
        Eval.initClassPrototype regs classClassObj;
        Eval.initClassPrototype regs funClassObj;
        Eval.initClassPrototype regs ifaceClassObj;

        evalFiles regs otherFrags;

        runObjectConstructorOnGlobalObject 
            regs objClass objClassObj objClassClosure;

        Eval.evalTopFragment regs objFrag;
        Eval.evalTopFragment regs clsFrag;
        Eval.evalTopFragment regs funFrag;
        Eval.evalTopFragment regs ifaceFrag;

        Mach.setBooting regs false;
        Mach.resetProfile regs;
        describeGlobal regs;
        regs
    end
end

