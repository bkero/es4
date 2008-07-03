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


fun lookupRoot (rootFixtureMap:Ast.FIXTURE_MAP)
               (n:Ast.NAME)
    : Ast.CLASS =
    let
        val fix = Fixture.getFixture rootFixtureMap (Ast.PropName n)
    in
        case fix of
            Ast.ClassFixture cls => cls
          | _ => error [LogErr.name n, " did not resolve to a class fixture"]
    end
    

fun instantiateRootClass (regs:Mach.REGS) 
                         (fullName:Ast.NAME) 
                         (proto:Mach.OBJECT)
    : (Ast.CLASS * Mach.OBJECT * ((Ast.CLASS * Mach.SCOPE) list)) =
  let
      val rootFixtureMap = (#rootFixtureMap regs)
      val class = lookupRoot rootFixtureMap fullName
      val tag = Mach.PrimitiveTag (Mach.TypePrimitive (Ast.ClassType class))

      val _ = trace ["allocating class ", LogErr.name fullName];
      val (obj, contexts) = Eval.allocateByTag regs tag (Mach.ObjectValue proto)

      val _ = trace ["object identity of  ", LogErr.name fullName];
      val _ = Eval.bindAnySpecialIdentity regs obj

      val _ = trace ["binding class name of  ", LogErr.name fullName];
      val Mach.Object { propertyMap, ... } = (#global regs)
      val _ = if Mach.hasProp propertyMap fullName
              then error ["global object already has a binding for ", LogErr.name fullName]
              else ()
      val _ = Mach.addProp propertyMap fullName
                           { ty = Ast.ClassType class,
                             state = Mach.ValueProperty (Mach.ObjectValue obj),
                             attrs = { removable = false,
                                       enumerable = false,
                                       writable = Mach.ReadOnly,
                                       fixed = true } }
  in
      (class, obj, contexts)
  end

fun loadFile (rootFixtureMap:Ast.FIXTURE_MAP)
             (f:string) 
    : (Ast.FIXTURE_MAP * Ast.PROGRAM) =
    let
        val _ = trace ["parsing boot file ", f]
        val prog = Parser.parseFile f
        val _ = trace ["defining boot file ", f]
    in
        Defn.defProgram rootFixtureMap prog langEd
    end

fun loadFiles (rootFixtureMap:Ast.FIXTURE_MAP) 
              (fs:string list)
    : (Ast.FIXTURE_MAP * ((string * Ast.PROGRAM) list)) =
    let
        fun f rootFixtureMap accum (file::files) = 
            let 
                val _ = trace ["parsing and defining boot file ", file]
                val prog = Parser.parseFile file
                val (rootFixtureMap', prog') = Defn.defProgram rootFixtureMap prog langEd
            in
                f rootFixtureMap' ((file, prog')::accum) files
            end
          | f rootFixtureMap accum _ = (rootFixtureMap, List.rev accum)
    in
        f rootFixtureMap [] fs
    end

val verifyBuiltins = ref false (* FIXME *)

fun verifyFiles rootFixtureMap fs =
    let
        fun ver (file, prog) =
            (trace ["verifying boot file ", file];
             (file,
              Verify.verifyProgram rootFixtureMap (!verifyBuiltins) prog))
    in
        map ver fs
    end

fun evalFiles (regs:Mach.REGS)
              (fs:(string * Ast.PROGRAM) list)
    : Mach.VALUE list =
    let
        fun eval (file, prog) =
            (trace ["evaluating boot file ", file];
             Eval.evalProgram regs prog)
    in
        map eval fs
    end


fun printProp ((n:Ast.NAME), (p:Mach.PROPERTY)) =
    let
	val ps = case (#state p) of
		         Mach.ValueProperty _ => "[val]"
		       | Mach.VirtualProperty _ => "[virtual val]"
    in
	trace [LogErr.name n, " -> ", ps]
    end


fun describeGlobal (regs:Mach.REGS) = 
    if !doTrace
    then
        (trace ["contents of global object:"];
         Mach.inspect (Mach.ObjectValue (#global regs)) 1;
         trace ["contents of top fixtureMap:"];
         Fixture.printFixtureMap (#rootFixtureMap regs))
    else 
        ()

fun filterOutRootClasses (prog:Ast.PROGRAM) : Ast.PROGRAM =
    let
        fun nonRootClassFixture ((Ast.PropName n), _) = if n = Name.public_Object orelse
                                                           n = Name.public_Function
                                                        then false
                                                        else true
          | nonRootClassFixture _ = true
        fun filterFixtureMap fixtureMap = 
            List.filter nonRootClassFixture fixtureMap
        fun filterHeadOpt (SOME (Ast.Head (fixtureMap, inits))) = SOME (Ast.Head (filterFixtureMap fixtureMap, inits))
          | filterHeadOpt NONE = NONE
    in
        case prog of 
            Ast.Program (Ast.Block { pragmas, defns, head, body, loc }) => 
            Ast.Program (Ast.Block { pragmas = pragmas, 
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
        val rootFixtureMap = Defn.initFixtureMap

        (*
         * We have to do a small bit of delicate work here because we have to 
         * construct the regs that *uses* the global object inbetween allocating
         * it and running its constructor. 
         *
         * There is no provision for this in the standard object-construction
         * protocol Eval.constructClassInstance, so we inline it here.
         *
         * There are also 2 "root" classes that require special processing
         * during startup to avoid feedback loops in their definition: Object 
         * and Function.
         *)

        val (rootFixtureMap, objProg) = loadFile rootFixtureMap (builtin "Object.es")
        val (rootFixtureMap, funProg) = loadFile rootFixtureMap (builtin "Function.es")

        val (rootFixtureMap, otherProgs) = 
            loadFiles rootFixtureMap 
                      [builtin "Namespace.es",
                       builtin "Arguments.es",
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
                val cls = lookupRoot rootFixtureMap Name.public_Object
            in
                Mach.newObject (Mach.InstanceTag cls) Mach.NullValue rootFixtureMap
            end

        val objProg = Verify.verifyProgram rootFixtureMap (!verifyBuiltins) objProg 
        val funProg = Verify.verifyProgram rootFixtureMap (!verifyBuiltins) funProg
        val _ = verifyFiles rootFixtureMap otherProgs

        val regs = Mach.makeInitialRegs rootFixtureMap glob
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
        val objClass = lookupRoot rootFixtureMap Name.public_Object
        val Ast.Class { instanceFixtureMap, ... } = objClass
        val objPrototype = Mach.newObject (Mach.InstanceTag objClass) Mach.NullValue instanceFixtureMap
        val funPrototype = Mach.newObject (Mach.InstanceTag objClass) (Mach.ObjectValue objPrototype) instanceFixtureMap

        val (objClass, objClassObj, objClassContexts) = instantiateRootClass regs Name.public_Object funPrototype
        val (_, funClassObj, funClassContexts) = instantiateRootClass regs Name.public_Function funPrototype

        val objContexts = [(objClass, Eval.getClassScope regs objClassObj)]

        val _ = describeGlobal regs;
    in
        (* NB: order matters here. *)
        trace ["initializing class prototypes"];
        Eval.initClassPrototype regs funClassObj;
        Eval.initClassPrototype regs objClassObj;

        trace ["evaluating other files"];
        evalFiles regs otherProgs;

        Eval.initializeAndConstruct regs objContexts [] glob;
        Eval.initializeAndConstruct regs objContexts [] objPrototype;
        Eval.initializeAndConstruct regs objContexts [] funPrototype;

        Eval.initializeAndConstruct regs objClassContexts [] objClassObj;
        Eval.initializeAndConstruct regs funClassContexts [] funClassObj;

        Eval.evalProgram regs (filterOutRootClasses objProg);
        Eval.evalProgram regs (filterOutRootClasses funProg);

        Mach.setBooting regs false;
        Mach.resetProfile regs;
        describeGlobal regs;

        regs
    end
end

