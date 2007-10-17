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
structure Fixture :> FIXTURE = struct

val doTrace = ref false
fun log ss = LogErr.log ("[fixture] " :: ss)
fun error ss = LogErr.fixtureError ss
fun trace ss = if (!doTrace) then log ss else ()

fun cmpMname (a:Ast.MULTINAME, b:Ast.MULTINAME) 
    : order = 
    case Ustring.compare (#id a) (#id b) of
	    LESS => LESS
      | GREATER => GREATER
      | EQUAL => (NameKey.collate (NameKey.collate NameKey.cmpNS)) ((#nss a), (#nss b))
                     

structure FixtureKey = struct 
type ord_key = (Ast.FRAME_ID option * Ast.MULTINAME)
fun compare (a,b) = 
    case (a,b) of 
        ((NONE, an), (NONE, bn)) => cmpMname (an, bn)
      | ((NONE, an), ((SOME bi), bn)) => LESS
      | (((SOME ai), an), (NONE, bn)) => GREATER
      | (((SOME ai), an), ((SOME bi), bn)) => 
        if ai = bi
        then cmpMname (an, bn)
        else Int.compare (ai,bi)
end
structure FixtureMap = SplayMapFn (FixtureKey);

structure StrVecKey = struct 
type ord_key = (Ustring.STRING vector) 
val compare = (Vector.collate (fn (a,b) => Ustring.compare a b)) 
end
structure StrVecMap = SplayMapFn (StrVecKey);

(* 
 * Operations on FRAMEs. FRAMEs must be vaguely mutable in order to
 * support constructing a type closure that refers to other members of
 * its own scope out-of-order. This is more common than we might like.
 * 
 * For pure type definitions we could theoretically sort them by dependency
 * before instantiating them into RIBs, but even putting aside how miserable
 * that would be, we'd still lose on classes: classes with signatures may
 * introduce circular dependencies on other classes at same scope.
 *
 * They *need* to go through a recursion-permitting identifier / ref cell.
 * Sad but true.
 * 
 * FIXME: possibly make this non-global. Since it's not keyed by name it's less
 * of an issue than units, but it'd be nice not to leak them. Requires rewriting
 * much of defn to thread PROGRAM values back up from each defFoo function. 
 *)

structure IntKey = struct type ord_key = Int.int val compare = Int.compare end
structure IntMap = SplayMapFn (IntKey);

(* Safe: will overflow when it runs out of identities. *)
val currFrameId = ref 0
fun nextFrameId _ =    
    (currFrameId := (((!currFrameId) + 1) 
                    handle Overflow => error ["overflowed maximum frame ID"]);
     !currFrameId)

type FRAME = { curr: int,
               parent: int option,
               rib: Ast.RIB }
            
val (frameMap:(FRAME IntMap.map) ref) = ref IntMap.empty

fun allocFrame (parent:int option)
    : int =
    let 
        val frameId = nextFrameId ()
        val frame = { curr = frameId,
                      parent = parent,
                      rib = [] }
    in
        frameMap := IntMap.insert (!frameMap, frameId, frame);
        frameId
    end

fun getFrame (frameId:int)
    : FRAME = 
    case IntMap.find(!frameMap, frameId) of
        NONE => error ["no stored frame found for frame #", Int.toString frameId ]
      | SOME frame => frame

fun saveFrame (frameId:int)
              (rib:Ast.RIB)
    : unit = 
    let
        val oldFrame = getFrame frameId
        val newFrame = { curr = frameId, 
                         parent = (#parent oldFrame),
                         rib = rib }
    in
        frameMap := IntMap.insert (!frameMap, frameId, newFrame)
    end



(* -----------------------------------------------------------------------------
 * Operations on FIXTURE
 * ----------------------------------------------------------------------------- *)

type TYEQ = (Ast.TY -> Ast.TY -> bool)

fun printFixture ((n:Ast.FIXTURE_NAME), (f:Ast.FIXTURE)) = 
    let
	val fs = case f of 
		     Ast.NamespaceFixture _ => "[namespace]"
		   | Ast.ClassFixture _ => "[class]"
		   | Ast.InterfaceFixture _ => "[interface]"
		   | Ast.TypeVarFixture => "[typeVar]"
		   | Ast.TypeFixture t => ("[type] = " ^ LogErr.ty (AstQuery.typeExprOf t))
		   | Ast.MethodFixture _ => "[method]"
		   | Ast.ValFixture _ => "[val]"
		   | Ast.VirtualValFixture _ => "[virtualVal]"
    in
	case n of
	    Ast.TempName n => log ["temp #", Int.toString n, " -> ", fs]
      | Ast.PropName n => log [LogErr.name n, " -> ", fs]
    end

(* -----------------------------------------------------------------------------
 * Operations on FIXTURES
 * ----------------------------------------------------------------------------- *)

fun getFixture (b:Ast.RIB) 
               (n:Ast.FIXTURE_NAME) 
    : Ast.FIXTURE = 
    let 
        fun search [] = LogErr.fixtureError ["fixture binding not found: ", 
                                             (LogErr.fname n)]
          | search ((k,v)::bs) = 
            if k = n 
            then v
            else search bs
    in
        search b
    end

fun hasFixture (b:Ast.RIB) 
               (n:Ast.FIXTURE_NAME) 
    : bool = 
    let 
        fun search [] = false
          | search ((k,v)::bs) = 
            if k = n 
            then true
            else search bs
    in
        search b    
    end

fun replaceFixture (b:Ast.RIB) 
                   (n:Ast.FIXTURE_NAME) 
                   (v:Ast.FIXTURE)
    : Ast.RIB = 
    let 
        fun search [] = LogErr.fixtureError ["fixture binding not found: ", 
                                             (LogErr.fname n)]
          | search ((k,v0)::bs) = 
            if k = n 
            then (k,v)::bs
            else (k,v0) :: (search bs)
    in
        search b
    end

fun printRib (rib:Ast.RIB) =
    List.app printFixture rib

(* -----------------------------------------------------------------------------
 * Operations on PROGRAM
 * ----------------------------------------------------------------------------- *)
                
type PROGRAM = { fixtureCache: (Ast.FIXTURE FixtureMap.map) ref, (* mirrors the ribs *)
                 instanceOfCache: (bool StrVecMap.map) ref,
                 cacheSize: int,
                 topRib: Ast.RIB,
                 topBlocks: Ast.BLOCK list,
                 packageNames: (Ast.IDENT list) list,
                 unitRibs: (Ast.RIB StrVecMap.map),
                 langEd: int }

fun processTopRib (prog:PROGRAM) 
                  (f:Ast.RIB -> Ast.RIB)
    : PROGRAM = 
    let
        val { fixtureCache, instanceOfCache, cacheSize, 
              topRib, topBlocks, packageNames, unitRibs, langEd } = prog
    in
        frameMap := IntMap.map (fn v => ({ curr = (#curr v),
                                           parent = (#parent v),
                                           rib = f (#rib v) } )) (!frameMap);
        { fixtureCache = ref FixtureMap.empty,
          instanceOfCache = ref StrVecMap.empty,
          cacheSize = 1024,
          topRib = f topRib,
          topBlocks = topBlocks,
          packageNames = packageNames,
          unitRibs = StrVecMap.map f unitRibs,
          langEd = langEd }
    end

fun mkProgram (langEd:int) 
              (topRib:Ast.RIB)
    : PROGRAM =
    { fixtureCache = ref FixtureMap.empty,
      instanceOfCache = ref StrVecMap.empty,
      cacheSize = 1024,
      topRib = topRib,
      topBlocks = [],
      packageNames = [],
      unitRibs = StrVecMap.empty,
      langEd = langEd }

fun updateLangEd (langEd:int)
                 (prog:PROGRAM)
    : PROGRAM = 
    let
        val { fixtureCache, instanceOfCache, cacheSize, topRib,
              topBlocks, packageNames, unitRibs, ... } = prog
    in
        { fixtureCache = fixtureCache,
          instanceOfCache = instanceOfCache, 
          cacheSize = cacheSize, 
          topRib = topRib,
          topBlocks = topBlocks, 
          packageNames = packageNames, 
          unitRibs = unitRibs, 
          langEd = langEd }
    end

fun langEd (prog:PROGRAM)
    : int = 
    (#langEd prog)


fun mergeVirtuals (tyeq:Ast.TY -> Ast.TY -> bool)
                  (fName:Ast.FIXTURE_NAME)
                  (vnew:Ast.VIRTUAL_VAL_FIXTURE)
                  (vold:Ast.VIRTUAL_VAL_FIXTURE) =
    let
        val ty = case ((#ty vold), (#ty vnew)) of
                     (Ast.Ty {expr=Ast.SpecialType Ast.Any, ...}, tnew) => tnew
                   | (told, Ast.Ty {expr=Ast.SpecialType Ast.Any, ...}) => told
                   | (t1, t2) => if tyeq t1 t2
                                 then t1
                                 else error ["mismatched get/set types on fixture ",
                                         LogErr.fname fName]
        fun either a b =
            case (a,b) of
                (SOME x, NONE) => SOME x
              | (NONE, SOME x) => SOME x
              | (NONE, NONE) => NONE
              | _ => error ["multiply defined get/set functions on fixture ",
                            LogErr.fname fName]
    in
        { ty = ty,
          getter = either (#getter vold) (#getter vnew),
          setter = either (#setter vold) (#setter vnew) }
    end

fun mergeFixtures (tyeq:Ast.TY -> Ast.TY -> bool)
                  ((newName:Ast.FIXTURE_NAME, newFix:Ast.FIXTURE),oldRib:Ast.RIB)
    : Ast.RIB =
    if hasFixture oldRib newName
    then
        case (newFix, getFixture oldRib newName) of
            (Ast.VirtualValFixture vnew,
             Ast.VirtualValFixture vold) =>
            replaceFixture oldRib newName
                           (Ast.VirtualValFixture
                                (mergeVirtuals tyeq newName vnew vold))
          | (Ast.ValFixture new, Ast.ValFixture old) =>
            if (tyeq (#ty new) (#ty old)) 
               andalso (#readOnly new) = (#readOnly old)
            then oldRib
            else error ["incompatible redefinition of fixture name: ", LogErr.fname newName]
          | (Ast.MethodFixture new, Ast.MethodFixture old) =>
            replaceFixture oldRib newName (Ast.MethodFixture new) (* FIXME: types *)
          | (Ast.MethodFixture new, Ast.ValFixture old) =>
            replaceFixture oldRib newName (Ast.MethodFixture new) (* FIXME: types *)
          | (Ast.ValFixture new, Ast.MethodFixture old) =>
            replaceFixture oldRib newName (Ast.ValFixture new) (* FIXME: types *)
          | _ => error ["mergeFixtures: redefining fixture name: ", LogErr.fname newName]
    else
        (newName,newFix) :: oldRib

fun extendTopRib (prog:PROGRAM) 
                 (additions:Ast.RIB) 
                 (tyeq:TYEQ) 
    : PROGRAM = 
    let
        val { fixtureCache, instanceOfCache, cacheSize, 
              topRib, topBlocks, packageNames, unitRibs, langEd } = prog
        val newTopRib = List.rev (List.foldl (mergeFixtures tyeq) topRib additions)
    in
        { cacheSize = cacheSize, 
          (* flush caches *)
          fixtureCache = ref FixtureMap.empty,
          instanceOfCache = ref StrVecMap.empty,
          (* replace updated immutable parts *)
          topRib = newTopRib,
          topBlocks = topBlocks,
          packageNames = packageNames,
          unitRibs = unitRibs,
          langEd = langEd  }
    end

fun closeTopFragment (prog:PROGRAM)
                     (frag:Ast.FRAGMENT)
                     (tyeq:TYEQ)
    : PROGRAM = 
    let
        val { fixtureCache, instanceOfCache, cacheSize, 
              topRib, topBlocks, packageNames, unitRibs, langEd } = prog
        val newUnitRibs = 
            case frag of 
                Ast.Unit { name=SOME nm, ... } => 
                StrVecMap.insert (unitRibs, (Vector.fromList nm), topRib)
              | _ => 
                unitRibs
        fun fragBlocks (Ast.Unit {name, fragments}) = 
            List.concat (map fragBlocks fragments)
          | fragBlocks (Ast.Package {name, fragments}) = 
            List.concat (map fragBlocks fragments)
          | fragBlocks (Ast.Anon block) = [block]

        fun fragPackages (Ast.Unit {name, fragments}) = 
            List.concat (map fragPackages fragments)
          | fragPackages (Ast.Package {name, fragments}) = 
            name :: (List.concat (map fragPackages fragments))
          | fragPackages (Ast.Anon block) = []               
    in        
        { cacheSize = cacheSize, 
          (* flush caches *)
          fixtureCache = ref FixtureMap.empty,
          instanceOfCache = ref StrVecMap.empty,
          (* replace updated immutable parts *)
          topRib = topRib,
          topBlocks = topBlocks @ (fragBlocks frag),
          packageNames = packageNames @ (fragPackages frag),
          unitRibs = newUnitRibs,
          langEd = langEd  }
    end


fun resolveToRibs (frameId:Ast.FRAME_ID) 
    : Ast.RIBS = 
    let
        val frame = getFrame frameId 
        val parents = case (#parent frame) of 
                          NONE => []
                        | SOME pid => resolveToRibs pid
    in
        (#rib frame) :: parents
    end


fun getTopFixture (prog:PROGRAM)
                  (n:Ast.NAME)
    : Ast.FIXTURE =
    let
        val { fixtureCache, cacheSize, topRib, ... } = prog
        val c = !fixtureCache
        val k = (NONE,{id=(#id n), nss=[[(#ns n)]]})
    in
        case FixtureMap.find (c, k) of
            SOME v => v
          | NONE => 
            let 
                val v = getFixture topRib (Ast.PropName n)
            in
                if (FixtureMap.numItems c) < cacheSize
                then (fixtureCache := FixtureMap.insert (c, k, v); v)
                else v
            end
    end

fun resolveToFixture (prog:PROGRAM)
                     (mn:Ast.MULTINAME)
                     (fid:Ast.FRAME_ID option)
    : (Ast.FIXTURE option) =
    let
        val { fixtureCache, cacheSize, topRib, ... } = prog
        val c = !fixtureCache
        val k = (fid,mn)
    in
        case FixtureMap.find (c, k) of
            SOME v => SOME v
          | NONE => 
            let
                val ribs = case fid of 
                               SOME f => (resolveToRibs f) @ [topRib]
                             | NONE => [topRib]
            in
                case Multiname.resolveInRibs mn ribs of 
                    NONE => NONE
                  | SOME (ribs, n) => 
                    let 
                        val (fix:Ast.FIXTURE) = getFixture (List.hd ribs) (Ast.PropName n)
                    in
                        if (FixtureMap.numItems c) < cacheSize
                        then (fixtureCache := FixtureMap.insert (c, k, fix); SOME fix)
                        else SOME fix
                    end
            end
    end

val (getTopRib:PROGRAM -> Ast.RIB) = #topRib

fun getTopRibForUnit (prog:PROGRAM)
                     (unit:Ast.UNIT_NAME)
    : Ast.RIB option = 
    StrVecMap.find ((#unitRibs prog), Vector.fromList unit)

val (getTopBlocks:PROGRAM -> Ast.BLOCK list) = #topBlocks

val (getPackageNames:PROGRAM -> Ast.IDENT list list) = #packageNames

fun getCurrFullRibs (prog:PROGRAM)
                    (nonTopRibs:Ast.RIBS)
    : Ast.RIBS = 
    nonTopRibs @ [getTopRib prog]

fun getFullRibsForTy (prog:PROGRAM) 
                     (ty:Ast.TY)                
    : (Ast.RIBS * bool) = 
    (* 
     * Returns the full ribs of TY, plus a flag indicating whether the
     * full ribs are the result of prepending a closed unit -- which, if 
     * true, makes name-lookup failures a hard error in the resulting
     * full ribs. 
     *)
    let       
        val Ast.Ty { frameId, topUnit, ... } = ty
        val ribs = case frameId of 
                       NONE => []
                     | SOME fid => resolveToRibs fid
        val (topRib, closed) = 
            case topUnit of 
                NONE => (getTopRib prog, false)
              | SOME u => (case getTopRibForUnit prog u of
                               NONE => (getTopRib prog, false)
                             | SOME closedRib => (closedRib, true))                          
        val fullRibs = ribs @ [topRib]
    in
        (fullRibs, closed)
    end


(* -----------------------------------------------------------------------------
 * Class operations on PROGRAMs
 * ----------------------------------------------------------------------------- *)

(* FIXME: these really belong in type.sml but then I can't keep the
 *        instanceOfCache in PROGRAM if it's an abstract type
 *)

fun instanceTy (prog:PROGRAM)
               (n:Ast.NAME)
    : Ast.TY =
    case getTopFixture prog n of
        Ast.ClassFixture (Ast.Cls cls) => (#instanceType cls)
      | Ast.InterfaceFixture (Ast.Iface iface) => (#instanceType iface)
      | _ => error ["type not an instance type ", LogErr.name n]

fun isClass (prog:PROGRAM)
            (t:Ast.NAME)
    : bool =
    case getTopFixture prog t of 
        Ast.ClassFixture cls => true
      | _ => false

fun getClass (prog:PROGRAM)
             (t:Ast.NAME)
    : Ast.CLS =
    case getTopFixture prog t of 
        Ast.ClassFixture cls => cls
      | Ast.InterfaceFixture iface =>   (* FIXME: not sure what to do here. getClass gets called when a nominal
                                            type is used in various ways. Just return class Object for now *)
        let
            val Ast.ClassFixture objCls = getTopFixture prog Name.nons_Object
        in
            objCls
        end
      | _ => error ["getClass returned non-class fixture for ", LogErr.name t]



(* FIXME: this is a bad name. subclassOf isn't quite right either. isDerivedType? inheritsFrom? *)
fun instanceOf (prog:PROGRAM)
               (t0:Ast.TYPE_EXPR)
               (t:Ast.TYPE_EXPR)
               (tyexpreq:Ast.TYPE_EXPR -> Ast.TYPE_EXPR -> bool)
               (serialize:Ast.TYPE_EXPR -> Ustring.STRING list)
               (sep:Ustring.STRING)
    : bool =
    let
        fun searchFrom (curr:Ast.TYPE_EXPR) = 
            if tyexpreq curr t
            then true
            else 
                case curr of 
                    Ast.InstanceType {superTypes, ...} => 
                    List.exists searchFrom superTypes
                  | _ => false

        val { instanceOfCache, cacheSize, ... } = prog
        val c = !instanceOfCache
        val init = serialize t0
        val targ = serialize t
        val k = Vector.fromList (init @ [sep] @ targ)
    in
        case StrVecMap.find (c, k) of
            SOME v => v
          | NONE => 
            let
                val v = searchFrom t0
            in
                if (StrVecMap.numItems c) < cacheSize
                then (instanceOfCache := StrVecMap.insert (c, k, v); v)
                else v
            end
    end

end
