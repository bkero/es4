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

structure NmKey = struct type ord_key = Ast.NAME val compare = NameKey.compare end
structure NmMap = SplayMapFn (NmKey);

structure StrVecKey = struct type ord_key = (Ustring.STRING vector) val compare = (Vector.collate (fn (a,b) => Ustring.compare a b)) end
structure StrVecMap = SplayMapFn (StrVecKey);

(* -----------------------------------------------------------------------------
 * Operations on FIXTURE
 * ----------------------------------------------------------------------------- *)

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

(* -----------------------------------------------------------------------------
 * Operations on FIXTURES
 * ----------------------------------------------------------------------------- *)

fun getFixture (b:Ast.RIB) 
               (n:Ast.FIXTURE_NAME) 
    : Ast.FIXTURE = 
    let 
        fun search [] = LogErr.hostError ["fixture binding not found: ", 
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
        fun search [] = LogErr.hostError ["fixture binding not found: ", 
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

type PROGRAM = { fixtureCache: (Ast.FIXTURE NmMap.map) ref, (* mirrors the top rib *)
                 instanceOfCache: (bool StrVecMap.map) ref,
                 cacheSize: int,
                 topRib: Ast.RIB,
                 topBlocks: Ast.BLOCK list,
                 unitRibs: (Ast.RIB StrVecMap.map) }

fun mkProgram (topRib:Ast.RIB)
    : PROGRAM =
    { fixtureCache = ref NmMap.empty,
      instanceOfCache = ref StrVecMap.empty,
      cacheSize = 1024,
      topRib = topRib,
      topBlocks = [],
      unitRibs = StrVecMap.empty }

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
            then (trace ["skipping fixture ",LogErr.fname newName]; oldRib)
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

fun addTopFragment (prog:PROGRAM)
                   (frag:Ast.FRAGMENT, newRib:Ast.RIB)
                   (tyeq:Ast.TY -> Ast.TY -> bool)
    : PROGRAM = 
    let
        val { fixtureCache, instanceOfCache, cacheSize, 
              topRib, topBlocks, unitRibs } = prog
        val newTopRib = List.foldl (mergeFixtures tyeq) topRib newRib
        val newUnitRibs = 
            case frag of 
                Ast.Unit { name=SOME nm, ... } => 
                StrVecMap.insert (unitRibs, (Vector.fromList nm), newTopRib)
              | _ => 
                unitRibs
        fun fragBlocks (Ast.Unit {name, fragments}) = 
            List.concat (map fragBlocks fragments)
          | fragBlocks (Ast.Package {name, fragments}) = 
            List.concat (map fragBlocks fragments)
          | fragBlocks (Ast.Anon block) = [block]
    in
        
        { cacheSize = cacheSize, 
          (* flush caches *)
          fixtureCache = ref NmMap.empty,
          instanceOfCache = ref StrVecMap.empty,
          (* replace updated immutable parts *)
          topRib = newTopRib,
          topBlocks = topBlocks @ (fragBlocks frag),
          unitRibs = newUnitRibs }
    end


fun getTopFixture (prog:PROGRAM)
                  (n:Ast.NAME)
    : Ast.FIXTURE =
    let
        val { fixtureCache, cacheSize, topRib, ... } = prog
        val c = !fixtureCache
    in
        case NmMap.find (c, n) of
            NONE => 
            let
                val v = getFixture topRib (Ast.PropName n)
            in
                if (NmMap.numItems c) < cacheSize
                then (fixtureCache := NmMap.insert (c, n, v); v)
                else v
            end
          | SOME v => v
    end

val (getTopRib:PROGRAM -> Ast.RIB) = #topRib

fun getTopRibForUnit (prog:PROGRAM)
                     (unit:Ast.UNIT_NAME)
    : Ast.RIB option = 
    StrVecMap.find ((#unitRibs prog), Vector.fromList unit)

val (getTopBlocks:PROGRAM -> Ast.BLOCK list) = #topBlocks

fun printTopRib (prog:PROGRAM) =
    printRib (getTopRib prog)

(* -----------------------------------------------------------------------------
 * Class operations on PROGRAMs
 * ----------------------------------------------------------------------------- *)

(* FIXME: these really belong in type.sml but then I can't keep the
 *        instanceOfCache in PROGRAM if it's an abstract type
 *)

fun instanceType (prog:PROGRAM)
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
            NONE => 
            let
                val v = searchFrom t0
            in
                if (StrVecMap.numItems c) < cacheSize
                then (instanceOfCache := StrVecMap.insert (c, k, v); v)
                else v
            end
          | SOME v => v
    end

end
