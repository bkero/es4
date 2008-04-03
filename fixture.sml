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
type ord_key = (Ast.RIB_ID option * Ast.MULTINAME)
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

structure IntKey = struct type ord_key = Int.int val compare = Int.compare end
structure IntMap = SplayMapFn (IntKey);


(* 
 * Rib records have a lifecycle. 
 *
 * There is always exactly one root rib, and it never closes.
 * 
 * Under the root rib, there are a variety of other ribs: 
 * 
 *   - open top unit ribs, which are placeholders with no content.
 * 
 *   - closed top unit ribs, which store a snapshot of the root rib at
 *     the moment they closed.
 * 
 *   - general ribs, which may be children of either form of top rib
 *     *or* direct children of the root.
 * 
 * If a general rib has no parent, or has an open top-unit rib as its
 * parent, it's implicitly a child of the root rib and is implicitly open.
 * 
 * If any rib has a closed top-unit rib as a parent, the rib is closed by 
 * extension and all type reasoning in it should actually resolve; there should
 * never be partial-evaluation stalls. Failure to find a fixture name
 * (say a type) under a closed top-unit rib is always a hard error.
 * 
 *)

datatype RIB_RECORD = 
         GeneralRib of { parent: Ast.RIB_ID option,
                         rib: Ast.RIB }


(* -----------------------------------------------------------------------------
 * Operations on FIXTUREs and RIBs
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


type TYEQ = (Ast.TYPE_EXPR -> Ast.TYPE_EXPR -> bool)


fun mergeVirtuals (tyeq:TYEQ)
                  (fName:Ast.FIXTURE_NAME)
                  (vnew:Ast.VIRTUAL_VAL_FIXTURE)
                  (vold:Ast.VIRTUAL_VAL_FIXTURE) =
    let
        val ty = case ((#ty vold), (#ty vnew)) of
                     (Ast.SpecialType Ast.Any, tnew) => tnew
                   | (told, Ast.SpecialType Ast.Any) => told
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


fun mergeFixtures (tyeq:TYEQ)
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


fun mergeRibs (tyeq:TYEQ)
              (oldRib:Ast.RIB)
              (additions:Ast.RIB)
    : Ast.RIB = 
    List.rev (List.foldl (mergeFixtures tyeq) (List.rev oldRib) additions)


fun printFixture ((n:Ast.FIXTURE_NAME), (f:Ast.FIXTURE)) = 
    let
	val fs = case f of 
		     Ast.NamespaceFixture _ => "[namespace]"
		   | Ast.ClassFixture _ => "[class]"
		   | Ast.InterfaceFixture _ => "[interface]"
		   | Ast.TypeVarFixture _ => "[typeVar]"
		   | Ast.TypeFixture t => ("[type] = " ^ LogErr.ty t)
		   | Ast.MethodFixture _ => "[method]"
		   | Ast.ValFixture _ => "[val]"
		   | Ast.VirtualValFixture _ => "[virtualVal]"
           | Ast.InheritedFixture _ => "[inherited]"
    in
	case n of
	    Ast.TempName n => log ["temp #", Int.toString n, " -> ", fs]
      | Ast.PropName n => log [LogErr.name n, " -> ", fs]
    end


fun printRib (rib:Ast.RIB) =
    List.app printFixture rib


(* -----------------------------------------------------------------------------
 * Operations on PROGRAMs -- NB: much of PROGRAM is mutable.
 * ----------------------------------------------------------------------------- *)
                
type PROGRAM = { rootRib: Ast.RIB ref,
                 nextRibId: Ast.RIB_ID ref,
                 ribs: (RIB_RECORD IntMap.map) ref,
                 packageNames: ((Ast.IDENT list) list) ref,
                 langEd: int ref,
                 
                 (* fixtureCache lazily mirrors the contents of the rib records *)
                 fixtureCache: ((Ast.NAME * Ast.FIXTURE) FixtureMap.map) ref, 
                 cacheSize: int }


fun mkProgram (langEd:int) 
              (topRib:Ast.RIB)
    : PROGRAM =
    { rootRib = ref topRib, 
      nextRibId = ref 0,
      ribs = ref IntMap.empty,
      packageNames = ref [],
      langEd = ref langEd,
      
      fixtureCache = ref FixtureMap.empty,
      cacheSize = 4096 }


fun updateLangEd (prog:PROGRAM) 
                 (langEd:int)                  
    : unit = 
    ((#langEd prog) := langEd)
    

fun getLangEd (prog:PROGRAM) 
    : int = 
    (!(#langEd prog))


fun updateFixtureCache (prog:PROGRAM)
                       (ribId:Ast.RIB_ID option)
                       (mname:Ast.MULTINAME)
                       (name:Ast.NAME)
                       (fixture:Ast.FIXTURE) 
    : unit = 
    let
        val c = (#fixtureCache prog)
    in
        c := FixtureMap.insert ((!c), (ribId, mname), (name, fixture))
    end


fun writeRibRec (prog:PROGRAM)
                (ribId:Ast.RIB_ID)
                (ribRec:RIB_RECORD)
    : unit = 
    let 
        val { ribs, fixtureCache, ... } = prog
    in
        fixtureCache := FixtureMap.empty;
        ribs := IntMap.insert ((!ribs), ribId, ribRec)
    end


fun allocRibRec (prog:PROGRAM)
                (ribRec:RIB_RECORD)
    : Ast.RIB_ID = 
    let 
        val { nextRibId, ... } = prog
        val ribId = !nextRibId
    in        
        nextRibId := (ribId + 1
                      handle Overflow => error ["overflowed maximum rib ID"]);
        writeRibRec prog ribId ribRec;
        ribId
    end


fun allocGeneralRib (prog:PROGRAM)
                    (parent:Ast.RIB_ID option)
    : Ast.RIB_ID =
    allocRibRec prog (GeneralRib { parent = parent, rib = [] })


fun getRibRec (prog:PROGRAM)
              (ribId:Ast.RIB_ID)
    : RIB_RECORD = 
    case IntMap.find(!(#ribs prog), ribId) of
        NONE => error ["no rib record found for rib #", Int.toString ribId ]
      | SOME rr => rr


(* 
 * Convert a ribId to a specific chain of RIBS and an indicator of whether it's closed.
 * Possibly we can do without this if we fix up the multiname algorithm to use the 
 * fixture cache explicitly. For now we do it the old way.
 *)

fun getRibs (prog:PROGRAM)
            (ribId:Ast.RIB_ID option)
    : Ast.RIBS =
    case ribId of 
        NONE => [!(#rootRib prog)]
      | SOME rid =>         
        case getRibRec prog rid of 
            GeneralRib { parent, rib } => (rib :: (getRibs prog parent))

fun ribIsClosed (prog:PROGRAM)
                (ribId:Ast.RIB_ID option)
    : bool = false


fun saveRib (prog:PROGRAM)
            (ribId:Ast.RIB_ID option)
            (rib:Ast.RIB)
    : unit = 
    case ribId of 
        NONE => (#rootRib prog) := rib
      | SOME rid => 
        case getRibRec prog rid of 
            GeneralRib { parent, ... } => 
            writeRibRec prog rid (GeneralRib { parent=parent, rib=rib })


fun extendRib (prog:PROGRAM)
              (ribId:Ast.RIB_ID option)
              (additions:Ast.RIB)
              (tyeq:TYEQ)
    : unit = 
    let
        val oldRib = case ribId of 
                         NONE => !(#rootRib prog)
                       | SOME rid => 
                         case getRibRec prog rid of 
                             GeneralRib { rib, ... } => rib
        val newRib = mergeRibs tyeq oldRib additions
    in
        saveRib prog ribId newRib
    end
        

fun resolveToFixture (prog:PROGRAM)
                     (mn:Ast.MULTINAME)
                     (rid:Ast.RIB_ID option)
    : ((Ast.NAME * Ast.FIXTURE) option) =
    let
        val { fixtureCache, cacheSize, ... } = prog
        val c = !fixtureCache
        val k = (rid,mn)
    in
        case FixtureMap.find (c, k) of
            SOME v => SOME v
          | NONE => 
            let
                val ribs = getRibs prog rid
            in
                case Multiname.resolveInRibs mn ribs of 
                    NONE => NONE
                  | SOME (ribs, n) => 
                    let 
                        val (fix:Ast.FIXTURE) = getFixture (List.hd ribs) (Ast.PropName n)
                    in
                        if (FixtureMap.numItems c) < cacheSize
                        then (fixtureCache := FixtureMap.insert (c, k, (n, fix)); SOME (n, fix))
                        else SOME (n, fix)
                    end
            end
    end

fun getRootRib (prog:PROGRAM)
    : Ast.RIB = 
    !(#rootRib prog)


fun addPackageName (prog:PROGRAM)
                   (packageName:Ast.IDENT list)
    : unit = 
    (#packageNames prog) := packageName :: (!(#packageNames prog))


fun getPackageNames (prog:PROGRAM)
    : Ast.IDENT list list = 
    !(#packageNames prog)

end
