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
structure Fixture = struct

val doTrace = ref false
fun log ss = LogErr.log ("[fixture] " :: ss)
fun error ss = LogErr.fixtureError ss
fun trace ss = if (!doTrace) then log ss else ()


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
		   | Ast.TypeFixture t => ("[type]" ^ LogErr.ty t)
		   | Ast.MethodFixture _ => "[method]"
		   | Ast.ValFixture _ => "[val]"
		   | Ast.VirtualValFixture _ => "[virtualVal]"
    in
	case n of
	    Ast.TempName n => log ["temp #", Int.toString n, " -> ", fs]
      | Ast.PropName n => log [LogErr.name n, " -> ", fs]
    end


fun printRib (rib:Ast.RIB) =
    List.app printFixture rib


(* -----------------------------------------------------------------------------
 * Operations on PROGRAMs 
 * ----------------------------------------------------------------------------- *)
                
type PROGRAM = { rootRib: Ast.RIB,
                 packageNames: ((Ast.IDENT list) list) }

               
fun mkProgram (topRib:Ast.RIB)
    : PROGRAM =
    { rootRib = topRib, 
      packageNames = [] }


fun extendRootRib (prog:PROGRAM)
                  (additions:Ast.RIB)
                  (tyeq:TYEQ)
    : PROGRAM = 
    let
        val { packageNames, ... } = prog
        val oldRib = (#rootRib prog)
        val newRib = mergeRibs tyeq oldRib additions
    in
        { rootRib = newRib,
          packageNames = packageNames }
    end
        

fun getRootRib (prog:PROGRAM)
    : Ast.RIB = 
    (#rootRib prog)


fun addPackageName (prog:PROGRAM)
                   (packageName:Ast.IDENT list)
    : PROGRAM = 
    let
        val { rootRib, ... } = prog
    in
        { rootRib = rootRib,
          packageNames = (packageName :: (#packageNames prog)) }
    end


fun getPackageNames (prog:PROGRAM)
    : Ast.IDENT list list = 
    (#packageNames prog)


(* -----------------------------------------------------------------------------
 * Static variant of name-resolution algorithm: mirrors Mach.findName
 * ----------------------------------------------------------------------------- *)

type IDENTIFIER = Ast.IDENT
type NAMESPACE = Ast.NAMESPACE

type CLASS = Ast.CLS
type NAME = Ast.NAME

type NAMESPACE_SET = NAMESPACE list
type OPEN_NAMESPACES = NAMESPACE_SET list 

fun head x = hd x (* return the first element of a list *)
fun tail x = tl x (* return all but the first element of a list *)

fun compareNamespaces (n1: NAMESPACE, n2: NAMESPACE) : bool =
    case (n1, n2) of
        (Ast.TransparentNamespace s1, Ast.TransparentNamespace s2) => s1 = s2
      | (Ast.OpaqueNamespace i1, Ast.OpaqueNamespace i2) => i1 = i2
      | _ => false

fun intersectNamespaces (ns1: NAMESPACE_SET, ns2: NAMESPACE_SET)
    : NAMESPACE_SET =
    (* compute the intersection of two NAMESPACE_SETs *)
    (* INFORMATIVE *)
    List.filter (fn n1 => List.exists (fn n2 => compareNamespaces (n1, n2)) ns2) ns1

fun selectNamespacesByRootRib (identifier: IDENTIFIER,
                               namespaces: NAMESPACE_SET,
                               rootRib: Ast.RIB)
    : NAMESPACE_SET = 
    List.filter (fn ns => hasFixture rootRib (Ast.PropName {id=identifier, ns=ns})) namespaces

fun selectNamespacesByOpenNamespaces ([], _) = []
 |  selectNamespacesByOpenNamespaces (namespacesList: NAMESPACE_SET list,
                                      namespaces: NAMESPACE_SET)
    : NAMESPACE list =
    let
        val matches = intersectNamespaces (head (namespacesList), namespaces)
    in
        case matches of
            [] => selectNamespacesByOpenNamespaces (tail (namespacesList), namespaces)
          | _ => matches
    end

fun ribSearch (rib: Ast.RIB, 
               namespaces: NAMESPACE_SET, 
               identifier: IDENTIFIER)
    : (Ast.RIB * NAMESPACE_SET) option =
    case List.filter (fn ns => hasFixture rib (Ast.PropName {ns=ns,id=identifier})) namespaces of
        [] => NONE
      | m => SOME (rib, m)

fun ribListSearch ([], _, _) = NONE
  | ribListSearch (ribs: Ast.RIBS, 
                   namespaces: NAMESPACE_SET, 
                   identifier: IDENTIFIER)
    : (Ast.RIBS * NAMESPACE_SET) option =
    let
        val rib = head (ribs)
        val matches = ribSearch (rib, namespaces, identifier)
    in
        case matches of
            NONE => ribListSearch (tail (ribs), namespaces, identifier)
          | SOME (_, m) => SOME (ribs, m)
    end

fun getInstanceBindingNamespaces (class: CLASS, 
                                  identifier: IDENTIFIER,
                                  namespaces: NAMESPACE_SET)
    : NAMESPACE_SET =
    (* 
     * get the namespaces of names that have a certain identifier
     * and any of a certain set of namespaces, and are bound in 
     * instances of a certain class. 
     *)
    (* INFORMATIVE *)
    (* FIXME: implement! *)
    namespaces

fun selectNamespacesByClass ([], namespaces, _) = namespaces
 |  selectNamespacesByClass (classes: CLASS list, 
                             namespaces: NAMESPACE_SET, 
                             identifier: IDENTIFIER)
    : NAMESPACE list =
    let
        val class = head (classes)
        val bindingNamespaces = getInstanceBindingNamespaces (class, identifier, namespaces)
        val matches = intersectNamespaces (bindingNamespaces, namespaces)
    in
        case matches of
            [] => selectNamespacesByClass (tail (classes), namespaces, identifier)
          | _ => matches
    end

fun findName (ribs: Ast.RIBS, identifier: IDENTIFIER, openNamespaces: OPEN_NAMESPACES, rootRib: Ast.RIB option)
    : (Ast.RIBS * NAME) option =
    let
        val namespaces = List.concat (openNamespaces)
        val matches = ribListSearch (ribs, namespaces, identifier)
    in
        case matches of
            NONE => NONE
          | SOME (ribs, namespace :: []) => SOME (ribs, {ns=namespace, id=identifier})
          | SOME (ribs, namespaces') =>
            let
                val matches' = selectNamespacesByOpenNamespaces (openNamespaces, namespaces')
            in
                case matches' of
                    namespace :: [] => SOME (ribs, {ns=namespace, id=identifier})
                  | [] => NONE
                  | _ =>
                    let
                        (* FIXME: we don't know how to map from a rib back to the declaring class. Bleah. *)
                        val classList = [] (* inheritedClassesOf (ribs) *)
                        val matches'' = selectNamespacesByClass (classList, namespaces, identifier)
                    in 
                        case matches'' of
                            namespace :: [] => SOME (ribs, {ns=namespace, id=identifier})
                          | [] => raise (LogErr.NameError "internal error")
                          | _ => 
                            if length ribs = 1
                            then 
                                (case rootRib of 
                                     NONE => raise (LogErr.NameError "ambiguous reference")
                                   | SOME rr => 
                                     (case selectNamespacesByRootRib (identifier, matches'', rr) of
                                          namespace :: [] => SOME (ribs, {ns=namespace, id=identifier})
                                        | [] => raise (LogErr.NameError "internal error")
                                        | _ => raise (LogErr.NameError "ambiguous reference")))
                            else
                                raise (LogErr.NameError "ambiguous reference")
                    end
            end
    end





end
