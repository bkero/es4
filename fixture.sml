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
 * Operations on FIXTUREs and FIXTURE_MAPs
 * ----------------------------------------------------------------------------- *)

fun findFixture (b:Ast.FIXTURE_MAP) 
                (n:Ast.FIXTURE_NAME) 
    : Ast.FIXTURE option = 
    let 
        fun search [] = NONE
          | search ((k,v)::bs) = 
            if k = n 
            then SOME v
            else search bs
    in
        search b    
    end

fun getFixture (b:Ast.FIXTURE_MAP) 
               (n:Ast.FIXTURE_NAME) 
    : Ast.FIXTURE = 
    case findFixture b n of
        NONE => LogErr.fixtureError ["fixture binding not found: ", (LogErr.fname n)]
      | SOME v => v


fun hasFixture (b:Ast.FIXTURE_MAP) 
               (n:Ast.FIXTURE_NAME) 
    : bool = 
    case findFixture b n of
        NONE => false
      | SOME v => true


fun replaceFixture (b:Ast.FIXTURE_MAP) 
                   (n:Ast.FIXTURE_NAME) 
                   (v:Ast.FIXTURE)
    : Ast.FIXTURE_MAP = 
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


type TYEQ = (Ast.TYPE -> Ast.TYPE -> bool)


fun mergeVirtuals (tyeq:TYEQ)
                  (fName:Ast.FIXTURE_NAME)
                  (vnew:Ast.VIRTUAL_VAL_FIXTURE)
                  (vold:Ast.VIRTUAL_VAL_FIXTURE) =
    let
        val ty = case ((#ty vold), (#ty vnew)) of
                     (Ast.AnyType, tnew) => tnew
                   | (told, Ast.AnyType) => told
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
                  ((newName:Ast.FIXTURE_NAME, newFix:Ast.FIXTURE),oldFixtureMap:Ast.FIXTURE_MAP)
    : Ast.FIXTURE_MAP =
    if hasFixture oldFixtureMap newName
    then
        case (newFix, getFixture oldFixtureMap newName) of
            (Ast.VirtualValFixture vnew,
             Ast.VirtualValFixture vold) =>
            replaceFixture oldFixtureMap newName
                           (Ast.VirtualValFixture
                                (mergeVirtuals tyeq newName vnew vold))
          | (Ast.ValFixture new, Ast.ValFixture old) =>
            if (tyeq (#ty new) (#ty old)) 
               andalso (#writable new) = (#writable old)
            then oldFixtureMap
            else error ["incompatible redefinition of fixture name: ", LogErr.fname newName]
          | (Ast.MethodFixture new, Ast.MethodFixture old) =>
            replaceFixture oldFixtureMap newName (Ast.MethodFixture new) (* FIXME: types *)
          | (Ast.MethodFixture new, Ast.ValFixture old) =>
            replaceFixture oldFixtureMap newName (Ast.MethodFixture new) (* FIXME: types *)
          | (Ast.ValFixture new, Ast.MethodFixture old) =>
            replaceFixture oldFixtureMap newName (Ast.ValFixture new) (* FIXME: types *)
          | _ => error ["mergeFixtures: redefining fixture name: ", LogErr.fname newName]
    else
        (newName,newFix) :: oldFixtureMap


fun mergeFixtureMaps (tyeq:TYEQ)
              (oldFixtureMap:Ast.FIXTURE_MAP)
              (additions:Ast.FIXTURE_MAP)
    : Ast.FIXTURE_MAP = 
    List.rev (List.foldl (mergeFixtures tyeq) (List.rev oldFixtureMap) additions)


fun printFixture ((n:Ast.FIXTURE_NAME), (f:Ast.FIXTURE)) = 
    let
	val fs = case f of 
		     Ast.NamespaceFixture _ => "[namespace]"
		   | Ast.ClassFixture _ => "[class]"
		   | Ast.InterfaceFixture _ => "[interface]"
		   | Ast.TypeVarFixture _ => "[typeVar]"
		   | Ast.TypeFixture (ids,t) => ("[type] lam(" ^  (LogErr.identList ids) ^ ") " ^LogErr.ty t)
		   | Ast.MethodFixture _ => "[method]"
		   | Ast.ValFixture _ => "[val]"
		   | Ast.VirtualValFixture _ => "[virtualVal]"
    in
	case n of
	    Ast.TempName n => log ["temp #", Int.toString n, " -> ", fs]
      | Ast.PropName n => log [LogErr.name n, " -> ", fs]
    end


fun printFixtureMap (fixtureMap:Ast.FIXTURE_MAP) =
    List.app printFixture fixtureMap


(* -----------------------------------------------------------------------------
 * Static variant of name-resolution algorithm: mirrors Mach.findName
 * ----------------------------------------------------------------------------- *)

type IDENTIFIER = Ast.IDENTIFIER
type NAMESPACE = Ast.NAMESPACE

type CLASS = Ast.CLASS
type NAME = Ast.NAME

type NAMESPACE_SET = NAMESPACE list
type OPEN_NAMESPACES = NAMESPACE_SET list 

fun intersectNamespaces (ns1: NAMESPACE_SET, ns2: NAMESPACE_SET)
    : NAMESPACE_SET =
    (* compute the intersection of two NAMESPACE_SETs *)
    (* INFORMATIVE *)
    List.filter (fn n1 => List.exists (fn n2 => n1 = n2) ns2) ns1

fun selectNamespacesByGlobalNames (identifier: IDENTIFIER,
                                   namespaces: NAMESPACE_SET,
                                   globalNames: Ast.NAME_SET)
    : NAMESPACE_SET = 
    let
        fun hasName ns = List.exists (fn gn => gn = {id=identifier, ns=ns }) globalNames
    in
        List.filter hasName namespaces
    end

fun selectNamespacesByOpenNamespaces ([], _) = []

 |  selectNamespacesByOpenNamespaces (namespacesList : NAMESPACE_SET list,
                                      namespaces     : NAMESPACE_SET)
    : NAMESPACE list =
    let
        val matches = intersectNamespaces (hd namespacesList, namespaces)
    in
        case matches of

            [] 
            => selectNamespacesByOpenNamespaces (tl namespacesList, namespaces)

          | _
            => matches
    end

fun fixtureMapSearch (fixtureMap        : Ast.FIXTURE_MAP, 
               namespaces : NAMESPACE_SET, 
               identifier : IDENTIFIER)
    : (Ast.FIXTURE_MAP * NAMESPACE_SET) option =
    case List.filter (fn ns => 
                         hasFixture fixtureMap (Ast.PropName {ns=ns, id=identifier}))
                     namespaces of
        [] => NONE
      | m  => SOME (fixtureMap, m)

fun fixtureMapListSearch ([], _, _) = NONE

  | fixtureMapListSearch (fixtureMaps       : Ast.FIXTURE_MAPS, 
                   namespaces : NAMESPACE_SET, 
                   identifier : IDENTIFIER)
    : (Ast.FIXTURE_MAPS * NAMESPACE_SET) option =
    case fixtureMapSearch (hd fixtureMaps, namespaces, identifier) of
        NONE 
        => fixtureMapListSearch (tl fixtureMaps, namespaces, identifier)

      | SOME (_, m) 
        => SOME (fixtureMaps, m)

fun getInstanceBindingNamespaces (fixtureMap: Ast.FIXTURE_MAP, 
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

 |  selectNamespacesByClass (instanceFixtureMaps : Ast.FIXTURE_MAPS,
                             namespaces   : NAMESPACE_SET, 
                             identifier   : IDENTIFIER)
    : NAMESPACE list =
    let
        val fixtureMap = hd instanceFixtureMaps
        val bindingNamespaces = 
            getInstanceBindingNamespaces (fixtureMap, identifier, namespaces)
        val matches = 
            intersectNamespaces (bindingNamespaces, namespaces)
    in
        case matches of

            [] 
            => selectNamespacesByClass (tl instanceFixtureMaps, 
                                        namespaces, 
                                        identifier)

          | _ 
            => matches
    end

fun selectNamespaces (identifier     : IDENTIFIER, 
                      namespaces     : NAMESPACE_SET, 
                      instanceFixtureMaps   : Ast.FIXTURE_MAPS, 
                      openNamespaces : OPEN_NAMESPACES)
    : NAMESPACE_SET =
    let
        val openNamespaceSet = List.concat (openNamespaces)
    in
        case namespaces of

            _ :: [] 
            => namespaces

          | _ =>
            let
                val matches' = 
                    selectNamespacesByClass (instanceFixtureMaps, 
                                             openNamespaceSet, 
                                             identifier)
            in
                case matches' of
                    [] 
                    => raise (LogErr.NameError "internal error")

                  | [_]
                    => matches'

                  | _ =>
                    let
                        val matches'' = 
                            selectNamespacesByOpenNamespaces (openNamespaces,
                                                              namespaces)
                    in 
                        case matches'' of

                            [] 
                            => raise (LogErr.NameError "internal error")

                          | _ 
                            => matches''
                    end
            end
    end

fun resolveQualifiedName (fixtureMaps          : Ast.FIXTURE_MAPS) 
                         (identifier    : IDENTIFIER) 
                         (namespaceExpr : Ast.NAMESPACE_EXPRESSION)
    : (Ast.FIXTURE_MAPS * NAME * Ast.FIXTURE) =
    let
        val ns = resolveNamespaceExpr fixtureMaps namespaceExpr
        val name = { ns = ns, id = identifier }
        fun search (r::rs) = if hasFixture r (Ast.PropName name) then
                                 (r::rs)
                             else
                                 search rs
          | search [] = []
    in
        case (search fixtureMaps) of 
            [] 
            => error ["qualified name not present in fixtureMaps: ", LogErr.name name]

          | fixtureMaps'
            => (fixtureMaps', name, getFixture (hd fixtureMaps') (Ast.PropName name))
    end


and resolveUnqualifiedName (fixtureMaps           : Ast.FIXTURE_MAPS) 
                           (identifier     : IDENTIFIER) 
                           (openNamespaces : OPEN_NAMESPACES)
    : (Ast.FIXTURE_MAPS * NAME) option =
    let
        val namespaces = List.concat (openNamespaces)
        val matches = fixtureMapListSearch (fixtureMaps, namespaces, identifier)
    in
        case matches of
            NONE
            => NONE

          | SOME (fixtureMaps, [namespace]) 
            => SOME (fixtureMaps, {ns=namespace, id=identifier})

          | SOME (fixtureMaps, namespaces) 
            => case selectNamespaces (identifier, 
                                      namespaces, 
                                      [], 
                                      openNamespaces) of

                   [namespace] 
                   => SOME (fixtureMaps, {ns=namespace, id=identifier})

                 | ns::nss 
                   => error ["ambiguous reference: ", Ustring.toAscii identifier]
    end

(*

    Resolve a static name expression.

*)

and resolveNameExpr (fixtureMaps : Ast.FIXTURE_MAPS) 
                    (ne   : Ast.NAME_EXPRESSION) 
    : (Ast.FIXTURE_MAPS * Ast.NAME * Ast.FIXTURE) = 

    case ne of
        Ast.QualifiedName { namespace, identifier } 
        => resolveQualifiedName fixtureMaps identifier namespace

      | Ast.UnqualifiedName { identifier, openNamespaces, ... } 
        => case (resolveUnqualifiedName fixtureMaps identifier openNamespaces) of

            NONE 
            => error ["unresolved name ", LogErr.nameExpr ne]

          | SOME ([], _)
            => error ["unresolved name ", LogErr.nameExpr ne]

          | SOME ([fixtureMap], name) 
            => ( reserveNames name openNamespaces ;
                 ([fixtureMap], name, getFixture fixtureMap (Ast.PropName name)) )

          | SOME (fixtureMaps, name) 
            => (fixtureMaps, name, getFixture (hd fixtureMaps) (Ast.PropName name))

and reserveNames (name) 
                 (openNamespaces) 
    = (* LDOTS *)
    ()  (* FIXME needs implementation *)

and resolveNamespaceExpr (fixtureMaps:Ast.FIXTURE_MAPS)
                         (nse:Ast.NAMESPACE_EXPRESSION)
    : Ast.NAMESPACE = 
    case nse of
        Ast.Namespace ns => ns
      | Ast.NamespaceName ne => 
        (case resolveNameExpr fixtureMaps ne of
             (_, _, Ast.NamespaceFixture ns) => ns
           | _ => error ["namespace expression resolved to non-namespace fixture: ", LogErr.nsExpr nse])

end
