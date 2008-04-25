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

end
