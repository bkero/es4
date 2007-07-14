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

(* The Multiname Algorithm *)

structure Multiname = struct

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[multiname] " :: ss) else ()
fun error ss = LogErr.nameError ss
fun fmtName n = if (!doTrace) then LogErr.name n else ""
fun fmtMultiname n = if (!doTrace) then LogErr.multiname n else ""

fun resolve (mname:Ast.MULTINAME)
		    (curr:'a)
            (matchNamespaces:('a -> Ast.IDENT -> (Ast.NAMESPACE list) -> Ast.NAME list))
		    (getParent:('a -> ('a option)))
    : ('a * Ast.NAME) option =
    let
        val _ = trace ["resolving multiname ", fmtMultiname mname]
        val id = (#id mname)

        (*
	     * Try each of the nested namespace sets in turn to see
         * if there is a match. Raise an exception if there is
         * more than one match. Continue up to "parent"
         * if there are none
	     *)

        fun tryMultiname [] = NONE
          | tryMultiname (x::xs:Ast.NAMESPACE list list) : Ast.NAME option =
            let
                val matches = matchNamespaces curr id x
            in case matches of
                   n :: [] => (trace ["resolved to specific name: ", fmtName n];
                               SOME n)
                 | [] => tryMultiname xs
                 | matches  => (List.app (fn m => trace ["matched:", fmtName m]) matches;
                                error ["ambiguous reference ",
					                   fmtMultiname mname])
            end
    in
        case tryMultiname (#nss mname) of
            SOME n => SOME (curr, n)
          | NONE =>
	        (case getParent curr of
		         NONE => (trace ["exhausted search for ", fmtMultiname mname];
                          NONE)
	           | SOME parent => (trace ["moving to parent"];
                                 resolve mname parent matchNamespaces getParent))
    end


fun matchFixtures  (fixtures:Ast.FIXTURES)
                   (searchId:Ast.IDENT)
                   (nss:Ast.NAMESPACE list)
    : Ast.NAME list =
    let
        fun matchFixture (fxn:Ast.FIXTURE_NAME,_) : Ast.NAME option =
            case fxn of
                Ast.TempName _ => NONE
              | Ast.PropName n =>
                let
                    val {id,ns} = n
                    fun matchNS candidateNS =
                        case candidateNS of
                            Ast.LimitedNamespace (ident,limNS) =>
                            if id = ident
                            then ns = limNS
                            else false
                          | _ => ns = candidateNS
                in
                    trace ["considering fixture: ", LogErr.fname fxn];
                    if searchId = id andalso (List.exists matchNS nss)
                    then SOME n
                    else NONE
                end
    in
        List.mapPartial matchFixture fixtures
    end

fun resolveInRibs (mname:Ast.MULTINAME)
                  (env:Ast.RIBS)
    : (Ast.RIBS * Ast.NAME) option =
    let
        fun f env ident nss = matchFixtures (List.hd env) ident nss
    in
        case resolve mname env f List.tl of
            SOME (env,n) => SOME (List.hd env, n)
          | NONE => NONE
    end

fun resolveInFixtures (mname:Ast.MULTINAME)
                      (env:'a)
                      (getEnvFixtures:('a -> Ast.FIXTURES))
                      (getEnvParent:('a -> ('a option)))
    : (Ast.FIXTURES * Ast.NAME) option =
    let
        fun f env ident nss = matchFixtures (getEnvFixtures env) ident nss
    in
        case resolve mname env f getEnvParent of
            SOME (env,n) => SOME (getEnvFixtures env, n)
          | NONE => NONE
    end

end

