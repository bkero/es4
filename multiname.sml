(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)

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

