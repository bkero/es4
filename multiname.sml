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
            (matchNamespaces:('a * Ast.IDENT * (Ast.NAMESPACE list)) -> Ast.NAME list)
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
                val matches = matchNamespaces (curr, id, x)
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
end

