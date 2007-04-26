(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)

(* The Multiname Algorithm *)

structure Multiname = struct

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[multiname] " :: ss) else ()
fun error ss = LogErr.nameError ss

fun resolve (mname:Ast.MULTINAME)
		    (curr:'a)
		    (nameExists:(('a * Ast.NAME) -> bool))
		    (getParent:('a -> ('a option)))
    : ('a * Ast.NAME) option =
    let     
        val _ = trace ["resolving multiname ", LogErr.multiname mname]
        val id = (#id mname)
		 
        (* Try each namespace in the set and accumulate matches. *)
		 
        fun tryName (matches:Ast.NAME list) [] = matches
          | tryName (matches:Ast.NAME list) (x::xs) : Ast.NAME list =
            let 
                val n = { ns=x, id=id } 
                val _ = trace ["trying name ", LogErr.name n]
            in case x of
                Ast.LimitedNamespace (ident,ns) =>
                    if id=ident
                    then
                        let
                            val n = {ns=ns,id=id}
                        in
                            (trace ["limited match ",Ustring.toString id];
                            if nameExists (curr, n)
                            then tryName (n::matches) xs
                            else tryName matches xs)
                        end
                    else 
                        tryName matches xs  (* skip it *)
              | _ =>
                    if nameExists (curr, n)
                    then tryName (n::matches) xs
                    else tryName matches xs
            end


        (*
	     * Try each of the nested namespace sets in turn to see
         * if there is a match. Raise an exception if there is
         * more than one match. Continue up to "parent" 
         * if there are none 
	     *)
	    
        fun tryMultiname [] = NONE  
          | tryMultiname (x::xs:Ast.NAMESPACE list list) : Ast.NAME option = 
            let 
                val matches = tryName [] x
            in case matches of
                   n :: [] => (trace ["resolved to specific name: ", LogErr.name n];
                               SOME n)
                 | [] => tryMultiname xs
                 | matches  => (List.app (fn m => trace ["matched:", LogErr.name m]) matches;
                                error ["ambiguous reference ", 
					                   LogErr.multiname mname])
            end
    in
        case tryMultiname (#nss mname) of
            SOME n => SOME (curr, n)
          | NONE => 
	        (case getParent curr of
		         NONE => (trace ["exhausted search for ", LogErr.multiname mname]; 
                          NONE)
	           | SOME parent => (trace ["moving to parent"];
                                 resolve mname parent nameExists getParent))
    end
end

