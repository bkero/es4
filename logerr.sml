structure LogErr = struct

fun posToString {file, line} =
    file ^ ":" ^ (Int.toString line)

val (pos:(Ast.POS option) ref) = ref NONE
fun setPos (p:Ast.POS option) = pos := p

val (lastReported:(Ast.POS option) ref) = ref NONE

fun log ss = 
    (if not ((!lastReported) = (!pos))
     then 
	 ((case !pos of 
	      NONE => ()
	    | SOME p => TextIO.print ("[posn] " ^ (posToString p) ^ "\n"));
	  lastReported := (!pos))
     else ();
     List.app TextIO.print ss; 
     TextIO.print "\n")

fun error ss = case !pos of 
		   NONE => log ("**ERROR** (unknown location)" :: ss)
		 | SOME p => log ("**ERROR** (near " :: (posToString p) :: ") " :: ss)

fun namespace (ns:Ast.NAMESPACE) = 
    case ns of 
        Ast.Intrinsic=> "[namespace intrinsic]"
      | Ast.OperatorNamespace=> "[namespace operator]"
      | Ast.Private i=> "[namespace private " ^ i ^ "]"
      | Ast.Protected i=> "[namespace protected " ^ i ^ "]"
      | Ast.Public i => "[namespace public " ^ i ^ "]"
      | Ast.Internal i => "[namespace internal " ^ i ^ "]"
      | Ast.UserNamespace i => "[namespace user " ^ i ^ "]"
      | Ast.AnonUserNamespace i => "[namespace user anon #" ^ (Int.toString i) ^ "]"
      | Ast.LimitedNamespace (i,n) => "[namespace limited " ^ i ^ " => " ^ (namespace n) ^ "]"

fun name ({ns,id}:Ast.NAME) = (namespace ns) ^ "::" ^ id ^ " "

fun fname (n:Ast.FIXTURE_NAME) = 
    case n of 
	Ast.TempName n => "<temp " ^ (Int.toString n) ^ ">"			  
      | Ast.PropName n => name n

fun multiname (mn:Ast.MULTINAME) = 
    case (#nss mn) of 
	[] => (String.concat ["{multiname: NO NAMESPACE :: ", (#id mn), "}"])
      | _ => String.concat
		 (["{multiname: "] @ (map String.concat
		  (List.map (List.map (fn ns => name {ns = ns, id = (#id mn)})) (#nss mn)) @
		  ["}"]))
	     
exception LexError
exception ParseError
exception NameError
exception DefnError
exception VerifyError
exception EvalError
exception MachError
exception HostError
exception UnimplError

fun lexError ss = 
    (error ("lexError " :: ss); 
     raise LexError)

fun parseError ss = 
    (error ("parseError " :: ss); 
     raise ParseError)

fun nameError ss = 
    (error ("nameError " :: ss); 
     raise NameError)

fun defnError ss = 
    (error ("defnError " :: ss); 
     raise DefnError)

fun verifyError ss = 
    (error ("verifyError " :: ss); 
     raise VerifyError)

fun evalError ss = 
    (error ("evalError " :: ss); 
     raise EvalError)

fun machError ss = 
    (error ("machError " :: ss); 
     raise MachError)

fun hostError ss = 
    (error ("hostError " :: ss); 
     raise HostError)

fun unimplError ss = 
    (error ("unimplError " :: ss); 
     raise UnimplError)

fun internalError ss = 
    (error ("internalError " :: ss); 
     raise UnimplError)

end
