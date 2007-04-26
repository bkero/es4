structure LogErr = struct

fun posToString {file, span, sm, post_newline} =
    let val (pos1, pos2) = span
    in
        file ^ ":" ^
        (Int.toString (StreamPos.lineNo sm pos1)) ^ "." ^ (Int.toString (StreamPos.colNo sm pos1)) ^ "-" ^
        (Int.toString (StreamPos.lineNo sm pos2)) ^ "." ^ (Int.toString (StreamPos.colNo sm pos2))
    end

val (pos:(Ast.POS option) ref) = ref NONE
fun setPos (p:Ast.POS option) = pos := p

val (lastReported:(Ast.POS option) ref) = ref NONE

fun pos_equal (NONE, NONE) = true
  | pos_equal (SOME (p:Ast.POS), SOME (q:Ast.POS)) =
    ((#file p) = (#file q)) andalso ((#span p) = (#span q))
  | pos_equal (_, _) = false

fun log ss = 
    let
        val pos_changed = not (pos_equal (!lastReported, !pos))
    in
        if pos_changed
        then 
            ((case !pos of 
              NONE => ()
            | SOME p => TextIO.print ("[posn] " ^ (posToString p) ^ "\n"));
            lastReported := (!pos))
        else ();
        List.app TextIO.print ss;
        TextIO.print "\n"
    end

fun error ss = case !pos of 
		   NONE => log ("**ERROR** (unknown location)" :: ss)
		 | SOME p => log ("**ERROR** (near " :: (posToString p) :: ") " :: ss)

fun namespace (ns:Ast.NAMESPACE) = 
    case ns of 
        Ast.Intrinsic=> "[namespace intrinsic]"
      | Ast.OperatorNamespace=> "[namespace operator]"
      | Ast.Private i=> "[namespace private " ^ (Ustring.toString i) ^ "]"
      | Ast.Protected i=> "[namespace protected " ^ (Ustring.toString i) ^ "]"
      | Ast.Public i => "[namespace public " ^ (Ustring.toString i) ^ "]"
      | Ast.Internal i => "[namespace internal " ^ (Ustring.toString i) ^ "]"
      | Ast.UserNamespace i => "[namespace user " ^ (Ustring.toString i) ^ "]"
      | Ast.AnonUserNamespace i => "[namespace user anon #" ^ (Int.toString i) ^ "]"
      | Ast.LimitedNamespace (i,n) => "[namespace limited " ^ (Ustring.toString i) ^ " => " ^ (namespace n) ^ "]"

fun name ({ns,id}:Ast.NAME) = (namespace ns) ^ "::" ^ (Ustring.toString id) ^ " "

fun fname (n:Ast.FIXTURE_NAME) = 
    case n of 
	Ast.TempName n => "<temp " ^ (Int.toString n) ^ ">"			  
      | Ast.PropName n => name n

fun multiname (mn:Ast.MULTINAME) = 
    case (#nss mn) of 
	[] => (String.concat ["{multiname: NO NAMESPACE :: ", Ustring.toString (#id mn), "}"])
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
