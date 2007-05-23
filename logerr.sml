structure LogErr = struct

fun locToString {file, span, post_newline} =
    let
        val ({line=line1, col=col1}, {line=line2, col=col2}) = span
    in
        file ^ ":" ^
        (Int.toString line1) ^ "." ^ (Int.toString col1) ^ "-" ^
        (Int.toString line2) ^ "." ^ (Int.toString col2)
    end

val (loc:(Ast.LOC option) ref) = ref NONE
fun setLoc (p:Ast.LOC option) = loc := p

val (lastReported:(Ast.LOC option) ref) = ref NONE

fun log ss = 
    let
        val loc_changed = not (!lastReported = !loc)
    in
        if loc_changed
        then 
            ((case !loc of 
              NONE => ()
            | SOME l => TextIO.print ("[locn] " ^ (locToString l) ^ "\n"));
            lastReported := (!loc))
        else ();
        List.app TextIO.print ss;
        TextIO.print "\n"
    end

fun locstr ss = 
    case !loc of 
	NONE => String.concat ss
      | SOME l => String.concat (ss @ [" (near ", (locToString l), ")"])

fun error ss = case !loc of 
		   NONE => log ("**ERROR** (unknown location)" :: ss)
		 | SOME l => log ("**ERROR** (near " :: (locToString l) :: ") " :: ss)

fun namespace (ns:Ast.NAMESPACE) = 
    case ns of 
        Ast.Intrinsic=> "[ns intrinsic]"
      | Ast.OperatorNamespace=> "[ns operator]"
      | Ast.Private i=> "[ns private '" ^ (Ustring.toAscii i) ^ "']"
      | Ast.Protected i=> "[ns protected '" ^ (Ustring.toAscii i) ^ "']"
      | Ast.Public i => "[ns public '" ^ (Ustring.toAscii i) ^ "']"
      | Ast.Internal i => "[ns internal '" ^ (Ustring.toAscii i) ^ "']"
      | Ast.UserNamespace i => "[ns user '" ^ (Ustring.toAscii i) ^ "']"
      | Ast.AnonUserNamespace i => "[ns user anon #" ^ (Int.toString i) ^ "]"
      | Ast.LimitedNamespace (i,n) => "[ns limited '" ^ (Ustring.toAscii i) ^ "' => " ^ (namespace n) ^ "]"

fun name ({ns,id}:Ast.NAME) = (namespace ns) ^ "::" ^ (Ustring.toAscii id) ^ " "

fun fname (n:Ast.FIXTURE_NAME) = 
    case n of 
	Ast.TempName n => "<temp " ^ (Int.toString n) ^ ">"			  
      | Ast.PropName n => name n

fun multiname (mn:Ast.MULTINAME) = 
    case (#nss mn) of 
	[] => (String.concat ["{multiname: NO NAMESPACE :: ", Ustring.toAscii (#id mn), "}"])
      | _ => String.concat
		 (["{multiname: "] @ (map String.concat
		  (List.map (List.map (fn ns => name {ns = ns, id = (#id mn)})) (#nss mn)) @
		  ["}"]))
	     
fun join sep ss = 
    case ss of 
        [] => ""
      | [x] => x
      | x :: xs => x ^ sep ^ (join sep xs)

exception LexError of string
exception ParseError of string
exception NameError of string
exception DefnError of string
exception VerifyError of string
exception EvalError of string
exception MachError of string
exception HostError of string
exception UnimplError of string

fun lexError ss = 
     raise LexError (locstr ss)

fun parseError ss = 
     raise ParseError (locstr ss)

fun nameError ss = 
     raise NameError (locstr ss)

fun defnError ss = 
     raise DefnError (locstr ss)

fun verifyError ss = 
     raise VerifyError (locstr ss)

fun evalError ss = 
     raise EvalError (locstr ss)

fun machError ss = 
     raise MachError (locstr ss)

fun hostError ss = 
     raise HostError (locstr ss)

fun unimplError ss = 
     raise UnimplError (locstr ss)

fun internalError ss = 
     raise UnimplError (locstr ss)

end
