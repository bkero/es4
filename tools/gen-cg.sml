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

structure GenCG =
struct

(* 
 * This file generates a static call graph for the input file.
 *)

open MLAst

fun elide id = 
    List.exists 
	(fn x => x = id) 
	
	[ "error", "throwTypeErr", 

	  "reportProfile",

	  "o",

	  "Name", "Multiname",

	  "NONE", "SOME", "EQUAL", "LESS", "GREATER", "pow", "mod", "min",
	  "!", "+", "-", "/", "*", "<", ">", ">=", "<=", "=", "<<",
	  ">>", "~>>", "^", "@",


	  "take", "drop", "length", "take", "tabulate", "last", "app",
	  "map", "zip", "nth", "::", "find", "exists",


	  "andb", "orb", "xorb", 

	  "push", "pop",

	  "not", "orelse", "andalso", 

	  "ref", ":=", 
	  
	  "valOf", "trace", "log", "null"
	]
     
fun patVars (IDpat id) = [id]
  | patVars (TYPEDpat (pat, ty)) = patVars pat
  | patVars (RECORDpat (subpats,_)) = 
    let
	fun subPat (_, p) = patVars p
    in
	List.concat (List.map subPat subpats)
    end
  | patVars (LISTpat (ps, p)) = 
    let
	val subs = List.concat (List.map patVars ps)
    in
	case p of 
	    NONE => subs
	  | SOME s => subs @ (patVars s)
    end     
  | patVars (TUPLEpat ps) = List.concat (List.map patVars ps)
  | patVars (VECTORpat ps) = List.concat (List.map patVars ps)
  | patVars (CONSpat (_, SOME p)) = patVars p
  | patVars _ = []

fun walkClause (entry:string) (locals:string list) (CLAUSE (pats,_,exp)) = 
    walkExp entry ((List.concat (List.map patVars pats))@locals) exp

and walkValBind (entry:string) ((VALbind (pat, exp)), locals) = 
    let
	val locals = (patVars pat) @ locals
    in
	walkExp entry locals exp;
	locals
    end

and walkFunBind (entry:string) ((FUNbind (id, clauses)), locals) = 
    let
	val locals = id :: locals
    in
	List.app (walkClause entry locals) clauses;
	locals
    end

and walkDecl (entry:string) (locals:string list) (VALdecl valbinds) = List.foldl (walkValBind entry) locals valbinds
  | walkDecl (entry:string) (locals:string list) (FUNdecl funbinds) = List.foldl (walkFunBind entry) locals funbinds
  | walkDecl (entry:string) (locals:string list) (MARKdecl (_, d)) = walkDecl entry locals d
  | walkDecl (entry:string) (locals:string list) _ = locals

and walkExp (entry:string) (locals:string list) (APPexp (a,b)) = (walkExp entry locals a; walkExp entry locals b)
  | walkExp (entry:string) (locals:string list) (TYPEDexp (exp, _)) = walkExp entry locals exp
  | walkExp (entry:string) (locals:string list) (MARKexp (_, exp)) = walkExp entry locals exp
  | walkExp (entry:string) (locals:string list) (CASEexp (exp, clauses)) = (walkExp entry locals exp; 
																			List.app (walkClause entry locals) clauses)
  | walkExp (entry:string) (locals:string list) (HANDLEexp (exp, clauses)) = (walkExp entry locals exp; 
																			  List.app (walkClause entry locals) clauses)
  | walkExp (entry:string) (locals:string list) (TUPLEexp exps) = List.app (walkExp entry locals) exps
  | walkExp (entry:string) (locals:string list) (RECORDexp idexps) = List.app (fn (id, exp) => (walkExp entry locals exp)) idexps
  | walkExp (entry:string) (locals:string list) (LISTexp (exps, eo)) = (List.app (walkExp entry locals) exps;
																		(Option.app (walkExp entry locals) eo))
  | walkExp (entry:string) (locals:string list) (SEQexp exps) = List.app (walkExp entry locals) exps
  | walkExp (entry:string) (locals:string list) (LAMBDAexp clauses) = List.app (walkClause entry locals) clauses
  | walkExp (entry:string) (locals:string list) (RAISEexp exp) = walkExp entry locals exp
  | walkExp (entry:string) (locals:string list) (IFexp (a,b,c)) = List.app (walkExp entry locals) [a,b,c]
  | walkExp (entry:string) (locals:string list) (LETexp (decls, exps)) = 
    let
	val locals = List.foldl (fn (decl, locals) => walkDecl entry locals decl) locals decls
    in
	List.app (walkExp entry locals) exps
    end
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Ast"], _))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Name"], _))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["LogErr"], _))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Ustring"], _))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Boolean"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Double"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Decimal"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "String"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Namespace"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Class"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Interface"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Function"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Type"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "NativeFunction"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Scope"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "WithScope"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "GlobalScope"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "InstanceScope"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "ActivationScope"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "TempScope"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "TypeArgScope"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "UninitTemp"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "ValTemp"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "TypeVarProp"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "TypeProp"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "ValProp"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Obj"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Object"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Undef"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "Null"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "ObjectTag"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "ArrayTag"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "FunctionTag"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "ClassTag"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (["Mach"], "NoTag"))) = ()
  | walkExp (entry:string) (locals:string list) (IDexp (IDENT (_, id))) = 
    if elide id orelse List.exists (fn x => id = x) locals
    then ()
    else List.app print [entry, " -> ", id, "\n"]
  | walkExp _ _ _ = ()

	 
fun genFile (infile:string) 
    : unit = 
    let
	(* Why exactly we're using MLRISC, I do not know. *)
	val mlast = MLParser.load infile
	fun dig (MARKdecl (_, d)) = dig d
	  | dig (STRUCTUREdecl (_, _, _, DECLsexp decls)) = List.app dig decls
	  | dig (FUNdecl binds) = (List.app (fn (FUNbind (id, clauses)) => List.app (walkClause id []) clauses) binds)
	  | dig (SEQdecl decls) = List.app dig decls
	  | dig (LOCALdecl (d0, d1)) = (List.app dig d0; List.app dig d1)
	  | dig _ = ()
    in
	List.app dig mlast
    end
	
	
fun main (argv0:string, argvRest:string list) =	
    case argvRest of 
	[infile] => 
	(genFile infile; 
	 0)
      | _ => (print ("usage: " 
		     ^ argv0 
		     ^ " <infile>\n"); 
	      1)
end
