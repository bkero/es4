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

structure GenEnglish =
struct

open MLAst

fun first (q:'a -> 'b option) [] = NONE
  | first (q:'a -> 'b option) (x::xs) = 
    (case q x of NONE => first q xs
	       | SOME r => SOME r)

type context = { locals: string list,
		 indent: int }


fun withLocals ss (ctx:context) = { locals = ss @ (#locals ctx),
				    indent = (#indent ctx)}

fun withCode (ctx:context) = { locals = (#locals ctx),
			       indent = (#indent ctx) }

fun withIndent (ctx:context) n = { locals = (#locals ctx),
				   indent = (#indent ctx) + n }


(* 
 * This module implements a translator from a small subset of SML to 
 * "spec-ese", a small and very dry-sounding subset of english.
 * 
 * You can attempt to translate any SML function you like to spec-ese.
 * If the translator has any trouble with the output, it will mark the 
 * troubling cases in red with question marks. It's very easy to see 
 * where you're using a bit of SML that is too complicated for the
 * translator.
 *
 * While technically we can imagine many of the "unhandled" SML 
 * constructs mapping to something *like* english, the awkwardness 
 * in achieving full compositionality, without the strong scoping
 * and evaluation rules of a formal language like SML make most 
 * forays into extended translation futile: the best english you 
 * can even imagine a genius translator achieving still, at best,
 * reads poorly. 
 * 
 * Thus we concentrate on a small, core (turing complete) fragment of
 * SML, and suggest that for any code you wish to translate, you
 * simply rewrite the code until it lives entirely within this core
 * fragment.
 *)


fun spaces (ctx:context) = 
    List.tabulate ((#indent ctx), (fn _ => "  "))
			   
fun li ctx (content:string list) = 
    ["\n"] @ (spaces ctx) @ ["<li>"] @ content @ ["</li>"]

fun ol ctx (elts:string list list) = 
    ["\n"] @ (spaces ctx) @ ["<ol>\n"]
    @ (List.concat (List.map (li (withIndent ctx 1)) elts))
    @ ["\n"] @ (spaces ctx) @ ["</ol>\n"]

fun span n inner = [" <span class=\"", n, "\">"] @ inner @ ["</span>"]

fun bad str = span "bad" ["?", str, "?"]

fun commaList [x] = x
  | commaList [x,y] = x @ [" and"] @ y
  | commaList (x::xs) = x @ [", "] @ (commaList xs)
  | commaList [] = []

(* 
 * 
 * Converting patterns is a little tricky. 
 * We use them in three contexts: 
 * 
 *   - "Given <definite-pats>, the function "f x y" evalutes to <expr>"
 *   - "Choose the next step by inspecting <expr>:"
 *     "If the inspected value is <indefinite-pat>, then evaluate <expr>
 *   - "Let <definite-pat> be <expr>"
 * 
 *)

fun cvtTy (IDty (IDENT (["Mach"], "SCOPE"))) = [" scope"]
  | cvtTy (IDty (IDENT (["Mach"], "MAGIC"))) = [" magic value"]
  | cvtTy (IDty (IDENT (["Mach"], "OBJ"))) = [" object"]
  | cvtTy (IDty (IDENT (["Mach"], "ATTRS"))) = [" property attribute set"]
  | cvtTy (IDty (IDENT (["Mach"], "VAL"))) = [" value"]
  | cvtTy (IDty (IDENT (["Mach"], "FUN_CLOSURE"))) = [" function closure"]
  | cvtTy (IDty (IDENT (["Mach"], "CLS_CLOSURE"))) = [" class closure"]
  | cvtTy (IDty (IDENT (["Mach"], "IFACE_CLOSURE"))) = [" interface closure"]
  | cvtTy (IDty (IDENT (["Mach"], "DECIMAL_CONTEXT"))) = [" decimal context"]
  | cvtTy (IDty (IDENT (["Mach"], "REGS"))) = [" register set"]
  | cvtTy (IDty (IDENT (["Mach"], "NATIVE_FUNCTION"))) = [" native function"]
  | cvtTy (IDty (IDENT (["Mach"], "OBJ_IDENT"))) = [" object identity"]
  | cvtTy (IDty (IDENT (["Mach"], "PROP"))) = [" property"]
  | cvtTy (IDty (IDENT (["Mach"], "PROP_BINDINGS"))) = [" object properties"]

  | cvtTy (IDty (IDENT (["Ustring"], "STRING"))) = [" string"]
  | cvtTy (IDty (IDENT (["Int32"], "int"))) = [" integer"]
  | cvtTy (IDty (IDENT (["Word32"], "word"))) = [" unsigned integer"]
  | cvtTy (IDty (IDENT (["Real64"], "real"))) = [" double number"]
  | cvtTy (IDty (IDENT (["Decimal"], "DEC"))) = [" decimal number"]

  | cvtTy (IDty (IDENT (["Ast"], "PRAGMA"))) = [" pragma"]
  | cvtTy (IDty (IDENT (["Ast"], "FUNC_NAME_KIND"))) = [" function name kind"]
  | cvtTy (IDty (IDENT (["Ast"], "TY"))) = [" type closure"]
  | cvtTy (IDty (IDENT (["Ast"], "CLS"))) = [" class"]
  | cvtTy (IDty (IDENT (["Ast"], "IFACE"))) = [" interface"]
  | cvtTy (IDty (IDENT (["Ast"], "CTOR"))) = [" constructor"]
  | cvtTy (IDty (IDENT (["Ast"], "FUNC"))) = [" function"]
  | cvtTy (IDty (IDENT (["Ast"], "DEFN"))) = [" definition"]
  | cvtTy (IDty (IDENT (["Ast"], "FUNC_SIG"))) = [" function signature"]
  | cvtTy (IDty (IDENT (["Ast"], "BINDING"))) = [" binding"]
  | cvtTy (IDty (IDENT (["Ast"], "BINDING_IDENT"))) = [" binding identifier"]
  | cvtTy (IDty (IDENT (["Ast"], "INIT_STEP"))) = [" initialization step"]
  | cvtTy (IDty (IDENT (["Ast"], "TYPE_EXPR"))) = [" type expression"]
  | cvtTy (IDty (IDENT (["Ast"], "STMT"))) = [" statement"]
  | cvtTy (IDty (IDENT (["Ast"], "EXPR"))) = [" expression"]
  | cvtTy (IDty (IDENT (["Ast"], "INIT_TARGET"))) = [" initialization target"]
  | cvtTy (IDty (IDENT (["Ast"], "FIXTURE_NAME"))) = [" fixture name"]
  | cvtTy (IDty (IDENT (["Ast"], "IDENT_EXPR"))) = [" identifier expression"]
  | cvtTy (IDty (IDENT (["Ast"], "LITERAL"))) = [" literal"]
  | cvtTy (IDty (IDENT (["Ast"], "BLOCK"))) = [" block"]
  | cvtTy (IDty (IDENT (["Ast"], "FIXTURE"))) = [" fixture"]
  | cvtTy (IDty (IDENT (["Ast"], "HEAD"))) = [" head"]
  | cvtTy (IDty (IDENT (["Ast"], "BINDINGS"))) = [" bindings"]
  | cvtTy (IDty (IDENT (["Ast"], "RIB"))) = [" rib"]
  | cvtTy (IDty (IDENT (["Ast"], "RIBS"))) = [" ribs"]
  | cvtTy (IDty (IDENT (["Ast"], "INITS"))) = [" initializers"]
  | cvtTy (IDty (IDENT (["Ast"], "INSTANCE_TYPE"))) = [" instance type"]
  | cvtTy (IDty (IDENT (["Ast"], "FIELD"))) = [" field"]
  | cvtTy (IDty (IDENT (["Ast"], "FIELD_TYPE"))) = [" field type"]
  | cvtTy (IDty (IDENT (["Ast"], "FUNC_TYPE"))) = [" function type"]
  | cvtTy (IDty (IDENT (["Ast"], "FUNC_DEFN"))) = [" function definition"]
  | cvtTy (IDty (IDENT (["Ast"], "CTOR_DEFN"))) = [" constructor definition"]
  | cvtTy (IDty (IDENT (["Ast"], "VAR_DEFN"))) = [" variable definition"]
  | cvtTy (IDty (IDENT (["Ast"], "NAMESPACE_DEFN"))) = [" namespace definition"]
  | cvtTy (IDty (IDENT (["Ast"], "CLASS_DEFN"))) = [" class definition"]
  | cvtTy (IDty (IDENT (["Ast"], "INTERFACE_DEFN"))) = [" interface definition"]
  | cvtTy (IDty (IDENT (["Ast"], "TYPE_DEFN"))) = [" type definition"]
  | cvtTy (IDty (IDENT (["Ast"], "CLASS_BLOCK"))) = [" class block"]
  | cvtTy (IDty (IDENT (["Ast"], "FOR_ENUM_STATEMENT"))) = [" foreach statement"]
  | cvtTy (IDty (IDENT (["Ast"], "FOR_STMT"))) = [" for statement"]
  | cvtTy (IDty (IDENT (["Ast"], "WHILE_STMT"))) = [" while statement"]
  | cvtTy (IDty (IDENT (["Ast"], "DIRECTIVES"))) = [" directives"]
  | cvtTy (IDty (IDENT (["Ast"], "CASE"))) = [" case"]
  | cvtTy (IDty (IDENT (["Ast"], "CATCH_CLAUSE"))) = [" catch clause"]
  | cvtTy (IDty (IDENT (["Ast"], "FUNC_NAME"))) = [" function name"]
  | cvtTy (IDty (IDENT (["Ast"], "VIRTUAL_VAL_FIXTURE"))) = [" virtual value fixture"]
  | cvtTy (IDty (IDENT (["Ast"], "FRAGMENT"))) = [" fragment"]

  | cvtTy (IDty (IDENT (_, id))) = [" ", id]
  | cvtTy _ = []

fun lint x = [LargeInt.toString x]

fun cvtLit (WORDlit w) =  (lint (Word.toLargeInt w))
  | cvtLit (WORD32lit w) = (lint (Word32.toLargeInt w))
  | cvtLit (INTlit w) = (lint (Int.toLarge w))
  | cvtLit (INT32lit w) = (lint (Int32.toLarge w))
  | cvtLit (INTINFlit w) = (lint (IntInf.toLarge w))
  | cvtLit (STRINGlit s) = ["\""] @ [String.toCString s] @ ["\""]
  | cvtLit (CHARlit c) = ["'"] @ [Char.toString c] @ ["'"]
  | cvtLit (BOOLlit b) = span "const" [Bool.toString b]
  | cvtLit (REALlit s) = [s]

fun cvtPat def pat = 
    let
	fun isVowel c = List.exists (fn x => c = x) 
				    [#"a", #"e", #"i", #"o", #"u"]

	fun conspat (path:id list, id:id)
	    : string option =
	    case (path,id) of
		(["Mach"], "Boolean") => SOME "boolean"
	      | (["Mach"], "Byte") => SOME "byte"
	      | (["Mach"], "UInt") => SOME "unsigned integer"
	      | (["Mach"], "Int") => SOME "integer"
	      | (["Mach"], "Double") => SOME "double number"
	      | (["Mach"], "Decimal") => SOME "decimal number"
	      | (["Mach"], "String") => SOME "string"
	      | (["Mach"], "Namespace") => SOME "namespace"
	      | (["Mach"], "Class") => SOME "class"
	      | (["Mach"], "Interface") => SOME "interface"
	      | (["Mach"], "Function") => SOME "function"
	      | (["Mach"], "Type") => SOME "type"
	      | (["Mach"], "NativeFunction") => SOME "native function"

	      | (["Ast"], "SpecialType") => SOME "special type"
	      | (["Ast"], "UnionType") => SOME "union type"
	      | (["Ast"], "ArrayType") => SOME "array type"
	      | (["Ast"], "TypeName") => SOME "type name"
	      | (["Ast"], "ElementTypeRef") => SOME "element type reference"
	      | (["Ast"], "FieldTypeRef") => SOME "field type reference"
	      | (["Ast"], "FunctionType") => SOME "function type"
	      | (["Ast"], "ObjectType") => SOME "object type"
	      | (["Ast"], "AppType") => SOME "application type"
	      | (["Ast"], "LamType") => SOME "lambda type"
	      | (["Ast"], "NullableType") => SOME "nullability-indication type"
	      | (["Ast"], "InstanceType") => SOME "instance type"

	      | _ => NONE

	fun relevantRecordField (_, (IDpat "loc")) = false
	  | relevantRecordField _ = true
	    
	fun res (CONSpat ((IDENT id), sub)) =
	    let 
		val prefix : string list = 
		    case conspat id of 
			NONE => []
		      | SOME v => 
			if def
			then [" the ", v]
			else 
			    if String.size v > 0 
			       andalso isVowel (String.sub (v,0))
			    then [" an ", v]
			    else [" a ", v]
	    in
		case sub of 
		    NONE => prefix
		  | SOME s => prefix @ (res s)
	    end
	    
	  | res (IDpat id) = span "var" [id]
	  | res (RECORDpat (subpats,_)) =
	    let
		fun subPat (id1, (IDpat id2)) =
		    if id1 = id2
		    then res (IDpat id2)
		    else bad "pat"
			 
		  | subPat _ = 
		    bad "pat"
	    in
		commaList (List.map subPat (List.filter relevantRecordField subpats))
	    end
	  | res (WILDpat) = []
	  | res _ = bad "pat"
    in
	case pat of 
	    (IDpat "NONE") => [" missing"]
	  | (LITpat lit) => cvtLit lit
	  | (CONSpat (IDENT ([], "NONE"), NONE)) => [" missing"]
	  | (CONSpat (IDENT ([], "SOME"), SOME WILDpat)) => [" present"]
	  | (CONSpat (IDENT ([], "SOME"), SOME sub)) => [" present, and is"] @ (res sub)
	  | (TYPEDpat (pat, ty)) => (cvtTy ty) @ (cvtPat def pat)
	  | (WILDpat) => [" any value"]
	  | _ => res pat
    end

fun isDestructuringPat (CONSpat (_, SOME p)) = isDestructuringPat p
  | isDestructuringPat (ASpat (_, p)) = isDestructuringPat p
  | isDestructuringPat (TYPEDpat (p, _)) = isDestructuringPat p
  | isDestructuringPat (LISTpat _) = true
  | isDestructuringPat (TUPLEpat _) = true
  | isDestructuringPat (VECTORpat _) = true
  | isDestructuringPat (RECORDpat _) = true
  | isDestructuringPat _ = false

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

fun relevantApp (IDENT (["Mach"], "Obj")) = false
  | relevantApp (IDENT (["Mach"], "Object")) = false
  | relevantApp (IDENT (["Mach"], "Boolean")) = false
  | relevantApp (IDENT (["Mach"], "Byte")) = false
  | relevantApp (IDENT (["Mach"], "UInt")) = false
  | relevantApp (IDENT (["Mach"], "Int")) = false
  | relevantApp (IDENT (["Mach"], "Double")) = false
  | relevantApp (IDENT (["Mach"], "Decimal")) = false
  | relevantApp (IDENT (["Mach"], "String")) = false
  | relevantApp (IDENT (["Mach"], "Namespace")) = false
  | relevantApp (IDENT (["Mach"], "Class")) = false
  | relevantApp (IDENT (["Mach"], "Function")) = false
  | relevantApp (IDENT (["Mach"], "Type")) = false
  | relevantApp (IDENT (["Mach"], "NativeFunction")) = false
  | relevantApp (IDENT ([], "SOME")) = false
  | relevantApp (IDENT ([], "valOf")) = false
  | relevantApp _ = true


fun relevantExp (APPexp (IDexp (IDENT ([], "log")), _)) = false
  | relevantExp (APPexp (IDexp (IDENT ([], "trace")), _)) = false
  | relevantExp (APPexp (IDexp (IDENT (["LogErr"], _)), _)) = false
  | relevantExp _ = true


fun relevantValBinding (VALbind (WILDpat, exp)) = relevantExp exp
  | relevantValBinding _ = true


fun translateFn (outfile:string)
		(funbind:funbind)
    : unit = 
    let
	val (out:TextIO.outstream) = TextIO.openOut outfile
				     
	val put = List.app (fn s => TextIO.output (out, s))
		  
	fun cvtPats (definite:bool) pats = commaList (List.map (cvtPat definite) pats)

	fun withPatLocals ctx pats = 
	    withLocals (List.concat (List.map patVars pats)) ctx

	fun cvtPatId (TYPEDpat (pat, ty)) = cvtPatId pat
	  | cvtPatId (IDpat id) = span "var" [id]
	  | cvtPatId (LITpat lit) = [" "] @ (cvtLit lit)
	  | cvtPatId _ = bad "patid"

	fun cvtPatIds pats = List.concat (List.map cvtPatId pats)

	fun cvtValBind ctx (VALbind (pat, exp)) =
	    (if isDestructuringPat pat
	     then ["Get "] @ (cvtPat true pat) @ [" from"]
	     else ["Let "] @ (cvtPat true pat) @ [" be"])
	    @ (cvtExp ctx exp)
							
	and valBindPat (VALbind (pat, _)) = pat

	and declPats (VALdecl valbinds) = List.map valBindPat 
						   (List.filter 
							relevantValBinding 
							valbinds)
	  | declPats (MARKdecl (_, d)) = declPats d
	  | declPats _ = []

	and cvtDecl ctx (VALdecl valbinds) = List.map (cvtValBind ctx) (List.filter 
									    relevantValBinding 
									    valbinds)
	  | cvtDecl ctx (FUNdecl funbinds) = List.map (cvtFunBind ctx) funbinds
	  | cvtDecl ctx (MARKdecl (_, d)) = cvtDecl ctx d
	  | cvtDecl _ _ = [bad "decl"]
	    
	and cvtExps' ctx [] : (string list list) = []
	  | cvtExps' ctx [x] = [["The result is"] @ (cvtExp ctx x)]
	  | cvtExps' ctx (x::xs) = (((cvtExp ctx x) @ ["."]) :: (cvtExps ctx xs))

	and cvtExps ctx xs = cvtExps' ctx (List.filter relevantExp xs)

	and cvtIdent ctx id = if List.exists (fn x => x = id) (#locals ctx)
			      then span "var" [id]
			      else span "const" [id]
				   
	and cvtExp ctx (LITexp lit) = [" "] @ (cvtLit lit)
	  | cvtExp ctx (MARKexp (_, exp)) = cvtExp ctx exp
	  | cvtExp ctx (TYPEDexp (exp, _)) = cvtExp ctx exp
	  | cvtExp ctx (APPexp (a,b)) = 
	    let
		val prefix = 
		    case a of 
			IDexp id => if relevantApp id
				    then (cvtExp ctx a)
				    else []
		      | _ => (cvtExp ctx a)
	    in
		span "expr" (prefix @ (cvtExp ctx b))
	    end
	  | cvtExp ctx (CASEexp (exp, clauses)) =
	    let


		fun cvtCaseClause (CLAUSE (pats, _, exp)) = 
		    [" If the inspected value is"]
		    @ (cvtPats false pats)
		    @ [", then the result is specified as"]
		    @ (cvtExp (withIndent ctx 2) exp)
	    in
		[" the result of inspecting"] @ (cvtExp ctx exp) @ [":"]
		 @ (ol (withIndent ctx 1)
		       (List.map cvtCaseClause clauses))
	    end
	  | cvtExp ctx (IDexp (IDENT (["Name"], id))) = 
	    span "name"
		 (case String.tokens (fn c => c = #"_") id of 
		      ["ES4", id] => ["__ES4__::", id]
		    | [ns, id] => [ns, "::", id]
		    | _ => [id])

	  | cvtExp ctx (IDexp (IDENT ([], id))) = cvtIdent ctx id
	  | cvtExp ctx (IDexp (IDENT (_, id))) = span "const" [id]
	  | cvtExp ctx (IFexp (a, b, c)) = 
	    ([" a value determined by the value of "] @ (cvtExp ctx a) @ [":"]
	     @ (ol (withIndent ctx 1)
		   [["If the value is "] @ (span "const" ["true"]) @ [", then "] @ (cvtExp (withIndent ctx 2) b),
		    ["Otherwise "] @ (cvtExp (withIndent ctx 2) c)]))
	    
	  | cvtExp ctx (LETexp (decls, exps)) = 
	    let
		val ctx = withPatLocals ctx (List.concat (List.map declPats decls))
	    in
		[" the result of these steps:"]
		@ (ol (withIndent ctx 1) 
		      ((List.concat (List.map (cvtDecl (withIndent ctx 2)) decls))
		       @ (cvtExps (withIndent ctx 2) exps)))
	    end
	  | cvtExp ctx (SEQexp exps) = List.concat (cvtExps ctx exps)
	  | cvtExp _ _ = bad "expr"

					    
	and cvtFunBindClause (ctx:context)
			     (id:string)
			     (CLAUSE (pats, NONE, exp)) = 
	    [" Given "] 
	    @ (cvtPats true pats) 
	    @ [", the function"]
	    @ (span "expr" ((span "const" [id]) @ (cvtPatIds pats)))
	    @ [" evaluates to"]
	    @ (cvtExp (withPatLocals ctx pats) exp)
	  | cvtFunBindClause _ _ _ = bad "funBindClause"

	and cvtFunBind (ctx:context) (FUNbind (id, [clause])) = 
	    cvtFunBindClause ctx id clause
	  | cvtFunBind (ctx:context) (FUNbind (id, clauses)) =
	    [" Let the function ", id, " be defined by the following cases:"]
	    @ (ol (withIndent ctx 1) 
		  (List.map (cvtFunBindClause (withIndent ctx 2) id) clauses))
	    
	val ctx = { locals = [],
		    indent = 0 }
	val head = ["<html>\n",
		    " <head>\n",
		    "  <style>\n",
		    "    body\n",
		    "      {\n",
		    "        line-height: 150%;\n",
		    "      }\n",
		    "    span.var\n",
		    "      {\n",		    
		    "        font-style: italic;\n",
		    "      }\n",		    
		    "    span.name\n",
		    "      {\n",
		    "        font-weight: bold;\n",
		    "      }\n",		    
		    "    span.const\n",
		    "      {\n",
		    "        font-weight: bold;\n",
		    "      }\n",		    
		    "    span.bad\n",
		    "      {\n",
		    "        color: #ff0000;\n",
		    "        font-weight: bold;\n",
		    "      }\n",		    
		    "    span.expr\n",
		    "      {\n",
		    "        background-color: #f0f0f0;\n",
		    "        padding: 0.25em;\n",
		    "      }\n",
		    "  </style>\n",
		    " </head>\n",
		    " <body>"]
	val foot = [" </body>",
		    "</html>"]
    in
	put (head @ (cvtFunBind ctx funbind) @ foot);
	put ["\n"]
    end
    
    
fun genFile (infile:string) 
	    (defname:string) 
	    (outfile:string) 
    : unit = 
    let
	(* Why exactly we're using MLRISC, I do not know. *)
	val mlast = MLParser.load infile
	fun dig (MARKdecl (_, d)) = dig d
	  | dig (STRUCTUREdecl (_, _, _, DECLsexp decls)) = first dig decls
	  | dig (FUNdecl binds) = 
	    let
		fun q d = 
		    let 
			val FUNbind (id, clauses) = d
		    in
			if id = defname
			then SOME d
			else NONE
		    end
	    in
		first q binds
	    end
	  | dig _ = NONE
    in      
	case dig (List.hd mlast) of 
	    SOME (FUNbind (id, clauses)) => 
	    (print ("found binding for " ^ id ^ "\n");
	     translateFn outfile (FUNbind (id, clauses)))
	  | _ => print ("did not find binding for " ^ defname ^ "\n")
    end
	
	
fun main (argv0:string, argvRest:string list) =	
    case argvRest of 
	[infile, defname, outfile] => 
	(genFile infile defname outfile; 
	 0)
      | _ => (print ("usage: " 
		     ^ argv0 
		     ^ " <infile> <defname> <outfile>\n"); 
	      1)
end
