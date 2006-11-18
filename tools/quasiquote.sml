structure QuasiQuote =
struct

    open MLAst MLIdent SMLofNJ

    fun parseMLExp s =
        case MLParser.parseString ("val a = " ^ s) of
             [MARKdecl (_, VALdecl [VALbind (_, e)])] => e
           | _ => raise (Fail "parser returned something unexpected")

    fun parseMLPat s =
        case MLParser.parseString ("val (" ^ s ^ ") = 0") of
             [MARKdecl (_, VALdecl [VALbind (p, _)])] => p
           | _ => raise (Fail "parser returned something unexpected")

    fun lookup (x, []) = NONE
      | lookup (x, ((y,z)::pairs)) = if (x = y) then SOME z else lookup (x, pairs)

    datatype hole = SRChole of string
                  | EXPhole of exp
                  | PAThole of pat

    exception HoleType of string * string

    fun holeType (SRChole _) = "raw"
      | holeType (EXPhole _) = "expression"
      | holeType (PAThole _) = "pattern"

    (* TODO: could be generalized to produce either exps or pats *)

    fun parseQuasiML frags =
        let fun skolemize [] = ([], [])
              | skolemize (((QUOTE s) | (ANTIQUOTE (SRChole s)))::frags) =
                    let val (ss, skolems) = skolemize frags
                    in
                        (s::ss, skolems)
                    end
              | skolemize ((ANTIQUOTE x)::frags) =
                    let val skolem = gensym "qqSkolem"
                        val (ss, skolems) = skolemize frags
                    in
                        (skolem::ss, (skolem, x)::skolems)
                    end
            val (ss, skolems) = skolemize frags
            fun exp _ (e as (IDexp (IDENT ([], x)))) =
                    (case lookup (x, skolems) of
                          NONE => e
                        | SOME (EXPhole e') => e'
                        | SOME h => raise (HoleType ("exp", holeType h)))
              | exp _ e = e
            fun pat _ (p as (IDpat x)) =
                    (case lookup (x, skolems) of
                          NONE => p
                        | SOME (PAThole p') => p'
                        | SOME h => raise (HoleType ("pat", holeType h)))
              | pat _ p = p
            val NIL = MLAstRewriter.noRewrite
            val rw = #exp(MLAstRewriter.rewrite
                              {exp=exp,pat=pat,decl=NIL,ty=NIL,sexp=NIL})
        in
            rw (parseMLExp (String.concat ss))
        end

    val % = parseQuasiML

end
