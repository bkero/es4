structure MLAst = MDLAst
structure MLUtil = MDLAstUtil(MDLAst)
structure MLPP = MDLAstPrettyPrinter(MLUtil)
structure MLParser = MDLParserDriver(structure AstPP = MLPP
                                     val MDLmode = false
                                     val extraCells = [])
structure MLAstRewriter = MDLAstRewriter(MDLAst)

(*
 * structure Convert
 *)
structure Convert = struct

    open MLAst MLUtil List SMLofNJ

    datatype cvtdecl = DECLcvt of cvttypebind list

         and cvttypebind = DATATYPEcvt of string * cvtconsbind list
                         | TYPEcvt of string * cvtty

         and cvtconsbind = CONScvt of string * cvtty option

         and cvtty = IDcvt of string
                   | TUPLEcvt of cvtty list
                   | RECORDcvt of (string * cvtty) list
                   | LISTcvt of cvtty
                   | OPTIONcvt of cvtty
                   | REFcvt of cvtty
                   | STRINGcvt
                   | UNITcvt
                   | BOOLcvt
                   | INTcvt
                   | REALcvt

    (* Utilities *)

    val gensym =
        let val count = ref 0
        in
            fn base =>
            (
                let val s = base ^ (Int.toString (!count))
                in
                    count := (!count) + 1;
                    s
                end
            )
        end

    fun capitalize s =
        let val c = Char.toUpper (String.sub (s, 0))
            val suffix = String.extract (s, 1, NONE)
        in
            (String.implode [c]) ^ suffix
        end

(*
    fun zip ([], []) = []
      | zip (x::xs, y::ys) = (x,y)::(zip (xs, ys))
      | zip _ = raise Empty
*)

    fun typeName (DATATYPEcvt (id, _)) = id
      | typeName (TYPEcvt (id, _)) = id

    fun ident id = IDENT ([], id)

    fun cvtFunctionName id = "cvt" ^ (capitalize id)

    (* Quasiquote parser *)

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

    (* Part 1. Extract type declarations from the source program. *)

    fun makeCvtType names ty =
        case ty of
             IDty (IDENT ([], "int")) => INTcvt
           | IDty (IDENT ([], "string")) => STRINGcvt
           | IDty (IDENT ([], "unit")) => UNITcvt
           | IDty (IDENT ([], "bool")) => BOOLcvt
           | IDty (IDENT ([], "real")) => REALcvt
           | IDty (IDENT ([], name)) =>
                 if exists (fn name' => (name' = name)) names then
                     IDcvt name
                 else
                     raise (Fail "out of scope")
           | IDty (IDENT (_, name)) => raise (Fail "unknown type")
           | VARty _ => raise (Fail "var type")
           | TUPLEty tys => TUPLEcvt (map (makeCvtType names) tys)
           | RECORDty pairs => RECORDcvt (map (fn (id, ty') => (id, makeCvtType names ty')) pairs)
           | APPty ((IDENT ([], "option")), [ty]) => OPTIONcvt (makeCvtType names ty)
           | APPty ((IDENT ([], "option")), _) => raise (Fail "too many type arguments")
           | APPty ((IDENT ([], "list")), [ty]) => LISTcvt (makeCvtType names ty)
           | APPty ((IDENT ([], "list")), _) => raise (Fail "too many type arguments")
           | APPty ((IDENT ([], "ref")), [ty]) => REFcvt (makeCvtType names ty)
           | APPty ((IDENT ([], "ref")), _) => raise (Fail "too many type arguments")
           | APPty ((IDENT (_, name)), _) => raise (Fail "type constructor")
           | TYVARty (VARtv s) => raise (Fail "type variable")
           | TYVARty (INTtv s) => raise (Fail "type variable")
           | INTVARty i => raise (Fail "int variable type")
           | FUNty (_, _) => raise (Fail "function type")
           | POLYty _ => raise (Fail "polymorphic type")
           | LAMBDAty _ => raise (Fail "lambda type")
           | CELLty _ => raise (Fail "cell type")

    fun makeCvtConsBind names (CONSbind {id,ty,...}) =
        case ty of
             NONE => CONScvt (id, NONE)
           | SOME ty' => CONScvt (id, SOME (makeCvtType names ty'))

    fun makeCvtDatatypeBind names dt =
        case dt of
             DATATYPEbind {id=id,tyvars=[],cbs=cbs,...} => DATATYPEcvt (id, map (makeCvtConsBind names) cbs)
           | DATATYPEbind _ => raise (Fail "type constructor")
           | DATATYPEEQbind _ => raise (Fail "weird eq-type thing")

    fun makeCvtTypeBind names (TYPEbind (id, [], ty)) = TYPEcvt (id, makeCvtType names ty)
      | makeCvtTypeBind names (TYPEbind (id, _, _)) = raise (Fail "type constructor")

    fun makeCvtDecl names (dts, ts) =
        DECLcvt ((map (makeCvtDatatypeBind names) dts) @ (map (makeCvtTypeBind names) ts))

    fun declaredTypeNames (dts, ts) =
        let fun datatypeBindId (DATATYPEbind {id,...}) = id
              | datatypeBindId (DATATYPEEQbind {id,...}) = id
            fun typeBindId (TYPEbind (id,_,_)) = id
        in
            (map datatypeBindId dts) @ (map typeBindId ts)
        end

    fun extractCvtDecls ds =
        let val groundTypes = ["bool", "int", "real", "string", "unit"]
            val names = groundTypes @ (concat (map declaredTypeNames ds))
        in
            map (makeCvtDecl names) ds
        end

    (* Part 2. Generate conversion functions. *)

    fun genCvtTy ty =
        case ty of
             IDcvt id => let val sym = gensym "x"
                             val cvt = cvtFunctionName id
                         in
                             (IDpat sym, APPexp (ID cvt, ID sym))
                         end
           | INTcvt => let val sym = gensym "n"
                       in
                           (IDpat sym, APPexp (ID "Int", ID sym))
                       end
           | REALcvt => let val sym = gensym "r"
                        in
                            (IDpat sym, APPexp (ID "Real", ID sym))
                        end
           | BOOLcvt => let val sym = gensym "b"
                        in
                            (IDpat sym, APPexp (ID "Bool", ID sym))
                        end
           | STRINGcvt => let val sym = gensym "s"
                          in
                              (IDpat sym, APPexp (ID "String", ID sym))
                          end
           | UNITcvt => (TUPLEpat [], IDexp (ident "Unit"))
           | OPTIONcvt ty' => let val (pat, tem) = genCvtTy ty'
                                  val pat = PAThole pat
                                  val tem = EXPhole tem
                                  val sym = gensym "opt"
                                  val x = EXPhole (ID sym)
                              in
                                  (IDpat sym, %`case ^x of
                                                     NONE => Ctor ("NONE", NONE)
                                                   | SOME ^pat => Ctor ("SOME", SOME ^tem)`)
                              end
           | TUPLEcvt tys => let val pairs = map genCvtTy tys
                                 val pats = map #1 pairs
                                 val tems = EXPhole (LISTexp (map #2 pairs, NONE))
                             in
                                 (TUPLEpat pats, %`Tuple (^tems)`)
                             end
           | RECORDcvt elts => let val ids = map #1 elts
                                   val pairs = map genCvtTy (map #2 elts)
                                   val pats = map #1 pairs
                                   val tems = map #2 pairs
                                   val elts = EXPhole (LISTexp (map (fn (id, tem) =>
                                                                         let val s = EXPhole (STRINGexp id)
                                                                             val tem = EXPhole tem
                                                                         in
                                                                             %`(^s, ^tem)`
                                                                         end)
                                                                    (ListPair.zip (ids, tems)),
                                                                NONE))
                               in
                                   (RECORDpat (ListPair.zip (ids, pats), false), %`Rec (^elts)`)
                               end
           | LISTcvt ty' => let val (pat, tem) = genCvtTy ty'
                                val pat = PAThole pat
                                val tem = EXPhole tem
                                val sym = gensym "ls"
                                val x = EXPhole (ID sym)
                            in
                                (IDpat sym, %`List (List.map (fn ^pat => ^tem) ^x)`)
                            end
           (* TODO: ref types *)
           | _ => raise (Fail "not yet implemented")

    fun genCvtClause (CONScvt (name, NONE)) =
            let val s = EXPhole (STRINGexp name)
            in
                CLAUSE ([CONSpat (ident name, NONE)],
                        NONE,
                        %`Ctor (^s, NONE)`)
            end
      | genCvtClause (CONScvt (name, SOME ty)) =
            let val (pat, tem) = genCvtTy ty
                val tem = EXPhole tem
                val s = EXPhole (STRINGexp name)
            in
                CLAUSE ([CONSpat (ident name, SOME pat)],
                        NONE,
                        %`Ctor (^s, SOME ^tem)`)
            end

    fun genCvtFunction tb =
        case tb of
             DATATYPEcvt (id, cs) =>
                 FUNbind (cvtFunctionName id, map genCvtClause cs)
           | TYPEcvt (id, ty) =>
                 let val (pat, tem) = genCvtTy ty
                 in
                     FUNbind (cvtFunctionName id, [CLAUSE ([pat], NONE, tem)])
                 end

    fun genCvtFunctions (DECLcvt ds) =
        (SOME (map genCvtFunction ds)) handle (Fail s) => NONE

end


(*
 * structure GenPretty
 *)
structure GenPretty = struct

    open MLAst List TextIO Convert

    fun unmarkDecl (MARKdecl (_, d)) = d
      | unmarkDecl d = d

    fun structureBody (STRUCTUREdecl (id, _, _, e)) = (id, e)
      | structureBody _ = raise (Fail "not a structure declaration")

    fun structureContents (CONSTRAINEDsexp (e, _)) = structureContents e
      | structureContents (DECLsexp ds) = map unmarkDecl ds
      | structureContents _ = raise (Fail "structure body not a declaration")

    fun typeDeclContents (DATATYPEdecl x) = SOME x
      | typeDeclContents _ = NONE

    fun extractTypeDecls decls = mapPartial typeDeclContents decls

    fun contents ast =
        let val (id, e) = structureBody (unmarkDecl (hd ast))
        in
            (id, extractTypeDecls (structureContents e))
        end

    fun genPretty ast =
        let val (structin, typedecls) = contents ast
            val funbinds = concat (mapPartial genCvtFunctions (extractCvtDecls typedecls))
            val body = DECLsexp [OPENdecl [IDENT ([], "Ast"), IDENT ([], "PrettyRep")],
				 FUNdecl funbinds]
        in
            STRUCTUREdecl ("PrettyCvt", [], NONE, body)
        end

    fun genFile (infile, outfile) =
        let val src = PP.text (MLPP.decl (genPretty (MLParser.load infile)))
            val f = openOut outfile
        in
            (output (f, src)
             handle (e as IO.Io {name,function,cause}) =>
                 (closeOut f; raise e));
            closeOut f
        end

    fun main (argv0:string, argvRest:string list) =
        (genFile (nth (argvRest, 0), nth (argvRest, 1)); 0)
            handle IO.Io {name,function,cause} => (print ("I/O error in " ^ name ^ " due to " ^ function ^ "\n"); 1)
                 | Fail s => (print ("error: " ^ s ^ "\n"); 1)

end
