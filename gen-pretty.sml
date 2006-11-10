(*
 * structure Convert
 *)
structure Convert = struct

    structure MLAst = MDLAst

    open MLAst List

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

    fun zip ([], []) = []
      | zip (x::xs, y::ys) = (x,y)::(zip (xs, ys))
      | zip _ = raise Empty

    fun typeName (DATATYPEcvt (id, _)) = id
      | typeName (TYPEcvt (id, _)) = id

    fun strlit s = LITexp (STRINGlit s)

    fun ident id = IDENT ([], id)

    fun cvtFunctionName id = "cvt" ^ (capitalize id)

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
                         in
                             (IDpat sym, APPexp (IDexp (ident (cvtFunctionName id)), IDexp (ident sym)))
                         end
           | INTcvt => let val sym = gensym "n"
                       in
                           (IDpat sym, APPexp (IDexp (ident "Int"), IDexp (ident sym)))
                       end
           | REALcvt => let val sym = gensym "r"
                        in
                            (IDpat sym, APPexp (IDexp (ident "Real"), IDexp (ident sym)))
                        end
           | BOOLcvt => let val sym = gensym "b"
                        in
                            (IDpat sym, APPexp (IDexp (ident "Bool"), IDexp (ident sym)))
                        end
           | STRINGcvt => let val sym = gensym "s"
                          in
                              (IDpat sym, APPexp (IDexp (ident "String"), IDexp (ident sym)))
                          end
           | UNITcvt => (TUPLEpat [], IDexp (ident "Unit"))
           | OPTIONcvt ty' => let val (pat, tem) = genCvtTy ty'
                                  val sym = gensym "o"
                              in
                                  (IDpat sym,
                                   CASEexp (IDexp (ident sym),
                                            [CLAUSE ([CONSpat (ident "NONE", NONE)],
                                                     NONE,
                                                     APPexp (IDexp (ident "Ctor"),
                                                             APPexp (IDexp (ident "SOME"),
                                                                     APPexp (IDexp (ident "Tuple"),
                                                                             LISTexp ([strlit "NONE"], NONE))))),
                                             CLAUSE ([CONSpat (ident "SOME", SOME pat)],
                                                     NONE,
                                                     APPexp (IDexp (ident "Ctor"),
                                                             APPexp (IDexp (ident "SOME"),
                                                                     APPexp (IDexp (ident "Tuple"),
                                                                             LISTexp ([strlit "SOME", tem], NONE)))))]))
                              end
           | TUPLEcvt tys => let val pairs = map genCvtTy tys
                                 val pats = map #1 pairs
                                 val tems = map #2 pairs
                             in
                                 (TUPLEpat pats, APPexp (IDexp (ident "Tuple"), LISTexp (tems, NONE)))
                             end
           | RECORDcvt elts => let val ids = map #1 elts
                                   val pairs = map genCvtTy (map #2 elts)
                                   val pats = map #1 pairs
                                   val tems = map #2 pairs
                               in
                                   (RECORDpat (zip (ids, pats), false),
                                    APPexp (IDexp (ident "Rec"),
                                            LISTexp ((map (fn (id, tem) => TUPLEexp [strlit id, tem])
                                                          (zip (ids, tems))), NONE)))
                               end
           | LISTcvt ty' => let val (pat, tem) = genCvtTy ty'
                                val sym = gensym "ls"
                            in
                                (IDpat sym,
                                 APPexp (APPexp (IDexp (IDENT (["List"], "map")),
                                                 LAMBDAexp [CLAUSE ([pat], NONE, tem)]),
                                         IDexp (ident sym)))
                            end
           (* TODO: ref types *)
           | _ => raise (Fail "not yet implemented")

    fun genCvtClause (CONScvt (name, NONE)) =
            CLAUSE ([CONSpat (ident name, NONE)],
                    NONE,
                    APPexp (IDexp (ident "Ctor"), TUPLEexp [strlit name, IDexp (ident "NONE")]))
      | genCvtClause (CONScvt (name, SOME ty)) =
            let val (pat, tem) = genCvtTy ty
            in
                CLAUSE ([CONSpat (ident name, SOME pat)],
                        NONE,
                        APPexp (IDexp (ident "Ctor"), TUPLEexp [strlit name, APPexp (IDexp (ident "SOME"), tem)]))
            end

    fun genCvtFunction tb =
        case tb of
             DATATYPEcvt (id, cs) =>
                 FUNdecl [FUNbind (cvtFunctionName id, map genCvtClause cs)]
           | TYPEcvt (id, ty) =>
                 let val (pat, tem) = genCvtTy ty
                 in
                     FUNdecl [FUNbind (cvtFunctionName id, [CLAUSE ([pat], NONE, tem)])]
                 end

    fun genCvtFunctions (DECLcvt ds) =
        (SOME (map genCvtFunction ds)) handle (Fail s) => NONE

end


(*
 * structure GenPretty
 *)
structure GenPretty = struct

    structure MLAst = MDLAst
    structure MLUtil = MDLAstUtil(MDLAst)
    structure MLPP = MDLAstPrettyPrinter(MLUtil)
    structure MLParser = MDLParserDriver(structure AstPP = MLPP
                                         val MDLmode = false
                                         val extraCells = [])

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
            val fundecls = concat (mapPartial genCvtFunctions (extractCvtDecls typedecls))
            val body = DECLsexp ((OPENdecl [IDENT ([], "Ast"), IDENT ([], "PrettyRep")])::fundecls)
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
