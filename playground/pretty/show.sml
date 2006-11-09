structure Show =
struct

    structure Ast = MDLAst
    open Ast List

    exception Unshowable of string * string

    datatype showdecl = DECLshow of showtypebind list

         and showtypebind = DATATYPEshow of string * (showconsbind list)
                          | TYPEshow of string * showty

         and showconsbind = CONSshow of string * (showty option)

         and showty = IDshow of string
                    | TUPLEshow of showty list
                    | RECORDshow of (string * showty) list
                    | LISTshow of showty
                    | OPTIONshow of showty
                    | REFshow of showty
                    | INTshow
                    | STRINGshow
                    | UNITshow
                    | BOOLshow
                    | REALshow

    val i2s = Int.toString

    fun showableType names ty =
        case ty of
             IDty (IDENT ([], "int")) => INTshow
           | IDty (IDENT ([], "string")) => STRINGshow
           | IDty (IDENT ([], "unit")) => UNITshow
           | IDty (IDENT ([], "bool")) => BOOLshow
           | IDty (IDENT ([], "real")) => REALshow
           | IDty (IDENT ([], name)) =>
                 if exists (fn name' => (name' = name)) names then
                     IDshow name
                 else
                     raise Unshowable (name, "out of scope")
           | IDty (IDENT (_, name)) => raise Unshowable (name, "unknown type")
           | VARty _ => raise Unshowable ("?", "var type")
           | TUPLEty tys => TUPLEshow (map (showableType names) tys)
           | RECORDty pairs => RECORDshow (map (fn (id, ty') => (id, showableType names ty')) pairs)
           | APPty ((IDENT ([], "option")), [ty]) => OPTIONshow (showableType names ty)
           | APPty ((IDENT ([], "option")), _) => raise Unshowable ("option", "too many type arguments")
           | APPty ((IDENT ([], "list")), [ty]) => LISTshow (showableType names ty)
           | APPty ((IDENT ([], "list")), _) => raise Unshowable ("list", "too many type arguments")
           | APPty ((IDENT ([], "ref")), [ty]) => REFshow (showableType names ty)
           | APPty ((IDENT ([], "ref")), _) => raise Unshowable ("ref", "too many type arguments")
           | APPty ((IDENT (_, name)), _) => raise Unshowable (name, "type constructor")
           | TYVARty (VARtv s) => raise Unshowable (s, "type variable")
           | TYVARty (INTtv s) => raise Unshowable (s, "type variable")
           | INTVARty i => raise Unshowable (i2s i, "int variable type")
           | FUNty (_,_) => raise Unshowable ("?", "function type")
           | POLYty _ => raise Unshowable ("?", "polymorphic type")
           | LAMBDAty _ => raise Unshowable ("?", "lambda type")
           | CELLty _ => raise Unshowable ("?", "cell type")

    fun showableConsBind names (CONSbind {id,ty,...}) =
        case ty of
             NONE => CONSshow (id, NONE)
           | SOME ty' => CONSshow (id, SOME (showableType names ty'))

    fun showableDatatypeBind names ast =
        case ast of
             DATATYPEbind {id=id,tyvars=[],cbs=cbs,...} => DATATYPEshow (id, map (showableConsBind names) cbs)
           | DATATYPEbind {id,tyvars,...} => raise Unshowable (id, "type constructor")
           | DATATYPEEQbind {id,...} => raise Unshowable (id, "weird eqtype thingy")

    fun showableTypeBind names (TYPEbind (id, [], ty)) = TYPEshow (id, showableType names ty)
      | showableTypeBind names (TYPEbind (id, _, _)) = raise Unshowable (id, "type constructor")

    fun showableDecl names (dts, ts) =
        DECLshow ((map (showableDatatypeBind names) dts) @ (map (showableTypeBind names) ts))

    fun datatypeBindId (DATATYPEbind {id,...}) = id
      | datatypeBindId (DATATYPEEQbind {id,...}) = id

    fun typeBindId (TYPEbind (id,_,_)) = id

    fun declaredTypeNames (dts, ts) = (map datatypeBindId dts) @ (map typeBindId ts)

    fun showableDecls ds =
        let val groundTypes = ["bool", "int", "real", "string", "unit"]
            val names = groundTypes @ (concat (map declaredTypeNames ds))
        in
            map (showableDecl names) ds
        end

    fun typeName (DATATYPEshow (id, _)) = id
      | typeName (TYPEshow (id, _)) = id

    fun showFunctionName tb = (typeName tb) ^ "ToString"

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

    fun genStrApp (t1, t2) = APPexp (IDexp (IDENT (["String"], "^")), TUPLEexp [t1, t2])

    fun genStrCat ts = APPexp (IDexp (IDENT (["String"], "concat")), LISTexp (ts, NONE))

    fun genStrCatCommas es = APPexp (APPexp (IDexp (IDENT (["String"], "concatWith")), LITexp (STRINGlit ",")),
                                     LISTexp (es , NONE))

    fun genRecEltStr (id, t) = genStrApp (LITexp (STRINGlit (id ^ "=")), t)

    fun zip ([], []) = []
      | zip (x::xs, y::ys) = (x,y)::(zip (xs, ys))
      | zip _ = raise Empty

    fun id2ident id = IDENT ([], id)

    fun showTy ty =
        case ty of
             IDshow id => let val sym = gensym "x"
                          in
                              (IDpat sym, APPexp (IDexp (IDENT ([], id ^ "ToString")), IDexp (IDENT ([], sym))))
                          end
           | TUPLEshow tys => let val pairs = map showTy tys
                              in
                                  (TUPLEpat (map #1 pairs),
                                   APPexp (IDexp (IDENT (["String"], "^")),
                                           TUPLEexp [APPexp (IDexp (IDENT (["String"], "^")),
                                                             TUPLEexp [LITexp (STRINGlit "("), genStrCatCommas (map #2 pairs)]),
                                                     LITexp (STRINGlit ")")]))
                              end
           | RECORDshow elts => let val ids = map #1 elts
                                    val syms = map gensym ids
                                    val pairs = map showTy (map #2 elts)
                                    val ps = map #1 pairs
                                    val ts = map #2 pairs
                                in
                                    (RECORDpat (zip (ids, map IDpat syms), false),
                                     LETexp ((map (fn (p, sym) => VALdecl [VALbind (p, IDexp (IDENT ([], sym)))])
                                                  (zip (ps, syms))),
                                             [genStrCat [LITexp (STRINGlit "{"),
                                                         genStrCatCommas (map genRecEltStr (zip (ids, ts))),
                                                         LITexp (STRINGlit "}")]]))
                                end
           | LISTshow ty' => let val sym = gensym "ls"
                                 val (p, t) = showTy ty'
                             in
                                 (IDpat sym,
                                  genStrCat [LITexp (STRINGlit "["),
                                             APPexp (APPexp (IDexp (IDENT (["String"], "concatWith")), LITexp (STRINGlit ",")),
                                                     APPexp (APPexp (IDexp (IDENT (["List"], "map")),
                                                                     LAMBDAexp [CLAUSE ([p], NONE, t)]),
                                                             IDexp (IDENT ([], sym)))),
                                             LITexp (STRINGlit "]")])
                             end
           | OPTIONshow ty' => let val (p, t) = showTy ty'
                                   val sym = gensym "opt"
                               in
                                   (IDpat sym, CASEexp (IDexp (IDENT([], sym)),
                                                        [CLAUSE ([CONSpat (IDENT ([], "NONE"), NONE)],
                                                                 NONE,
                                                                 LITexp (STRINGlit "NONE")),
                                                         CLAUSE ([CONSpat (IDENT ([], "SOME"), SOME p)],
                                                                 NONE,
                                                                 genStrApp (LITexp (STRINGlit "SOME "), t))]))
                               end
           | REFshow ty' => let val sym = gensym "r"
                            in
                                (IDpat sym, APPexp (IDexp (IDENT (["General"], "!")), IDexp (IDENT ([], sym))))
                            end
           | INTshow => let val sym = gensym "n"
                        in
                            (IDpat sym, APPexp (IDexp (IDENT (["Int"], "toString")), IDexp (IDENT ([], sym))))
                        end
           | STRINGshow => let val sym = gensym "s"
                           in
                               (IDpat sym,
                                genStrCat [LITexp (STRINGlit "\""),
                                           APPexp (IDexp (IDENT (["String"], "toString")), IDexp (IDENT ([], sym))),
                                           LITexp (STRINGlit "\"")])
                           end
           | UNITshow => (TUPLEpat [], LITexp (STRINGlit "()"))
           | BOOLshow => let val sym = gensym "b"
                         in
                             (IDpat sym, APPexp (IDexp (IDENT (["Bool"], "toString")), IDexp (IDENT ([], sym))))
                         end
           | REALshow => let val sym = gensym "r"
                         in
                             (IDpat sym, APPexp (IDexp (IDENT (["Real"], "toString")), IDexp (IDENT ([], sym))))
                         end

    fun showClause (CONSshow (name, NONE)) = CLAUSE ([CONSpat (IDENT ([], name), NONE)],
                                                     NONE, 
                                                     LITexp (STRINGlit name))
      | showClause (CONSshow (name, SOME ty)) =
            let val (p, t) = showTy ty
            in
                CLAUSE ([CONSpat (IDENT ([], name), SOME p)],
                        NONE,
                        APPexp (IDexp (IDENT (["String"], "^")),
                                TUPLEexp [LITexp (STRINGlit (name ^ " ")), t]))
            end

    fun showFunction tb =
        case tb of
             DATATYPEshow (id, cs) =>
                 FUNdecl [FUNbind (showFunctionName tb, map showClause cs)]
           | TYPEshow (id, ty) => raise Unshowable ("NYI", "!!!")

    fun showFunctions (DECLshow ds) = map showFunction ds

end
