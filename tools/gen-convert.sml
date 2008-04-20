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

structure GenConvert =
struct

    open MLAst MLUtil MLIdent List SMLofNJ QuasiQuote

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
                   | REAL64cvt
                   | DECcvt
                   | DECRMcvt
                   | USTRINGcvt

    fun typeName (DATATYPEcvt (id, _)) = id
      | typeName (TYPEcvt (id, _)) = id

    fun cvtFunctionName id = "cvt" ^ (capitalize id)

    (* Part 1. Extract type declarations from the source program. *)

    fun makeCvtType names ty =
        case ty of
             IDty (IDENT ([], "int")) => INTcvt
           | IDty (IDENT ([], "string")) => STRINGcvt
           | IDty (IDENT ([], "unit")) => UNITcvt
           | IDty (IDENT ([], "bool")) => BOOLcvt

	   (* Some module-qualified type names we want to support. *)
           | IDty (IDENT (["Real64"], "real")) => REAL64cvt
           | IDty (IDENT (["Decimal"], "DEC")) => DECcvt
           | IDty (IDENT (["Decimal"], "ROUNDING_MODE")) => DECRMcvt
           | IDty (IDENT (["Ustring"], "STRING")) => USTRINGcvt

           | IDty (IDENT ([], name)) =>
                 if exists (fn name' => (name' = name)) names then
                     IDcvt name
                 else
                     raise (Fail ("out of scope: " ^ name))
           | IDty (IDENT (_, name)) => raise (Fail ("unknown type: " ^ name))
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
                           (IDpat sym, APPexp (ID "PrettyRep.Int", ID sym))
                       end
           | REAL64cvt => let val sym = gensym "r"
                        in
                            (IDpat sym, APPexp (ID "PrettyRep.Real64", ID sym))
                        end
           | DECcvt => let val sym = gensym "d"
                        in
                            (IDpat sym, APPexp (ID "PrettyRep.Dec", ID sym))
                        end
           | DECRMcvt => let val sym = gensym "r"
                        in
                            (IDpat sym, APPexp (ID "PrettyRep.DecRm", ID sym))
                        end
           | BOOLcvt => let val sym = gensym "b"
                        in
                            (IDpat sym, APPexp (ID "PrettyRep.Bool", ID sym))
                        end
           | STRINGcvt => let val sym = gensym "s"
                          in
                              (IDpat sym, APPexp (ID "PrettyRep.String", ID sym))
                          end
           | USTRINGcvt => let val sym = gensym "s"
                           in
                               (IDpat sym, APPexp (ID "PrettyRep.UniStr", ID sym))
                           end
           | UNITcvt => (TUPLEpat [], IDexp (ident "Unit"))
           | OPTIONcvt ty' => let val (pat, tem) = genCvtTy ty'
                                  val pat = PAThole pat
                                  val tem = EXPhole tem
                                  val sym = gensym "opt"
                                  val x = EXPhole (ID sym)
                              in
                                  (IDpat sym, %`case ^x of
                                                     NONE => PrettyRep.Ctor ("NONE", NONE)
                                                   | SOME ^pat => PrettyRep.Ctor ("SOME", SOME ^tem)`)
                              end
           | TUPLEcvt tys => let val pairs = map genCvtTy tys
                                 val pats = map #1 pairs
                                 val tems = EXPhole (LISTexp (map #2 pairs, NONE))
                             in
                                 (TUPLEpat pats, %`PrettyRep.Tuple (^tems)`)
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
                                   (RECORDpat (ListPair.zip (ids, pats), false), %`PrettyRep.Rec (^elts)`)
                               end
           | LISTcvt ty' => let val (pat, tem) = genCvtTy ty'
                                val pat = PAThole pat
                                val tem = EXPhole tem
                                val sym = gensym "ls"
                                val x = EXPhole (ID sym)
                            in
                                (IDpat sym, %`PrettyRep.List (List.map (fn ^pat => ^tem) ^x)`)
                            end
           | REFcvt ty' => let val (pat, tem) = genCvtTy ty'
                               val pat = PAThole pat
                               val tem = EXPhole tem
                               val sym = gensym "r"
                               val x = EXPhole (ID sym)
                           in
                               (IDpat sym, %`case !(^x) of ^pat => PrettyRep.Ref (^tem)`)
                           end

    fun genCvtClause (CONScvt (name, NONE)) =
            let val s = EXPhole (STRINGexp name)
            in
                CLAUSE ([CONSpat (ident name, NONE)],
                        NONE,
                        %`PrettyRep.Ctor (^s, NONE)`)
            end
      | genCvtClause (CONScvt (name, SOME ty)) =
            let val (pat, tem) = genCvtTy ty
                val tem = EXPhole tem
                val s = EXPhole (STRINGexp name)
            in
                CLAUSE ([CONSpat (ident name, SOME pat)],
                        NONE,
                        %`PrettyRep.Ctor (^s, SOME ^tem)`)
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

    fun genCvtFunctions (DECLcvt ds) = map genCvtFunction ds

end
