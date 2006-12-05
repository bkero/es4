structure Main =
struct

    open MLAst List TextIO GenConvert

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

    fun mapSoft f = mapPartial (fn x => (SOME (f x))
                                        handle Fail s => (TextIO.print ("error: " ^ s ^ "; abandoning code generation for all related types\n"); NONE))

    fun genPretty ast =
        let val (structin, typedecls) = contents ast
            val funbinds = concat (mapSoft genCvtFunctions (extractCvtDecls typedecls))
            val body = DECLsexp [OPENdecl [IDENT ([], "Ast")],
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

    (* TODO: abstract this out! it's copied from main.sml *)
    fun printStackTrace e =
        let val ss = SMLofNJ.exnHistory e
            val s = General.exnMessage e
            val name = General.exnName e
            val details = if s = name then "" else (" [" ^ s ^ "]")
        in
            TextIO.print ("uncaught exception " ^ name ^ details ^ "\n");
            case ss of
                 [] => ()
               | (s::ss') => (
                                 TextIO.print ("  raised at: " ^ s ^ "\n");
                                 List.app (fn s' => TextIO.print ("             " ^ s' ^ "\n")) ss'
                             )
        end
    (* val monitor = BackTrace.monitor *)
    fun monitor f = f() handle e => (printStackTrace e; 1)

    fun main (argv0:string, argvRest:string list) =
        monitor (fn () =>
                 (
                     genFile (nth (argvRest, 0), nth (argvRest, 1));
                     0
                ))

end
