structure Pretty =
struct

    structure Ast     = MDLAst
    structure AstUtil = MDLAstUtil(MDLAst)
    structure AstPP   = MDLAstPrettyPrinter(AstUtil) 
    structure Parser = MDLParserDriver
        (structure AstPP = AstPP
         val MDLmode = false
         val extraCells = [])

    open Ast MDLError AstUtil List AstPP PP Show TextIO

    exception Unparsed

    fun unmarkDecl (MARKdecl (_, d)) = d
      | unmarkDecl d = d

    fun structureBody (STRUCTUREdecl (id, _, _, e)) = (id, e)
      | structureBody _ = raise Unparsed

    fun structureContents (CONSTRAINEDsexp (e, _)) = structureContents e
      | structureContents (DECLsexp ds) = map unmarkDecl ds
      | structureContents _ = raise Unparsed

    fun typeDeclContents (DATATYPEdecl x) = SOME x
      | typeDeclContents _ = NONE

    fun extractTypeDecls decls = mapPartial typeDeclContents decls

    fun contents ast =
        let val (id, e) = structureBody (unmarkDecl (hd ast))
        in
            (id, extractTypeDecls (structureContents e))
        end

    fun codegen ast =
        let val (structin, typedecls) = contents ast
            val fundecls = concat (map showFunctions (showableDecls typedecls))
            val body = DECLsexp ((OPENdecl [IDENT ([], structin)])::fundecls)
        in
            STRUCTUREdecl ("Show" ^ structin, [], NONE, body)
        end

    fun printTo (filename, s) =
        let val f = openOut filename
        in
            (output (f, s)
             handle IO.Io {name,function,cause} =>
                 (closeOut f; raise IO.Io {name=name,function=function,cause=cause}));
            closeOut f
        end

    fun transform (infile, outfile) = printTo (outfile, text (decl (codegen (Parser.load infile))))

end
