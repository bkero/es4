structure MLAst = MDLAst
structure MLUtil = MDLAstUtil(MDLAst)
structure MLPP = MDLAstPrettyPrinter(MLUtil)
structure MLParser = MDLParserDriver(structure AstPP = MLPP
                                     val MDLmode = false
                                     val extraCells = [])
structure MLAstRewriter = MDLAstRewriter(MDLAst)

structure MLIdent =
struct

    open MLAst

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

    fun ident id = IDENT ([], id)

end
