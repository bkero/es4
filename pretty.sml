structure Pretty = struct

(*
 * The pretty-printer has 2 parts.
 *
 * Part 1 defines a datatype that is of a subset of the AST of SML itself,
 * and a single recursive function for pretty-printing that datatype.
 *
 * Part 2 defines a conversion from the ES4 AST (as defined in ast.sml) to the minimal
 * SML AST, such that it can be printed in a way that can be read back in.
 *)
open PrettyRep PrettyCvt

fun ppRep rep =
    let val dev = SimpleTextIODev.openDev {dst=TextIO.stdOut, wid=80}
        val stream = PP.openStream dev
    in
        (ppSmlDataRep stream rep;
         TextIO.print "\n";
         PP.flushStream stream)
        handle Fail s =>
               (TextIO.print "exception while pretty-printing: ";
                TextIO.print s;
                TextIO.print "\n")
    end

val ppNamespace = ppRep o cvtNAMESPACE

val ppProgram = ppRep o cvtPROGRAM

val ppExpr = ppRep o cvtEXPR

val ppStmt = ppRep o cvtSTMT

val ppDefinition = ppRep o cvtDEFN

val ppVarDefn = ppRep o cvtBINDING

val ppType = ppRep o cvtTYPE_EXPR

val ppBinop = ppRep o cvtBINOP

val ppNumericMode = ppRep o cvtNUMERIC_MODE

val ppPragma = ppRep o cvtPRAGMA

val ppFixtures = ppRep o cvtFIXTURES

val ppFunc = ppRep o cvtFUNC

end
