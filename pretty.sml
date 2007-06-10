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

val ppFixture = ppRep o cvtFIXTURE

val ppFunc = ppRep o cvtFUNC

end
