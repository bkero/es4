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

structure GenPretty =
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

    fun main (argv0:string, argvRest:string list) =
        BackTrace.monitor (fn () =>
                           (
                               genFile (nth (argvRest, 0), nth (argvRest, 1));
                               0
                           ))

end
