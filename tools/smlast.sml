(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 * 
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 * 
 *    2. All liability and responsibility for the implementation or other
 * use of this Reference Implementation rests with the implementor, and
 * not with any of the parties who contribute to, or who own or hold any
 * copyright in, this Reference Implementation.
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
