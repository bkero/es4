(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
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

signature FIXTURE = sig

    (* Basic FIXTURE / RIB operations *)

    val getFixture : Ast.RIB -> Ast.FIXTURE_NAME -> Ast.FIXTURE
    val hasFixture : Ast.RIB -> Ast.FIXTURE_NAME -> bool
    val replaceFixture : Ast.RIB -> Ast.FIXTURE_NAME -> Ast.FIXTURE -> Ast.RIB
    val printFixture : Ast.FIXTURE_NAME * Ast.FIXTURE -> unit
    val printRib : Ast.RIB -> unit

    (* FIXME: the coupling between type.sml and fixture.sml suggests re-merging them *)
    type TYEQ = (Ast.TYPE_EXPR -> Ast.TYPE_EXPR -> bool)
    val mergeRibs : TYEQ -> Ast.RIB -> Ast.RIB -> Ast.RIB

    type PROGRAM
    val mkProgram : Ast.RIB -> PROGRAM

    val getRootRib : PROGRAM -> Ast.RIB
    val extendRootRib : PROGRAM -> Ast.RIB -> TYEQ -> PROGRAM
    val addPackageName : PROGRAM -> Ast.IDENT list -> PROGRAM
    val getPackageNames : PROGRAM -> Ast.IDENT list list

end
