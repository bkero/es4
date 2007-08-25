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

    val printFixture : Ast.FIXTURE_NAME * Ast.FIXTURE -> unit
    val getFixture : Ast.RIB -> Ast.FIXTURE_NAME -> Ast.FIXTURE
    val hasFixture : Ast.RIB -> Ast.FIXTURE_NAME -> bool
    val replaceFixture : Ast.RIB -> Ast.FIXTURE_NAME -> Ast.FIXTURE -> Ast.RIB
    val printRib : Ast.RIB -> unit

(* 
 * An ES4 program is an endless sequence of fragments. Fragments come in 3 flavours:
 *
 *  - Anonymous fragments, that are just free-form declarations and statements.
 *  - Package fragments, that enter a package namespace during their body but
 *    can revisit the same package name many times.
 *  - Unit fragments, that enter and then leave a unit name (which may have an 
 *    associated namespace) but *never re-enter* the unit name once they leave. 
 *    This is important: it means that a unit of a given name has a place where
 *    it "ends", inside a program.
 *
 * Units nest. Conceptually, the program can be considered "a unit that never closes". 
 * Or "the level 0" unit. Just beneath the program, there are special "top level" (level 1) 
 * units, that do close.
 * 
 * Top level units are important because they are what the verifier acts on. When a top
 * level unit closes, the verifier may check that unit. The verifier will try to resolve
 * type names in the unit. Any undefined type name in a top level unit (covering the
 * rib that hoists from the unit, and all its sub-units, and the rib that was present
 * in the program before the top level unit opened) is a strict-mode error. So top level units
 * represent the granularity of circular references the verifier will tolerate in strict mode.
 *
 * The parser reads a single top fragment. Any nested units in the fragment resolve as it goes.
 *
 * Once a top fragment is parsed, it is defined. Some implementations may define while 
 * parsing: it can be done in 1 pass. In this implementation they're separate passes.
 *
 * When defined, a fragment produces a hoisted rib.
 *
 * The fragment and rib are added to the program. Adding a fragment and its rib causes
 * the fragment's rib to merge into the top rib, and the fragment's blocks to be appended
 * to the top block list.
 *
 * If the added fragment is a unit and the interpreter is running in strict mode,
 * it may run the verifier. It should only verify the new fixtures in the fragment's
 * rib (to avoid redundant verification), and it should verify them as a group, and it
 * should verify them in an environment under the (merged) top rib, including all 
 * fixtures from units that came before the fragment.
 *
 * Ast.TY values close over an environment. That environment contains ribs but it
 * also implicitly contains a link to the extensible "outer" environment: the top
 * rib, that may be extended with new types at any time, as new fragments arrive. 
 * 
 * One might think this means that "failure to resolve a type name" can never be 
 * seen as a hard error, but this is not true. When we form an Ast.TY closure inside
 * a unit, we record the top unit name in the Ast.TY. When performing a type judgment,
 * we look at the TY:
 * 
 *  - *IF* the TY has an associated top-level unit name (#unit of Ast.TY)
 *  - *AND* the PROGRAM has a top rib associated with that unit name, meaning that
 *          the unit has closed and we have a definite maximum amount of knowledge
 *          to use for judging type expressions in one of its sub-units.
 *  - *THEN* failure to resolve type names in the TY, under that top rib, is an error
 *  - *ELSE* failure to resolve type names in the TY represents a possibly-transient
 *           error, and we need to back off and wait unless we're at the last possible
 *           moment (eg. trying to find a class for a 'new' expression or something).
 *
 * 
 *)

    (* FIXME: the coupling between type.sml and fixture.sml suggests re-merging them *)
    type TYEQ = (Ast.TY -> Ast.TY -> bool)
    val mergeFixtures : TYEQ -> 
                        ((Ast.FIXTURE_NAME * Ast.FIXTURE) * Ast.RIB) -> 
                        Ast.RIB

    type PROGRAM
    val mkProgram : Ast.RIB -> PROGRAM
    val closeTopFragment : PROGRAM -> Ast.FRAGMENT -> TYEQ -> PROGRAM
    val getTopFixture : PROGRAM -> Ast.NAME -> Ast.FIXTURE 
    val getTopRib : PROGRAM -> Ast.RIB
    val extendTopRib : PROGRAM -> Ast.RIB -> TYEQ -> PROGRAM
    val getTopRibForUnit : PROGRAM -> Ast.UNIT_NAME -> Ast.RIB option
    val getTopBlocks : PROGRAM -> Ast.BLOCK list
    val getPackageNames : PROGRAM -> Ast.IDENT list list
    val getCurrFullRibs : PROGRAM -> Ast.RIBS -> Ast.RIBS
    val getFullRibsForTy : PROGRAM -> Ast.TY -> (Ast.RIBS * bool)

    val instanceType : PROGRAM -> Ast.NAME -> Ast.TY
    val isClass : PROGRAM -> Ast.NAME -> bool
    val getClass : PROGRAM -> Ast.NAME -> Ast.CLS
    val instanceOf : PROGRAM -> 
                     Ast.TYPE_EXPR -> 
                     Ast.TYPE_EXPR -> 
                     (Ast.TYPE_EXPR -> Ast.TYPE_EXPR -> bool) -> 
                     (Ast.TYPE_EXPR -> Ustring.STRING list) -> 
                     (Ustring.STRING) -> 
                     bool

end
