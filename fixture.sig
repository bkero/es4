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
    type TYEQ = (Ast.TY -> Ast.TY -> bool)
    val mergeRibs : TYEQ -> Ast.RIB -> Ast.RIB -> Ast.RIB

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
 * Units nest. Conceptually, the program can be considered "a unit that never closes", or 
 * "the level 0" unit; it contains a root RIB. Just beneath the program, there are special 
 * "top level" (level 1) units, that do close.
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
 * the fragment's rib to merge into the root rib, and the fragment's blocks to be appended
 * to the top block list.
 *
 * If the added fragment is a unit and the interpreter is running in strict mode,
 * it may run the verifier. It should only verify the new fixtures in the fragment's
 * rib (to avoid redundant verification), and it should verify them as a group, and it
 * should verify them in an environment under the (merged) root rib, including all 
 * fixtures from units that came before the fragment.
 *
 * Ast.TY values close over an environment. That environment refers to a RIB by ID. The
 * RIB it refers to -- and the parents of that RIB -- may be extended during definition 
 * time. If the TY is not in a closed top-level unit, the outermost enclosing RIB may
 * even be extended at runtime by dynamic evaluation constructs.
 * 
 * One might think this means that "failure to resolve a type name" can never be 
 * seen as a hard error, but this is not true. When we form an Ast.TY closure inside
 * a unit, the parent RIB_ID chain winds its way up to the root. When
 * performing a type judgment, we look at the TY:
 * 
 *  - *IF* the TY occurs within a closed top-level unit (meaning we have received the
      maximum amount of information we're going to get about the names in this unit).
 *  - *THEN* failure to resolve type names in the TY, under that top rib, is an error.
 *  - *ELSE* failure to resolve type names in the TY represents a possibly-transient
 *           error, and we need to back off and wait unless we're at the last possible
 *           moment (eg. trying to find a class for a 'new' expression or something).
 *
 * 
 *)


    type PROGRAM
    val mkProgram : int -> Ast.RIB -> PROGRAM
    val updateLangEd : PROGRAM -> int -> unit
    val getLangEd : PROGRAM -> int

    val resolveToFixture : PROGRAM -> Ast.MULTINAME -> (Ast.RIB_ID option) -> ((Ast.NAME * Ast.FIXTURE) option)
    val updateFixtureCache : PROGRAM -> (Ast.RIB_ID option) -> Ast.MULTINAME -> Ast.NAME -> Ast.FIXTURE -> unit

    val inGeneralRib : PROGRAM -> (Ast.RIB_ID option) -> bool
    val inTopUnitRib : PROGRAM -> (Ast.RIB_ID option) -> bool

    val allocGeneralRib : PROGRAM -> (Ast.RIB_ID option) -> Ast.RIB_ID
    val allocTopUnitRib : PROGRAM -> Ast.RIB_ID
    val getRibs : PROGRAM -> (Ast.RIB_ID option) -> (Ast.RIBS * bool)
    val ribIsClosed : PROGRAM -> (Ast.RIB_ID option) -> bool
    val saveRib : PROGRAM -> (Ast.RIB_ID option) -> (Ast.RIB) -> unit
    val extendRib : PROGRAM -> (Ast.RIB_ID option) -> (Ast.RIB) -> TYEQ -> unit
    val closeFragment : PROGRAM -> Ast.FRAGMENT -> (Ast.RIB_ID option) -> unit
    val getRootRib : PROGRAM -> Ast.RIB
    val addPackageName : PROGRAM -> Ast.IDENT list -> unit
    val getPackageNames : PROGRAM -> Ast.IDENT list list
    val getRibsForTy : PROGRAM -> Ast.TY -> (Ast.RIBS * bool)

end
