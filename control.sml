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

(*
 * This library provides low-level continuation management for the
 * implementation of generators.
 * 
 * For more information about the implementation technique, see:
 * 
 * David Herman, Functional Pearl: The Great Escape, or how to jump the
 *   border without getting caught. ICFP 2007.
 *   http://doi.acm.org/10.1145/1291220.1291177
 *   http://www.ccs.neu.edu/home/dherman
 *)

signature ESCAPE =
sig
    type VOID
    val coerce : VOID -> 'a
    val escape : (('a -> VOID) -> 'a) -> 'a
end;

structure Escape : ESCAPE =
struct
    datatype VOID = Void of VOID;
    fun coerce (Void v) = coerce v;
    open SMLofNJ.Cont;
    fun abort thunk = throw (isolate thunk) ();
    fun escape f =
        callcc (fn k => abort (fn () => (f (fn x => throw k x); ())));
end;

signature CONTROL =
sig
    (* for our purposes, this will be instantiated at Mach.GEN_SIGNAL *)
    type RESULT
    val shift : (('a -> RESULT) -> RESULT) -> 'a
    val reset : (unit -> RESULT) -> RESULT
end;

functor Control (type RESULT) : CONTROL =
struct
    open Escape;

    type RESULT = RESULT

    (* representation of a suspended "stack unwinding" *)
    datatype SUSPENDED = Return of RESULT
                       | Raise of exn

    val metak : (SUSPENDED -> VOID) list ref = ref []

    fun push k =
        metak := k::(!metak)

    fun pop () =
        case !metak of
            (* XXX: LogErr.hostError? *)
            [] => raise (Fail "missing reset")
          | k::ks => (metak := ks; k)

    fun reflect x =
        case x of
            Return v => v
          | Raise exn => raise exn

    fun reify thunk =
        (Return (thunk ()))
        handle exn => Raise exn

    fun reset thunk =
        reflect
        (escape (fn k => (push k;
                          let
                              val result = reify thunk
                              val return = pop ()
                          in
                              coerce (return result)
                          end)))
    fun shift f =
        (escape (fn k =>
                    let
                        fun captured v = reset (fn () => coerce (k v))
                        val result = Return (f captured)
                                     handle exn => Raise exn
                        val return = pop ()
                    in
                        coerce (return result)
                    end))
end;
