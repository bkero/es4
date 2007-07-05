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

(* Coroutines implemented via CML threads. *)

functor ThreadCoroutine (type result) : COROUTINE =
struct
    open CML

    type result = result

    exception Death

    (* To close a coroutine, we send it NONE. *)
    datatype COROUTINE = Newborn of result option chan
                       | Paused of result option chan
                       | Running of result option chan
                       | Closed

    type t = COROUTINE ref

    fun new f = let val c = channel ()
                    val r = ref (Newborn c)
                    val thread = fn () =>
                                 (
                                     (case recv c of
                                           NONE => raise Death
                                         | SOME s => (
                                                         r := Running c;
                                                         let val s' = f (r, s) in
                                                             r := Closed;
                                                             send (c, SOME s')
                                                         end
                                                     ))
                                     handle Death => ()
                                 )
                in
                    spawn (thread);
                    r
                end

    fun switch (r, x) =
        case !r of
             Newborn c => (
                              r := Running c;
                              send (c, SOME x);
                              case recv c of
                                   NONE => raise Value.InternalError "coroutine protocol"
                                 | SOME x => x
                          )
           | Paused c => (
                             r := Running c;
                             send (c, SOME x);
                             case recv c of
                                  NONE => raise Value.InternalError "coroutine protocol"
                                | SOME x => x
                         )
           | Running c => (
                              r := Paused c;
                              send (c, SOME x);
                              case recv c of
                                   NONE => raise Death
                                 | SOME x => x
                          )
           | Closed => raise Value.InternalError "dead coroutine"

    fun kill r =
        case !r of
             Newborn c => (send (c, NONE); r := Closed)
           | Paused c => (send (c, NONE); r := Closed)
           | Running c => raise Value.InternalError "already executing"
           | Closed => ()

    fun newborn r =
        case !r of
             Newborn _ => true
           | _ => false

    fun alive r =
        case !r of
             Closed => false
           | _ => true

    fun running r =
        case !r of
             Running _ => true
           | _ => false

    fun run f = let val r : exn option ref = ref NONE
                in
                    (* CML ignores exceptions, so catch and save it. *)
                    RunCML.doit ((fn () =>
                                     f ()
                                     handle x => r := SOME x),
                                 NONE);
                    (* Propagate the saved exception to top-level. *)
                    case !r of
                         SOME x => raise x
                       | NONE => ()
                end
end;
