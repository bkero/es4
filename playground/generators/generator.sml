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

(* The semantics of ES4 generators, built on top of the library
   of coroutines.
 *)

functor Generator(functor MkCoroutine : MK_COROUTINE) : GENERATOR =
struct
    open Value

    datatype signal = Yield of VALUE (* client <=  generator *)
                    | Throw of VALUE (* client <=> generator *)
                    | Send of VALUE  (* client  => generator *)
                    | Close          (* client <=  generator *)

    structure Coroutine : COROUTINE = MkCoroutine (type result = signal);

    (* This unnecessary polymorphism is a workaround for an SML/NJ compiler bug. *)
    datatype 'a generator = Generator of 'a
    type t = Coroutine.t generator

    fun yield (Generator c, v) =
        if not (Coroutine.running c) then
            raise InternalError "yield from dormant or dead generator"
        else
            case Coroutine.switch (c, Yield v) of
                 Send v' => v'
               | Throw e => raise Thrown e
               | _ => raise InternalError "generator protocol"

    fun send (Generator c, v) =
        if Coroutine.running c then
            raise InternalError "already running"
        else if (Coroutine.newborn c) andalso (v <> Undefined) then
            raise Thrown (String "newborn generator")
        else if not (Coroutine.alive c) then
            raise Thrown StopIteration
        else
            case Coroutine.switch (c, Send v) of
                 Yield v' => v'
               | Throw e => raise Thrown e
               | Close => raise Thrown StopIteration
               | _ => raise InternalError "generator protocol"

    fun throw (Generator c, v) =
        if Coroutine.running c then
            raise InternalError "already running"
        else if not (Coroutine.alive c) then
            raise Thrown v
        else
            case Coroutine.switch (c, Throw v) of
                 Yield v' => v'
               | Throw e => raise Thrown e
               | Close => raise Thrown v
               | _ => raise InternalError "generator protocol"

    fun close (Generator c) =
        if Coroutine.running c then
            raise InternalError "already running"
        else
            Coroutine.kill c

    fun make f =
        Generator (Coroutine.new (fn (c, _) =>
                                  (
                                      (f (Generator c); Close)
                                      handle Thrown v => Throw v
                                  )))

    val run = Coroutine.run
end;
