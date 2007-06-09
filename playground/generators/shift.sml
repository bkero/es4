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

(* An implementation of delimited continuations in terms of
   undelimited continuations, following:

   - http://calculist.blogspot.com/2006/11/filinskis-implementation-of-shift-and.html
   - http://calculist.blogspot.com/2007/01/non-native-shiftreset-with-exceptions.html
 *)

signature SHIFT =
sig
    type result
    val shift : (('a -> result) -> result) -> 'a
    val reset : (unit -> result) -> result
end

functor Shift (type result) : SHIFT =
struct
    open Callcc

    type result = result

    datatype meta_result = Succeeded of result
                         | Failed of exn

    exception MissingReset

    val mk : (meta_result -> void) ref =
        ref (fn _ => raise MissingReset)

    fun abort x =
        jump (!mk) x

    fun reset thunk =
        (case callcc (fn k => let val mk0 = !mk
                              in
                                  mk := (fn r => (mk := mk0; k r));
                                  let val v = (Succeeded (thunk ()))
                                              handle x => Failed x
                                  in
                                      abort v
                                  end
                              end) of
             Succeeded v => v
           | Failed exn => raise exn)

    fun shift f =
        callcc (fn k =>
                   let val v = f (fn v =>
                                     reset (fn () => jump k v))
                   in
                       abort (Succeeded v)
                   end)
end
