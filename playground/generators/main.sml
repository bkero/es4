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

(* An example ES4 program generating 10 Fibonacci numbers. *)

functor Example (structure G : GENERATOR) =
struct
    open Value

    fun fib () =
        G.make (fn (g) =>
                    let val rec fib = fn (i,j) =>
                                      (
                                          G.yield (g, Number i);
                                          fib (j, i+j)
                                      )
                    in
                        fib (0,1)
                    end)

    fun main (arg0:string, restArgs:string list) =
    (
        G.run(fn () =>
                 let val g = fib()
                     val rec loop = fn i =>
                                       if i < 10 then
                                       (
                                           print (G.send (g, Undefined));
                                           loop (i+1)
                                       )
                                       else ()
                 in
                     loop 0
                 end);
        0
    )
    handle InternalError s => (TextIO.print ("internal error: " ^ s ^ "\n"); 1)
end;

structure Main = Example(structure G = Generator(functor MkCoroutine = ShiftCoroutine));
