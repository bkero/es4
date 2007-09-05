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

(*
 * This is the main entry point for SML/NJ builds of the ES4 reference evaluator.
 *)

structure SMLofNJEntry = struct

exception noboot

fun main (argv0:string, argvRest:string list) =
    BackTrace.monitor
        (fn () =>
            let
                val argvRest = Main.startup argvRest
                val regs = Boot.boot ()
                    handle
                    LogErr.LexError e => (print ("**BOOT ERROR** LexError: " ^ e ^ "\n"); raise noboot)
                  | LogErr.ParseError e => (print ("**BOOT ERROR** ParseError: " ^ e ^ "\n"); raise noboot)
                  | LogErr.NameError e => (print ("**BOOT ERROR** NameError: " ^ e ^ "\n"); raise noboot)
                  | LogErr.TypeError e => (print ("**BOOT ERROR** TypeError: " ^ e ^ "\n"); raise noboot)
                  | LogErr.FixtureError e => (print ("**BOOT ERROR** FixtureError: " ^ e ^ "\n"); raise noboot)
                  | LogErr.DefnError e => (print ("**BOOT ERROR** DefnError: " ^ e ^ "\n"); raise noboot)
                  | LogErr.EvalError e => (print ("**BOOT ERROR** EvalError: " ^ e ^ "\n"); raise noboot)
                  | LogErr.MachError e => (print ("**BOOT ERROR** MachError: " ^ e ^ "\n"); raise noboot)
                  | LogErr.VerifyError e => (print ("**BOOT ERROR** VerifyError: " ^ e ^ "\n"); raise noboot)
                  | LogErr.HostError e => (print ("**BOOT ERROR** HostError: " ^ e ^ "\n"); raise noboot)
                  | LogErr.UnimplError e => (print ("**BOOT ERROR** UnimplError: " ^ e ^ "\n"); raise noboot)
                                            
                fun main' (argv0:string, argvRest:string list) =
                    BackTrace.monitor
                        (fn () => Main.main (regs, argv0, argvRest))
            in
                case argvRest of
                    ["-dump", filename] => (SMLofNJ.exportFn (filename, main'); 0)
                  | _ => main' (argv0, argvRest)
            end)
end
