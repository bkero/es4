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
 * This is the main entry point for MLton builds of the ES4 reference evaluator.
 *)

open MLton.World;

(*
fun main regs name args =
    case Main.main (regs, name, args) of
        0 => OS.Process.exit OS.Process.success
      | _ => OS.Process.exit OS.Process.failure;

fun error s =
    (TextIO.output (TextIO.stdErr, "error: " ^ s); TextIO.flushOut TextIO.stdErr);

fun resume world =
    if OS.FileSys.access (world, [OS.FileSys.A_READ])
    then load world
    else (error (world ^ " not found\n");
          OS.Process.exit OS.Process.failure);

fun sibling path1 file =
    let
        val {dir, ...} = OS.Path.splitDirFile path1
    in
        OS.Path.joinDirFile {dir = dir, file = file}
    end;
*)

fun dump (s:string) : bool =
    (case save s of
         Original => false
       | Clone => true)

val _ = Main.main' dump;

(*
val _ =
    let
        val exe = CommandLine.name()
        val argvRest = Main.startup (CommandLine.arguments())
    in
        case argvRest of
            ["-dump", filename] =>
                let
                    val regs = Boot.boot()
                in
                    case save filename of
                        Original => ()
                      | Clone => main regs exe argvRest
                end
          | _ => resume (sibling exe "es4.world")
    end;
*)
