(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 * 
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 * 
 *    2. All liability and responsibility for the implementation or other
 * use of this Reference Implementation rests with the implementor, and
 * not with any of the parties who contribute to, or who own or hold any
 * copyright in, this Reference Implementation.
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

structure Utils =
struct

fun when (b : bool) (thunk : unit -> 'a) : unit =
    if b then (thunk(); ()) else ()

fun unless (b : bool) (thunk : unit -> 'a) : unit =
    when (not b) thunk

end

structure ListUtils =
struct

fun contains pred ls = case List.find pred ls of
                            NONE => false
                          | SOME _ => true

fun repeat (f:unit -> 'a option) =
    let fun repeat' (f, ls) =
            case f() of
                 NONE => List.rev ls
               | SOME x => repeat' (f, x::ls)
    in
        repeat' (f,[])
    end

fun sep x [] = []
  | sep x [y] = [y]
  | sep x (y::ls) = y::(x::(sep x ls))

end

structure StringUtils =
struct

open ListUtils

fun join s ss = String.concat (sep s ss)

fun stripLeft s =
    let fun stripLeft' s i = if (i >= (String.size s)) then
                                 ""
                             else if (Char.isSpace(String.sub (s, i))) then
                                 stripLeft' s (i + 1)
                             else
                                 String.extract (s, i, NONE)
    in
        stripLeft' s 0
    end

fun stripRight s =
    let fun stripRight' s i = if (i < 0) then
                                  ""
                              else if (Char.isSpace(String.sub (s, i))) then
                                  stripRight' s (i - 1)
                              else
                                  String.extract (s, 0, SOME (i + 1))
    in
        stripRight' s ((String.size s) - 1)
    end

fun strip s = stripRight (stripLeft s)

fun split cs s = String.fields (fn c => (contains (fn c' => c' = c) cs)) s

(*fun splitWords s = List.map strip (String.fields (fn c => (c = #",")) s)*)

val toLower = String.map Char.toLower

val toUpper = String.map Char.toUpper

end
