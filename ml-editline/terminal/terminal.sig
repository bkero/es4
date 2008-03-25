(*
 * Copyright (c) 2007 David Herman
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

signature TERMINAL = sig

datatype KEY =
         KEYchar of char
       | KEYup
       | KEYdown
       | KEYright
       | KEYleft
       | KEYhome
       | KEYend
       | KEYpageup
       | KEYpagedown
       | KEYtab
       | KEYbackspace
       | KEYdelete
       | KEYinsert
       | KEYenter;

val read        : unit -> (KEY * bool) option;
val size        : unit -> int * int;
val clearLine   : unit -> unit;
val clearToEnd  : unit -> unit;
val move        : int * int -> unit;
val moveUp      : unit -> unit;
val moveDown    : unit -> unit;
val moveToStart : unit -> unit;
val autowrap    : unit -> bool;

(* TODO: val isatty : unit -> bool *)

end;
