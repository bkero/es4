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

signature TRIE = sig

type 'a t;

exception Ambiguity of int list;

datatype 'a cursor =
         LOST
       | FOUND of 'a
       | NEXT of 'a t;

val empty : 'a t;
val build : (int list * 'a) list -> 'a t;
val fromDict : (string * 'a) list -> 'a t;
val insert : 'a t * int list * 'a -> 'a t;
val insertString : 'a t * string * 'a -> 'a t;
val find : 'a t * int list -> 'a cursor;
val findString : 'a t * string -> 'a cursor;
val next : 'a t * int -> 'a cursor;
val print : 'a t * ('a -> string) -> unit;

end;
