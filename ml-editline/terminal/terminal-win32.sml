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

structure Terminal : TERMINAL = struct

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

val FFI_readKey       = _import "ReadKey" : char ref * int ref * bool ref -> unit;
val FFI_getWindowSize = _import "GetWindowSize" : int ref * int ref -> unit;
val FFI_getPosition   = _import "GetPosition" : int ref * int ref -> unit;
val FFI_setPosition   = _import "SetPosition" : int * int -> unit;
val FFI_fill          = _import "FillOutput" : int * int * int * char -> unit;

fun read () =
    let
        val ch : char ref = ref #"\000";
        val special : int ref = ref 0;
        val alt : bool ref = ref false;
    in
        FFI_readKey (ch, special, alt);
        case !ch of
            #"\000" => (case !special of
                            16 => read ()
                          | 17 => read ()
                          | 18 => read ()
                          | 33 => SOME (KEYpageup, !alt)
                          | 34 => SOME (KEYpagedown, !alt)
                          | 35 => SOME (KEYend, !alt)
                          | 36 => SOME (KEYhome, !alt)
                          | 37 => SOME (KEYleft, !alt)
                          | 38 => SOME (KEYup, !alt)
                          | 39 => SOME (KEYright, !alt)
                          | 40 => SOME (KEYdown, !alt)
                          | 45 => SOME (KEYinsert, !alt)
                          | 46 => SOME (KEYdelete, !alt)
                          | _ => read ())
          | #"\026" => SOME (KEYchar #"\004", !alt)
          | #"\r" => SOME (KEYchar #"\n", !alt)
          | ch => SOME (KEYchar ch, !alt)
    end;

fun size () =
    let
        val rows : int ref = ref 0;
        val cols : int ref = ref 0;
    in
        FFI_getWindowSize (rows, cols);
        (!rows, !cols)
    end;

fun position () =
    let
        val row : int ref = ref 0;
        val col : int ref = ref 0;
    in
        FFI_getPosition (row, col);
        (!row, !col)
    end;

fun clearLine () =
    let
        val (_, cols) = size ();
        val (row, _) = position ();
    in
        FFI_fill (row, 0, cols, #" ")
    end;

fun clearToEnd () =
    let
        val (_, cols) = size ();
        val (row, col) = position ();
    in
        FFI_fill (row, col, cols - col, #" ")
    end;

fun move (rows, cols) =
    let
        val (row, col) = position ();
    in
        FFI_setPosition (row + rows, col + cols)
    end;

fun moveUp () =
    let
        val (row, col) = position ();
    in
        FFI_setPosition (row - 1, col)
    end;

fun moveDown () =
    let
        val (row, col) = position ();
    in
        FFI_setPosition (row + 1, col)
    end;

fun moveToStart () =
    let
        val (row, _) = position ();
    in
        FFI_setPosition (row, 0)
    end;

fun autowrap () = true;

end;
