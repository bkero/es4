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

fun charStr ch =
    if (Char.<= (#" ", ch)) andalso (Char.<= (ch, #"~")) then
        String.str ch
    else if (ch = #"\000") then
        "\\000"
    else if (Char.<= (#"\^A", ch)) andalso (Char.<= (ch, #"\^Z")) then
        "\\^" ^ (String.str (Char.chr ((Char.ord ch) + (Char.ord #"A") - 1)))
    else
        "\\" ^ (StringCvt.padLeft #"0" 3 (Int.toString (Char.ord ch)));

fun keyName (KEYchar c) = "KEYchar #\"" ^ (charStr c) ^ "\""
  | keyName KEYup = "KEYup"
  | keyName KEYdown = "KEYdown"
  | keyName KEYright = "KEYright"
  | keyName KEYleft = "KEYleft"
  | keyName KEYhome = "KEYhome"
  | keyName KEYend = "KEYend"
  | keyName KEYpageup = "KEYpageup"
  | keyName KEYpagedown = "KEYpagedown"
  | keyName KEYtab = "KEYtab"
  | keyName KEYbackspace = "KEYbackspace"
  | keyName KEYdelete = "KEYdelete"
  | keyName KEYinsert = "KEYinsert"
  | keyName KEYenter = "KEYenter";

val setupterm  = _import "setupterm" : int * int * int -> unit;
val tigetnum'  = _import "tigetnum" : string -> int;
val tigetflag' = _import "tigetflag" : string -> bool;
val tigetstr'  = _import "LookupEscapeSequence" : string * char array -> unit;
val tputs'     = _import "PrintEscapeSequence" : string -> unit;
val tparm'     = _import "BuildEscapeSequence" : string * int * char array -> unit;

fun tigetnum str = tigetnum' (str ^ "\000");

fun tigetflag str = tigetflag' (str ^ "\000");

fun contents buffer =
    let
        fun from 1024 = []
          | from i = (case Array.sub (buffer, i) of
                          #"\000" => []
                        | c => c :: (from (i+1)));
    in
        String.implode (from 0)
    end;

val tigetstr =
    let
        open Array;

        val buffer : char array = array (1024, #"\000");
        fun wrapped s = (update (buffer, 0, #"\000");
                         tigetstr' (s ^ "\000", buffer);
                         contents buffer);
    in
        wrapped
    end;

val tparm =
    let
        val buffer : char array = Array.array (1024, #"\000");
        fun wrapped (s, i) = (tparm' (s, i, buffer); contents buffer);
    in
        wrapped
    end;

fun tputs str = tputs' (str ^ "\000");

val _ = setupterm (0, Posix.FileSys.stdin, 0);

val (updateTTY, revertTTY) =
    let
        open Posix.TTY Posix.TTY.TC;

        val original = fieldsOf (getattr Posix.FileSys.stdin);

        val lflags =
            let
                open Posix.TTY.L;
            in
                clear (flags [icanon, echo, isig, iexten], #lflag original)
            end;

        val iflags =
            let
                open Posix.TTY.I;
            in
                clear (flags [istrip, inpck], #iflag original)
            end;

        val updated =
            { iflag = iflags,
              oflag = #oflag original,
              cflag = #cflag original,
              lflag = lflags,
              cc = #cc original,
              ispeed = #ispeed original,
              ospeed = #ospeed original };

        fun set fields =
            setattr (Posix.FileSys.stdin, sadrain, termios fields);
    in
        ((fn () => set updated), (fn () => set original))
    end;

val _ = updateTTY ();

val _ = OS.Process.atExit revertTTY;

(* http://www.zsh.org/mla/workers/1999/msg02076.html *)
val autowrap =
    let
        val flag = (tigetflag "am") andalso (not (tigetflag "xenl"));
    in
        fn () => flag
    end;

fun size () = (tigetnum "lines", tigetnum "cols");

fun clearLine () =
    let
        open TextIO;
    in
        output1 (stdOut, #"\r");
        tputs (tigetstr "el");
        flushOut stdOut
    end;

fun clearToEnd () =
    let
        open TextIO;
    in
        tputs (tigetstr "el");
        flushOut stdOut
    end;

fun move (rows, cols) =
    let
        val left = tigetstr "cub";
        val right = tigetstr "cuf";
        val up = tigetstr "cuu";
        val down = tigetstr "cud";
    in
        if rows < 0 then
            tputs (tparm (up, ~rows))
        else if rows > 0 then
            tputs (tparm (down, rows))
        else ();
        if cols < 0 then
            tputs (tparm (left, ~cols))
        else if cols > 0 then
            tputs (tparm (right, cols))
        else ()
    end;

fun moveUp () = tputs (tigetstr "cuu1");

fun moveDown () = tputs (tigetstr "cud1");

fun moveToStart () = TextIO.print "\r";

val specials : KEY Trie.t =
    let
        open List;

        val term = OS.Process.getEnv "TERM";

        val xterm = case term of
                        SOME "xterm" => true
                      | _ => false;

        fun nonxterm s = if xterm then NONE else SOME s;

        fun lookup s =
            case tigetstr s of
                "" => NONE
              | s' => SOME s';

        val dict = [(* http://invisible-island.net/ncurses/man/terminfo.5.html *)
                    (* http://www.gnu.org/software/termutils/manual/termutils-2.0/html_mono/tput.html *)

                    (lookup "cuu1",  KEYup),
                    (lookup "kcuu1", KEYup),
                    (lookup "kcud1", KEYdown),
                    (lookup "cuf1",  KEYright),
                    (lookup "kcuf1", KEYright),
                    (lookup "kbs",   KEYbackspace),
                    (lookup "cub1",  KEYbackspace),
                    (lookup "kcub1", KEYleft),
                    (lookup "dch1",  KEYdelete),
                    (lookup "kdch1", KEYdelete),
                    (lookup "kich1", KEYinsert),
                    (lookup "knp",   KEYpagedown),
                    (lookup "kpp",   KEYpageup),
                    (lookup "khome", KEYhome),
                    (lookup "kend",  KEYend),
                    (lookup "kent",  KEYenter),

                    (* http://cygwin.com/ml/cygwin-apps/2007-08/msg00155.html *)

                    (SOME "\027[A", KEYup),
                    (SOME "\027[B", KEYdown),
                    (SOME "\027[C", KEYright),
                    (SOME "\027[D", KEYleft),

                    (SOME "\027[a", KEYup),
                    (SOME "\027[b", KEYdown),
                    (SOME "\027[c", KEYright),
                    (SOME "\027[d", KEYleft),

                    (SOME "\027OA", KEYup),
                    (SOME "\027OB", KEYdown),
                    (SOME "\027OC", KEYright),
                    (SOME "\027OD", KEYleft),

                    (SOME "\027oA", KEYup),
                    (SOME "\027oB", KEYdown),
                    (SOME "\027oC", KEYright),
                    (SOME "\027oD", KEYleft),

                    (* xterm-specific escape sequences *)
 
                    (* on xterm this is inexplicably mapped to #"\n" *)
                    (if xterm then NONE else lookup "cud1", KEYdown),

                    (nonxterm "\027[1~", KEYhome),
                    (nonxterm "\027[3~", KEYdelete),
                    (nonxterm "\027[4~", KEYend)];

        val valsOf = mapPartial (fn (NONE,_) => NONE
                                  | (SOME s,k) => SOME (s,k));

        fun removeDups ls =
            let
                open List;

                fun removeDups' ([], accum) = accum
                  | removeDups' ((x :: ls'), accum) =
                    let
                        val accum' = if exists (fn y => y = x) accum then
                                         accum
                                     else
                                         x :: accum;
                    in
                        removeDups' (ls', accum')
                    end;
            in
                removeDups' (ls, [])
            end;
    in
        (* XXX: should fail/recover more gracefully from ambiguities *)
        Trie.fromDict (removeDups (valsOf dict))
    end;

fun raiseSignal signal =
    let
        open Posix.Process;

        val pid = Posix.ProcEnv.getpid ();
    in
        kill (K_PROC pid, signal)
    end;

fun read () =
    let
        type result = (KEY * bool) option;

        type 'a trie = 'a Trie.t;

        open Trie;

        fun getc (k : char -> result) : result =
            case TextIO.input1 TextIO.stdIn of
                (* Cygwin returns EOF on SIGCONT *)
                NONE => NONE
              | SOME #"\^C" => (raiseSignal Posix.Signal.int; NONE)
              | SOME #"\^Z" => (raiseSignal Posix.Signal.stop; NONE)
              | SOME ch => k ch;

        val meta : bool ref = ref false;

        fun step0 (ch0 : char) : result =
            case next (specials, Char.ord ch0) of
                LOST => SOME (KEYchar ch0, !meta)
              | FOUND key => SOME (key, !meta)
              | NEXT trie => step1 (trie, ch0)

        and step1 (trie : KEY trie, ch0 : char) : result =
            getc (fn ch =>
                     case next (trie, Char.ord ch) of
                         LOST => (meta := true; step0 ch)
                       | FOUND key => SOME (key, !meta)
                       | NEXT trie' => loop (trie', ch0))

        and loop (trie : KEY trie, ch0 : char) : result =
            getc (fn ch =>
                     case next (trie, Char.ord ch) of
                         LOST => NONE
                       | FOUND key => SOME (key, !meta)
                       | NEXT trie' => loop (trie', ch0));
    in
        getc step0
    end;

end;
