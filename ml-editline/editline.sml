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

structure EditLine : EDITLINE = struct

(* -------------------------------------------------------------------------- *
 * structure EditLine.Debug                                                   *
 * -------------------------------------------------------------------------- *)

structure Debug = struct

    fun msg s =
        let
            open TextIO
        in
            output (stdErr, s ^ "\n");
            flushOut stdErr
        end;

end;

(* -------------------------------------------------------------------------- *
 * structure EditLine.Text                                                    *
 * -------------------------------------------------------------------------- *)

structure Text = struct

    local

        val killed : string ref = ref "";

    in

    fun insert (s, into, at) =
        let
            val seq = Substring.extract (into, 0, NONE);
            val (prefix, suffix) = Substring.splitAt (seq, at);
        in
            Substring.concatWith s [prefix, suffix]
        end;

    fun remove (s, at, len) =
        let
            val prefix = Substring.substring (s, 0, at);
            val suffix = Substring.extract (s, at + len, NONE);
        in
            Substring.concat [prefix, suffix]
        end;

    fun kill s = (killed := s);

    fun yank () = (!killed);

    fun leftWord (s, at) =
        let
            fun firstWordChar (s, at) =
                if (at < 0) orelse (Char.isAlphaNum (String.sub (s, at))) then
                    at
                else
                    firstWordChar (s, at - 1);

            fun lastWordChar (s, at) =
                if (at < 0) orelse (not (Char.isAlphaNum (String.sub (s, at)))) then
                    at
                else
                    lastWordChar (s, at - 1);
        in
            at - (lastWordChar (s, (firstWordChar (s, at - 1)))) - 1
        end;

    fun rightWord (s, at) =
        let
            fun firstWordChar (s, at) =
                if (at >= (String.size s)) orelse (Char.isAlphaNum (String.sub (s, at))) then
                    at
                else
                    firstWordChar (s, at + 1);

            fun lastWordChar (s, at) =
                if (at >= (String.size s)) orelse (not (Char.isAlphaNum (String.sub (s, at)))) then
                    at
                else
                    lastWordChar (s, at + 1);
        in
            (lastWordChar (s, (firstWordChar (s, at)))) - at
        end;

    fun forwardsRest (s, at) =
        Substring.extract (s, at, NONE);

    end;

end;

(* -------------------------------------------------------------------------- *
 * structure EditLine.Display                                                 *
 * -------------------------------------------------------------------------- *)

structure Display = struct

    type DISPLAY = { prompt: string,
                     text: string ref,
                     position: int };

    local

    fun physicalPosition (position : int)
        : int * int =
        let
            val (_, cols) = Terminal.size ();
        in
            (Int.quot (position, cols), Int.rem (position, cols))
        end;

    fun motion (position : int, offset : int)
        : int * int =
        let
            val (thisRow, thisCol) = physicalPosition position;
            val (newRow, newCol) = physicalPosition (position + offset);
        in
            (newRow - thisRow, newCol - thisCol)
        end;

    in

    fun flush () = TextIO.flushOut TextIO.stdOut;

    (* clears entire display from properly synched cursor position *)
    fun clear ({ position, prompt, text, ... } : DISPLAY) : unit =
        let
            val promptSize = String.size prompt;
            val textSize = String.size (!text);

            val (thisRow, _) = physicalPosition (promptSize + position);
            val (lastRow, _) = physicalPosition (promptSize + textSize);

            fun gotoLastRowFrom row =
                if row = lastRow then () else
                (Terminal.moveDown (); gotoLastRowFrom (row + 1));

            fun clearRowsFrom row =
                (Terminal.moveToStart ();
                 Terminal.clearToEnd ();
                 if row = 0 then () else
                 (Terminal.moveUp (); clearRowsFrom (row - 1)));
        in
            gotoLastRowFrom thisRow;
            clearRowsFrom lastRow;
            Terminal.moveToStart ()
        end;

    (* moves properly synched cursor position *)
    fun moveCursor (from : int, to : int) =
        let
            val offset = to - from;
        in
            Terminal.move (motion (from, offset))
        end;

    fun wrap (position, prompt) =
        if Terminal.autowrap () then () else
        let
            val promptSize = String.size prompt;
            val (_, col) = physicalPosition (promptSize + position);
        in
            if col <> 0 then () else
                TextIO.print "\r\n"
        end;

    (* draws entire display wherever the console cursor happens to be *)
    fun draw (display as { position, prompt, ... } : DISPLAY) =
        let
            open TextIO
    
            val text = !(#text display);
            val textSize = String.size text;
            val promptSize = String.size prompt;
        in
            output (stdOut, prompt);
            output (stdOut, text);
            flush ();
            wrap (textSize, prompt);
            moveCursor (promptSize + textSize, promptSize + position);
            flush ()
        end;

    end;

end;

(* -------------------------------------------------------------------------- *
 * structure EditLine.History                                                 *
 * -------------------------------------------------------------------------- *)

structure History = struct

    type ENTRY = { index: int,
                   original: string,
                   contents: string ref };

    val history : ENTRY list ref = ref [];

    fun makeBuffer () : string ref = ref "";

    fun nextIndex [] = 0
      | nextIndex ({ index, ... } :: _) = index + 1;

    fun save (buffer : string ref) : string option =
        (case !buffer of
             "" => SOME ""
           | s =>
             let
                 val old = !history;
                 val entry = { index = nextIndex old,
                               original = s,
                               contents = buffer };
             in
                 history := entry :: old;
                 SOME s
             end);

    fun dup ({ contents, original, ... } : ENTRY) : string option =
        (case !contents of
             "" => (contents := original; SOME "")
           | s =>
             let
                 val old = !history;
                 val entry = { index = nextIndex old,
                               original = s,
                               contents = ref s };
             in
                 history := entry :: old;
                 contents := original;
                 SOME s
             end);

end;

fun isPrintable ch =
    (Char.<= (#" ", ch)) andalso (Char.<= (ch, #"~"));

fun loop (display as { position, prompt, text } : Display.DISPLAY,
          up : History.ENTRY list,
          down : History.ENTRY list,
          top : string ref)
    : string option =
    let
        fun continue () =
            loop (display, up, down, top);

        fun moveVertical (this', up', down') =
            let
                val display' = { prompt = prompt,
                                 text = this',
                                 position = String.size (!this') };
            in
                Display.clear display;
                Display.draw display';
                loop (display', up', down', top)
            end;

        fun moveHorizontal i =
            let
                val promptSize = String.size prompt;
                val position' = Int.min (Int.max (i, 0), String.size (!text));
                val display' = { prompt = prompt,
                                 text = text,
                                 position = position' };
            in
                Display.moveCursor (promptSize + position, promptSize + position');
                Display.flush ();
                loop (display', up, down, top)
            end;

        fun moveUp () =
            (case up of
                 [] => continue ()
               | e :: es => moveVertical (#contents e, es, e :: down));

        fun moveDown () =
            (case down of
                 [] => continue ()
               | [e] => moveVertical (top, e :: up, [])
               | (e1 :: e2 :: es) => moveVertical (#contents e2, e1 :: up, e2 :: es));

        fun moveLeft n =
            moveHorizontal (position - n);

        fun moveRight n =
            moveHorizontal (position + n);

        fun moveHome () =
            moveHorizontal 0;

        fun moveEnd () =
            moveHorizontal (String.size (!text));

        fun insertString s =
            let
                open TextIO;
                val size = String.size s;
                val position' = position + size;
                val current = !text;
                val display' = { prompt = prompt,
                                 text = text,
                                 position = position' };
            in
                (if position = (String.size current) then
                     (output (stdOut, s);
                      Display.wrap (position', prompt);
                      Display.flush ();
                      text := (current ^ s))
                 else
                     (Display.clear display;
                      text := Text.insert (s, current, position);
                      Display.draw display';
                      Display.flush ()));
                loop (display', up, down, top)
            end;

        (* TODO: take UNIT, DIRECTION *)
        fun backspace n =
            let
                val n = Int.min (position, n);
                val position' = position - n;
                val current = !text;
                val display' = { prompt = prompt,
                                 text = text,
                                 position = position' };
            in
                Display.clear display;
                text := (Text.remove (current, position', n));
                Display.draw display';
                Display.flush ();
                loop (display', up, down, top)
            end;

        fun delete n =
            let
                val current = !text;
                val n = Int.min ((String.size current) - position, n);
            in
                Display.clear display;
                text := (Text.remove (current, position, n));
                Display.draw display;
                Display.flush ();
                continue ()
            end;

        fun killLeft n =
            let
                val sub = String.substring (!text, position - n, n);
            in
                Text.kill sub;
                backspace n
            end;

        fun killRight n =
            let
                val sub = String.substring (!text, position, n);
            in
                Text.kill sub;
                delete n
            end;

        fun accept () =
            let
                val promptSize = String.size prompt;
                val textSize = String.size (!text);
            in
                Display.moveCursor (promptSize + position, promptSize + textSize);
                Display.flush ();
                TextIO.print "\r\n";
                case down of
                    [] => History.save text
                  | (h :: _) => History.dup h
            end;

        fun message s =
            let
                val promptSize = String.size prompt;
                val textSize = String.size (!text);
            in
                Display.moveCursor (promptSize + position, promptSize + textSize);
                Display.flush ();
                TextIO.print ("\r\n" ^ s ^ "\r\n");
                Display.draw display;
                Display.flush();
                continue ()
            end;
    in
        case Terminal.read () of
            SOME (Terminal.KEYchar #"\n", false) => accept ()
          | SOME (Terminal.KEYchar #"\r", false) => accept ()
          | SOME (Terminal.KEYenter, false) => accept ()
          | SOME (Terminal.KEYchar #"\b", false) => backspace 1
          | SOME (Terminal.KEYchar #"\127", false) => backspace 1
          | SOME (Terminal.KEYbackspace, false) => backspace 1
          | SOME (Terminal.KEYchar #"\^A", false) => moveHome ()
          | SOME (Terminal.KEYchar #"\^B", false) => moveLeft 1
          | SOME (Terminal.KEYchar #"\^D", false) => (case !text of
                                                          "" => NONE
                                                        | _ => delete 1)
          | SOME (Terminal.KEYchar #"\^E", false) => moveEnd ()
          | SOME (Terminal.KEYchar #"\^F", false) => moveRight 1
          | SOME (Terminal.KEYchar #"\^K", false) => killRight ((String.size (!text)) - position)
          | SOME (Terminal.KEYchar #"\^P", false) => moveUp ()
          | SOME (Terminal.KEYchar #"\^N", false) => moveDown ()
          (* (Terminal.KEYchar #"\^T", false) *)
          | SOME (Terminal.KEYchar #"\^Y", false) => insertString (Text.yank ())
          | SOME (Terminal.KEYhome, false) => moveHome ()
          | SOME (Terminal.KEYend, false) => moveEnd ()
          | SOME (Terminal.KEYup, false) => moveUp ()
          | SOME (Terminal.KEYdown, false) => moveDown ()
          | SOME (Terminal.KEYleft, false) => moveLeft 1
          | SOME (Terminal.KEYright, false) => moveRight 1
          (* this is just for debugging *)
       (* | (Terminal.KEYinsert, false) =>
            insertString "12345678901234567890123456789012345678901234567890123456789012345678901234567"
           *)
          | SOME (Terminal.KEYdelete, false) => delete 1
          | SOME (Terminal.KEYchar ch, false) =>
            if isPrintable ch then
                insertString (String.str ch)
            else
                continue ()
          | SOME (Terminal.KEYchar #"b", true) => moveLeft (Text.leftWord (!text, position))
          | SOME (Terminal.KEYchar #"d", true) => killRight (Text.rightWord (!text, position))
          | SOME (Terminal.KEYchar #"f", true) => moveRight (Text.rightWord (!text, position))
          | SOME (Terminal.KEYchar #"\127", true) => killLeft (Text.leftWord (!text, position))
          | SOME (Terminal.KEYchar #"\b", true) => killLeft (Text.leftWord (!text, position))
          | SOME (Terminal.KEYbackspace, true) => killLeft (Text.leftWord (!text, position))
          | _ => continue ()
    end;

fun readLine (prompt : string) : string option =
    let
        val top = History.makeBuffer ();
        val display = { prompt = prompt,
                        text = top,
                        position = 0 };
    in
        Display.draw display;
        Display.flush ();
        loop (display, !History.history, [], top)
    end;

end;
