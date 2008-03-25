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

structure Trie :> TRIE = struct

exception Ambiguity of int list;

datatype 'a node =
         LEAF of int * 'a
       | BRANCH of int * 'a trie

withtype 'a trie = 'a node list;

type 'a t = 'a trie;

datatype 'a cursor =
         LOST
       | FOUND of 'a
       | NEXT of 'a t;

val empty : 'a t = [];

fun nodeKey (LEAF (key, _)) = key
  | nodeKey (BRANCH (key, _)) = key;

fun explodeKeys (s : string) =
    map Char.ord (String.explode s);

fun split (trie : 'a trie, key : int) =
    let
        val (left, right) = List.partition (fn x => (nodeKey x) < key) trie;
    in
        case right of
            [] => (left, NONE, right)
          | (first :: rest) =>
            if (nodeKey first) = key then
                (left, SOME first, rest)
            else
                (left, NONE, right)
    end;

fun tail (key : int, keys : int list, value : 'a)
    : 'a node =
    case keys of
        [] => LEAF (key, value)
      | (key' :: keys') => BRANCH (key, [tail (key', keys', value)]);

fun insert (trie : 'a trie, keys : int list, value : 'a)
    : 'a trie =
    let
        fun ambiguous prefix =
            raise Ambiguity (rev prefix);

        fun insert' (trie : 'a trie, keys : int list, value : 'a, prefix : int list) =
            case keys of
                [] => raise List.Empty
              | [key] =>
                (case split (trie, key) of
                     (left, NONE, right) => left @ ((LEAF (key, value)) :: right)
                   | _ => ambiguous (key :: prefix))
              | (key :: keys') =>
                (case split (trie, key) of
                     (left, NONE, right) =>
                     left @ (tail (key, keys', value)) :: right
                   | (left, SOME (LEAF _), right) =>
                     ambiguous (key :: prefix)
                   | (left, SOME (BRANCH (_, trie')), right) =>
                     left @ (BRANCH (key, insert' (trie', keys', value, key :: prefix))) :: right);
    in
        insert' (trie, keys, value, [])
    end;

fun insertString (trie : 'a trie, keys : string, value : 'a)
    : 'a trie =
    insert (trie, explodeKeys keys, value);

fun next (trie : 'a trie, key : int) =
    case split (trie, key) of
        (_, NONE, _) => LOST
      | (_, SOME (LEAF (_, value)), _) => FOUND value
      | (_, SOME (BRANCH (_, trie')), _) => NEXT trie';

fun find (trie : 'a trie, keys : int list) =
    let
        fun loop (LOST, _) = LOST
          | loop (cursor, []) = cursor
          | loop (FOUND _, (_ :: _)) = LOST
          | loop (NEXT trie', (key :: keys)) =
            loop (next (trie', key), keys);
    in
        loop (NEXT trie, keys)
    end;

fun findString (trie : 'a trie, keys : string) =
    find (trie, explodeKeys keys);

fun build lists =
    foldl (fn ((list, value), trie) => insert (trie, list, value))
          []
          lists;

fun fromDict dict =
    let
        fun toList (string, value) = (explodeKeys string, value);
    in
        build (map toList dict)
    end;

fun print (trie, toString) =
    let
        open TextIO;

        fun string s = output (stdOut, s);

        fun char i =
            let
                val c = Char.chr i;
            in
                if (Char.<= (#" ", c)) andalso (Char.<= (c, #"~")) then
                    string (String.str c)
                else
                    string ("\\" ^ (Int.toString i))
            end;

        fun node prefix n =
            (string prefix;
             char (nodeKey n);
             case n of
                 LEAF (_, value) =>
                 string (" (" ^ (toString value) ^ ")")
               | BRANCH (_, t) =>
                 app (fn n' => (string "\n"; node (" " ^ prefix) n')) t);
    in
        node "" (BRANCH (0, trie));
        print "\n"
    end;

end;
