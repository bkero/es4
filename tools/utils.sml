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
