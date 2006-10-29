signature COROUTINE =
sig
    type 'a C

    val new     : (('a C * 'a) -> 'a) -> 'a C
    val switch  : ('a C * 'a) -> 'a
    val kill    : 'a C -> unit
    val newborn : 'a C -> bool
    val alive   : 'a C -> bool
    val running : 'a C -> bool
    val run     : (unit -> unit) -> unit
end
