signature COROUTINE =
sig
    type result
    type C

    val new     : ((C * result) -> result) -> C
    val switch  : (C * result) -> result
    val kill    : C -> unit
    val newborn : C -> bool
    val alive   : C -> bool
    val running : C -> bool
    val run     : (unit -> unit) -> unit
end
