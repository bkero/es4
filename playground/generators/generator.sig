(* The interface for ES4 generators. *)

signature GENERATOR =
sig
    type G

    val make  : (G -> unit) -> G
    val yield : G * Value.VALUE -> Value.VALUE
    val send  : G * Value.VALUE -> Value.VALUE
    val throw : G * Value.VALUE -> Value.VALUE
    val close : G -> unit
end
