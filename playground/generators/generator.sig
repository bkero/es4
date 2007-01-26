(* The interface for ES4 generators. *)

signature GENERATOR =
sig
    type t

    val make  : (t -> unit) -> t
    val yield : t * Value.VALUE -> Value.VALUE
    val send  : t * Value.VALUE -> Value.VALUE
    val throw : t * Value.VALUE -> Value.VALUE
    val close : t -> unit

    val run : (unit -> unit) -> unit
end;
