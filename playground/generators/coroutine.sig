(* An abstract interface for coroutines, since I have written three
   different implementations (see coroutine_*.sml).
 *)

signature COROUTINE =
sig
    type result
    type C

    (* Operations on coroutines. *)
    val new     : ((C * result) -> result) -> C
    val switch  : (C * result) -> result
    val kill    : C -> unit

    (* Test the state of a coroutine. *)
    val newborn : C -> bool
    val alive   : C -> bool
    val running : C -> bool

    (* A wrapper for the main function; only really needed for CML. *)
    val run     : (unit -> unit) -> unit
end;

funsig MK_COROUTINE (type result) = COROUTINE where type result = result;
