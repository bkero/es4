(* An abstract interface for coroutines, since I have written three
   different implementations (see coroutine_*.sml).
 *)

signature COROUTINE =
sig
    type result
    type t

    (* Operations on coroutines. *)
    val new     : ((t * result) -> result) -> t
    val switch  : (t * result) -> result
    val kill    : t -> unit

    (* Test the state of a coroutine. *)
    val newborn : t -> bool
    val alive   : t -> bool
    val running : t -> bool

    (* A wrapper for the main function; only really needed for CML. *)
    val run     : (unit -> unit) -> unit
end;

funsig MK_COROUTINE (type result) = COROUTINE
    where type result = result;
