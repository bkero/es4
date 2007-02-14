(* A functional interface to SML/NJ's primitive callcc facility,
   similar in style to Scheme's call/cc.
 *)

signature CALLCC =
sig
    type void
    val coerce : void -> 'a
    val jump : ('a -> void) -> 'a -> 'b
    val callcc : (('a -> void) -> 'a) -> 'a
end

structure Callcc : CALLCC =
struct
    structure Native = SMLofNJ.Cont

    datatype void = VOID of void

    fun coerce (VOID v) = coerce v
    fun jump k v = coerce (k v)
    fun callcc f =
        Native.callcc (fn k => f (fn x => Native.throw k x))
end
