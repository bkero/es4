signature ESCAPE =
sig
    type void
    val coerce : void -> 'a
    val escape : (('a -> void) -> 'a) -> 'a
end

structure Escape : ESCAPE =
struct
    datatype void = VOID of void
    fun coerce (VOID v) = coerce v
    local open SMLofNJ.Cont
    in
        fun escape f =
            callcc (fn k => f (fn x => throw k x))
    end
end

signature DELIMITED_CONTROL =
sig
    type result
    val shift : (('a -> result) -> result) -> 'a
    val reset : (unit -> result) -> result
end

functor DelimitedControl (type result) : DELIMITED_CONTROL =
struct
    open Escape

    exception MissingReset

    val mk : (result -> void) ref =
        ref (fn _ => raise MissingReset)

    fun abort x =
        coerce (!mk x)

    type result = result

    fun reset thunk =
        escape (fn k => let val mk0 = !mk
                        in
                            mk := (fn r => (mk := mk0; k r));
                            abort (thunk ())
                        end)

    fun shift f =
        escape (fn k => abort (f (fn v => reset (fn () => coerce (k v)))))
end
