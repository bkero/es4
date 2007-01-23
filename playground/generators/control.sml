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

    type result = result

    datatype meta_result = Succeeded of result
                         | Failed of exn

    exception MissingReset

    val mk : (meta_result -> void) ref =
        ref (fn _ => raise MissingReset)

    fun abort x =
        coerce (!mk x)

    fun reset thunk =
        (case escape (fn k => let val mk0 = !mk
                              in
                                  mk := (fn r => (mk := mk0; k r));
                                  let val v = (Succeeded (thunk ()))
                                              handle x => Failed x
                                  in
                                      abort v
                                  end
                              end) of
             Succeeded v => v
           | Failed exn => raise exn)

    fun shift f =
        escape (fn k =>
                   let val v = f (fn v =>
                                     reset (fn () =>
                                               coerce (k v)))
                   in
                       abort (Succeeded v)
                   end)
end
