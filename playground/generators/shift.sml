(* An implementation of delimited continuations in terms of
   undelimited continuations, following:

   - http://calculist.blogspot.com/2006/11/filinskis-implementation-of-shift-and.html
   - http://calculist.blogspot.com/2007/01/non-native-shiftreset-with-exceptions.html
 *)

signature SHIFT =
sig
    type result
    val shift : (('a -> result) -> result) -> 'a
    val reset : (unit -> result) -> result
end

functor Shift (type result) : SHIFT =
struct
    open Callcc

    type result = result

    datatype meta_result = Succeeded of result
                         | Failed of exn

    exception MissingReset

    val mk : (meta_result -> void) ref =
        ref (fn _ => raise MissingReset)

    fun abort x =
        jump (!mk) x

    fun reset thunk =
        (case callcc (fn k => let val mk0 = !mk
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
        callcc (fn k =>
                   let val v = f (fn v =>
                                     reset (fn () => jump k v))
                   in
                       abort (Succeeded v)
                   end)
end
