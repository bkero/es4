(* The semantics of ES4 generators, built on top of the library
   of coroutines.
 *)

functor Generator(functor MkCoroutine : MK_COROUTINE) : GENERATOR =
struct
    open Value

    datatype signal = Yield of VALUE (* client <=  generator *)
                    | Throw of VALUE (* client <=> generator *)
                    | Send of VALUE  (* client  => generator *)
                    | Close          (* client <=  generator *)

    structure Coroutine : COROUTINE = MkCoroutine (type result = signal);

    (* This unnecessary polymorphism is a workaround for an SML/NJ compiler bug. *)
    datatype 'a generator = Generator of 'a
    type t = Coroutine.t generator

    fun yield (Generator c, v) =
        if not (Coroutine.running c) then
            raise InternalError "yield from dormant or dead generator"
        else
            case Coroutine.switch (c, Yield v) of
                 Send v' => v'
               | Throw e => raise Thrown e
               | _ => raise InternalError "generator protocol"

    fun send (Generator c, v) =
        if Coroutine.running c then
            raise InternalError "already running"
        else if (Coroutine.newborn c) andalso (v <> Undefined) then
            raise Thrown (String "newborn generator")
        else if not (Coroutine.alive c) then
            raise Thrown StopIteration
        else
            case Coroutine.switch (c, Send v) of
                 Yield v' => v'
               | Throw e => raise Thrown e
               | Close => raise Thrown StopIteration
               | _ => raise InternalError "generator protocol"

    fun throw (Generator c, v) =
        if Coroutine.running c then
            raise InternalError "already running"
        else if not (Coroutine.alive c) then
            raise Thrown v
        else
            case Coroutine.switch (c, Throw v) of
                 Yield v' => v'
               | Throw e => raise Thrown e
               | Close => raise Thrown v
               | _ => raise InternalError "generator protocol"

    fun close (Generator c) =
        if Coroutine.running c then
            raise InternalError "already running"
        else
            Coroutine.kill c

    fun make f =
        Generator (Coroutine.new (fn (c, _) =>
                                  (
                                      (f (Generator c); Close)
                                      handle Thrown v => Throw v
                                  )))

    val run = Coroutine.run
end;
