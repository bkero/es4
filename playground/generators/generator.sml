signature GENERATOR =
sig
    type G

    val make  : (G -> unit) -> G
    val yield : G * Value.value -> Value.value
    val send  : G * Value.value -> Value.value
    val throw : G * Value.value -> Value.value
    val close : G -> unit
end

functor GeneratorFun(structure Coroutine : COROUTINE) : GENERATOR =
    struct
        open Value

        datatype signal = Yield of value (* client <=  generator *)
                        | Throw of value (* client <=> generator *)
                        | Send of value  (* client  => generator *)
                        | Close          (* client <=  generator *)

        type coroutine = signal Coroutine.C

        datatype G = Generator of coroutine

        fun yield (Generator c, v) =
            if not (Coroutine.running c) then
                raise InternalError "yield from dormant or dead generator"
            else
                case Coroutine.switch (c, Yield v) of
                     Send v' => v'
                   | Throw e => raise (Thrown e)
                   | _ => raise InternalError "generator protocol"

        fun send (Generator c, v) =
            if Coroutine.running c then
                raise InternalError "already running"
            else if (Coroutine.newborn c) andalso (v <> Undefined) then
                raise (Thrown (String "newborn generator"))
            else if not (Coroutine.alive c) then
                raise (Thrown StopIteration)
            else
                case Coroutine.switch (c, Send v) of
                     Yield v' => v'
                   | Throw e => raise (Thrown e)
                   | Close => raise (Thrown StopIteration)
                   | _ => raise InternalError "generator protocol"

        fun throw (Generator c, v) =
            if Coroutine.running c then
                raise InternalError "already running"
            else if not (Coroutine.alive c) then
                raise (Thrown v)
            else
                case Coroutine.switch (c, Throw v) of
                     Yield v' => v'
                   | Throw e => raise (Thrown e)
                   | Close => raise (Thrown v)
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
    end
