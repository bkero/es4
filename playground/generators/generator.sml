structure Generator =
    struct
        open CML
        open Value

        datatype signal = Yield of value (* client <=  producer *)
                        | Throw of value (* client <=> producer *)
                        | Send of value  (* client  => producer *)
                        | Close          (* client  => producer *)

        datatype state = NEWBORN (* waiting for first send or next *)
                       | OPEN    (* started via next               *)
                       | RUNNING (* currently active               *)
                       | CLOSED  (* closed and no longer usable    *)

        exception WrongDirection (* internal error: signal sent in the wrong direction *)
        exception AbortGenerator (* internal: raised to abort a generator function     *)

        datatype generator = Generator of (signal chan * state ref)

        (* blocks a new generator function before the first send *)
        fun waitForStart (Generator (c, _)) =
        (
            case (recv c) of
                 Send Undefined => ()
               | Send v => raise (Thrown (String "newborn generator"))
               | Close => raise AbortGenerator
               | Throw e => raise (Thrown e)
               | _ => raise WrongDirection
        )

        (* implements yield v *)
        fun yieldFromGen (Generator (c, r), v) =
        (
            r := OPEN;
            send (c, Yield v);
            let val s = recv c in
                r := RUNNING;
                case s of
                     Send v' => v'
                   | Close => raise AbortGenerator
                   | Throw e => raise (Thrown e)
                   | _ => raise WrongDirection
            end
        )

        (* implements gen.send(v) *)
        fun sendToGen (Generator (c, r), v) =
        (
            case !r of
                 RUNNING => raise (Thrown (String "already executing"))
               | CLOSED => raise (Thrown StopIteration)
               | _ => (send (c, Send v);
                       case (recv c) of
                            Yield v' => v'
                          | Throw e => raise (Thrown e)
                          | _ => raise WrongDirection)
        )

        (* implements gen.throw(v) *)
        fun throwToGen (Generator (c, r), v) =
        (
            case !r of
                 NEWBORN => raise (Thrown (String "newborn generator"))
               | OPEN => (send (c, Throw v);
                          case (recv c) of
                               Yield v' => v'
                             | Throw e => raise (Thrown e)
                             | _ => raise WrongDirection)
               | RUNNING => raise (Thrown (String "already executing"))
               | CLOSED => raise (Thrown v)
        )

        (* implements gen.close() *)
        fun closeGen (Generator (c, r)) =
        (
            case !r of
                 RUNNING => raise (Thrown (String "already executing"))
               | CLOSED => ()
               | _ => (send (c, Close); recv c; ())
        )

        val makeGen : (generator -> unit) -> generator =
            fn body =>
                let val c = channel ()
                    val r = ref NEWBORN
                    val g = Generator (c, r)
                    val thread = fn () =>
                                 (
                                     (waitForStart (g);
                                      r := RUNNING;
                                      body (g);
                                      raise AbortGenerator)
                                         handle AbortGenerator => (r := CLOSED;
                                                                   send (c, Close))
                                              | Thrown v => (r := CLOSED;
                                                             send (c, Throw v))
                                 )
                in
                    spawn (thread);
                    g
                end

    end
