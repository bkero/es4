structure ThreadGen : GENERATOR =
    struct
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

        datatype G = Generator of (signal CML.chan * state ref)

        (* blocks a new generator function before the first send *)
        fun wait (Generator (c, _)) =
        (
            case (CML.recv c) of
                 Send Undefined => ()
               | Send v => raise (Thrown (String "newborn generator"))
               | Close => raise AbortGenerator
               | Throw e => raise (Thrown e)
               | _ => raise WrongDirection
        )

        (* implements yield v *)
        fun yield (Generator (c, r), v) =
        (
            r := OPEN;
            CML.send (c, Yield v);
            let val s = CML.recv c in
                r := RUNNING;
                case s of
                     Send v' => v'
                   | Close => raise AbortGenerator
                   | Throw e => raise (Thrown e)
                   | _ => raise WrongDirection
            end
        )

        (* implements gen.send(v) *)
        fun send (Generator (c, r), v) =
        (
            case !r of
                 RUNNING => raise (Thrown (String "already executing"))
               | CLOSED => raise (Thrown StopIteration)
               | _ => (CML.send (c, Send v);
                       case (CML.recv c) of
                            Yield v' => v'
                          | Throw e => raise (Thrown e)
                          | _ => raise WrongDirection)
        )

        (* implements gen.throw(v) *)
        fun throw (Generator (c, r), v) =
        (
            case !r of
                 NEWBORN => raise (Thrown (String "newborn generator"))
               | OPEN => (CML.send (c, Throw v);
                          case (CML.recv c) of
                               Yield v' => v'
                             | Throw e => raise (Thrown e)
                             | _ => raise WrongDirection)
               | RUNNING => raise (Thrown (String "already executing"))
               | CLOSED => raise (Thrown v)
        )

        (* implements gen.close() *)
        fun close (Generator (c, r)) =
        (
            case !r of
                 RUNNING => raise (Thrown (String "already executing"))
               | CLOSED => ()
               | _ => (CML.send (c, Close); CML.recv c; ())
        )

        val make : (G -> unit) -> G =
            fn body =>
                let val c = CML.channel ()
                    val r = ref NEWBORN
                    val g = Generator (c, r)
                    val thread = fn () =>
                                 (
                                     (wait (g);
                                      r := RUNNING;
                                      body (g);
                                      raise AbortGenerator)
                                         handle AbortGenerator => (r := CLOSED;
                                                                   CML.send (c, Close))
                                              | Thrown v => (r := CLOSED;
                                                             CML.send (c, Throw v))
                                 )
                in
                    CML.spawn (thread);
                    g
                end

    end
