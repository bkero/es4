structure ThreadCoroutine : COROUTINE =
    struct
        open Value
        open CML

        exception Death

        (* To close a coroutine, we send it NONE. *)
        datatype 'a thread = Newborn of 'a option chan
                           | Paused of 'a option chan
                           | Running of 'a option chan

        (* A closed coroutine is represented as NONE. *)
        type 'a C = 'a thread option ref

        fun new f = let val c = channel ()
                        val r = ref (SOME (Newborn c))
                        val thread = fn () =>
                                     (
                                         (case recv c of
                                               NONE => raise Death
                                             | SOME s => (
                                                             r := SOME (Running c);
                                                             let val s' = f (r, s) in
                                                                 r := NONE;
                                                                 send (c, SOME s')
                                                             end
                                                         ))
                                         handle Death => ()
                                     )
                    in
                        spawn (thread);
                        r
                    end

        fun switch (r, x) =
            case !r of
                 NONE => raise InternalError "dead coroutine"
               | SOME (Newborn c) => (
                                         r := SOME (Running c);
                                         send (c, SOME x);
                                         case recv c of
                                              NONE => raise InternalError "coroutine protocol"
                                            | SOME x => x
                                     )
               | SOME (Paused c) => (
                                        r := SOME (Running c);
                                        send (c, SOME x);
                                        case recv c of
                                             NONE => raise InternalError "coroutine protocol"
                                           | SOME x => x
                                    )
               | SOME (Running c) => (
                                         r := SOME (Paused c);
                                         send (c, SOME x);
                                         case recv c of
                                              NONE => raise Death
                                            | SOME x => x
                                     )

        fun kill r =
            case !r of
                 NONE => ()
               | SOME (Newborn c) => (send (c, NONE); r := NONE)
               | SOME (Paused c) => (send (c, NONE); r := NONE)
               | SOME (Running c) => raise InternalError "already executing"

        fun newborn r =
            case !r of
                 SOME (Newborn _) => true
               | _ => false

        fun alive r =
            case !r of
                 NONE => false
               | _ => true

        fun running r =
            case !r of
                 SOME (Running _) => true
               | _ => false

        fun run f = (RunCML.doit (fn () =>
                                  (
                                      f ()
                                      handle InternalError s => TextIO.print ("internal error: " ^ s ^ "\n")
                                  ), NONE); ())
    end
