functor ThreadCoroutine (type result) : COROUTINE =
struct
    open CML

    type result = result

    exception Death

    (* To close a coroutine, we send it NONE. *)
    datatype thread = Newborn of result option chan
                    | Paused of result option chan
                    | Running of result option chan
                    | Closed

    type C = thread ref

    fun new f = let val c = channel ()
                    val r = ref (Newborn c)
                    val thread = fn () =>
                                 (
                                     (case recv c of
                                           NONE => raise Death
                                         | SOME s => (
                                                         r := Running c;
                                                         let val s' = f (r, s) in
                                                             r := Closed;
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
             Newborn c => (
                              r := Running c;
                              send (c, SOME x);
                              case recv c of
                                   NONE => raise Value.InternalError "coroutine protocol"
                                 | SOME x => x
                          )
           | Paused c => (
                             r := Running c;
                             send (c, SOME x);
                             case recv c of
                                  NONE => raise Value.InternalError "coroutine protocol"
                                | SOME x => x
                         )
           | Running c => (
                              r := Paused c;
                              send (c, SOME x);
                              case recv c of
                                   NONE => raise Death
                                 | SOME x => x
                          )
           | Closed => raise Value.InternalError "dead coroutine"

    fun kill r =
        case !r of
             Newborn c => (send (c, NONE); r := Closed)
           | Paused c => (send (c, NONE); r := Closed)
           | Running c => raise Value.InternalError "already executing"
           | Closed => ()

    fun newborn r =
        case !r of
             Newborn _ => true
           | _ => false

    fun alive r =
        case !r of
             Closed => false
           | _ => true

    fun running r =
        case !r of
             Running _ => true
           | _ => false

    fun run f = (RunCML.doit (fn () =>
                              (
                                  f ()
                                  handle Value.InternalError s => TextIO.print ("internal error: " ^ s ^ "\n")
                              ), NONE); ())
end
