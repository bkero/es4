functor ShiftCoroutine (type result) : COROUTINE =
struct
    type result = result

    structure S = DelimitedControl (type result = result)

    open S

    type cont = result -> result

    datatype thread = Newborn of result -> result
                    | Paused of cont
                    | Running

    type C = thread option ref

    exception NYI

    fun new f = let val r = ref NONE
                in
                    r := SOME (Newborn (fn s =>
                                            reset (fn () =>
                                                       let val s' = f (r, s) in
                                                           r := NONE;
                                                           s'
                                                       end)));
                    r
                end

    fun switch (r, x) =
        case !r of
             NONE => raise Value.InternalError "dead coroutine"
           | SOME (Newborn f) => (r := SOME Running; f x)
           | SOME (Paused k) => (r := SOME Running; k x)
           | SOME Running => shift (fn k => (r := SOME (Paused k); x))

    fun kill r =
        case !r of
             NONE => ()
           | SOME Running => raise Value.InternalError "already executing"
           | _ => r := NONE

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
             SOME Running => true
           | _ => false

    fun run f = f ()
                handle Value.InternalError s => TextIO.print ("internal error: " ^ s ^ "\n")
end
