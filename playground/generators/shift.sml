functor ShiftCoroutine (type result) : COROUTINE =
struct
    type result = result

    structure S = DelimitedControl (type result = result)

    open S

    datatype computation = Newborn of result -> result
                         | Paused of result -> result
                         | Running
                         | Closed

    type C = computation ref

    fun new f = let val r = ref Closed (* temporary *)
                in
                    r := Newborn (fn s => reset (fn () =>
                                                     let val s' = f (r, s) in
                                                         r := Closed;
                                                         s'
                                                     end));
                    r
                end

    fun switch (r, x) =
        case !r of
             Newborn f => (r := Running; f x)
           | Paused k => (r := Running; k x)
           | Running => shift (fn k => (r := Paused k; x))
           | Closed => raise Value.InternalError "dead coroutine"

    fun kill r =
        case !r of
             Closed => ()
           | Running => raise Value.InternalError "already executing"
           | _ => r := Closed

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
             Running => true
           | _ => false

    fun run f = f ()
                handle Value.InternalError s => TextIO.print ("internal error: " ^ s ^ "\n")
end
