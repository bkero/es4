functor CallccCoroutine (type result) : COROUTINE =
struct
    open SMLofNJ.Cont

    type result = result

    (* renamed to distinguish clearly from ECMAScript throw *)
    val jump = throw

    datatype computation = Newborn of (result -> result)
                         | Paused of result cont
                         | Running of result cont
                         | Closed

    type C = computation ref

    fun new f = let val r = ref Closed (* temporary *)
                in
                    r := Newborn (fn s =>
                                      let val s' = f (r, s)
                                      in
                                          r := Closed;
                                          s'
                                      end);
                    r
                end

    fun switch (r, x) =
        case !r of
             Newborn f => callcc (fn maink =>
                                  (
                                      r := Running maink;
                                      f x
                                  ))
           | Paused coroutinek => callcc (fn maink =>
                                          (
                                              r := Running maink;
                                              jump coroutinek x
                                          ))
           | Running maink => callcc (fn coroutinek =>
                                      (
                                          r := Paused coroutinek;
                                          jump maink x
                                      ))
           | Closed => raise Value.InternalError "dead coroutine"

    fun kill r =
        case !r of
             Closed => ()
           | Running _ => raise Value.InternalError "already executing"
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
             Running _ => true
           | _ => false

    fun run f = f ()
                handle Value.InternalError s => TextIO.print ("internal error: " ^ s ^ "\n")
end
