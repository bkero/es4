structure CallccCoroutine : COROUTINE =
    struct
        open Value
        open SMLofNJ.Cont

        (* renamed to distinguish clearly from ECMAScript throw *)
        val jump = throw

        datatype 'a thread = Newborn of ('a -> 'a)
                           | Paused of 'a cont
                           | Running of 'a cont

        type 'a C = 'a thread option ref

        fun new f = let val r = ref NONE
                    in
                        r := SOME (Newborn (fn s =>
                                                let val s' = f (r, s) in
                                                    r := NONE;
                                                    s'
                                                end));
                        r
                    end

        fun switch (r, x) =
            case !r of
                 NONE => raise InternalError "dead coroutine"
               | SOME (Newborn f) => callcc (fn maink =>
                                             (
                                                 r := SOME (Running maink);
                                                 f x
                                             ))
               | SOME (Paused coroutinek) => callcc (fn maink =>
                                                     (
                                                         r := SOME (Running maink);
                                                         jump coroutinek x
                                                     ))
               | SOME (Running maink) => callcc (fn coroutinek =>
                                                 (
                                                     r := SOME (Paused coroutinek);
                                                     jump maink x
                                                 ))

        fun kill r =
            case !r of
                 NONE => ()
               | SOME (Running _) => raise InternalError "already executing"
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
                 SOME (Running _) => true
               | _ => false

        fun run f = f ()
                    handle InternalError s => TextIO.print ("internal error: " ^ s ^ "\n")
    end
