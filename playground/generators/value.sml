structure Value =
    struct

        (* sufficient for the purposes of this example *)
        datatype value = Undefined
                       | Null
                       | String of string
                       | Number of int
                       | StopIteration

        (* we model ES exceptions as ML exceptions *)
        exception Thrown of value

        exception InternalError of string

        fun print x =
            let val s = case x of
                             Undefined => "undefined"
                           | Null => "null"
                           | String s => s
                           | Number i => Int.toString i
                           | StopIteration => "StopIteration"
            in
                TextIO.print(s);
                TextIO.print("\n")
            end

    end
