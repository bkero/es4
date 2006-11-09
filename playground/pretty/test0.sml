structure Test0 =
struct

    datatype color = RED of int * real
                   | GREEN of string option
                   | BLUE of real * int * bool * unit * int
                   | INDIGO of { foo: int, bar: real }
                   | VIOLET of int list

end
