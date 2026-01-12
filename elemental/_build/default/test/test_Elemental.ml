open Elemental

let input1 = "jamie";;
let t1 = Elemental.elements_of_string elements input1;;
assert (t1 = None);;

let input2 = "biscuino";;
let t2 = elements_of_string elements input2;;
assert(Option.is_some t2 && List.length (Option.get t2) == 12)
