isAccepting dfaThreeA "start";;
isAccepting dfaThreeA "one" = false;;
isAccepting dfaThreeA "two" = false;;


at_least 0 (fun x -> x) [];;
at_least 1 (fun x -> x) [] = false;;
at_least 0 (fun x -> x) [true;true;false];;
at_least 1 (fun x -> x > 0) [2;3;0];;
at_least 2 (fun x -> x > 0) [2;3;0];;
at_least 3 (fun x -> x > 0) [2;3;0] = false;;