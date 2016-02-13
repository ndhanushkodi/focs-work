findTransitions (dfaThreeA,"start",'a') = [("start", 'a', "one")];;
findTransitions (dfaThreeA,"start",'b') = [("start", 'b', "start")];;
findTransitions (dfaThreeA,"one",'b') = [("one", 'b', "one")];;
findTransitions (nfaLastThreeB,0,'a') = [(0, 'a', 0)];;
findTransitions (nfaLastThreeB,0,'b') = [(0, 'b', 0); (0, 'b', 1)];;


isAccepting (dfaThreeA,"start");;
isAccepting (dfaThreeA,"one") = false;;
isAccepting (dfaThreeA,"two") = false;;
isAccepting (nfaLastThreeB,3);;
isAccepting (nfaLastThreeB,0) = false;;


step (dfaThreeA, "start",'a') = "one";;
step (dfaThreeA, "start",'b') = "start";;
step (dfaThreeA, "one",'a') = "two";;
step (dfaThreeA, "one",'b') = "one";;
step (dfaThreeA, "two",'a') = "start";;
step (dfaThreeA, "two",'b') = "two";;

steps (dfaThreeA, "start", []) = "start";;
steps (dfaThreeA, "start", ['a']) = "one";;
steps (dfaThreeA, "start", ['a';'b']) = "one";;
steps (dfaThreeA, "start", ['a';'b';'a']) = "two";;
steps (dfaThreeA, "one", []) = "one";;
steps (dfaThreeA, "one", ['a']) = "two";;
steps (dfaThreeA, "one", ['a';'b']) = "two";;
steps (dfaThreeA, "one", ['a';'b';'a']) = "start";;


# acceptDFA (dfaThreeA,"");;
- : bool = true
# acceptDFA (dfaThreeA,"a");;
- : bool = false
# acceptDFA (dfaThreeA,"b");;
- : bool = true
# acceptDFA (dfaThreeA,"aa");;
- : bool = false
# acceptDFA (dfaThreeA,"aaa");;
- : bool = true
# acceptDFA (dfaThreeA,"ababa");;
- : bool = true
# acceptDFA (dfaThreeA,"abababa");;
- : bool = false