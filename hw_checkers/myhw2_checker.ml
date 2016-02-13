prepend("",[]) = [];;
prepend("",["hello";"world"]) = ["hello"; "world"];;
prepend("test",[]) = [];;
prepend("test",["hello";"world"]) = ["testhello"; "testworld"];;


concatenate([],[]) = [];;
concatenate([],["hello";"world"]) = [];;
concatenate(["a"],["hello";"world"]) = ["ahello"; "aworld"];;
concatenate(["a";"b"],["hello";"world"]) = ["ahello"; "aworld"; "bhello"; "bworld"];;
concatenate(["a";"b"],[]) = [];;
concatenate(["a";"b"],["hello"]) = ["ahello"; "bhello"];;

all_strings([],4) = [""];;
setEqual(all_strings(["a"],4) ,[""; "a"; "aa"; "aaa"; "aaaa"]);;
setEqual(all_strings(["a";"b"],4),[""; "a"; "aa"; "aaa"; "aaaa"; "aaab"; "aab"; "aaba"; "aabb"; "ab"; "aba";"abaa"; "abab"; "abb"; "abba"; "abbb"; "b"; "ba"; "baa"; "baaa"; "baab";"bab"; "baba"; "babb"; "bb"; "bba"; "bbaa"; "bbab"; "bbb"; "bbba"; "bbbb"]);;
setEqual(all_strings(["a";"b";"c"],4),[""; "a"; "aa"; "aaa"; "aaaa"; "aaab"; "aaac"; "aab"; "aaba"; "aabb"; "aabc";"aac"; "aaca"; "aacb"; "aacc"; "ab"; "aba"; "abaa"; "abab"; "abac"; "abb";"abba"; "abbb"; "abbc"; "abc"; "abca"; "abcb"; "abcc"; "ac"; "aca"; "acaa";"acab"; "acac"; "acb"; "acba"; "acbb"; "acbc"; "acc"; "acca"; "accb";"accc"; "b"; "ba"; "baa"; "baaa"; "baab"; "baac"; "bab"; "baba"; "babb";"babc"; "bac"; "baca"; "bacb"; "bacc"; "bb"; "bba"; "bbaa"; "bbab"; "bbac";"bbb"; "bbba"; "bbbb"; "bbbc"; "bbc"; "bbca"; "bbcb"; "bbcc"; "bc"; "bca";"bcaa"; "bcab"; "bcac"; "bcb"; "bcba"; "bcbb"; "bcbc"; "bcc"; "bcca";"bccb"; "bccc"; "c"; "ca"; "caa"; "caaa"; "caab"; "caac"; "cab"; "caba";"cabb"; "cabc"; "cac"; "caca"; "cacb"; "cacc"; "cb"; "cba"; "cbaa"; "cbab";"cbac"; "cbb"; "cbba"; "cbbb"; "cbbc"; "cbc"; "cbca"; "cbcb"; "cbcc"; "cc";"cca"; "ccaa"; "ccab"; "ccac"; "ccb"; "ccba"; "ccbb"; "ccbc"; "ccc"; "ccca";"cccb"; "cccc"]);;
all_strings(["a";"b"],1) = [""; "a"; "b"];;
all_strings(["a";"b"],0) = [""];;

restrict([],4) = [];;
restrict(["a";"b"],4) = ["a"; "b"];;
restrict(["a";"b"],0) = [];;
restrict(["a";"b"],1) = ["a"; "b"];;
restrict(["a";"b";"abc"],1) = ["a"; "b"];;
restrict(["a";"b";"abc"],2) = ["a"; "b"];;
restrict(["a";"b";"abc"],3) = ["a"; "b"; "abc"];;

langUnion([],[],4) = [];;
setEqual(langUnion(["a";"b"],["c";"d"],4) ,["a"; "b"; "c"; "d"]);;
setEqual(langUnion(["a";"b"],["abc";"abcd";"abcde"],4) ,["a"; "b"; "abc"; "abcd"]);;
setEqual(langUnion(["abc";"abcd";"abcde"],["a";"b"],4) ,["abc"; "abcd"; "a"; "b"]);;
setEqual(langUnion(["abc";"abcd";"abcde"],[],4) , ["abc"; "abcd"]);;

langUnion([],["abc";"abcd";"abcde"],4) = ["abc"; "abcd"];;

langConcat([],[],4) = [];;
langConcat(["a";"b"],[],4) = [];;
langConcat([],["c";"d"],4) = [];;
langConcat(["a";"b"],["c";"d"],4) = ["ac"; "ad"; "bc"; "bd"];;
langConcat(["ab";"abb"],["c";"cc";"ccc"],4) = ["abc"; "abcc"; "abbc"];;

langStar([],4) = [""];;
setEqual(langStar(["a"],4) , [""; "a"; "aa"; "aaa"; "aaaa"]);;
setEqual(langStar(["a";"b"],4),[""; "a"; "b"; "aa"; "ab"; "aaa"; "aab"; "aaaa"; "aaab"; "aaba"; "aabb";"aba"; "abb"; "abaa"; "abab"; "abba"; "abbb"; "ba"; "bb"; "baa"; "bab";"baaa"; "baab"; "baba"; "babb"; "bba"; "bbb"; "bbaa"; "bbab"; "bbba";"bbbb"]);;
setEqual(langStar(["a";"b";"c"],4),[""; "a"; "b"; "c"; "aa"; "ab"; "ac"; "aaa"; "aab"; "aac"; "aaaa"; "aaab";"aaac"; "aaba"; "aabb"; "aabc"; "aaca"; "aacb"; "aacc"; "aba"; "abb"; "abc";"abaa"; "abab"; "abac"; "abba"; "abbb"; "abbc"; "abca"; "abcb"; "abcc";"aca"; "acb"; "acc"; "acaa"; "acab"; "acac"; "acba"; "acbb"; "acbc"; "acca";"accb"; "accc"; "ba"; "bb"; "bc"; "baa"; "bab"; "bac"; "baaa"; "baab";"baac"; "baba"; "babb"; "babc"; "baca"; "bacb"; "bacc"; "bba"; "bbb"; "bbc";"bbaa"; "bbab"; "bbac"; "bbba"; "bbbb"; "bbbc"; "bbca"; "bbcb"; "bbcc";"bca"; "bcb"; "bcc"; "bcaa"; "bcab"; "bcac"; "bcba"; "bcbb"; "bcbc"; "bcca";"bccb"; "bccc"; "ca"; "cb"; "cc"; "caa"; "cab"; "cac"; "caaa"; "caab";"caac"; "caba"; "cabb"; "cabc"; "caca"; "cacb"; "cacc"; "cba"; "cbb"; "cbc";"cbaa"; "cbab"; "cbac"; "cbba"; "cbbb"; "cbbc"; "cbca"; "cbcb"; "cbcc";"cca"; "ccb"; "ccc"; "ccaa"; "ccab"; "ccac"; "ccba"; "ccbb"; "ccbc"; "ccca";"cccb"; "cccc"]);;
setEqual(langStar(["a";"bc"],4),[""; "a"; "bc"; "aa"; "abc"; "aaa"; "aabc"; "aaaa"; "abca"; "bca"; "bcbc";"bcaa"]);;
setEqual(langStar(["a";"bc";"def"],4),[""; "a"; "bc"; "def"; "aa"; "abc"; "adef"; "aaa"; "aabc"; "aaaa"; "abca";"bca"; "bcbc"; "bcaa"; "defa"]);;

setEqual(lang(regexp_a,6),["aaa";"aab";"aba";"abb";"baa";"bab";"bba";"bbb"]);;
setEqual(lang(regexp_b,6),["";"aaa";"aab";"aba";"abb";"baa";"bab";"bba";"bbb";"aaaaaa";"aaaaab";"aaaaba";"aaaabb";"aaabaa";"aaabab";"aaabba";"aaabbb";"aabaaa";"aabaab";"aababa";"aababb";"aabbaa";"aabbab";"aabbba";"aabbbb";"abaaaa";"abaaab";"abaaba";"abaabb";"ababaa";"ababab";"ababba";"ababbb";"abbaaa";"abbaab";"abbaba";"abbabb";"abbbaa";"abbbab";"abbbba";"abbbbb";"baaaaa";"baaaab";"baaaba";"baaabb";"baabaa";"baabab";"baabba";"baabbb";"babaaa";"babaab";"bababa";"bababb";"babbaa";"babbab";"babbba";"babbbb";"bbaaaa";"bbaaab";"bbaaba";"bbaabb";"bbabaa";"bbabab";"bbabba";"bbabbb";"bbbaaa";"bbbaab";"bbbaba";"bbbabb";"bbbbaa";"bbbbab";"bbbbba";"bbbbbb"]);;
setEqual(lang(regexp_c,6),["a";"ab";"abb";"abbb";"abbbb";"abbbbb";"ba";"bab";"babb";"babbb";"babbbb";"bba";"bbab";"bbabb";"bbabbb";"bbba";"bbbab";"bbbabb";"bbbba";"bbbbab";"bbbbba"]);;
setEqual(lang(regexp_e,6),["";"ba";"baa";"baaa";"baaaa";"baaaaa";"baba";"babaa";"babaaa";"bababa";"baaba";"baabaa";"baaaba";"a";"aba";"abaa";"abaaa";"abaaaa";"ababa";"ababaa";"abaaba";"aa";"aaba";"aabaa";"aabaaa";"aababa";"aaa";"aaaba";"aaabaa";"aaaa";"aaaaba";"aaaaa";"aaaaaa"]);;

(* setEqual(lang(regexp_d,6),["a";"aaa";"aaab";"aaabb";"aaabbb";"aaba";"aabab";"aababb";"aabba";"aabbab";"aabbba";"aaaaa";"aaaaab";"aaaaba";"aaabaa";"aabaaa";"ab";"abaa";"abaab";"abaabb";"ababa";"ababab";"ababba";"abaaaa";"abb";"abbaa";"abbaab";"abbaba";"abbb";"abbbaa";"abbbb";"abbbbb";"ba";"baaa";"baaab";"baaabb";"baaba";"baabab";"baabba";"baaaaa";"bab";"babaa";"babaab";"bababa";"babb";"babbaa";"babbb";"babbbb";"bba";"bbaaa";"bbaaab";"bbaaba";"bbab";"bbabaa";"bbabb";"bbabbb";"bbba";"bbbaaa";"bbbab";"bbbabb";"bbbba";"bbbbab";"bbbbba"]);; *)