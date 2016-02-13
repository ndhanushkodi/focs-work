lang("abc", 10) ;;
lang("a(b+c)", 10) ;;
lang("a*", 10) ;;
lang("(a+b)*", 10) ;;
(* split up string each of which is matching a or b, then it matches this *)

printLanguage ;;
