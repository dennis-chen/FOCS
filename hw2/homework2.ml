(* 

HOMEWORK 2

Name: Dennis

Email: dennis.chen@students.olin.edu

Remarks, if any:

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * It has to load without any errors.
 *
 *)



(* QUESTION 1 *)


let rec prepend (letter, lang) = 
    match lang with
    | [] -> []
    | h::t -> (letter ^ h) :: prepend(letter, t)

let rec join xs ys = 
    (*helper function for list concatenation*)
    match xs,ys with
    | [],ys -> ys
    | h::t,ys -> h::join t ys

let rec concatenate (alphabet, lang) = 
    match alphabet,lang with
    | [],_ -> []
    | _,[] -> []
    | h::t,ss2 -> join (prepend(h,ss2)) (concatenate(t,lang))

let rec allStringsHelper (alphabet, n) = 
    (*returns all strings of length n that can be made by the alphabet*)
    match n with 
    | 0 -> [""]
    | n -> concatenate(alphabet,allStringsHelper(alphabet,n-1))

let rec all_strings (alphabet, n) =
    match n with 
    | 0 -> allStringsHelper(alphabet,0)
    | n -> join (allStringsHelper(alphabet,n)) (all_strings(alphabet,n-1))

(* QUESTION 2 *)

let rec restrict (xs,n) = 
    match xs with
    | [] -> []
    | h::t -> let prev = restrict(t,n) in 
        if (String.length h) <= n then h::prev else prev

let rec contains xs x = 
    match xs with
    | [] -> false
    | h::t -> if h = x then true else contains t x

let rec removeDups xs = 
    match xs with
    | [] -> []
    | h::t -> if contains t h then removeDups t else h::(removeDups t)

let langUnion (xs,ys,n) = restrict(removeDups(join xs ys),n)

let rec langConcatHelper xs ys = 
    match xs with 
    | [] -> []
    | h::t -> join (prepend(h,ys)) (langConcatHelper t ys)

let langConcat (xs,ys,n) = restrict(removeDups(langConcatHelper xs ys),n)

let rec langStar (xs,n) = 
    match n with
    | 0 -> [""]
    | n -> let prev = langStar(xs,n-1) in 
        removeDups (join (prev) (langConcat(xs,prev,n)) )

(* QUESTION 3 *)


(* some helper code -- vaguely advanced OCaml in here, but barely *)

type re = Empty | Unit | Letter of string | Plus of re * re | Times of re * re | Star of re

let lang (s,n) = 
  let fromChar c = String.make 1 c in
  let explode s = 
    let rec loop i result = 
      if i < 0 then result
      else loop (i-1) (s.[i]::result) in
    loop (String.length s - 1) []  in
  (* Grammar: 
   *
   * R ::= R1 + R
   *       R1
   * 
   * R1 ::= R2 R1
   *        R2
   * 
   * R2 ::= R3*
   *        R3
   * 
   * R3 ::= a
   *        1
   *        0 
   *        ( R )
   *)
  let isalpha = function 'A'..'Z'|'a'..'z' -> true | _ -> false in
  let expect c cs = 
    match cs with 
      f::cs when f = c -> Some cs
    | _ -> None in
  let expect_alpha cs = 
    match cs with
      f::cs when isalpha f -> Some (f,cs)
    | _ -> None  in
  let rec parse_R cs = 
    match parse_R1 cs with
      None -> None
    | Some (r1,cs) -> 
        (match expect '+' cs with
           None -> Some (r1,cs)
         | Some cs -> 
             (match parse_R cs with
                None -> None
              | Some (r2,cs) -> Some (Plus(r1,r2),cs)))
  and parse_R1 cs = 
    match parse_R2 cs with
      None -> None
    | Some (r1,cs) -> 
        (match parse_R1 cs with
           None -> Some (r1,cs)
         | Some (r2,cs) -> Some (Times(r1,r2),cs))  
  and parse_R2 cs = 
    match parse_R3 cs with
      None -> None
    | Some (r1,cs) -> 
        (match expect '*' cs with
           None -> Some (r1,cs)
         | Some cs -> Some (Star(r1),cs))
  and parse_R3 cs = 
    match expect_alpha cs with
      Some (a,cs) -> Some (Letter(fromChar(a)),cs)
    | None -> 
        (match expect '1' cs with
           Some cs -> Some (Unit, cs)
         | None -> 
             (match expect '0' cs with
                Some cs -> Some (Empty,cs)
              | None -> parse_parens cs))
  and parse_parens cs = 
    match expect '(' cs with
      None -> None
    | Some cs -> 
        (match parse_R cs with
           None -> None
         | Some (r,cs) -> 
             (match expect ')' cs with
                None -> None
              | Some cs -> Some (r,cs)))  in
  let parse s = 
    let cs = explode s in
    match parse_R cs with
      Some (re,[]) -> re
    | _ -> failwith ("Cannot parse "^s)  in
  let rec eval re = 
    match re with
      Empty -> []
    | Unit -> [""]
    | Letter (a) -> [a]
    | Plus (r1,r2) -> langUnion(eval r1,eval r2,n)
    | Times (r1,r2) -> langConcat(eval r1,eval r2,n)
    | Star r -> langStar(eval r,n)  in
    eval (parse s)

let dump l = 
  List.iter (fun s -> match s with "" -> print_string "  <empty>\n" 
                                 | s -> print_string ("  "^s^"\n")) l

(* Placeholder for your regular expression. Replace "0" by your actual answer *)

let regexp_a = "(a+b)(a+b)(a+b)"

let regexp_b = "((a+b)(a+b)(a+b))*"

let regexp_c = "(b*)a(b*)"

let regexp_d = "((((b*)a(b*)a(b*))+(b*))*)a((((b*)a(b*)a(b*))+(b*))*)"

let regexp_e = "((a)+(ba))*"

(* Functions for testing *)

let rec setIn (e,xs) = 
    match xs with 
    | [] -> false
    | h::t -> if (compare h e) == 0 then true else setIn (e,t)

let rec setSub (xs,ys) = 
    match xs with
    | [] -> true
    | h::t -> if setIn(h,ys) then setSub(t,ys) else false

let setEqual (xs,ys) = setSub(xs,ys) && setSub(ys,xs)

let testPrepend = 
    setEqual(prepend("",[]),[]) &&
    setEqual(prepend("",["hello";"world"]),["hello"; "world"]) &&
    setEqual(prepend("test",[]),[]) &&
    setEqual(prepend("test",["hello";"world"]),["testhello"; "testworld"])

let testConcatenate = 
    setEqual(concatenate([],[]),[]) &&
    setEqual(concatenate([],["hello";"world"]),[]) &&
    setEqual(concatenate(["a"],["hello";"world"]),["ahello"; "aworld"]) &&
    setEqual(concatenate(["a";"b"],["hello";"world"]),["ahello"; "aworld"; "bhello"; "bworld"]) &&
    setEqual(concatenate(["a";"b"],[]),[]) &&
    setEqual(concatenate(["a";"b"],["hello"]),["ahello"; "bhello"])

let testAllStrings = 
    setEqual(all_strings([],4),[""]) &&
    setEqual(all_strings(["a"],4),[""; "a"; "aa"; "aaa"; "aaaa"]) &&
    setEqual(all_strings(["a";"b"],4),[""; "a"; "aa"; "aaa"; "aaaa"; "aaab"; "aab"; "aaba"; "aabb"; "ab"; "aba"; "abaa"; "abab"; "abb"; "abba"; "abbb"; "b"; "ba"; "baa"; "baaa"; "baab"; "bab"; "baba"; "babb"; "bb"; "bba"; "bbaa"; "bbab"; "bbb"; "bbba"; "bbbb"]) &&
    setEqual(all_strings(["a";"b";"c"],4), [""; "a"; "aa"; "aaa"; "aaaa"; "aaab"; "aaac"; "aab"; "aaba"; "aabb"; "aabc"; "aac"; "aaca"; "aacb"; "aacc"; "ab"; "aba"; "abaa"; "abab"; "abac"; "abb"; "abba"; "abbb"; "abbc"; "abc"; "abca"; "abcb"; "abcc"; "ac"; "aca"; "acaa"; "acab"; "acac"; "acb"; "acba"; "acbb"; "acbc"; "acc"; "acca"; "accb"; "accc"; "b"; "ba"; "baa"; "baaa"; "baab"; "baac"; "bab"; "baba"; "babb"; "babc"; "bac"; "baca"; "bacb"; "bacc"; "bb"; "bba"; "bbaa"; "bbab"; "bbac"; "bbb"; "bbba"; "bbbb"; "bbbc"; "bbc"; "bbca"; "bbcb"; "bbcc"; "bc"; "bca"; "bcaa"; "bcab"; "bcac"; "bcb"; "bcba"; "bcbb"; "bcbc"; "bcc"; "bcca"; "bccb"; "bccc"; "c"; "ca"; "caa"; "caaa"; "caab"; "caac"; "cab"; "caba"; "cabb"; "cabc"; "cac"; "caca"; "cacb"; "cacc"; "cb"; "cba"; "cbaa"; "cbab"; "cbac"; "cbb"; "cbba"; "cbbb"; "cbbc"; "cbc"; "cbca"; "cbcb"; "cbcc"; "cc"; "cca"; "ccaa"; "ccab"; "ccac"; "ccb"; "ccba"; "ccbb"; "ccbc"; "ccc"; "ccca"; "cccb"; "cccc"]) &&
    setEqual(all_strings(["a";"b"],1),[""; "a"; "b"]) &&
    setEqual(all_strings(["a";"b"],0),[""])

let testRestrict = 
    setEqual(restrict([],4),[]) &&
    setEqual(restrict(["a";"b"],4),["a"; "b"]) &&
    setEqual(restrict(["a";"b"],0),[]) &&
    setEqual(restrict(["a";"b"],1),["a"; "b"]) &&
    setEqual(restrict(["a";"b";"abc"],1),["a"; "b"]) &&
    setEqual(restrict(["a";"b";"abc"],2),["a"; "b"]) &&
    setEqual(restrict(["a";"b";"abc"],3),["a"; "b"; "abc"])

let testLangUnion = 
    setEqual(langUnion([],[],4) ,[]) &&
    setEqual(langUnion(["a";"b"],["c";"d"],4) ,["a"; "b"; "c"; "d"]) &&
    setEqual(langUnion(["a";"b"],["abc";"abcd";"abcde"],4) ,["a"; "b"; "abc"; "abcd"]) &&
    setEqual(langUnion(["abc";"abcd";"abcde"],["a";"b"],4) ,["abc"; "abcd"; "a"; "b"]) &&
    setEqual(langUnion(["abc";"abcd";"abcde"],[],4) ,["abc"; "abcd"]) &&
    setEqual(langUnion([],["abc";"abcd";"abcde"],4) ,["abc"; "abcd"])

let testLangConcat = 
    setEqual(langConcat([],[],4), []) &&
    setEqual(langConcat(["a";"b"],[],4) ,[]) &&
    setEqual(langConcat([],["c";"d"],4) ,[]) &&
    setEqual(langConcat(["a";"b"],["c";"d"],4) ,["ac"; "ad"; "bc"; "bd"]) &&
    setEqual(langConcat(["ab";"abb"],["c";"cc";"ccc"],4) ,["abc"; "abcc"; "abbc"])

let testLangstar = 
    setEqual(langStar([],4), [""]) &&
    setEqual(langStar(["a"],4),
    [""; "a"; "aa"; "aaa"; "aaaa"]) &&
    setEqual(langStar(["a";"b"],4),
    [""; "a"; "b"; "aa"; "ab"; "aaa"; "aab"; "aaaa"; "aaab"; "aaba"; "aabb";
 "aba"; "abb"; "abaa"; "abab"; "abba"; "abbb"; "ba"; "bb"; "baa"; "bab";
 "baaa"; "baab"; "baba"; "babb"; "bba"; "bbb"; "bbaa"; "bbab"; "bbba";
 "bbbb"]) &&
    setEqual(langStar(["a";"b";"c"],4),
    [""; "a"; "b"; "c"; "aa"; "ab"; "ac"; "aaa"; "aab"; "aac"; "aaaa"; "aaab";
 "aaac"; "aaba"; "aabb"; "aabc"; "aaca"; "aacb"; "aacc"; "aba"; "abb"; "abc";
 "abaa"; "abab"; "abac"; "abba"; "abbb"; "abbc"; "abca"; "abcb"; "abcc";
 "aca"; "acb"; "acc"; "acaa"; "acab"; "acac"; "acba"; "acbb"; "acbc"; "acca";
 "accb"; "accc"; "ba"; "bb"; "bc"; "baa"; "bab"; "bac"; "baaa"; "baab";
 "baac"; "baba"; "babb"; "babc"; "baca"; "bacb"; "bacc"; "bba"; "bbb"; "bbc";
 "bbaa"; "bbab"; "bbac"; "bbba"; "bbbb"; "bbbc"; "bbca"; "bbcb"; "bbcc";
 "bca"; "bcb"; "bcc"; "bcaa"; "bcab"; "bcac"; "bcba"; "bcbb"; "bcbc"; "bcca";
 "bccb"; "bccc"; "ca"; "cb"; "cc"; "caa"; "cab"; "cac"; "caaa"; "caab";
 "caac"; "caba"; "cabb"; "cabc"; "caca"; "cacb"; "cacc"; "cba"; "cbb"; "cbc";
 "cbaa"; "cbab"; "cbac"; "cbba"; "cbbb"; "cbbc"; "cbca"; "cbcb"; "cbcc";
 "cca"; "ccb"; "ccc"; "ccaa"; "ccab"; "ccac"; "ccba"; "ccbb"; "ccbc"; "ccca";
 "cccb"; "cccc"]) &&
    setEqual(langStar(["a";"bc"],4),
    [""; "a"; "bc"; "aa"; "abc"; "aaa"; "aabc"; "aaaa"; "abca"; "bca"; "bcbc";
 "bcaa"]) &&
    setEqual(langStar(["a";"bc";"def"],4),
    [""; "a"; "bc"; "def"; "aa"; "abc"; "adef"; "aaa"; "aabc"; "aaaa"; "abca";
 "bca"; "bcbc"; "bcaa"; "defa"])

let a_answer = 
["aaa"; "aab"; "aba"; "abb"; "baa"; "bab"; "bba"; "bbb"]

let b_answer = 
["";"aaa"; "aab"; "aba"; "abb"; "baa"; "bab"; "bba"; "bbb"; "aaaaaa"; "aaaaab"; "aaaaba"; "aaaabb"; "aaabaa"; "aaabab"; "aaabba"; "aaabbb"; "aabaaa"; "aabaab"; "aababa"; "aababb"; "aabbaa"; "aabbab"; "aabbba"; "aabbbb"; "abaaaa"; "abaaab"; "abaaba"; "abaabb"; "ababaa"; "ababab"; "ababba"; "ababbb"; "abbaaa"; "abbaab"; "abbaba"; "abbabb"; "abbbaa"; "abbbab"; "abbbba"; "abbbbb"; "baaaaa"; "baaaab"; "baaaba"; "baaabb"; "baabaa"; "baabab"; "baabba"; "baabbb"; "babaaa"; "babaab"; "bababa"; "bababb"; "babbaa"; "babbab"; "babbba"; "babbbb"; "bbaaaa"; "bbaaab"; "bbaaba"; "bbaabb"; "bbabaa"; "bbabab"; "bbabba"; "bbabbb"; "bbbaaa"; "bbbaab"; "bbbaba"; "bbbabb"; "bbbbaa"; "bbbbab"; "bbbbba"; "bbbbbb"]

let c_answer = 
["a"; "ab"; "abb"; "abbb"; "abbbb"; "abbbbb"; "ba"; "bab"; "babb"; "babbb"; "babbbb"; "bba"; "bbab"; "bbabb"; "bbabbb"; "bbba"; "bbbab"; "bbbabb"; "bbbba"; "bbbbab"; "bbbbba"]

let d_answer = 
["a"; "aaa"; "aaab"; "aaabb"; "aaabbb"; "aaba"; "aabab"; "aababb"; "aabba"; "aabbab"; "aabbba"; "aaaaa"; "aaaaab"; "aaaaba"; "aaabaa"; "aabaaa"; "ab"; "abaa"; "abaab"; "abaabb"; "ababa"; "ababab"; "ababba"; "abaaaa"; "abb"; "abbaa"; "abbaab"; "abbaba"; "abbb"; "abbbaa"; "abbbb"; "abbbbb"; "ba"; "baaa"; "baaab"; "baaabb"; "baaba"; "baabab"; "baabba"; "baaaaa"; "bab"; "babaa"; "babaab"; "bababa"; "babb"; "babbaa"; "babbb"; "babbbb"; "bba"; "bbaaa"; "bbaaab"; "bbaaba"; "bbab"; "bbabaa"; "bbabb"; "bbabbb"; "bbba"; "bbbaaa"; "bbbab"; "bbbabb"; "bbbba"; "bbbbab"; "bbbbba"]

let e_answer = 
["";"ba"; "baa"; "baaa"; "baaaa"; "baaaaa"; "baba"; "babaa"; "babaaa"; "bababa"; "baaba"; "baabaa"; "baaaba"; "a"; "aba"; "abaa"; "abaaa"; "abaaaa"; "ababa"; "ababaa"; "abaaba"; "aa"; "aaba"; "aabaa"; "aabaaa"; "aababa"; "aaa"; "aaaba"; "aaabaa"; "aaaa"; "aaaaba"; "aaaaa"; "aaaaaa"]

let testThree = 
    setEqual(a_answer,lang(regexp_a,6)) &&
    setEqual(b_answer,lang(regexp_b,6)) &&
    setEqual(c_answer,lang(regexp_c,6)) &&
    setEqual(d_answer,lang(regexp_d,6)) &&
    setEqual(e_answer,lang(regexp_e,6))

let testAll = 
    testPrepend && testConcatenate && testAllStrings && testRestrict 
    && testLangUnion && testLangstar && testThree
