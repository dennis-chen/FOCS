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
