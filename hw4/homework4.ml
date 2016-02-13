(*************************************************** 

HOMEWORK 4

Name: Dennis Chen

Email: dennis.chen@students.olin.edu

Remarks, if any:

***************************************************)



(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * Do that in a _fresh_ OCaml shell 
 * It has to load without any errors.
 *
 *)


(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode str = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.get str index)::result) in
  acc (String.length(str)-1) []

let implode cs = 
  List.fold_right (fun a r -> (String.make 1 a)^r) cs ""



(*
 *  The type of a DETERMINISTIC finite automaton
 * 
 *  Note that the delta here is _actually_ a function
 *  from states and symbols to states
 * 
 *)

type 'a dfa = { states: 'a list;
		alphabet: char list;
		delta: 'a -> char -> 'a;
		start : 'a;
		accepting : 'a list }


(*
 *  A sample DFA that accepts all strings over
 *  {a,b} with a multiple-of-3 number of a's
 *
 *)

let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = (fun q a -> 
             match (q,a) with
	       ("start",'a') -> "one"
	     | ("one",'a') -> "two"
	     | ("two",'a') -> "start"
	     | ("start",'b') -> "start"
	     | ("one",'b') -> "one"
	     | ("two",'b') -> "two");
  start = "start";
  accepting = ["start"]
} 




(* QUESTION 1 *)


let isAccepting dfa s = failwith "isAccepting not implemented"


let steps dfa q syms = failwith "steps not implemented"


let acceptDFA dfa input = failwith "acceptDFA not implemented"

(* This function loops through all the strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is basically the same as in the last homework
 *)

let langDFA dfa n = 
  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in
  let rec take n default l = 
    if n <= 0 then []
    else (match l with
          | [] -> default::(take (n-1) default l)
          | x::xs -> x::(take (n-1) default xs)) in
  let to_base_n base size n = 
    let rec loop n = 
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
    take size 0 (loop n)  in
  let to_string alphabet size n = 
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
    implode (List.map (fun i -> List.nth alphabet i) num_base) in
  if n < 0 then ()
  else
    let print_str s = if s = "" then print_string "  <epsilon>\n"
                      else print_string ("  "^s^"\n")  in
    let rec loop i = 
      if i <= n then 
	let ts = to_string dfa.alphabet i  in
  	let bound = expt (List.length dfa.alphabet) i in
  	let rec loop2 j = 
  	  if j < bound then (if acceptDFA dfa (ts j) 
                               then print_str (ts j)
                             else ();
  			     loop2 (j+1))
  	  else ()  in
  	(loop2 0; loop (i+1))
      else ()  in
      loop 0





(* QUESTION 2 *)

let max_positive xs =  List.fold_right max xs 0

let at_least n p xs =  (List.length (List.filter p xs)) >= n

let map_funs fs x =  List.map (fun f -> f x) fs

let map_cross fs xs =  List.fold_right (fun x acc -> (map_funs fs x)@acc) xs []

let all_pairs x = List.map (fun y -> (x,y))

let all_pairings xs ys = List.fold_right (fun x acc -> (all_pairs x ys)@acc) xs []

(* QUESTION 3 *)

let front xs = List.rev (List.tl (List.rev xs))

let prefixes xs =  List.fold_right (fun _ acc -> [(front(List.hd acc))]@acc) xs [xs]

let suffixes xs =  List.fold_right (fun x acc -> [[x]@(List.hd acc)]@acc) xs [[]]

let injectAt n a xs = 
  if n < 0 || n > List.length(xs) then failwith "Invalid index!" else
  if n = 0 then a::xs else
    match List.fold_right (fun x (i,acc) -> if i = n then (i-1,x::a::acc) else (i-1,x::acc)) 
      xs (List.length(xs),[]) 
    with | (_,res) -> res

let rec inject a xs = 
if xs = [] then [[a]] else
match
  let xsLong = (List.hd xs)::xs in (*Ugly, best way I could think of increasing the length of the list by one*)
  List.fold_right (fun x (i,acc) -> (i-1,(injectAt i a xs)::acc)) xsLong (List.length(xs),[])
with | (_,res) -> res

let rec injectAll a xss = List.fold_right (fun xs acc -> (inject a xs)@acc) xss []

let permutations xs = List.fold_right (fun x acc -> injectAll x acc) xs [[]]
