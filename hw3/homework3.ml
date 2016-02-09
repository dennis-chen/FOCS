(* 

HOMEWORK 3

Name: Dennis Chen

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

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode (cs) = 
  let str = String.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
      [] -> str
    | c::cs -> (String.set str index c; loop(cs,index+1))
  in
    loop(cs,0)



(*
 *  The type of a finite automaton
 * 
 *  When the transition relation is a function
 *  (i.e., every p,a has q such that (p,a,q) is in 
 *  delta) then this is a deterministic finite automaton  
 * 
 *)

type 'a fa = { states: 'a list;
               alphabet: char list;
               delta: ('a * char * 'a) list;
               start : 'a;
               accepting : 'a list }

(* QUESTION 1 *)

let rec findTransitionsHelper ts q a =
    match ts with
    | [] -> []
    | (x,y,z)::t -> if (x = q && y = a) then (x,y,z)::findTransitionsHelper t q a
                    else findTransitionsHelper t q a

let findTransitions (fa,q,a) = findTransitionsHelper fa.delta q a

let rec contains xs x = 
    match xs with 
    | [] -> false
    | h::t -> if h = x then true else contains t x

let isAccepting (fa,s) = contains fa.accepting s

let rec stepHelper ts q a = 
    match ts with
    | [] -> failwith "transtion from q with symbol a doesn't exist!"
    | (x,y,z)::t -> if (x = q && y = a) then z else stepHelper t q a

let step (fa,q,a) = stepHelper fa.delta q a

let rec reverse xs = 
    match xs with 
    | [] -> []
    | h::t -> (reverse t) @ [h]

let rec stepsHelper fa q syms = 
    match syms with 
    | [] -> q
    | h::t -> let prev = stepsHelper fa q t in
              step(fa,prev,h)

let steps (fa,q,syms) = stepsHelper fa q (reverse syms)

let rec count ts q a = 
    (* count number of transitions in t with q and a *)
    match ts with 
    | [] -> 0
    | (x,y,z)::t -> if (x = q && y = a) then 1 + count t q a
                    else count t q a

let rec tryAz ts q az = 
    (* tries every possible combination of q with list of as *)
    match az with 
    | [] -> true
    | h::t -> if (count ts q h) = 1 then tryAz ts q t else false

let rec isDFAHelper ts qs az = 
    (* tries every possible q *)
    match qs with
    | [] -> true
    | h::t -> if tryAz ts h az then isDFAHelper ts t az else false

let isDFA (fa) = isDFAHelper fa.delta fa.states fa.alphabet

let acceptDFA (fa,input) = 
    if isDFA (fa) then 
        let lastState = steps(fa,fa.start,explode(input)) in
        contains fa.accepting lastState
    else failwith "Not a DFA!"
    
(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY AUTOMATA *)
(* REPLACE BY YOUR OWN DEFINITIONS *)

let dfa_q2_a = { 
    states = [0;1;2;3];
    alphabet = ['a';'b'];
    delta = [(0,'a',0);
          (0,'b',1);
          (1,'a',0);
          (1,'b',2);
          (2,'a',0);
          (2,'b',3);
          (3,'a',3);
          (3,'b',3);
         ];
    start = 0;
    accepting = [0;1;2]
}


let dfa_q2_b = { states = [0;1;2;3;4;5;6;7];
     alphabet = ['a';'b'];
     delta = [(0,'a',1);
              (0,'b',5);
              (1,'a',2);
              (1,'b',5);
              (2,'a',3);
              (2,'b',5);
              (3,'a',4);
              (3,'b',5);
              (4,'a',4);
              (4,'b',4);
              (5,'a',1);
              (5,'b',6);
              (6,'a',1);
              (6,'b',7);
              (7,'a',1);
              (7,'b',4)
             ];
     start = 0;
     accepting = [0;1;2;3;5;6;7]
}

let dfa_q2_c = { states = [0;1;2;3;4;5;6;7];
     alphabet = ['a';'b'];
     delta = [(0,'a',1);
              (0,'b',4);
              (1,'a',2);
              (1,'b',7);
              (2,'a',3);
              (2,'b',7);
              (3,'a',3);
              (3,'b',4);
              (4,'a',7);
              (4,'b',5);
              (5,'a',7);
              (5,'b',6);
              (6,'a',1);
              (6,'b',6);
              (7,'a',7);
              (7,'b',7)
             ];
     start = 0;
     accepting = [3;6]
}

let nfa_q2_d = { states = [0;1;2;3;4;5];
		 alphabet = ['a';'b'];
         delta = [(0,'a',1);
                  (0,'b',5);
                  (1,'a',2);
                  (1,'b',5);
                  (2,'a',3);
                  (2,'b',5);
                  (3,'a',3);
                  (3,'b',4);
                  (4,'a',3);
                  (4,'b',4);
                  (5,'a',5);
                  (5,'b',5);
                 ];
		 start = 0;
		 accepting = [1;2;3;5]
}

(* QUESTION 3 *)

let rec keepTarget (trs) = 
    match trs with 
    | [] -> []
    | (x,y,z)::t -> let prev = keepTarget(t) in
              if contains prev z then prev else z::prev

let rec isAcceptingAny (fa,qs) = 
    match qs with
    | [] -> false
    | h::t -> if contains fa.accepting h then true else isAcceptingAny (fa,t)

let rec removeDups xs = 
    match xs with
    | [] -> []
    | h::t -> if contains t h then removeDups t else h::(removeDups t)

let rec stepAllHelper fa qs a = 
    match qs with 
    | [] -> []
    | h::t -> (findTransitions(fa,h,a)) @ stepAllHelper fa t a

let rec trsToStates trs =
    match trs with
    | [] -> []
    | (x,y,z)::t -> z::(trsToStates t)

let rec stepAll (fa,qs,a) = removeDups (trsToStates (stepAllHelper fa qs a))

let rec stepsAllHelper fa qs syms = 
    match syms with 
    | [] -> qs
    | h::t -> let prev = stepsAllHelper fa qs t in
              stepAll(fa,prev,h)

let rec stepsAll (fa,qs,syms) = stepsAllHelper fa qs (reverse syms)

let acceptNFA (fa,input) = 
    let qs = stepsAll(fa,fa.start::[],explode(input)) in
    isAcceptingAny (fa,qs)


(* 
 * A sample DFA for testing
 *
 * It accepts the language of all strings over {a,b} with a
 * multiple-of-3 number of a's.
 *
 *)

let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = [ ("start",'a',"one");
	    ("one",'a',"two");
	    ("two",'a',"start");
	    ("start",'b',"start");
	    ("one",'b',"one");
	    ("two",'b',"two") ];
  start = "start";
  accepting = ["start"]
} 



(* A sample NFA for testing
 *
 * It accepts the language of all strings over {a,b,c} 
 * whose last three symbols are b's.
 *
 *)

let nfaLastThreeB = {
  states = [0;2;3;4];
  alphabet = ['a';'b';'c'];
  delta = [ (0,'a',0);
	    (0,'b',0);
	    (0,'c',0);
	    (0,'b',1);
	    (1,'b',2);
	    (2,'b',3); ];
  start = 0;
  accepting = [3]
} 




(* This function is the base function that langDFA and
 * langNFA use -- it basically loops through all the strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is being way too clever to try to not blow the stack 
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet. 
 *
 * The key is that we can enumerate integers super easily
 *
 *)

let langFA accept (fa,n) = 

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
  	  let ts = to_string fa.alphabet i  in
  	  let bound = expt (List.length fa.alphabet) i in
  	  let rec loop2 j = 
  	    if j < bound then (if accept(fa,ts j) 
                                 then print_str (ts j)
                               else ();
  			       loop2 (j+1))
  	    else ()  in
  	  (loop2 0; loop (i+1))
        else ()  in
    loop 0


(* 
 * Tester functions that dump the language accepted by a
 * finite automaton, either deterministic or not
 *
 *)
 
let langDFA x = langFA acceptDFA x
let langNFA x = langFA acceptNFA x

(* Test functions *)
let testFindTransitions = 
    findTransitions (dfaThreeA,"start",'a') = [("start", 'a', "one")] &&
    findTransitions (dfaThreeA,"start",'b') = [("start", 'b', "start")] &&
    findTransitions (dfaThreeA,"one",'b') = [("one", 'b', "one")] &&
    findTransitions (nfaLastThreeB,0,'a') = [(0, 'a', 0)] &&
    findTransitions (nfaLastThreeB,0,'b') = [(0, 'b', 0); (0, 'b', 1)]

let testIsAccepting = 
    isAccepting (dfaThreeA,"start") = true &&
    isAccepting (dfaThreeA,"one") = false &&
    isAccepting (dfaThreeA,"two") = false &&
    isAccepting (nfaLastThreeB,3) = true &&
    isAccepting (nfaLastThreeB,0) = false

let testStep =
    step (dfaThreeA, "start",'a') = "one" &&
    step (dfaThreeA, "start",'b') = "start" &&
    step (dfaThreeA, "one",'a') = "two" &&
    step (dfaThreeA, "one",'b') = "one" &&
    step (dfaThreeA, "two",'a') = "start" &&
    step (dfaThreeA, "two",'b') = "two"

let testSteps = 
    steps (dfaThreeA, "start", []) = "start" &&
    steps (dfaThreeA, "start", ['a']) = "one" &&
    steps (dfaThreeA, "start", ['a';'b']) = "one" &&
    steps (dfaThreeA, "start", ['a';'b';'a']) = "two" &&
    steps (dfaThreeA, "one", []) = "one" &&
    steps (dfaThreeA, "one", ['a']) = "two" &&
    steps (dfaThreeA, "one", ['a';'b']) = "two" &&
    steps (dfaThreeA, "one", ['a';'b';'a']) = "start"

let testIsDFA = 
    isDFA (dfaThreeA) = true &&
    isDFA (nfaLastThreeB) = false &&
    isDFA {states=[0;1]; alphabet=['a']; delta=[(0,'a',1)]; start=0; accepting=[1]} = false

let testLangDFA = 
    acceptDFA (dfaThreeA,"") = true &&
    acceptDFA (dfaThreeA,"a") = false &&
    acceptDFA (dfaThreeA,"b") = true &&
    acceptDFA (dfaThreeA,"aa") = false &&
    acceptDFA (dfaThreeA,"aaa") = true &&
    acceptDFA (dfaThreeA,"ababa") = true &&
    acceptDFA (dfaThreeA,"abababa") = false

let testKeepTarget = 
    keepTarget [] = [] &&
    keepTarget [(1,'a',2);(1,'b',3)] = [2; 3] &&
    keepTarget [(1,'a',2);(1,'b',3);(2,'a',2)] = [3; 2] &&
    keepTarget (dfaThreeA.delta) = ["start"; "one"; "two"] &&
    keepTarget (nfaLastThreeB.delta) = [0; 1; 2; 3]

let testIsAcceptingAny = 
    isAcceptingAny (nfaLastThreeB, []) = false &&
    isAcceptingAny (nfaLastThreeB, [0]) = false &&
    isAcceptingAny (nfaLastThreeB, [0;1]) = false &&
    isAcceptingAny (nfaLastThreeB, [0;1;2]) = false &&
    isAcceptingAny (nfaLastThreeB, [0;1;2;3]) = true &&
    isAcceptingAny (nfaLastThreeB, [3]) = true

let testStepAll = 
    stepAll (dfaThreeA,[],'a') = [] &&
    stepAll (dfaThreeA,["start"],'a') = ["one"] &&
    stepAll (dfaThreeA,["start"],'b') = ["start"] &&
    stepAll (dfaThreeA,["start";"one"],'a') = ["one"; "two"] &&
    stepAll (dfaThreeA,["start";"one"],'b') = ["start"; "one"] &&
    stepAll (nfaLastThreeB,[0;1],'a') = [0] &&
    stepAll (nfaLastThreeB,[0;1],'b') = [0; 1; 2]

let testStepsAll = 
    stepsAll (dfaThreeA,[],[]) = [] &&
    stepsAll (dfaThreeA,[],['a']) = [] &&
    stepsAll (dfaThreeA,[],['a';'b']) = [] &&
    stepsAll (dfaThreeA,["start"],[]) = ["start"] &&
    stepsAll (dfaThreeA,["start"],['a']) = ["one"] &&
    stepsAll (dfaThreeA,["start"],['a';'b']) = ["one"] &&
    stepsAll (dfaThreeA,["start"],['a';'a']) = ["two"] &&
    stepsAll (dfaThreeA,["start";"one"],['a';'a']) = ["two"; "start"] &&
    stepsAll (dfaThreeA,["start";"one"],['a';'a';'b']) = ["two"; "start"] &&
    stepsAll (dfaThreeA,["start";"one"],['a';'a';'b';'a']) = ["start"; "one"] &&
    stepsAll (nfaLastThreeB,[0;1],['a';'b';'b';'b']) = [0; 1; 2; 3]

let testAcceptNFA = 
    acceptNFA (dfaThreeA,"babab") = false &&
    acceptNFA (dfaThreeA,"bababa") = true &&
    acceptNFA (dfaThreeA,"bababab") = true &&
    acceptNFA (nfaLastThreeB,"abb") = false &&
    acceptNFA (nfaLastThreeB,"abbb") = true

(* these need to be tested by hand *)
(*
langDFA (dfaThreeA,6);; //passes
langDFA(dfa_q2_a,6);; BROKEN, missing b?
langDFA (dfa_q2_b,7);; //passes
langDFA (dfa_q2_c,12);; //passes
langNFA (nfa_q2_d,7);; //passes
langNFA (nfaLastThreeB,7);; //passes
*)
