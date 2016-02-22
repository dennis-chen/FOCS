(* 

HOMEWORK 5

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
 * Do that in a _fresh_ OCaml shell 
 * It has to load without any errors.
 *
 *)




(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> string list
 *      returns the list of characters making up a string
 *
 *)

let explode str = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.sub str index 1)::result) in
  acc (String.length(str)-1) []


(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type symbol = string

type 'a tm = { states : 'a list;
	       input_alphabet : symbol list;
	       tape_alphabet : symbol list;
	       left_marker : symbol;
	       blank : symbol;
	       delta : ('a * symbol) -> ('a * symbol * int);   (* 0 = Left, 1 = Right *)
	       start : 'a;
	       accept : 'a;
	       reject : 'a }

type 'a config = { state : 'a;
		   before: symbol list;
		   after: symbol list }
      
(*
 * Helper function
 *
 * Pint a configuration (including newline) to standard output
 * and RETURN A VALUE
 * 
 *)

let printConfig m config value = 
    let mw = List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ = print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with 
             | [] -> Printf.printf "[%s]" m.blank
	     | a::v' -> let _ = Printf.printf "[%s]" a  in
	                print_syms v') in
    let _ = print_newline ()  in
    value


(* QUESTION 1 *)


let startConfig m w = {
  state = m.start;
  before = [];
  after = m.left_marker :: explode w
}

let acceptConfig m config = config.state = m.accept

let rejectConfig m config = config.state = m.reject

let haltConfig m c = (acceptConfig m c) || (rejectConfig m c)

let step m config = 
  let (q,b,dir) = m.delta (config.state, (List.hd config.after)) in
  if dir = 0 then (*left*)
    let u = List.rev (List.tl (List.rev config.before)) in 
    let c = List.hd (List.rev config.before) in
    let a::v = config.after in
    {
      state = q;
      before = u;
      after = c::b::v
    }
  else (*right*)
    let a::v = config.after in
    {
      state = q;
      before = config.before@[b];
      after = if v = [] then [m.blank] else v;
    }

let rec runHelper m config = 
  let v = [] in 
  let x = printConfig m config v in
  if acceptConfig m config then true else
  if rejectConfig m config then false else
    runHelper m (step m config)

let run m w = runHelper m (startConfig m w)

(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * anbncn is the non-regular language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { states = ["start"; "q1"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"_";">"];
	     blank = "_";
	     left_marker = ">";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", ">") -> ("start", ">", 1)
			 | ("start", "_") -> ("acc", "_", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "_") -> ("acc", "_", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", ">") -> ("acc", ">", 1)
			 | ("acc", "_") -> ("acc", "_", 1)
			 | (_,c) -> ("rej",c,1))}

let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"X";"/";"|"];
	     blank = "/";
	     left_marker = "|";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", "|") -> ("start", "|", 1)
			 | ("start", "/") -> ("q2", "/", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "/") -> ("q2", "/", 1)
			 | ("q2", "|") -> ("q3", "|", 1)
			 | ("q2", "a") -> ("q2", "a", 0)
			 | ("q2", "b") -> ("q2", "b", 0)
			 | ("q2", "X") -> ("q2", "X", 0)
			 | ("q2", "/") -> ("q2", "/", 0)
			 | ("q3", "X") -> ("q3", "X", 1)
			 | ("q3", "/") -> ("acc", "/", 1)
			 | ("q3", "a") -> ("q4", "X", 1)
			 | ("q4", "a") -> ("q4", "a", 1)
			 | ("q4", "X") -> ("q4", "X", 1)
			 | ("q4", "b") -> ("q2", "X", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", "|") -> ("acc", "|", 1)
			 | ("acc", "X") -> ("acc", "X", 1)
			 | ("acc", "/") -> ("acc", "/", 1)
			 | (_,c) -> ("rej",c,1))}


let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       input_alphabet = ["a";"b";"c"];
	       tape_alphabet = ["a";"b";"c";"X";"_";">"];
	       blank = "_";
	       left_marker = ">";
	       start = "start";
	       accept = "acc";
	       reject = "rej";
	       delta = (fun inp -> match inp with
	                | ("start", "a") -> ("start", "a", 1)
     			| ("start", "b") -> ("q1", "b", 1)
			| ("start", "c") -> ("q6", "c", 1)
			| ("start", ">") -> ("start", ">", 1)
			| ("start", "_") -> ("q2", "_", 1)
			| ("q1", "b") -> ("q1", "b", 1)
			| ("q1", "c") -> ("q6", "c", 1)
			| ("q1", "_") -> ("q2", "_", 1)
			| ("q2", ">") -> ("q3", ">", 1)
			| ("q2", "a") -> ("q2", "a", 0)
			| ("q2", "b") -> ("q2", "b", 0)
			| ("q2", "c") -> ("q2", "c", 0)
			| ("q2", "_") -> ("q2", "_", 0)
			| ("q2", "X") -> ("q2", "X", 0)
			| ("q3", "X") -> ("q3", "X", 1)
			| ("q3", "_") -> ("acc", "_", 1)
			| ("q3", "a") -> ("q4", "X", 1)
			| ("q4", "a") -> ("q4", "a", 1)
			| ("q4", "X") -> ("q4", "X", 1)
			| ("q4", "b") -> ("q5", "X", 1)
			| ("q5", "b") -> ("q5", "b", 1)
			| ("q5", "X") -> ("q5", "X", 1)
			| ("q5", "c") -> ("q2", "X", 1)
			| ("q6", "c") -> ("q6", "c", 1)
			| ("q6", "_") -> ("q2", "_", 1)
		        | ("acc", "a") -> ("acc", "a", 1)
		        | ("acc", "b") -> ("acc", "b", 1)
		        | ("acc", "c") -> ("acc", "c", 1)
		        | ("acc", ">") -> ("acc", ">", 1)
		        | ("acc", "X") -> ("acc", "X", 1)
		        | ("acc", "_") -> ("acc", "_", 1)
			| (_,c) -> ("rej", c,1))}



(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY TURING MACHINES *)
(* REPLACE BY YOUR OWN DEFINITIONS *)


let tm_q2_a = { states = ["start";"acc";"rej";"q1";"q2";"q3";"q4";"q5";"q6"];
	     input_alphabet = ["c";"d"];
       tape_alphabet = ["c";"d";"_";">";"X"];
	     blank = "_";
	     left_marker = ">";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
			 | ("start", ">") -> ("q1", ">", 1)
			 | ("q1", "c") -> ("q2", "X", 1)
			 | ("q1", "d") -> ("q5", "X", 1)
			 | ("q1", "X") -> ("acc", "X", 1)
			 | ("q1", "_") -> ("acc", "_", 1)
			 | ("q2", "c") -> ("q2", "c", 1)
			 | ("q2", "d") -> ("q2", "d", 1)
			 | ("q2", "X") -> ("q3", "X", 0)
			 | ("q2", "_") -> ("q3", "_", 0)
			 | ("q3", "c") -> ("q4", "X", 0)
			 | ("q3", "d") -> ("rej", "d", 0)
			 | ("q3", "X") -> ("acc", "X", 0)
			 | ("q3", "_") -> failwith "unexpected q3 state!"
			 | ("q4", "c") -> ("q4", "c", 0)
			 | ("q4", "d") -> ("q4", "d", 0)
			 | ("q4", "X") -> ("q1", "X", 1)
			 | ("q4", "_") -> failwith "unexpected q4 state!"
			 | ("q5", "c") -> ("q5", "c", 1)
			 | ("q5", "d") -> ("q5", "d", 1)
			 | ("q5", "X") -> ("q6", "X", 0)
			 | ("q5", "_") -> ("q6", "_", 0)
			 | ("q6", "c") -> ("rej", "c", 0)
			 | ("q6", "d") -> ("q4", "X", 0)
			 | ("q6", "X") -> ("acc", "X", 0)
			 | ("q6", "_") -> failwith "unexpected q6 state!"
			 | ("acc", "c") -> ("acc", "c", 1)
			 | ("acc", "d") -> ("acc", "d", 1)
			 | ("acc", ">") -> ("acc", ">", 1)
			 | ("acc", "_") -> ("acc", "_", 1)
			 | (_,c) -> ("rej",c,1))}

let tm_q2_b = { states = ["start";"acc";"rej";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"q8"];
		input_alphabet = ["a";"b"];
		tape_alphabet = ["a";"b";"X";">";"_"];
		blank = "_";
		left_marker = ">";
		start = "start";
		accept = "acc";
		reject = "rej";
     delta = (fun inp -> match inp with
     | ("start", ">") -> ("q1", ">", 1)
     | ("q1", "b") -> ("q1", "b", 1)
     | ("q1", "a") -> ("q2", "a", 1)
     | ("q1", "X") -> failwith "q1 unexpected X"
     | ("q1", "_") -> ("q3", "_", 0)
     | ("q2", "a") -> ("q2", "a", 1)
     | ("q2", "_") -> ("q3", "_", 0)
     | ("q3", "a") -> ("q3", "a", 0)
     | ("q3", "b") -> ("q3", "b", 0)
     | ("q3", "X") -> ("q3", "X", 0)
     | ("q3", "_") -> failwith "q3 unexpected _"
     | ("q3", ">") -> ("q4", ">", 1)
     | ("q4", "X") -> ("q4", "X", 1)
     | ("q4", "b") -> ("q5", "X", 1)
     | ("q4", "_") -> ("acc","_",1)
     | ("q5", "a") -> ("q6", "X", 1)
     | ("q5", "b") -> ("q5", "b", 1)
     | ("q5", "X") -> ("q5", "X", 1)
     | ("q5", "_") -> ("rej","_",1)
     | ("q6", "a") -> ("q7", "X", 1)
     | ("q6", "b") -> ("rej","b",1)
     | ("q6", "X") -> ("rej","X",1)
     | ("q6", "_") -> ("rej","_",1)
     | ("q7", "a") -> ("q8", "X", 1)
     | ("q7", "b") -> ("rej","b",1)
     | ("q7", "X") -> ("rej","X",1)
     | ("q7", "_") -> ("rej","_",1)
     | ("q8", "a") -> ("q3", "a", 0)
     | ("q8", "b") -> ("rej","b",1)
     | ("q8", "X") -> failwith "q8 unexpected X"
     | ("q8", "_") -> ("q3", "_", 0)
     | ("acc", "c") -> ("acc", "c", 1)
     | ("acc", "d") -> ("acc", "d", 1)
     | ("acc", ">") -> ("acc", ">", 1)
     | ("acc", "_") -> ("acc", "_", 1)
     | (_,c) -> ("rej",c,1))}

(* QUESTION 3 *)


let binaryAddition = { states = ["start";"acc";"rej";
    "q1";"q2";"q3";"q4";"q5";"q6";"q7";"q8";"q9";
    "q10";"q11";"q12";"q13";"q14";"q15";"q16";"q17";"q18"];
    input_alphabet = ["0";"1";"#"];
    tape_alphabet = ["0";"1";"#";"X";">";"_"];
		blank = "_";
		left_marker = ">";
		start = "start";
		accept = "acc";
		reject = "rej";
     delta = (fun inp -> match inp with
     | ("start", ">") -> ("q2", ">", 1)
       (* Rewind *)
     | ("q1", "0") -> ("q1", "0", 0)
     | ("q1", "1") -> ("q1", "1", 0)
     | ("q1", "#") -> ("q1", "#", 0)
     | ("q1", "X") -> ("q1", "X", 0)
     | ("q1", ">") -> ("q2", ">", 1)
       (* Eat up 0s and 1s until first # or X reached *)
     | ("q2", "0") -> ("q2", "0", 1)
     | ("q2", "1") -> ("q2", "1", 1)
     | ("q2", "#") -> ("q3", "#", 0)
     | ("q2", "X") -> ("q3", "X", 0)
       (* Look at last digit of first number *)
     | ("q3", "0") -> ("q12", "X", 1)
     | ("q3", "1") -> ("q4", "X", 1)
     | ("q3", ">") -> ("q15", ">", 1)
       (* 1,_ branch: eat up xs to go to second number *)
     | ("q4", "#") -> ("q5", "#", 1)
     | ("q4", "X") -> ("q4", "X", 1)
     | ("q4", _) -> failwith "q4"
       (* 1,_ branch: eat up 0s and 1s until first # or X reached *)
     | ("q5", "0") -> ("q5", "0", 1)
     | ("q5", "1") -> ("q5", "1", 1)
     | ("q5", "#") -> ("q6", "#", 0)
     | ("q5", "X") -> ("q6", "X", 0)
       (* 1,_ branch: examine last digit of second number *)
     | ("q6", "0") -> ("q7", "X", 1)
     | ("q6", "1") -> ("q9", "X", 0) (*go backwards to do carry*)
     | ("q6", "#") -> ("rej","#",1) (*num digits not equal*)
     | ("q6", "X") -> failwith "q6"
       (* 1,0 branch: go to last non X non _ char *)
     | ("q7", "#") -> ("q75","#",1)
     | ("q7", "X") -> ("q7","X",1)
     | ("q7", "_") -> failwith "q7"
       (* 1,0 branch: go to last non X non _ char *)
     | ("q75", "0") -> ("q75","0",1)
     | ("q75", "1") -> ("q75","1",1)
     | ("q75", "#") -> failwith "q75"
     | ("q75", "X") -> ("q8","X",0)
     | ("q75", "_") -> ("q8","_",0) 
       (* 1,0 branch: look at last non X non _ char *)
     | ("q8", "0") -> ("rej", "0", 1)
     | ("q8", "1") -> ("q1", "X", 0) (* rewind *)
     | ("q8", "#") -> ("q1", "#", 0) (* rewind *)
     | ("q8", "X") -> failwith "q8 X"
     | ("q8", "_") -> failwith "q8 _"
       (* 1,1 branch: add and carry *)
     | ("q9", "0") -> ("q10","1", 1) (* Add one *)
     | ("q9", "1") -> ("q9","0", 0) (* Carry left *)
     | ("q9", "#") -> ("q10", "#", 1) (* We carried into the hashtag *)
     | ("q9", "X") -> failwith "q9 X"
     | ("q9", "_") -> failwith "q9 _"
       (* 1,1 branch: go to last non X non _ char *)
     | ("q10", "#") -> ("q105","#",1)
     | ("q10", "X") -> ("q10","X",1)
     | ("q10", "0") -> ("q10","0",1)
     | ("q10", "1") -> ("q10","1",1)
     | ("q10", c) -> failwith "q10"
       (* 1,1 branch: go to last non X non _ char *)
     | ("q105", "0") -> ("q105", "0", 1)
     | ("q105", "1") -> ("q105", "1", 1)
     | ("q105", "#") -> failwith "q105"
     | ("q105", "X") -> ("q11","X",0)
     | ("q105", "_") -> ("q11","_",0)
       (* 1,1 branch: look at last non X non _ char *)
     | ("q11", "0") -> ("q1", "X", 0) (* rewind *)
     | ("q11", "1") -> ("rej", "1", 1) 
     | ("q11", "#") -> ("q1", "#", 0) (* rewind *)
     | ("q11", "X") -> failwith "q11 X"
     | ("q11", "_") -> failwith "q11 _"
       (* 0,_ branch: eat up xs to go to second number *)
     | ("q12", "#") -> ("q13", "#", 1)
     | ("q12", "X") -> ("q12", "X", 1)
     | ("q12", _) -> failwith "q12"
       (* 0,_ branch: eat up 0s and 1s until first # or X reached *)
     | ("q13", "0") -> ("q13", "0", 1)
     | ("q13", "1") -> ("q13", "1", 1)
     | ("q13", "#") -> ("q14", "#", 0)
     | ("q13", "X") -> ("q14", "X", 0)
       (* 0,_ branch: examine last digit of second number *)
     | ("q14", "0") -> ("q10", "X", 1)
     | ("q14", "1") -> ("q7", "X", 1) 
     | ("q14", "#") -> ("rej","#",1) (*num digits not equal*)
     | ("q14", "X") -> failwith "q14"
       (* Go through entire string and check that it's all X or hashtags *)
     | ("q15", "0") -> ("rej", "0", 1)
     | ("q15", "1") -> ("rej", "1", 1) 
     | ("q15", "#") -> ("q15","#",1) 
     | ("q15", "X") -> ("q15","X",1)
     | ("q15", "_") -> ("acc","_",1)
       (* Accepting State *)
     | ("acc", "0") -> ("acc", "0", 1)
     | ("acc", "1") -> ("acc", "1", 1)
     | ("acc", "#") -> ("acc", "#", 1)
     | ("acc", "X") -> ("acc", "X", 1)
     | ("acc", ">") -> ("acc", ">", 1)
     | ("acc", "_") -> ("acc", "_", 1)
       (* Rejecting *)
     | (_,c) -> ("rej",c,1))}

