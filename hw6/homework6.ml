(* 

HOMEWORK 6

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
 *
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
 *   Code to run a string tm machine
 *
 *)

let run m w = 

  let printConfig m config value = 
    let mw = 
      List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ = 
      print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with 
             | [] -> Printf.printf "[%s]" m.blank
	     | a::v' -> let _ = Printf.printf "[%s]" a  in
	       print_syms v') in
    let _ = print_newline ()  in
    value  in

  let acceptConfig m config = (config.state=m.accept) in

  let rejectConfig m config = (config.state=m.reject) in

  let haltConfig m c = (acceptConfig m c) || (rejectConfig m c) in

  let startConfig m w = 
    { state=m.start;before = [];after = m.left_marker::(explode w)} in

  let rec last u = 
    match u with
    | [] -> failwith "Moving Left from leftmost tape position"
    | [a] -> ([],a)
    | x::xs -> let (u',r) = last xs  in (x::u',r)   in

  let step m config = 
    if (haltConfig m config) then config
    else let (a,v') = match config.after with
                      | [] -> (m.blank,[])
		      | a::v' -> (a,v')  in
         let (q',b,dir) = m.delta(config.state,a) in
	 if dir = 0  (* left *)
	 then let (u',c) = last config.before in 
  	      {state=q';before=u';after=c::b::v'}
	 else {state=q';before=config.before@[b];after=v'} in

  let rec loop c = 
    let _ = printConfig m c c in
    if  (acceptConfig m c) then true
    else if (rejectConfig m c) then false
    else loop (step m c)  in

  loop (startConfig m w)


let rec pairs xs ys =
  List.fold_right (fun x r -> (List.map (fun y -> (x,y)) ys)@r) xs []

(* QUESTION 1 *)

let triples xs ys zs = 
  List.fold_right (fun x acc -> (List.map (fun (b,c) -> (x,b,c)) 
    (pairs ys zs))@acc) xs []

let quads xs ys zs ws = 
  List.fold_right (fun x acc -> (List.map (fun (b,c,d) -> (x,b,c,d)) 
    (triples ys zs ws))@acc) xs []

let rec range n = 
  if n < 0 then [] else n::range (n-1)

(* QUESTION 2 *)


let transformStates states f = List.map f states

let doesMap f x y = f x = y

let rec find_original states f target = 
  match states with
  | [] -> failwith "cannot find original value"
  | h::t -> if doesMap f h target then h else find_original t f target
  
let transformDelta states delta f = 
  let tStates = transformStates states f in
  let g (fp,a) = 
    let (p,b,d) = delta ((find_original states f fp),a) in
    (f p,b,d)
  in g
  
let transform m f = 
    { states = transformStates m.states f;
      input_alphabet = m.input_alphabet;
      tape_alphabet = m.tape_alphabet;
      left_marker = m.left_marker;
      blank = m.blank;
      delta = transformDelta m.states m.delta f;
      start = f m.start;
      accept = f m.accept;
      reject = f m.reject }

(* 
 * Some sample deterministic Turing machines with structured states
 *
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * add1  accepts strings u#v where v = u+1 in binary
 *
 *)


let anbn = { states = [ ("start",0); 
			("q",1);
			("q",2);
			("q",3);
			("q",4);
			("acc",0);
			("rej",0) ];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"X";"/";"|"];
	     blank = "/";
	     left_marker = "|";
	     start = ("start",0);
	     accept = ("acc",0);
	     reject = ("rej",0);
	     delta = (fun inp -> match inp with
	                 | (("start",0), "a") -> (("start",0), "a", 1)
     			 | (("start",0), "b") -> (("q",1), "b", 1)
			 | (("start",0), "|") -> (("start",0), "|", 1)
			 | (("start",0), "/") -> (("q",2), "/", 1)
			 | (("q",1), "b") -> (("q",1), "b", 1)
			 | (("q",1), "/") -> (("q",2), "/", 1)
			 | (("q",2), "|") -> (("q",3), "|", 1)
			 | (("q",2), "a") -> (("q",2), "a", 0)
			 | (("q",2), "b") -> (("q",2), "b", 0)
			 | (("q",2), "X") -> (("q",2), "X", 0)
			 | (("q",2), "/") -> (("q",2), "/", 0)
			 | (("q",3), "X") -> (("q",3), "X", 1)
			 | (("q",3), "/") -> (("acc",0), "/", 1)
			 | (("q",3), "a") -> (("q",4), "X", 1)
			 | (("q",4), "a") -> (("q",4), "a", 1)
			 | (("q",4), "X") -> (("q",4), "X", 1)
			 | (("q",4), "b") -> (("q",2), "X", 1)
			 | (("acc",0), s) -> (("acc",0),s,1)
			 | (_,c) -> (("rej",0),c,1))}


let add1 = 
  { states =    (* spelled out fully so as not to rely on 'triples' *)
[("start", -1, -1); ("start", -1, 0); ("start", -1, 1); ("start", 0, -1);
 ("start", 0, 0); ("start", 0, 1); ("start", 1, -1); ("start", 1, 0);
 ("start", 1, 1); ("check1", -1, -1); ("check1", -1, 0); ("check1", -1, 1);
 ("check1", 0, -1); ("check1", 0, 0); ("check1", 0, 1); ("check1", 1, -1);
 ("check1", 1, 0); ("check1", 1, 1); ("check2", -1, -1); ("check2", -1, 0);
 ("check2", -1, 1); ("check2", 0, -1); ("check2", 0, 0); ("check2", 0, 1);
 ("check2", 1, -1); ("check2", 1, 0); ("check2", 1, 1); ("rewind", -1, -1);
 ("rewind", -1, 0); ("rewind", -1, 1); ("rewind", 0, -1); ("rewind", 0, 0);
 ("rewind", 0, 1); ("rewind", 1, -1); ("rewind", 1, 0); ("rewind", 1, 1);
 ("go-end-1", -1, -1); ("go-end-1", -1, 0); ("go-end-1", -1, 1);
 ("go-end-1", 0, -1); ("go-end-1", 0, 0); ("go-end-1", 0, 1);
 ("go-end-1", 1, -1); ("go-end-1", 1, 0); ("go-end-1", 1, 1);
 ("go-end-2", -1, -1); ("go-end-2", -1, 0); ("go-end-2", -1, 1);
 ("go-end-2", 0, -1); ("go-end-2", 0, 0); ("go-end-2", 0, 1);
 ("go-end-2", 1, -1); ("go-end-2", 1, 0); ("go-end-2", 1, 1);
 ("skip", -1, -1); ("skip", -1, 0); ("skip", -1, 1); ("skip", 0, -1);
 ("skip", 0, 0); ("skip", 0, 1); ("skip", 1, -1); ("skip", 1, 0);
 ("skip", 1, 1); ("scan-1", -1, -1); ("scan-1", -1, 0); ("scan-1", -1, 1);
 ("scan-1", 0, -1); ("scan-1", 0, 0); ("scan-1", 0, 1); ("scan-1", 1, -1);
 ("scan-1", 1, 0); ("scan-1", 1, 1); ("scan-2", -1, -1); ("scan-2", -1, 0);
 ("scan-2", -1, 1); ("scan-2", 0, -1); ("scan-2", 0, 0); ("scan-2", 0, 1);
 ("scan-2", 1, -1); ("scan-2", 1, 0); ("scan-2", 1, 1);
 ("check-done", -1, -1); ("check-done", -1, 0); ("check-done", -1, 1);
 ("check-done", 0, -1); ("check-done", 0, 0); ("check-done", 0, 1);
 ("check-done", 1, -1); ("check-done", 1, 0); ("check-done", 1, 1)];
    input_alphabet = ["0";"1";"#"];
    tape_alphabet = ["0";"1";"#";"X";"_";">"];
    blank = "_";
    left_marker = ">";
    start = ("start",-1,-1);
    accept = ("acc",-1,-1);
    reject = ("rej",-1,-1);
    delta = (fun x -> match x with
    | (("start",-1,-1),">") -> (("check1",-1,-1),">",1)
    | (("check1",-1,-1),"0") -> (("check1",-1,-1),"0",1)
    | (("check1",-1,-1),"1") -> (("check1",-1,-1),"1",1)
    | (("check1",-1,-1),"#") -> (("check2",-1,-1),"#",1)
    | (("check2",-1,-1),"0") -> (("check2",-1,-1),"0",1)
    | (("check2",-1,-1),"1") -> (("check2",-1,-1),"1",1)
    | (("check2",-1,-1),"_") -> (("rewind",-1,1),"_",0)   (* start with a carry of 1! *)

    | (("rewind",-1,carry),">") -> (("go-end-1",-1,carry),">",1)
    | (("rewind",-1,carry),"0") -> (("rewind",-1,carry),"0",0)
    | (("rewind",-1,carry),"1") -> (("rewind",-1,carry),"1",0)
    | (("rewind",-1,carry),"#") -> (("rewind",-1,carry),"#",0)
    | (("rewind",-1,carry),"X") -> (("rewind",-1,carry),"X",0)

    | (("go-end-1",-1,carry),"#") -> (("scan-1",-1,carry),"#",0)
    | (("go-end-1",-1,carry),sym) -> (("go-end-1",-1,carry),sym,1)

    | (("scan-1",-1,carry),"X") -> (("scan-1",-1,carry),"X",0)
    | (("scan-1",-1,carry),"0") -> (("skip",0,carry),"X",1)
    | (("scan-1",-1,carry),"1") -> (("skip",1,carry),"X",1)
    | (("scan-1",-1,0),">") -> (("check-done",-1,-1),">",1)  (* carry should be 0 to be done *)

    | (("skip",v,carry),"#") -> (("go-end-2",v,carry),"#",1)
    | (("skip",v,carry),"X") -> (("skip",v,carry),"X",1)

    | (("go-end-2",v,carry),"_") -> (("scan-2",v,carry),"_",0)
    | (("go-end-2",v,carry),sym) -> (("go-end-2",v,carry),sym,1)

    | (("scan-2",v,carry),"X") -> (("scan-2",v,carry),"X",0)
    | (("scan-2",v,carry),"0") when (v+carry) mod 2 = 0 -> (("rewind",-1,(v+carry) / 2),"X",0)
    | (("scan-2",v,carry),"1") when (v+carry) mod 2 = 1 -> (("rewind",-1,(v+carry) / 2),"X",0)

    | (("check-done",-1,-1),"_") -> (("acc",-1,-1),"_",1)
    | (("check-done",-1,-1),"X") -> (("check-done",-1,-1),"X",1)
    | (("check-done",-1,-1),"#") -> (("check-done",-1,-1),"#",1)

    | (_,sym) -> (("rej",-1,-1),sym,1))}





(* QUESTION 3 *)

let isIn xs x = List.fold_right (fun y acc -> (x=y)||acc) xs false

let permutation = 
  let alphabet = ""::(explode "abcdefghijklmnopqrstuvwxyz") in
  let ia = "#"::alphabet in
  let ta = [">";"_";"X"]@ia in
  let s = pairs ["start";"acc";"rej";"v1";"v2";"checkFirst";
            "rewind";"lastU";"crossU";"crossV";"emptyU";"allCrossed"] 
          alphabet in
  { states = s;
    input_alphabet = ia;
    tape_alphabet = ta;
    start = ("start","");
    accept = ("acc","");
    reject = ("rej","");
    blank = "_";
    left_marker = ">";
    delta = (fun x -> 
    match x with
    (* v1 and v2 validate the input and check that strings 
     * contain valid symbols *)
    | (("start",""),">") -> (("v1",""),">",1)
    | (("v1",""),"#") -> (("v2",""),"#",1)
    | (("v1",""),l) -> if isIn alphabet l then (("v1",""),l,1)
                                            else (("rej",""),l,1)
    | (("v2",""),"_") -> (("rewind",""),"_",0)
    | (("v2",""),l) -> if isIn alphabet l then (("v2",""),l,1)
                                            else (("rej",""),l,1)
    | (("rewind",s),">") -> (("checkFirst",""),">",1)
    | (("rewind",s),l) -> (("rewind",s),l,0)

    | (("checkFirst",s),"#") -> (("emptyU",s),"#",1)
    | (("checkFirst",s),"X") -> (("allCrossed",s),"X",1)
    | (("checkFirst",s),l) -> (("lastU",s),l,1)

    | (("emptyU",s),"_") -> (("acc",""),"_",1)
    | (("emptyU",s),l) -> (("rej",""),l,1)

    | (("allCrossed",s),"X") -> (("allCrossed",s),"X",1)
    | (("allCrossed",s),"#") -> (("allCrossed",s),"#",1)
    | (("allCrossed",s),"_") -> (("acc",""),"_",1)
    | (("allCrossed",s),l) -> (("rej",""),l,1)

    | (("lastU",s),"X") -> (("crossU",s),"X",0)
    | (("lastU",s),"#") -> (("crossU",s),"#",0)
    | (("lastU",s),l) -> (("lastU",s),l,1)

    | (("crossU",s),l) -> (("crossV",l),"X",1)

    | (("crossV",s),"X") -> (("crossV",s),"X",1)
    | (("crossV",s),"#") -> (("crossV",s),"#",1)
    | (("crossV",s),"_") -> (("rej",""),"_",1)
    | (("crossV",s),l) -> if s = l then (("rewind",s),"X",0)
                                   else (("crossV",s),l,1)
  )}

let permTrans (x,y) = x^"|"^y;;

let p = transform permutation permTrans

let copies n = failwith "copies not implemented yet"
