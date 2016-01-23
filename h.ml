(* 

HOMEWORK 1

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



(* Question 1 *)

(* Used description of Euclid's algorithm from wikipedia: 
https://en.wikipedia.org/wiki/Greatest_common_divisor *)
let rec gcd (a,b) = 
    match (a,b) with 
    | (a,0) -> a
    | (0,b) -> b
    | _ -> gcd (b, (a mod b))

let is_coprime (a,b) = 
    gcd (a,b) == 1

(*returns number of coprimes when you pair nums between 1 
and x with n *)
let rec eulerHelper (n,x) = 
    match (n,x) with
    | (n,1) -> 1
    | _ -> let prev = eulerHelper(n,x-1) in 
        if is_coprime (n,x) then prev + 1 else prev

let euler (n) = eulerHelper (n,n)

let rec coprimeHelper (n,x) = 
    if n == x then 
        if is_coprime (n,x) then [x] else []
    else
        let prev = coprimeHelper(n,x+1) in 
        if is_coprime (n,x) then (x :: prev) else prev

let coprimes (n) = coprimeHelper(n,1)

(* Question 2 *)

let append (xs,ys) = 
   failwith "not implemented"


let flatten (xss) = 
   failwith "not implemented"


let nth (n,xs) = 
   failwith "not implemented"


let last (xs) = 
   failwith "not implemented"


let separate (xs) = 
   failwith "not implemented"



(* Question 3 *)

let setIn (e,xs) = 
   failwith "not implemented"


let setSub (xs,ys) = 
   failwith "not implemented"


let setEqual (xs,ys) = 
   failwith "not implemented"


let setUnion (xs,ys) = 
   failwith "not implemented"


let setInter (xs,ys) = 
   failwith "not implemented"


let setSize (xs) = 
   failwith "not implemented"

