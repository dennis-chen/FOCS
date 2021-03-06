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

let rec append (xs,ys) = 
    match (xs,ys) with
    | ([],ys) -> ys
    | (h::t,ys) -> h::append(t,ys)

let rec flatten (xss) = 
    match xss with
    | [] -> []
    | h::t -> append(h, flatten(t))

let rec nth (n,xs) = 
    match (n,xs) with 
    | (n,[]) -> failwith "out of bounds"
    | (0,h::t) -> h
    | (n,h::t) -> nth(n-1,t)

let rec last (xs) = 
    match xs with 
    | [] -> failwith "empty list"
    | h::[] -> h
    | h:: t -> last (t)

let addVals (x,y,ls) = 
    match ls with 
    | (a,b) -> (x::a,y::b)

let rec separate (xs) = 
    match xs with
    | [] -> ([],[])
    | (a,b)::t -> addVals (a,b,(separate (t)))

(* Question 3 *)

let rec setIn (e,xs) = 
    match xs with 
    | [] -> false
    | h::t -> if (compare h e) == 0 then true else setIn (e,t)

let rec setSub (xs,ys) = 
    match xs with
    | [] -> true
    | h::t -> if setIn(h,ys) then setSub(t,ys) else false

let setEqual (xs,ys) = setSub(xs,ys) && setSub(ys,xs)

let rec setUnion (xs,ys) = 
    match (xs,ys) with 
    | ([],[]) -> []
    | (h::t,ys) -> let prev = setUnion(t,ys) in
        if setIn(h,prev) then prev else h::prev
    | ([],h::t) -> let prev = setUnion([],t) in
        if setIn(h,prev) then prev else h::prev

let rec setInter (xs,ys) = 
    match xs with
    | [] -> []
    | h::t -> let prev = setInter(t,ys) in
        if setIn(h,ys) then h::prev else prev

let rec listLen (xs) = 
    match xs with
    | [] -> 0
    | h::t -> 1 + listLen(t)

(* I wrote setUnion to return unique values so we can
 * simply take the size of the union *)
let setSize (xs) = listLen(setUnion(xs,xs))
