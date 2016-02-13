let rec square xs = 
  match xs with
  | [] -> []
  | h::t -> (h*h)::(square t)

let rec square2 = List.map (fun x -> x*x)

let rec diags xs =
  match xs with 
  | [] -> []
  | h::t -> (h,h)::diags t

let rec diags2  = List.map (fun x -> (x,x)) 

let rec lengths ls = 
  match ls with 
  | [] -> []
  | h::t -> List.length(h)::lengths t

let rec lengths2  = List.map List.length 

let rec myMap f xs = 
  match xs with 
  | [] -> []
  | h::t -> (f h)::map f t

let diags3 = List.map (fun x -> (x,x,x))
let thirds = List.map (fun (a,b,c) -> c)
let distribute h = List.map (fun x->(h,x))

let rec myFilter f xs = 
  match xs with 
  | [] -> []
  | h::t -> let prev = myFilter f t in
            if f h then h::prev else prev

let rec mappend f xs = 
  match xs with
  | [] -> []
  | h::t -> (f h) @ (mappend f t)

let myFilter2 p xs = 
  List.fold_right (fun x accum -> if p x then x::accum else accum) xs []

let removeEmpty = List.filter (fun x -> (List.length x) > 0)

let oddFilter = List.filter (fun x -> x mod 2 = 1)

let flatten = mappend (fun x -> x)

let myFilter3 p = mappend (fun x -> if p x then [x] else [])

let sum xs = List.fold_right (+) xs 0

let rec mapGen0 comb f xs = 
  match xs with
  | [] -> []
  | h::t -> comb (f h) (mapGen0 comb f t)

let rec mapGen f xs = 
  match xs with
  | [] -> []
  | h::t -> f h (mapGen f t)

let rec myFold comb xs base =
  match xs with
  | [] -> base
  | h::t -> comb h (myFold comb t base)
