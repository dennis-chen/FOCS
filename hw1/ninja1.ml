let rec listMax xs = 
    match xs with
    | [] -> failwith "empty list"
    | h::[] -> h
    | h::t -> max h (listMax t)

let rec mergeLists (xs,ys) = 
    match (xs,ys) with
    | ([],[]) -> []
    | ([],_) -> failwith "not the same size"
    | (_,[]) -> failwith "not the same size"
    | (x::xt,y::yt) -> x::y::mergeLists(xt,yt)

let rec compress xs =
    match xs with
    | [] -> []
    | [x] -> [x]
    | h::t -> let prev::rest = compress t in
        if compare prev h == 0 then h::rest else h::prev::rest
