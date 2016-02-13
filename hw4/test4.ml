let testIsAccepting = 
  isAccepting dfaThreeA "start" = true &&
  isAccepting dfaThreeA "one"= false &&
  isAccepting dfaThreeA "two"= false

let testSteps = 
  steps dfaThreeA "start" []= "start" &&
  steps dfaThreeA "start" ['a']= "one" &&
  steps dfaThreeA "start" ['a';'b']= "one" &&
  steps dfaThreeA "start" ['a';'b';'a']= "two" &&
  steps dfaThreeA "one" []= "one" &&
  steps dfaThreeA "one" ['a']= "two" &&
  steps dfaThreeA "one" ['a';'b']= "two" &&
  steps dfaThreeA "one" ['a';'b';'a']= "start"

let testAcceptDFA = 
  acceptDFA dfaThreeA ""= true &&
  acceptDFA dfaThreeA "a"= false &&
  acceptDFA dfaThreeA "b"= true &&
  acceptDFA dfaThreeA "aa"= false &&
  acceptDFA dfaThreeA "aaa"= true &&
  acceptDFA dfaThreeA "ababa"= true &&
  acceptDFA dfaThreeA "abababa"= false 

let testAtLeast = 
  at_least 0 (fun x -> x) []= true &&
  at_least 1 (fun x -> x) []= false &&
  at_least 0 (fun x -> x) [true;true;false]= true &&
  at_least 1 (fun x -> x > 0) [2;3;0]= true &&
  at_least 2 (fun x -> x > 0) [2;3;0]= true &&
  at_least 3 (fun x -> x > 0) [2;3;0]= false 

let testMaxPos = 
  max_positive []= 0 &&
  max_positive [4]= 4 &&
  max_positive [4;5]= 5 &&
  max_positive [5;4]= 5 &&
  max_positive [4;6;5]= 6 &&
  max_positive [-1;-2;-3]= 0 

let dbl x = "double of "^(string_of_int x);;

let neg x = "negation of "^(string_of_int x);;

let testMapFuns = 
  map_funs [] 3= [] &&
  map_funs [dbl] 3= ["double of 3"] &&
  map_funs [dbl;neg] 3= ["double of 3"; "negation of 3"] &&
  map_funs [dbl;neg;dbl] 3= ["double of 3"; "negation of 3"; "double of 3"] &&
  map_funs [(fun x -> x * 2); (fun x -> x * x)] 10= [20; 100] &&
  map_funs [(fun x -> "+"^x); (fun x -> "-"^x)] "hello"= ["+hello"; "-hello"]

let testMapCross = 
  map_cross [] []= [] &&
  map_cross [] [1;2;3]= [] &&
  map_cross [dbl; neg] []= [] &&
  map_cross [dbl] [3]= ["double of 3"] &&
  map_cross [dbl] [1;2;3]= ["double of 1"; "double of 2"; "double of 3"] &&
  map_cross [dbl;neg] [3]= ["double of 3"; "negation of 3"] &&
  map_cross [dbl;neg] [1;2;3]= ["double of 1"; "negation of 1"; "double of 2"; "negation of 2"; "double of 3"; "negation of 3"] &&
  map_cross [(fun x -> "+"^x);(fun x -> "-"^x)] ["hello";"world"]= ["+hello"; "-hello"; "+world"; "-world"]

let testAllPairings = 
all_pairings [] []= [] &&
all_pairings [1;2] []= [] &&
all_pairings [] ["a";"b";"c"]= [] &&
all_pairings [1] ["a";"b";"c"]= [(1, "a"); (1, "b"); (1, "c")] &&
all_pairings [1;2] ["a"]= [(1, "a"); (2, "a")] &&
all_pairings [1;2] ["a";"b";"c"]= [(1, "a"); (1, "b"); (1, "c"); (2, "a"); (2, "b"); (2, "c")]

let testPrefixes = 
  prefixes []= [[]] &&
  prefixes [1]= [[]; [1]] &&
  prefixes [1;2;3;4]= [[]; [1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]] &&
  prefixes ["a";"b"]= [[]; ["a"]; ["a"; "b"]]

let testSuffixes = 
  suffixes []= [[]] &&
  suffixes [1]= [[1]; []] &&
  suffixes [1;2;3;4]= [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]; []] &&
  suffixes ["a";"b";"c"]= [["a"; "b"; "c"]; ["b"; "c"]; ["c"]; []]

let testInject = 
  inject 99 []= [[99]] &&
  inject 99 [1]= [[99; 1]; [1; 99]] &&
  inject 99 [1;2]= [[99; 1; 2]; [1; 99; 2]; [1; 2; 99]] &&
  inject 99 [1;2;3;4]= [[99; 1; 2; 3; 4]; [1; 99; 2; 3; 4]; [1; 2; 99; 3; 4]; [1; 2; 3; 99; 4]; [1; 2; 3; 4; 99]] && 
  inject "X" ["a";"b"]= [["X"; "a"; "b"]; ["a"; "X"; "b"]; ["a"; "b"; "X"]]

let testPermutations = 
permutations []= [[]] &&
permutations [1]= [[1]] &&
permutations [1;2]= [[1; 2]; [2; 1]] &&
permutations [1;2;3;4]=
  [[1; 2; 3; 4]; [2; 1; 3; 4]; [2; 3; 1; 4]; [2; 3; 4; 1]; [1; 3; 2; 4];
   [3; 1; 2; 4]; [3; 2; 1; 4]; [3; 2; 4; 1]; [1; 3; 4; 2]; [3; 1; 4; 2];
    [3; 4; 1; 2]; [3; 4; 2; 1]; [1; 2; 4; 3]; [2; 1; 4; 3]; [2; 4; 1; 3];
     [2; 4; 3; 1]; [1; 4; 2; 3]; [4; 1; 2; 3]; [4; 2; 1; 3]; [4; 2; 3; 1];
      [1; 4; 3; 2]; [4; 1; 3; 2]; [4; 3; 1; 2]; [4; 3; 2; 1]] &&
  permutations ["a";"b"]= [["a"; "b"]; ["b"; "a"]]
