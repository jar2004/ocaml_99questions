(*Q1*)
let rec last xs = 
    match xs with
    | [] -> None
    | [x] -> Some x
    | _::xs -> last xs

(*Q2*)
let rec last_two xs =
    match xs with 
    | [] -> None 
    | x::y::[] -> Some (x,y)
    | _::xs -> last_two xs

(*Q3*)
let rec nth n xs =
    match xs with
    | [] -> None  
    | x::t -> if n = 0 then Some x else nth (n-1) t

(*Q4*)
let length xs =
   let rec foo xs a =    
       match xs with 
            | [] -> Some a
            | _::xs -> foo xs (a+1) 
    in foo xs 0

(*Q5*)
let reverse xs =
    let rec rev rs xs = 
        match xs with 
        | [] -> rs 
        | x::xs -> rev (x::rs) xs (*rs is initally [] , we add the head of xs to this then call our function on the tail, we then add the head of this tail to rs effectivly reversing*) 
    in rev [] xs 

(*Q6*) 
let palindrome xs = 
    let rs = reverse xs in 
    rs = xs (*this returns bool of if they are equal or not *)

(*Q7*)
type 'a node =
  | One of 'a 
  | Many of 'a node list

