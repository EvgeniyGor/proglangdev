type expr = Nat of int 
          | Var of char 
          | Add of expr * expr
(*
let rec sem e s = 
  match e with 
  | Nat n -> n
  | Var c -> s c 
  | Add (x, y) -> sem x s + sem y s
*)
let rec sem e = 
  match e with
  | Nat n      -> (fun s -> n)
  | Var x      -> (fun s -> s x)
  | Add (x, y) -> (fun s -> (sem x s) + (sem y s))

let _ = 
  let expr = Add (Var 'x', Nat 3) in 
  
  let state x =
    match x with 
    | 'x' -> 2
    | c -> failwith (Printf.sprintf "uninitialized variable %c" c)
  in
  
  Printf.printf "%d\n" (sem expr state)









