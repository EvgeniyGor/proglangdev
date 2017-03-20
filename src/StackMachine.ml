open Expr

type instr =
    | READ
    | WRITE
    | PUSH of int
    | LOAD of string
    | STORE of string
    | ADD  
    | MUL

type prg = instr list

let srun prg input =
  let rec srun' prg ((stack, state, input, output) as conf) =
    match prg with
    | []        -> conf
    | instr :: prg' ->
        srun' prg' (
          match instr with
          | READ   -> let z :: input' = input in
                      (z :: stack, state, input', output)

          | WRITE  -> let z :: stack' = stack in
                      (stack', state, input, output @ [z])
	        
          | PUSH n -> (n :: stack, state, input, output)
          
          | LOAD   x -> (state x :: stack, state, input, output)
	        
          | STORE   x -> let z :: stack' = stack in
                      (stack', update state x z, input, output)
          
          | ADD    -> let y :: x :: stack' = stack in
                      ((+) x y :: stack', state, input, output)
          
          | MUL    -> let y :: x :: stack' = stack in
                      (( * ) x y :: stack', state, input, output)

        )
  in
  let (_, _, _, output) = 
    srun' prg ([], 
              (fun _ -> failwith "undefined variable"),
              input,
              []
             ) 
  in
  output

let rec comp_expr = function 
| Var x      -> [LOAD   x]
| Const n    -> [PUSH n]
| Add (x, y) -> (comp_expr x) @ (comp_expr y) @ [ADD]
| Mul (x, y) -> (comp_expr x) @ (comp_expr y) @ [MUL]

let rec comp = function
| Skip          -> []
| Assign (x, e) -> comp_expr e @ [STORE x]
| Read    x     -> [READ; STORE x]
| Write   e     -> comp_expr e @ [WRITE]
| Seq    (l, r) -> comp l @ comp r