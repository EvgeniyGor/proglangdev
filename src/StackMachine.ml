open Expr

module Program
  struct
      let eval =   let (_, _, _, output) = 
    srun' prg ([], 
              (fun _ -> failwith "undefined variable"),
              input,
              []
             ) 
  in
  output
 end

 module Instr = 
  struct


type instr =
    | READ
    | WRITE
    | PUSH of int
    | LOAD of string
    | STORE of string
    | ADD  
    | MUL

  end


module Program =
  struct

    type t = Instr.t list

  end


module Interpret = 
 struct

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

end


module Compile =
 struct

   open Instr

   module Expr =
    struct

      open Language.Expr

let rec compile = function 
| Var x      -> [LOAD   x]
| Const n    -> [PUSH n]
| Add (x, y) -> (compile x) @ (compile y) @ [ADD]
| Mul (x, y) -> (compile x) @ (compile y) @ [MUL]

    module Stmt =
      struct

        open Language.Stmt

let rec compile = function
| Skip          -> []
| Assign (x, e) -> Expr.compile e @ [STORE x]
| Read    x     -> [READ; STORE x]
| Write   e     -> Expr.compile e @ [WRITE]
| Seq    (l, r) -> compile l @ compile r


  module Program =
    struct

      let compile = Stmt.compile

    end
  end
end
