open Printf

type op = Add | Sub | Div | Mult
type expr = Num of int | Var of char | BinOp of expr * op * expr
type assigns = Assign of char * expr | Seq of assigns * assigns | Nil
type calc = assigns * expr

(*----------------------------------------Print functions------------------------------------*)

let stringify_expr x =
    let stringify_op op =
    match op with
    | Add       -> sprintf "+"
    | Sub       -> sprintf "-"
    | Div       -> sprintf "/"
    | Mult      -> sprintf "*"
    in
    
    let rec print = function 
    | Num i     -> sprintf "%d" i
    | Var c     -> sprintf "%c" c
    | BinOp (x, op, y) -> sprintf "%s %s %s" (print x) (stringify_op op) (print y) 
    in sprintf "%s" (print x)

let print_expr x = printf "%s\n%!" (stringify_expr x)

let stringify_assign x = 
  let rec print = function
  | Nil -> sprintf "[]"
  | Assign (x, e) -> sprintf "%c := %s" x (stringify_expr e)
  | Seq (x, y)    -> sprintf "%s;\n%s" (print x) (print y)
  in sprintf "%s" (print x)

let print_assign x = printf "%s\n%!" (stringify_assign x)

let stringify_calc (s, e) = sprintf "%s;\n%s" (stringify_assign s) (stringify_expr e)

let print_calc x = printf "%s\n%!" (stringify_calc x)

(*-------------------------------------------------------------------------------------------*)


(*------------------------------------Eval functions------------------------------------------*)

let eval_expr env expr =
    let get_op_func op =
        match op with
        | Add       -> (+)
        | Sub       -> (-)
        | Div       -> (/)
        | Mult      -> ( * )
    in

    let rec eval env = function
    | Num i     -> i
    | Var v     -> if (List.mem_assoc v env) then (List.assoc v env) else raise (Failure ("Variable " ^ (Char.escaped v) ^ " is not defined"))
    | BinOp (x, op, y) -> (get_op_func op) (eval env x) (eval env y)
    in eval env expr

let eval_assigns s =
    let rec eval env = function
    | Nil -> env
    | Assign (x, expr) -> (x, eval_expr env expr) :: env
    | Seq (x, y) -> let env' = eval env x in eval env' y
    in eval [] s 

let eval_calc (s, expr) = eval_expr (eval_assigns s) expr

(*-------------------------------------------------------------------------------------------*)


(*------------------------------------Tests---------------------------------------------------*)

let _ =
  
    printf "------------Test 1------------\n\n" ;
    let expression = BinOp(Num 2, Add, BinOp(Num 2, Mult, Num 2)) in
    printf "Expression: " ;
    print_expr expression ;
    printf "Answer: %d\n\n%!" (eval_calc (Nil, expression)) ;
    printf "------------------------------\n\n" ;


    printf "------------Test 2------------\n\n" ;
    let s1 = Assign('x', BinOp(Num 2, Mult, Num 5)) in
    let s2 = Assign('y', BinOp(Num 100, Div, Var 'x')) in
    let s3 = Assign('z', Num 2) in

    let assigns = Seq(s1, Seq(s2, s3)) in
    let expression = BinOp(Var 'x', Add, BinOp(Var 'y', Sub, Var 'z')) in
    printf "Expression: " ;
    print_expr expression ;
    printf "Assigns: \n" ;
    print_assign assigns ;
    printf "Answer: %d\n\n%!" (eval_calc (assigns, expression)) ;
    printf "------------------------------\n\n" ;


    printf "------------Test 3------------\n\n" ;
    let s1 = Assign('x', BinOp(Num 2, Mult, Num 5)) in
    let s2 = Assign('y', BinOp(Num 10, Mult, Var 'x')) in
    let s3 = Assign('z', BinOp(Num 6, Div, Num 2)) in

    let assigns = Seq(s1, Seq(s2, s3)) in
    let expression = BinOp(Var 'x', Mult, BinOp(Var 'y', Sub, Var 'b')) in
    printf "Expression: " ;
    print_expr expression ;
    printf "Assigns: \n" ;
    print_assign assigns ;
    printf "Answer: %d\n\n%!" (eval_calc (assigns, expression)) ;
    printf "------------------------------\n\n" ;
    
(*-------------------------------------------------------------------------------------------*)