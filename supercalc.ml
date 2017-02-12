open Printf

type op = Add | Sub | Div | Mult
type expr = Num of int | Var of char | BinOp of expr * op * expr
type assigns = Assign of char * expr | Seq of assigns * assigns 
type calc = assigns * expr

let string_of_expr x =
    let op_to_string op =
    match op with
    | Add       -> sprintf "+"
    | Sub       -> sprintf "-"
    | Div       -> sprintf "/"
    | Mult      -> sprintf "*"
    in
    
    let rec print = function 
    | Num i     -> sprintf "%d" i
    | Var c     -> sprintf "%c" c
    | BinOp (x, op, y) -> sprintf "%s %s %s" (print x) (op_to_string op) (print y) 
    in sprintf "%s" (print x)

let print_expr x = printf "%s\n%!" (string_of_expr x)

let string_of_assign x = 
  let rec print = function
  | Assign (x, e) -> sprintf "%c := %s" x (string_of_expr e)
  | Seq (x, y)    -> sprintf "%s;\n%s" (print x) (print y)
  in sprintf "%s" (print x)

let print_assigns x = printf "%s\n%!" (string_of_assign x)

let string_of_calc (s, e) = sprintf "%s;\n%s" (string_of_assign s) (string_of_expr e)

let print_calc x = printf "%s\n%!" (string_of_calc x)

let eval_expr env e =
    let get_op_func op =
        match op with
        | Add       -> (+)
        | Sub       -> (-)
        | Div       -> (/)
        | Mult      -> ( * )
    in

    let rec eval env = function
    | Num i     -> i
    | Var v     -> List.assoc v env
    | BinOp (x, op, y) -> (get_op_func op) (eval env x) (eval env y)
    in eval env e

let eval_assigns s =
    let rec eval env = function 
    | Assign (x, e) -> (x, eval_expr env e) :: env
    | Seq (x, y) -> let env' = eval env x in eval env' y
    in eval [] s 

let eval_calc (s, e) = eval_expr (eval_assigns s) e

let _ =
    (*let str = BinOp(Num (-3), Div, Var 'x') in    
    print_expr str;*)

    let s0 = Assign ('x', Num 5) in
    let s1 = Assign ('c', (BinOp (Num 2, Add, Num 3))) in
    let s2 = Assign ('y', Num 42) in
    let s3 = Seq (s0, (Seq (s1, Seq (s2, Assign ('z', Num 32))))) in
  
    printf "\n";
  
    let c0 = s3, (BinOp(Var 'c', Div, BinOp (Var 'z', Add, BinOp (Var 'x', Mult, Var 'y')))) in 

    print_calc c0 ;
  
    printf "\n%d\n%!" (eval_calc c0);