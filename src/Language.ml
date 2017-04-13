(* AST for expressions *)
module Expr =
 struct

    type expr =
    | Var   of string
    | Const of int
    | Add   of expr * expr
    | Mul   of expr * expr

end

module Stmt =
 struct

(* AST statements/commands *)
type stmt =
| Skip
| Assign of string * expr
| Read   of string
| Write  of expr
| Seq    of stmt * stmt

end

module Program =
    struct

        @type t = Stmt.t

    end