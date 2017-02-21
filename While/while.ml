type nat = Nat of int
type statement = Skip
               | Read of nat
               | Write of nat
               | While of nat * statement
               | If 
