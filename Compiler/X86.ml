open StackMachine

type opnd =     
    | Reg of int    (* номер регистра *)
    | Stack of int  (* позиция на стеке *)
    | Lit of int    (* литерал *) 
    | Mem of string (* переменная, значение которой находится в памяти *)

type instr =
    | Add  of opnd * opnd
    | Mul  of opnd * opnd
    | Mov  of opnd * opnd
    | Push of opnd
    | Pop  of opnd
    | Call of string
    | Ret

let regs  = [|"%eax"; "%ebx"; "%ecx"; "%esi"; "%edi"|]
let regsLength = Array.length regs
    
let allocate = function
    | []                                 -> Reg 0
    | Reg i :: _ when i < regsLength - 1 -> Reg (i+1)
    | Stack i :: _                       -> Stack (i+1)
    | _                                  -> Stack 0
    
let rec sint prg sstack =
      match prg with
      | []        -> [], []
      | i :: prg' ->
        let (code, sstack') = 
	    match i with
	    | PUSH n -> 
                let s = allocate sstack in
                [Mov (Lit n, s)], s :: sstack
        | LOAD x ->
                let s = allocate sstack in
                [Mov (Mem x, s)], s :: sstack
	    | STORE x ->
                let s :: sstack' = sstack in
                [Mov (s, Mem x)], sstack' 
          in
          let (code', sstack'') = sint prg' sstack' in
          code @ code', sstack''
(*
    let compile stmt = 
      sint (comp stmt) 
*)