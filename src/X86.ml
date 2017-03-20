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

let regs  = [|"%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi"|]
let regsLength = Array.length regs
    
let wordSize = 4
let eax = Reg 0
let edx = Reg 3

let allocate = function
    | []                                 -> Reg 0
    | Reg i :: _ when i < regsLength - 1 -> Reg (i + 1)
    | Stack i :: _                       -> Stack (i + 1)
    | _                                  -> Stack 0
    
let rec sint prg stack =
    match prg with
    | []        -> [], []
    | instr :: prg' ->
        let (code, stack') = 
	    match instr with
        | PUSH n -> 
                let s = allocate stack in
                [Mov (Lit n, s)], s :: stack
        
        | LOAD x ->
                let s = allocate stack in
                [Mov (Mem x, s)], s :: stack
	    
        | STORE x ->
                let s :: stack' = stack in
                [Mov (s, Mem x)], stack'
            
        | READ -> [Call "fnread"], [eax]

        | WRITE -> 
                let s :: stack' = stack in
                [Mov (s, eax); Push eax; Call "fnwrite"; Pop eax], stack'
        
        | ADD ->
                let x :: y :: stack' = stack in
                (
                    match x, y with
                    | Stack _, Stack _ -> [Mov (x, eax); Add (eax, y)], y :: stack'
                    | _                -> [Add (x, y)], y :: stack'
                )
            
        | MUL ->
                let x :: y :: stack' = stack in
                (
                    match x, y with
                    | Stack _, Stack _ -> [Mov (x, eax); Mul (eax, y)], y :: stack'
                    | _                -> [Mul (x, y)], y :: stack'
                )

        in
        let (code', stack'') = sint prg' stack' in
        code @ code', stack''