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

let to_string buf code = 
        let oprnd = function
        | Reg i   -> regs.(i)
        | Stack i -> Printf.sprintf "%d(%%ebp)" (-i * 4)
        | Lit i   -> Printf.sprintf "$%d"
        | Mem s   -> s
        let instr = function
        | Add (x, y) -> Printf.sprintf "addl\t%s,%s"  (oprnd x) (oprnd y)
        | Mul (x, y) -> Printf.sprintf "imull\t%s,%s" (oprnd x) (opernd y)
        | Mov (x, y) -> Printf.sprintf "movl\t%s,%s"  (oprnd x) (oprnd y)
        | Push x     -> Printf.sprintf "pushl\t%s"    (oprnd x)
        | Pop x      -> Printf.sprintf "popl\t%s"     (oprnd x)
        | Call x     -> Printf.sprintf "call\t%s"     x
        | Ret        -> "ret"
        in
        let buf = Buffer.create 1024 in
        let out s = 
                Buffer.add_string "\t"
                Buffer.add_string buf s; 
                Buffer.add_string buf "\n" 
                in
        List.iter (fun i -> out @@ instr i) code;
        Buffer.contents buf

let regs  = [|"%eax"; "%ebx"; "%ecx"; "%esi"; "%edi"; "%edx"; "%esp", "%ebp"|]
let regsLength = Array.length regs - 3
    
let [|eax, ebx, ecx, esi; edi; edx; esp; ebp|] = Array.mapi (fun i _ -> R i) regs

let wordSize = 4
let eax = Reg 0
let edx = Reg 3

module S = Set.Make (String)

class env =
    object (this)
        val locals = S.empty
        val depth = 0

        method allocate = function
        | []                                 -> this, Reg 0
        | Reg i :: _ when i < regsLength - 1 -> this, Reg (i + 1)
        | Stack i :: _                       -> {< depth = max depth (i+1) >}, Stack (i + 1)
        | _                                  -> {< depth = max depth 1 >}, Stack 1

        method local x = {< locals = S.add x locals >}
        method get_locals = S.elements locals
        method get_depth = depth
    end
    
let rec sint env prg stack =
    match prg with
    | []        -> env, [], []
    | instr :: prg' ->
        let env, code, stack' = 
	    match instr with
        | PUSH n -> 
                let env', s = env#allocate stack in
                env', [Mov (Lit n, s)], s :: stack
        
        | LOAD x ->
                let env' = env#local x in
                let env'', s = env'#allocate stack in
                env'', [Mov (Mem x, s)], s :: stack
	    
        | STORE x ->
                let env' = env#local x in
                let s :: stack' = stack in
                env, [Mov (s, Mem x)], stack'
            
        | READ -> env, [Call "fnread"], [eax]

        | WRITE -> env, [Push eax; Call "fnwrite"; Pop edx], []
                (*[Mov (s, eax); Push eax; Call "fnwrite"; Pop eax], stack'*)
        
        | ADD ->
                let x :: (y :: _ as stack') = stack in
                (
                    match x, y with
                    | Stack _, Stack _ -> env, [Mov (x, eax); Add (eax, y)], stack'
                    | _                -> env, [Add (x, y)], stack'
                )
            
        | MUL ->
                let x :: (y :: _ as stack') = stack in
                (
                    match x, y with
                    | Stack _, Stack _ -> env, [Mov (y, edx); Mul (x, y); Mov (edx, y)], stack'      (*Операнд у лежит на стеке*)
                    | _                -> env, [Mul (x, y)], stack'                                  (*оба операнда лежат на стеке, поэтому *)
                )

        in
        let env, code', stack'' = sint env prg' stack' in
        env, code @ code', stack''

let compile stmt prg =
        let env, code, [] = sint (new env) (comp prg) [] in
        let buf = Buffer.create 1024 in
        let out s = Buffer.add_string buf s in
        out "\t.data\n";
        List.iter (fun x -> out (Printf.sprintf "%s:\t.int 0\n" x)) env#get_locals;
        out "\t.text\n";
        out "\t.globl\tmain\n";
        out "main:\n";
        out "\tpushl\t%ebp\n";
        out "\tmovl\t%esp,%ebp\n";
        out (Printf.sprintf "\tsubl\t$%d,%%esp\n" (env#get_depth * 4));
        to_string buf code;
        out "\tmovl\t%ebp,%esp\n";
        out "\tpopl\t%ebp\n";
        out "\tret\n";
        Buffer.contents buf


(*
        gcc -m32 -c asm.s
        gcc -m32 -c runtime.c
        gcc -m32 -o p runtime.o asm.o
*)