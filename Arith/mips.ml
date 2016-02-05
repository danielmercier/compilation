open Format

type register =  string
let v0 : register = "$v0"
let v1 : register = "$v1"
let a0 : register = "$a0"
let a1 : register = "$a1"
let a2 : register = "$a2"
let a3 : register = "$a3"
let t0 : register = "$t0"
let t1 : register = "$t1"
let t2 : register = "$t2"
let t3 : register = "$t3"
let s0 : register = "$s0"
let s1 : register = "$s1"
let ra : register = "$ra"
let sp : register = "$sp"
let fp : register = "$fp"
let gp : register = "$gp"
let zero : register = "$zero"



type label = string
type 'a address = formatter -> 'a -> unit
let alab : label address = fun fmt  (s : label) -> fprintf fmt "%s" s
let areg : (int * register) address = fun fmt (x, y) -> fprintf fmt "%i(%s)" x y
type 'a operand = formatter -> 'a -> unit
let oreg : register operand = fun fmt (r : register) -> fprintf fmt "%s" r
let oi : int operand = fun fmt i -> fprintf fmt "%i" i
let oi32 : int32 operand = fun fmt i -> fprintf fmt "%li" i

type 'a asm =
  | Nop
  | S of string
  | Cat of 'a asm * 'a asm

type text = [`text ] asm
type data = [`data ] asm

let buf = Buffer.create 17
let fmt = formatter_of_buffer buf
let ins x =
  fprintf fmt "\t";
  kfprintf (fun _ ->
    fprintf fmt "@\n";
    pp_print_flush fmt () ;
    let s = Buffer.contents buf in
    Buffer.clear buf; S s
  ) fmt x


let pr_list fmt pr l =
  match l with
    [] -> ()
  | [ i ] -> pr fmt i
  | i :: ll -> pr fmt i;
      List.iter (fun i -> fprintf fmt ", %a" pr i) ll

let pr_ilist fmt l =
  pr_list fmt (fun fmt i -> fprintf fmt "%i" i) l

let pr_alist fmt l =
  pr_list fmt (fun fmt (a : label) -> fprintf fmt "%s" a) l

let abs r1 r2 = ins "abs %s, %s" r1 r2
let add a b (o : 'a operand) = ins "add %s, %s, %a" a b o
let clz r1 r2 = ins "clz %s, %s" r1 r2
let and_ r1 r2 r3 = ins "and %s, %s, %s" r1 r2 r3
let div a b (o : 'a operand) = ins "div %s, %s, %a" a b o
let mul a b (o : 'a operand) = ins "mul %s, %s, %a" a b o
let or_ r1 r2 r3 = ins "or %s, %s, %s" r1 r2 r3
let not_ r1 r2 = ins "not %s, %s" r1 r2
let rem a b (o : 'a operand) = ins "rem %s, %s, %a" a b o
let neg r1 r2 = ins "neg %s, %s" r1 r2
let sub a b (o : 'a operand) = ins "sub %s, %s, %a" a b o
let li r i = ins "li %s, %i" r i
let li32 r i = ins "li %s, %li" r i
let seq r1 r2 r3 = ins "seq %s, %s, %s" r1 r2 r3
let sge r1 r2 r3 = ins "sge %s, %s, %s" r1 r2 r3
let sgt r1 r2 r3 = ins "sgt %s, %s, %s" r1 r2 r3
let sle r1 r2 r3 = ins "sle %s, %s, %s" r1 r2 r3
let slt r1 r2 r3 = ins "slt %s, %s, %s" r1 r2 r3
let sne r1 r2 r3 = ins "sne %s, %s, %s" r1 r2 r3
let b (z : label) = ins "b %s" z
let beq x y (z : label) = ins "beq %s, %s, %s" x y z
let bne x y (z : label) = ins "bne %s, %s, %s" x y z
let bge x y (z : label) = ins "bge %s, %s, %s" x y z
let bgt x y (z : label) = ins "bgt %s, %s, %s" x y z
let ble x y (z : label) = ins "ble %s, %s, %s" x y z
let blt x y (z : label) = ins "blt %s, %s, %s" x y z

let beqz x (z : label) = ins "beqz %s, %s" x z
let bnez x (z : label) = ins "bnez %s, %s" x z
let bgez x (z : label) = ins "bgez %s, %s" x z
let bgtz x (z : label) = ins "bgtz %s, %s" x z
let blez x (z : label) = ins "blez %s, %s" x z
let bltz x (z : label) = ins "bltz %s, %s" x z

let jr r = ins "jr %s" r
let jal (z : label) = ins "jal %s" z
let jalr (z : register) = ins "jalr %s" z
let la x (p : 'a address)  = ins "la %s, %a" x p
let lb x (p : 'a address) = ins "lb %s, %a" x p
let lbu x (p : 'a address) = ins "lbu %s, %a" x p
let lw x (p : 'a address) = ins "lw %s, %a" x p
let sb x (p : 'a address) = ins "sb %s, %a" x p
let sw x (p : 'a address) = ins "sw %s, %a" x p
let move r1 r2 = ins "move %s, %s" r1 r2
let nop = Nop
let label (s : label) = S ( s ^ ":\n")
let syscall = S "\tsyscall\n"
let comment s = S ("#" ^ s ^ "\n")
let align i = ins ".align %i" i
let asciiz s = ins ".asciiz %S" s
let dword l = ins ".word %a" pr_ilist l
let address l = ins ".word %a" pr_alist l
let (@@) x y = Cat (x, y)


(*
  let push r =
  sub sp sp oi 4
  @@ sw r areg (0,sp)
  
  let peek r =
  lw r areg (0,sp)
  
  let pop r =
  peek r
  @@ add sp sp oi 4
*)

type program = { text : [ `text ] asm;
                 data : [ `data ] asm; }

let rec pr_asm fmt a =
  match a with
  | Nop -> ()
  | S s -> fprintf fmt "%s" s
  | Cat (a1, a2) ->
      let () = pr_asm fmt a1 in
      pr_asm fmt a2

let print_program fmt p =
  fprintf fmt ".text\n";
  pr_asm fmt p.text;
  fprintf fmt ".data\n";
  pr_asm fmt p.data
