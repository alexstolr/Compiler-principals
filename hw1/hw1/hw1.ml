(* hw1.ml *)

#use "pc.ml";;

(* *)

type reg = Reg of int;;

type imm = Imm of int;;

type opcode = Add | Sub | Mul | Div | Mov;;

type inst =
  | InstRR of opcode * reg * reg
  | InstRI of opcode * reg * imm;;

type scm_op = ScmAdd | ScmSub | ScmMul | ScmDiv;;
  
type expr =
  | ScmInt of int
  | ScmOp of scm_op * expr list;;

type expr' =
  | Uninitialized
  | ScmInt' of int
  | ScmOp' of scm_op * expr' list;;

exception X_This_should_not_happen;;
exception X_Not_yet_implemented;;
exception X_Expression_includes_uninitialized_values;;
exception X_Cannot_parse_expr_string;;
exception X_Cannot_parse_assembly_program_string;;

module type PARSERS = sig
  val nt_assembly_program : char list -> inst list * char list
  val nt_expr : char list -> expr * char list 
end;; (* end of signature PARSERS *)

module Parsers : PARSERS = struct 
open PC;;
  
let make_char_value base_char displacement =
  let base_char_value = Char.code base_char in
  fun ch -> (Char.code ch) - base_char_value + displacement;;

let nt_digit_0_9 = pack (range '0' '9') (make_char_value '0' 0);;
  
let nt_digit_1_9 = pack (range '0' '9') (make_char_value '0' 0);;
  
let nt_nat =
  let nt = range '1' '9' in
  let nt = pack nt (make_char_value '0' 0) in
  let nt' = range '0' '9' in
  let nt' = pack nt' (make_char_value '0' 0) in
  let nt' = star nt' in
  let nt = caten nt nt' in
  let nt = pack nt (fun (d, ds) -> (d :: ds)) in
  let nt = pack nt (fun s -> List.fold_left (fun a b -> a * 10 + b) 0 s) in
  let nt' = char '0' in
  let nt'' = char '0' in
  let nt''' = range '0' '9' in
  let nt'' = caten nt'' nt''' in
  let nt' = diff nt' nt'' in
  let nt' = pack nt' (fun e -> 0) in
  let nt = disj nt nt' in
  nt;;

(*
 * Description: 
 * Run example: PC.test_string Parsers.nt_register "r2";;
 * Output: - : reg * string = (Reg 2, "->[]")
 *)
let nt_register =
  let nt = char_ci 'r' in
  let nt = caten nt nt_nat in
  let nt = pack nt (fun (_r, n) -> (Reg n)) in
  nt;;

(*
 * Description: 
 * Run example: PC.test_string Parsers.nt_int "3";;
 * Output: - : int * string = (3, "->[]")
 *)
let nt_int =
  let nt = char '-' in
  let nt = pack nt (fun e -> -1) in
  let nt' = char '+' in
  let nt' = pack nt' (fun e -> 1) in
  let nt = disj nt nt' in
  let nt = maybe nt in
  let nt = pack nt (function | None -> 1 | Some(mult) -> mult) in
  
  let nt' = range '0' '9' in
  let nt' = pack nt' (make_char_value '0' 0) in
  let nt' = plus nt' in
  let nt' = pack nt' (fun s -> List.fold_left (fun a b -> a * 10 + b) 0 s) in

  let nt = caten nt nt' in
  let nt = pack nt (fun (mult, n) -> (mult * n)) in
  nt;;

(*
 * Description: 
 * Run example: PC.test_string Parsers.nt_imm "3";;
 * Output: - : imm * string = (Imm 3, "->[]")
 *)
let nt_imm = pack nt_int (fun n -> (Imm n));;

(*
 * Description: 
 * Run example: PC.test_string Parsers.nt_opcode "mov";;
 * Output: - : opcode * string = (Mov, "->[]")
 *)
let nt_opcode =
  let nt = word_ci "add" in
  let nt = pack nt (fun _ -> Add) in
  let nt' = word_ci "sub" in
  let nt' = pack nt' (fun _ -> Sub) in
  let nt'' = word_ci "mul" in
  let nt'' = pack nt'' (fun _ -> Mul) in
  let nt''' = word_ci "div" in
  let nt''' = pack nt''' (fun _ -> Div) in
  let nt'''' = word_ci "mov" in
  let nt'''' = pack nt'''' (fun _ -> Mov) in
  let nt = disj nt (disj nt' (disj nt'' (disj nt''' nt''''))) in
  nt;;

(* add your own code here after this comment *)
(* ------------------------- 2.2 -------------------*)



(*
 * Description:  Parses each operation to its' matching  ScmOp.
 * Run example:  PC.test_string Parsers.nt_ScmOp "+";;
 * Output : - : scm_op * string = (ScmAdd, "->[]")
 *)
let nt_ScmOp =
  let nt = char '+' in
  let nt = pack nt (fun _ -> ScmAdd) in
  let nt' = char '-' in
  let nt' = pack nt' (fun _ -> ScmSub) in
  let nt'' = char '*' in
  let nt'' = pack nt'' (fun _ -> ScmMul) in
  let nt''' = char '/' in
  let nt''' = pack nt''' (fun _ -> ScmDiv) in
  let nt = disj nt (disj nt' (disj nt'' nt''' )) in
  nt;;

(*
 * Description:  deals with op with white spaces in both sides. removes those white spaces.
 * Run example:   PC.test_string Parsers.nt_intNoSpc "        +   ";;
 * Output: - : expr * string = (ScmAdd, "->[]")
 *)
let nt_opNoSpc = 
  let nt_whtSpcStar = star nt_whitespace in
  let nt = caten nt_whtSpcStar nt_ScmOp in 
  let nt = pack nt (fun (spaces, op) -> op) in
  let nt = caten nt nt_whtSpcStar in
  let nt = pack nt (fun (op, spaces) -> op)in
  nt;;

(*
 * Description:  Parses each int to its' matching ScmInt.
 * Run example:  PC.test_string Parsers.nt_ScmInt "3";;
 * Output: - : expr * string = (ScmInt 3, "->[]")
 *)
let nt_ScmInt = pack nt_nat (fun num -> (ScmInt num));;

(*
 * Description:  deals with int with white spaces in both sides. removes those white spaces.
 * Run example:   PC.test_string Parsers.nt_intNoSpc "        5   ";;
 * Output: - : expr * string = (ScmInt 5, "->[]")
 *)
let nt_intNoSpc = 
  let nt_whtspcSTAR = star nt_whitespace in
  let nt = caten nt_whtspcSTAR nt_ScmInt in
  let nt = pack nt (fun (_,number) -> number) in 
  let nt = caten nt nt_whtspcSTAR in
  let nt = pack nt (fun (number,_) -> number) in
  nt;;

(*
 * Description:  Deals with many ints and as many spaces in between.
 * Run example:  PC.test_string Parsers.nt_ints "        5           8   8   ";;
 * Output: - : expr list * string = ([ScmInt 5; ScmInt 8; ScmInt 8], "->[]")
 *)
let nt_ints = 
  let nt_whtspcSTAR = star nt_whitespace in
  let nt = caten nt_whtspcSTAR nt_ScmInt in
  let nt = pack nt (fun (_,number) -> number) in 
  let nt = caten nt nt_whtspcSTAR in
  let nt = pack nt (fun (number,_) -> number) in
  let nt = plus nt in
  nt;;

(*
 * Description:  Deals with an OP and many ints and as many spaces in between.
 * Run example:  PC.test_string Parsers.nt_opAndInts "  +      5           8   8   ";;
 * Output: - : expr * string = (ScmOp (ScmAdd, [ScmInt 5; ScmInt 8; ScmInt 8]), "->[]")
 *)
let nt_opAndInts = 
  let nt = star nt_whitespace in
  let nt = caten nt nt_ScmOp in
  let nt = pack nt (fun (_,op) -> op) in
  let nt = caten nt nt_ints in
  let nt = pack nt (fun (op,ints) -> ScmOp (op,ints)) in
  nt;;

(*
 * Description: 
 * Run example: 
 * Output: 
 *)
let nt_openParent =
  let nt_whtSpcStar = star nt_whitespace in
  let nt = caten nt_whtSpcStar (char '(') in
  let nt = pack nt (fun (spaces,openparen) -> openparen) in
  let nt = caten nt nt_whtSpcStar in 
  let nt =  pack nt (fun (openparen, spaces) -> openparen) in
  nt;;

(*
 * Description: 
 * Run example: 
 * Output: 
 *)
let nt_closeParent = 
   let nt_whtSpcStar = star nt_whitespace in
  let nt = caten nt_whtSpcStar (char ')') in
  let nt = pack nt (fun (spaces,openparen) -> openparen) in
  let nt = caten nt nt_whtSpcStar in 
  let nt =  pack nt (fun (openparen, spaces) -> openparen) in
  nt;;

(*
 * Description: 
 * Run example: 
 * Output: 
 *)
let nt_Parentheses = 
  let nt' = star nt_whitespace in
  let nt = caten nt' (char '(') in
  let nt = pack nt (fun (_,par) -> par) in
  let nt = caten nt nt_opAndInts in
  let nt = pack nt (fun (par,oai) -> oai) in
  let nt = caten nt (char ')') in
  let nt = pack nt (fun (oai,par) -> oai) in
  let nt = caten nt nt' in
  let nt = pack nt (fun (oai,_) -> oai) in
  nt;;

let nt_div_helper =
  let nt = char '/'  in
  let nt = pack nt (fun _ -> ScmDiv) in 
  let nt = caten nt (plus nt_whitespace) in
  let nt = caten nt nt_ScmInt in
  let nt = pack nt (fun ((op,_),number) -> ScmOp(op,(ScmInt 1)::number::[])) in
  nt;;

let nt_div = 
  let nt' = star nt_whitespace in
  let nt = caten nt' (char '(') in
  let nt = pack nt (fun (_,par) -> par) in
  let nt = caten nt nt_div_helper in
  let nt = pack nt (fun (par,oai) -> oai) in
  let nt = caten nt (char ')') in
  let nt = pack nt (fun (oai,par) -> oai) in
  let nt = caten nt nt' in
  let nt = pack nt (fun (oai,_) -> oai) in
  nt;;



let nt_sub_helper =
  let nt = char '-'  in
  let nt = pack nt (fun _ -> ScmSub) in 
  let nt = caten nt (plus nt_whitespace) in
  let nt = caten nt nt_ScmInt in
  let nt = pack nt (fun ((op,_),number) -> ScmOp(op,(ScmInt 0)::number::[])) in
  nt;;


  
let nt_sub = 
  let nt' = star nt_whitespace in
  let nt = caten nt' (char '(') in
  let nt = pack nt (fun (_,par) -> par) in
  let nt = caten nt nt_sub_helper in
  let nt = pack nt (fun (par,oai) -> oai) in
  let nt = caten nt (char ')') in
  let nt = pack nt (fun (oai,par) -> oai) in
  let nt = caten nt nt' in
  let nt = pack nt (fun (oai,_) -> oai) in
  nt;;


(*
 * Description: parses lists of characters and generate the appropriate value of type expr.
 * Run example: PC.test_string Parsers.nt_expr "(+ 2 3 4 5)";;
 * Output: - : expr * string = (ScmOp (ScmAdd, [ScmInt 2; ScmInt 3; ScmInt 4; ScmInt 5]), "->[]")
 *)
let nt_expr = 
  let rec rec_nt_expr () = 
    let nt = caten nt_openParent nt_opNoSpc in
    let nt = pack nt (fun (paren,op) -> op) in 
    let nt' = star (delayed rec_nt_expr) in
    let nt'' = caten nt nt_whitespace in
    let nt'' = caten nt'' nt_ints in
    let nt = caten nt nt' in
    let nt = caten nt nt_closeParent in 
    let nt = pack nt (fun ((op,recu),a)->(ScmOp(op,recu))) in
    let nt =  disj_list [nt_sub ; nt_div ; nt_intNoSpc ; nt ] in
    nt in
    rec_nt_expr();;

(*
 * Description: used for running test
 * Run example: 
 * Output: 
 *)
let test1 = "CHANGE THIS FOR TESTING";;

(* ------------------------- 2.5 -------------------*)
  (* TODO add handleEnterStar which handles combinations of enters and stars*)

(*let nt_enter = const (fun ch -> ch <= '\n');;*)
let nt_enter = char '\n';;
let nt_enterStar = star nt_enter;;
let nt_whtSpcStar = star nt_whitespace;;

let handleWhtSpc nt = 
  let nt = caten nt_whtSpcStar nt in
  let nt = pack nt (fun (ws,op) -> op) in
  let nt = caten nt nt_whtSpcStar in
  let nt = pack nt (fun (op, ws) -> op) in
  nt;;

let handleEnter nt = 
  let nt = caten nt_enterStar nt in
  let nt = pack nt (fun (ws,op) -> op) in
  let nt = caten nt nt_enterStar in
  let nt = pack nt (fun (op, ws) -> op) in
  nt;;


(* PC.test_string Full_Cycle.nt_opWhtSpc "     mov           ";;*)
let nt_opWhtSpc = handleWhtSpc nt_opcode;;

(* PC.test_string Full_Cycle.nt_regWhtSpc "     r1          ";; *)
let nt_regWhtSpc = handleWhtSpc nt_register;;

(* PC.test_string Full_Cycle.nt_comma "     ,               ";; *)
let nt_comma = 
  let nt = char ',' in
  let nt = handleWhtSpc nt in
  nt;;

(* PC.test_string Full_Cycle.nt_regComma "  r1      ,       ";;
 * PC.test_string Full_Cycle.nt_regComma "  r1,             ";; *)
let nt_regComma = 
  let nt = caten nt_regWhtSpc nt_comma in
  let nt = pack nt (fun (reg,comma) -> reg) in
  let nt = handleWhtSpc nt in
  nt;;

let nt_imm2 = 
  let nt = char '$' in
  let nt = caten nt_whtSpcStar nt in (* _*$ -> $  *)
  let nt = pack nt (fun (spc,dlr) -> dlr) in
  let nt = caten nt nt_int  in 
  let nt = pack nt (fun (dlr,n) -> n) in
  let nt = caten nt nt_whtSpcStar in
  let nt = pack nt (fun (n,spc) ->(Imm n)) in
  nt;;

let nt_immWhtSpc = handleWhtSpc nt_imm2;;

let nt_ri = 
  let nt = caten nt_opWhtSpc (caten nt_regComma nt_immWhtSpc) in
  let nt = pack nt (fun (op,(reg,im)) -> (InstRI(op,reg,im))) in
  let nt = handleEnter nt in
  nt;;

let nt_rr = 
  let nt = caten nt_opWhtSpc (caten nt_regComma nt_regWhtSpc) in
  let nt = pack nt (fun (op,(reg1,reg2)) -> (InstRR(op,reg1,reg2))) in
  let nt = handleEnter nt in
  nt;;


let nt_assembly_program =
  let nt = star (disj nt_rr nt_ri) in
  nt;; 




end;; (* end of struct Parsers *)


module type FULL_CYCLE = sig

  val compile_arith : expr -> inst list
  val assembly_program_to_string : inst list -> string
  val decompile_assembly_program : inst list -> expr'
  val expr'_to_string : expr' -> string
  val full_cycle : string -> string
end;; (* end of signature FULL_CYCLE *)
 

module Full_Cycle : FULL_CYCLE = struct

let apply_append s = List.fold_right (@) s [];;

let find_max_register insts =  
  1 + (List.fold_right max 
		       (List.map
			  (function
			    | InstRI(_, (Reg i), _) -> i
			    | InstRR(_, (Reg i), (Reg j)) -> (max i j))
			  insts)
		       0);;




(* add your own code after this *)


(* ------------------------- 2.3 -------------------*)

let scm_op_to_opcode e = 
match e with
    | ScmAdd -> Add
    | ScmMul -> Mul
    | ScmSub -> Sub
    | ScmDiv -> Div
;;

let is_ScmInt num = 
   match num with
  | ScmOp (op,ints) -> false
  | ScmInt (c) -> true;;

let rec comp_arith_is_flat ints = 
  if ints=[] then true else
  let first_arg = List.hd ints in
  let ints = List.tl ints in
  if (is_ScmInt first_arg) = false then false else (comp_arith_is_flat ints);;

let comp_arith_helper_RI op reNum imNum = [InstRI (op, (Reg reNum), (Imm imNum))];;
let comp_arith_helper_RR op reNum1 reNum2 = [InstRR (op, (Reg reNum1), (Reg reNum2))];;

let rec comp_arith_helper_flat reNum =
  function 
  |(ScmInt imNum) -> comp_arith_helper_RI Mov reNum imNum
  |(ScmOp (op,hd::tl)) -> (comp_arith_helper_flat reNum hd) @ (List.flatten (List.map (comp_arith_helper_nested (scm_op_to_opcode op) reNum) tl)) 
  | _ -> [] 

and comp_arith_helper_nested formerOp reNum  =
  function 
  |(ScmInt imNum) -> comp_arith_helper_RI formerOp reNum imNum
  |(ScmOp (op,ints)) -> (comp_arith_helper_flat (reNum+1) (ScmOp (op,ints))) @ (comp_arith_helper_RR formerOp reNum (reNum+1));;

let compile_arith e =
 match e with
 | (ScmInt imNum) -> comp_arith_helper_RI Mov 0 imNum
 | ScmOp (op,ints) -> comp_arith_helper_flat 0 e;;

(* ------------------------- 2.4 -------------------*) 
(*
 * Description: used for running tests. this creates an "inst list"
 * Run example: Full_Cycle.test2;;
 * Output: 
 *)
let test2 = [InstRI (Mov, Reg 0, Imm 3)];;
let test3 = "tmp";;

(*
 * Description: generates the matching string for each op. 
 *)
let opToString = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Mov -> "mov";;

(* i left this for syntax refernce*)
(*let opToString s = 
  match s with
  | s -> "Add"
  | s -> "Sub"
  | s -> "Mul"
  | s -> "Div"
  | s -> "Mov";;*)

(*
 * Description: used for running tests. this creates an "inst list"
 * Run example: Full_Cycle.helper Full_Cycle.test2;;
 * Output: - : string = "mov r0, $3"
 *)
let helper = function
  | InstRI (op, Reg rNum,Imm iNum) -> 
     let nt_op1 = (opToString op) in
     let nt_reg1 = "r" in
     let nt_rnum = (Pervasives.string_of_int rNum) in
     let nt_imm1 = "$" in
     let nt_inum = (Pervasives.string_of_int iNum) in
     let nt_regandnum = String.concat "" [nt_reg1;nt_rnum;","] in
     let nt_immandnum = String.concat "" [nt_imm1;nt_inum] in
     let nt = String.concat " " [nt_op1;nt_regandnum;nt_immandnum] in
     nt
  | InstRR (op , Reg rNum1, Reg rNum2 ) ->
      let nt_op1 = (opToString op) in
     let nt_reg1 = "r" in
     let nt_rnum1 = (Pervasives.string_of_int rNum1) in
     let nt_rnum2 = (Pervasives.string_of_int rNum2) in
     let nt_regandnum1 = String.concat "" [nt_reg1;nt_rnum1;","] in
     let nt_regandnum2 = String.concat "" [nt_reg1;nt_rnum2] in
     let nt = String.concat " " [nt_op1;nt_regandnum1;nt_regandnum2] in
     nt
  | _ ->  raise X_This_should_not_happen;; 

(*
 * Description: runs 'helper' function on each member of the given list
 * and concatenates it to a single string (delimited with '\n')
 * Run example: see http://www.cs.bgu.ac.il/~comp161/wiki.files/hw1.html#sec-2-4 
 * Param: s of type inst list
 * Output: - : string = see run example.
 *)
let assembly_program_to_string s =
  let maphelper = List.map helper s in
  let nt = String.concat "\n" maphelper in
  nt;;

(* ------------------------- 2.6 -------------------*)


let opToScmOp = function
  | Add -> ScmAdd
  | Sub -> ScmSub
  | Mul -> ScmMul
  | Div -> ScmDiv;;

(* inst list -> expr' *)
let decompile_assembly_program insts = 
  let regArr = Array.make (find_max_register insts) Uninitialized in
  let rec helper6 = function
    | (InstRI (op ,Reg a, Imm b))::tail ->            
      if op = Mov then begin
                            (Array.set regArr a (ScmInt' b));
                            (helper6 tail);
                       end
      else if Array.get regArr a = Uninitialized then
                      Uninitialized
      else begin
                (Array.set regArr a (ScmOp' ((opToScmOp op), (Array.get regArr a)::[(ScmInt' b)])));
                (helper6 tail);
           end
    | (InstRR (op ,Reg a, Reg b))::tail ->
      if op = Mov then begin
                        if Array.get regArr b = Uninitialized then
                         Uninitialized
                       else begin
                            (Array.set regArr a (Array.get regArr b));
                            (helper6 tail);
                       end
                     end
      else if Array.get regArr a != Uninitialized && Array.get regArr b != Uninitialized then begin
                (Array.set regArr a (ScmOp' ((opToScmOp op), (Array.get regArr a)::[(Array.get regArr b)])));
                (helper6 tail);  
                end    
      else Uninitialized
    | [] -> Array.get regArr 0 in
helper6 insts;;

(* ------------------------- 2.7 -------------------*)

let scmOpToOp = function
  | ScmAdd -> "+ "
  | ScmSub -> "- "
  | ScmMul -> "* "
  | ScmDiv -> "/ ";;

let rec expr'_to_string = function
  | Uninitialized -> raise X_Expression_includes_uninitialized_values
  | (ScmInt' a) -> Pervasives.string_of_int a
  | (ScmOp' (op,lst)) -> "(" ^ (scmOpToOp op) ^ (String.concat " " (List.map expr'_to_string lst)) ^ ")";;   

(* do not add your own code after this *)


let full_cycle string =
  try (match (Parsers.nt_expr (string_to_list string)) with
       | (expr, []) ->
	  (try (match (Parsers.nt_assembly_program
			 (string_to_list
			    (assembly_program_to_string
			       (compile_arith expr)))) with 
		| (insts, []) ->
		   (expr'_to_string (decompile_assembly_program insts))
		| _ -> raise X_Cannot_parse_assembly_program_string)
	   with PC.X_no_match -> raise X_Cannot_parse_assembly_program_string)
	      | _ -> raise X_Cannot_parse_expr_string)
  with PC.X_no_match -> raise X_Cannot_parse_expr_string;;
 
end;; (* end of struct Full_Cycle *)
  
(* end of input *)
