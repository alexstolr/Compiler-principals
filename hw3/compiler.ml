(* compiler.ml
 * A compiler from Scheme to CISC
 *
 * Programmer: Alex Stoliar and Orian Zinger, 2015
*)

#use "pc.ml";;

exception X_not_yet_implemented;;
exception X_WTF;;
exception X_this_should_not_happen;;

let rec ormap f s =
  match s with
  | [] -> false
  | car :: cdr -> (f car) || (ormap f cdr);;

  let rec andmap f s =
  match s with
  | [] -> true
  | car :: cdr -> (f car) && (andmap f cdr);;	  

  let string_to_list str =
  let rec loop i limit =
  if i = limit then []
else (String.get str i) :: (loop (i + 1) limit)
in
loop 0 (String.length str);;

let list_to_string s =
  let rec loop s n =
  match s with
  | [] -> String.make n '?'
  | car :: cdr ->
  let result = loop cdr (n + 1) in
  String.set result n car;
  result
in
loop s 0;;

type fraction = {numerator : int; denominator : int};;

type number =
| Int of int
| Fraction of fraction;;

type sexpr =
| Void
| Bool of bool
| Nil
| Number of number
| Char of char
| String of string
| Symbol of string
| Pair of sexpr * sexpr
| Vector of sexpr list;;

module type SEXPR = sig
val sexpr_to_string : sexpr -> string
end;; (* signature SEXPR *)

module Sexpr : SEXPR = struct

open PC;;

exception X_invalid_fraction of fraction;;

let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
   (fun ch -> (ch = (Char.lowercase ch)))
 s) then str
else Printf.sprintf "|%s|" str;;

let string_of_bool b =
  match b with
  |false -> "#f"
  |true -> "#t"
;;

let rec sexpr_to_string sexpr = 
  match sexpr with
  | Pair ((Symbol "quasiquote"), Pair (Symbol qq, Nil)) -> "`" ^ qq
  | Pair ((Symbol "unquote"), Pair (Symbol uq, Nil)) -> "," ^ uq
  | Void -> ""
  | Bool (b) -> string_of_bool b
  | Number (Int n) ->  (string_of_int n)
  | Number (Fraction {numerator = num; denominator = denom}) -> 
  (string_of_int num) ^ "/" ^ (string_of_int denom)
  | Nil -> "()"
  | Char c -> (list_to_string [c])
  | String str -> str
  | Symbol sym -> sym 
  | Pair (car,cdr) -> "(" ^ (string_of_pair car cdr) ^ ")"
  | Vector v -> "#(" ^ (string_of_vector v)  ^ ")"
  (*| _ -> raise X_no_match*)

  and string_of_pair car cdr =
  let car_string = (sexpr_to_string car) in
  let cdr_string = match cdr with
  | Nil -> ""
  | Pair (nested_car, nested_cdr) -> (string_of_pair nested_car nested_cdr)
  | _ -> " . "  ^ (sexpr_to_string cdr) in 
  car_string ^ " " ^ cdr_string 

  and string_of_vector v = 
  match v with
  | [] -> ""
  | _ ->  (sexpr_to_string (List.hd v)) ^ " " ^ (string_of_vector (List.tl v));;

end;; (* struct Sexpr *)

module type PARSER = sig
val read_sexpr : string -> sexpr
val read_sexprs : string -> sexpr list
end;;

module Parser : PARSER = struct

open PC;;

let nt_bool =
let nt_true = pack (word_ci "#t") (fun (_) -> (Bool true)) in
let nt_false = pack (word_ci "#f") (fun (_) -> (Bool false)) in
let nt = disj nt_false nt_true in
nt ;;

let nt_void s = 
  match s with
  | ['V';'o';'i';'d'] -> (Void,[' '])
  | _ -> raise X_no_match;;

  let nt_whtSpcStar = star nt_whitespace;;
  let wrapWithBool nt = (pack nt (fun _ -> true));;

  let make_char_value base_char displacement =
  let base_char_value = Char.code base_char in
  fun ch -> (Char.code ch) - base_char_value + displacement;;

  let nt_digits = 
  let nt = range '0' '9' in
  let nt = pack nt (make_char_value '0' 0) in
  let nt = plus nt in
  let nt = pack nt (fun s -> List.fold_left (fun a b -> a * 10 + b) 0 s) in
  nt;;

  let nt_digits_with_hexa = 
  let nt_dec = range '0' '9' in
  let nt_dec = pack nt_dec (make_char_value '0' 0) in
  let nt_not_cap = range 'a' 'f' in
  let nt_not_cap = pack nt_not_cap (make_char_value 'a' 0xa) in
  let nt_cap = range 'A' 'F' in
  let nt_cap = pack nt_cap (make_char_value 'A' 0xA) in
  let nt = disj_list [nt_dec ; nt_cap ; nt_not_cap] in
  let nt = plus nt in
  let nt = pack nt (fun s -> List.fold_left (fun a b -> a * 16 + b) 0 s) in
  nt;;

  let nt_hexa = 
  let nt = caten (word_ci "0x") nt_digits_with_hexa in
  let nt = pack nt (fun (_,digits) -> digits) in
  nt;;

  let nt_int s = 
  let nt = char '-' in
  let nt = pack nt (fun e -> -1) in
  let nt' = char '+' in
  let nt' = pack nt' (fun e -> 1) in
  let nt = disj nt nt' in
  let nt = maybe nt in
  let nt = pack nt (function | None -> 1 | Some(mult) -> mult) in
  let nt_dec = caten nt nt_digits in
  let nt_hex = caten nt nt_hexa in
  let nt = disj nt_hex nt_dec in
  let nt = pack nt (fun (mult, n) -> Number(Int (mult * n))) in
  nt s;;

  let nt_int_for_fraction_dec = 
  let nt = char '-' in
  let nt = pack nt (fun e -> -1) in
  let nt' = char '+' in
  let nt' = pack nt' (fun e -> 1) in
  let nt = disj nt nt' in
  let nt = maybe nt in
  let nt = pack nt (function | None -> 1 | Some(mult) -> mult) in
  let nt = caten nt nt_digits in
  let nt = pack nt (fun (mult, n) -> (mult * n)) in
  nt;;

  let nt_int_for_fraction_hex = 
  let nt = char '-' in
  let nt = pack nt (fun e -> -1) in
  let nt' = char '+' in
  let nt' = pack nt' (fun e -> 1) in
  let nt = disj nt nt' in
  let nt = maybe nt in
  let nt = pack nt (function | None -> 1 | Some(mult) -> mult) in
  let nt = caten nt nt_hexa in
  let nt = pack nt (fun (mult, n) -> (mult * n)) in
  nt;;

  let rec gcd num denom = 
  if denom = 0 then num
else gcd denom (num mod denom);;

let nt_fraction_handler num denom=
if (denom = 0) then raise X_no_match 
else let the_gcd = (gcd num denom) in
if the_gcd = denom then Number(Int (num / denom))
else Number(Fraction ({numerator = num / (abs the_gcd) ;  denominator = denom / (abs the_gcd)}));;

let nt_fraction_handler_dec s = 
  let nt = caten nt_int_for_fraction_dec (char '/') in
  let nt = pack nt (fun (num,_) -> num) in
  let nt = caten nt nt_digits in
  let nt = pack nt (fun (num,denom)-> (nt_fraction_handler num denom) ) in
  nt s;;

  let nt_fraction_handler_hexa s = 
  let nt = caten nt_int_for_fraction_hex (char '/') in
  let nt = pack nt (fun (num,_) -> num) in
  let nt = caten nt nt_digits_with_hexa in
  let nt = pack nt (fun (num,denom)-> (nt_fraction_handler num denom) ) in
  nt s;;

  let nt_fraction s =
  let nt = disj nt_fraction_handler_hexa nt_fraction_handler_dec in
  nt s;;

  let nt_digit = range '0' '9';;
  let nt_lower = range 'a' 'z';;
  let nt_upper = range 'A' 'Z';;
  let nt_punc = disj_list [char '!' ; char '$' ; char '^' ; char '*' ; char '-' ; char '_' ; char '=' ; char '+' ; char '<' ; char '>' ; char '/' ; char '?'];;

  let nt_num s =
  let nt_first = disj nt_fraction nt_int in
  let nt_crap = (plus (disj_list [nt_lower ; nt_upper ; nt_punc])) in
  let nt = diff nt_first (caten nt_first nt_crap) in
  nt s;;

  let nt_symbol = 
  let nt_letters = pack (disj nt_lower nt_upper) (fun l -> Char.lowercase l) in
  let nt = (plus (disj_list [nt_letters ; nt_digit ; nt_punc])) in
  let nt = pack nt (fun sym -> Symbol(list_to_string sym)) in
  nt;;

  let nt_string_meta_n = pack (word "\\n") (fun (_) -> '\n');;
  let nt_string_meta_r = pack (word "\\r") (fun (_) -> '\r');;
  let nt_string_meta_t = pack (word "\\t") (fun (_) -> '\t');;
  let nt_string_meta_f = pack (word "\\f") (fun (_) -> '\012');;
  let nt_string_meta_back_slash = pack (word "\\\\") (fun (_) -> '\\');;

  let nt_string_meta = disj_list [nt_string_meta_n ; nt_string_meta_r ; nt_string_meta_t ; nt_string_meta_f ; nt_string_meta_back_slash];;

  let nt_chars = 
  let nt = diff nt_any (one_of "\\\"") in
  let nt = disj nt nt_string_meta in
  let nt = star nt in
  nt;;

  let nt_string = 
  let nt = caten (word "\"") nt_chars in  
  let nt = pack nt (fun (_,chars) -> chars) in
  let nt = caten nt (word "\"") in
  let nt = pack nt (fun (chars,_) -> chars) in
  let nt = pack nt list_to_string in
  let nt = pack nt (fun str -> String str) in
  nt;;


  let nt_char_meta_newline = pack (word_ci "newline") (fun (_) -> '\n');;
  let nt_char_meta_return = pack (word_ci "return") (fun (_) -> '\r');;
  let nt_char_meta_tab = pack (word_ci "tab") (fun (_) -> '\t');;
  let nt_char_meta_page = pack (word_ci "page") (fun (_) -> '\012');;
  let nt_char_meta_space = pack (word_ci "space") (fun (_) -> ' ');;

  let nt_char_meta = disj_list [nt_char_meta_newline ; nt_char_meta_return ; nt_char_meta_tab ; nt_char_meta_page ; nt_char_meta_space];;

  let nt_char_handler = 
  let nt = diff nt_any (one_of "\\\"") in
  let nt = disj nt_char_meta nt in
  nt;;

  let nt_char = 
  let nt = caten (word "#\\") nt_char_handler in  
  let nt = pack nt (fun (_,c) -> Char c) in
  nt;;

  let rec nt_sexpr s =
  let listWithoutCrap = List.map removeSkiped [nt_void; nt_bool; nt_nil ; nt_num ; nt_symbol ; nt_char ; nt_string ; nt_pair ; nt_vector ; nt_quotes] in
  let chooseParser = disj_list listWithoutCrap in
  chooseParser s

  and nt_lineComnt s= 
  let semicolon = (char ';') in
  let nt3 = diff nt_any (char '\n') in
  let anyStar = (star nt3) in
  let endLine = disj (char '\n') (pack nt_end_of_input (fun _ -> '\n')) in
  let nt = caten nt_whtSpcStar semicolon in
  let nt = pack nt (fun (ws,chr) -> chr) in
  let nt = caten nt anyStar in
  let nt = pack nt (fun (smclon,any) ->  "")  in
  let nt = caten nt endLine in 
  let nt = pack nt (fun (_,endLne) ->  "")  in
  nt s


  and nt_sexprComnt s= 
  let nt = caten (word "#;") nt_sexpr in
  nt s

  and removeSkiped nt s = 
  let starRemove = (star nt_rmv) in
  let nt' = caten starRemove nt in
  let nt' = pack nt' (fun (ws,op) -> op) in
  let nt' = caten nt' starRemove in
  let nt' = pack nt' (fun (op, ws) -> op) in
  nt' s

  and nt_rmv s=
  let nt = disj_list [(wrapWithBool nt_whitespace);(wrapWithBool nt_lineComnt);(wrapWithBool nt_sexprComnt)] in
  nt s


  and nt_nil s= 
  let nt = caten (char '(') (star nt_rmv)  in
    let nt = pack nt (fun (leftParent,crap) -> leftParent) in
    let nt = caten nt (char ')') in
  let nt = pack nt (fun (crap,rightParent) -> Nil) in
  nt s 

  and nt_pair s=
  let nt = caten  (char '(') (star nt_sexpr) in
    let nt = pack nt (fun (_,sex) -> sex) in
  let nt1 = char ')' in
  let nt1 = pack nt1 (fun _ -> Nil) in 
  
  let nt2 = caten nt_sexpr (char ')') in
let nt3 = caten  (char '.') nt2 in
let nt2 = pack nt3 (fun (_, (sex, _)) ->sex) in

let nt1 = disj nt1 nt2 in

let nt = caten nt nt1 in
let nt = pack nt (fun (s, e) ->
  List.fold_right 
  (fun a b -> Pair(a,b))
  s
e) in
nt s

and nt_vector s = 
let nt = caten (word "#(") (star nt_sexpr) in
  let nt = pack nt (fun (_,vec) -> vec) in
  let nt = caten nt (char ')') in
let nt = pack nt (fun (vec,_) -> (Vector vec)) in
nt s

and nt_quote s= 
let nt = caten (word "'") nt_sexpr in
let nt = pack nt (fun (_,sex) -> (Pair ((Symbol "quote") , Pair(sex , Nil)))) in
nt s

and nt_quasiquote s = 
let nt = caten (word "`") nt_sexpr in
let nt = pack nt (fun (_,sex) -> (Pair ((Symbol "quasiquote") , Pair(sex , Nil)))) in
nt s

and nt_unquotedSpliced s = 
let nt = caten (word ",@") nt_sexpr in
let nt = pack nt (fun (_,sex) -> (Pair ((Symbol "unquote-splicing") , Pair(sex , Nil)))) in
nt s

and nt_unquote s = 
let nt = caten (word ",") nt_sexpr in
let nt = pack nt (fun (_,sex) -> (Pair ((Symbol "unquote") , Pair(sex , Nil)))) in
nt s

and nt_quotes s = disj_list [nt_quote ; nt_quasiquote ; nt_unquotedSpliced ; nt_unquote] s;;

let read_sexpr string =  
  match (nt_sexpr (string_to_list string)) with 
  |(a,[]) ->a
  |(a,_) -> raise X_no_match;; 

  let read_sexprs string =
  let stl = string_to_list string in
  let rec helper stl =  
  match  (nt_sexpr stl) with
  |(frst,[]) -> [frst]
  |(frst,rest) -> frst::(helper rest) in
  helper stl;;

end;; (* struct Parser *)


open PC;;



(* work on the tag parser starts here *)

(* from assignment 2*)
(*
type expr =
  | Const of sexpr
  | Var of string  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list)
  | ApplicTP of expr * (expr list);;
*)

(*new - assignment 3*)
type expr =
| Const of sexpr
| Var of string  
| If of expr * expr * expr
| Seq of expr list
| Set of expr * expr
| Def of expr * expr
| Or of expr list
| LambdaSimple of string list * expr
| LambdaOpt of string list * string * expr
| Applic of expr * (expr list)

exception X_syntax_error;;

module type TAG_PARSER = sig
val read_expression : string -> expr
val read_expressions : string -> expr list
val expression_to_string : expr -> string
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
["and"; "begin"; "cond"; "define"; "do"; "else";
"if"; "lambda"; "let"; "let*"; "letrec"; "or";
"quasiquote"; "quote"; "set!"; "unquote";
"unquote-splicing"];;  

let rec symHelper sym l=
if l = [] 
then (Var sym) 
else (if ((List.hd l) = sym ) 
  then raise X_syntax_error 
else (symHelper sym (List.tl l)));;

let rec process_scheme_list s ret_nil ret_one ret_several =
  match s with
  | Nil -> ret_nil ()
  | (Pair(sexpr, sexprs)) ->
  process_scheme_list sexprs
  (fun () -> ret_one sexpr)
  (fun sexpr' -> ret_several [sexpr; sexpr'])
  (fun sexprs -> ret_several (sexpr :: sexprs))
  | _ -> raise X_syntax_error;;
  
  let scheme_list_to_ocaml_list args = 
  process_scheme_list args
  (fun () -> [])
  (fun sexpr -> [sexpr])
  (fun sexprs -> sexprs);;

  let ocaml_list_to_scheme_list l = List.fold_right (fun a b -> (Pair (a,b))) l Nil;;

  let expand_let_star ribs sexprs =
  let ribs = scheme_list_to_ocaml_list ribs in
  let params = List.map (function
   | (Pair(name, (Pair(expr, Nil)))) -> name
 | _ -> raise X_this_should_not_happen) ribs in
  let args = List.map
  (function
   | (Pair(name, (Pair(expr, Nil)))) -> expr
 | _ -> raise X_this_should_not_happen) ribs in
  let params_set = List.fold_right
  (fun a s ->
    if (ormap
     (fun b ->
      (match (a, b) with
       | (Symbol a, Symbol b) -> a = b
     | _ -> raise X_this_should_not_happen))
   s)
  then s else a :: s)
  params
  [] in
  let place_holders = List.fold_right
  (fun a s -> Pair(a, s))
  (List.map
    (fun var -> (Pair(var, (Pair((Bool false), Nil)))))
  params_set)
  Nil in
  let assignments = List.map2
  (fun var expr ->
   (Pair((Symbol("set!")),
     (Pair(var, (Pair(expr, Nil)))))))
  params args in
  let body = List.fold_right
  (fun a s -> Pair(a, s))
  assignments
  sexprs in
  (Pair((Symbol("let")), (Pair(place_holders, body))));;

  let expand_letrec ribs sexprs =
  let ribs = scheme_list_to_ocaml_list ribs in
  let params = List.map (function
   | (Pair(name, (Pair(expr, Nil)))) -> name
 | _ -> raise X_this_should_not_happen) ribs in
  let args = List.map
  (function
   | (Pair(name, (Pair(expr, Nil)))) -> expr
 | _ -> raise X_this_should_not_happen) ribs in
  let ribs = List.map
  (function
   | (Pair(name, (Pair(expr, Nil)))) ->
   (Pair(name, (Pair(Bool false, Nil))))
 | _ -> raise X_this_should_not_happen)
  ribs in
  let body = List.fold_right
  (fun a s -> Pair(a, s))
  (List.map2
    (fun var expr ->
     (Pair((Symbol("set!")),
      (Pair(var, (Pair(expr, Nil)))))))
  params args)
  sexprs in
  let ribs = List.fold_right
  (fun a s -> Pair(a, s))
  ribs
  Nil in
  (Pair((Symbol("let")), (Pair(ribs, body))));;

  exception X_unquote_splicing_here_makes_no_sense;;

  let expand_let ribs sexpr = raise X_not_yet_implemented;;

  (*macro-expander for the quasiquoted expressions*)
  let rec expand_qq sexpr = match sexpr with
  | (Pair((Symbol("unquote")), (Pair(sexpr, Nil)))) -> sexpr
  | (Pair((Symbol("unquote-splicing")), (Pair(sexpr, Nil)))) ->
  raise X_unquote_splicing_here_makes_no_sense
  | (Pair(a, b)) ->
  (match (a, b) with
    | ((Pair((Symbol("unquote-splicing")), (Pair(a, Nil)))), b) ->
    let b = expand_qq b in
    (Pair((Symbol("append")),
      (Pair(a, (Pair(b, Nil))))))
    | (a, (Pair((Symbol("unquote-splicing")), (Pair(b, Nil))))) ->
    let a = expand_qq a in
    (Pair((Symbol("cons")), (Pair(a, (Pair(b, Nil))))))
    | (a, b) ->
    let a = expand_qq a in
    let b = expand_qq b in
    (Pair((Symbol("cons")), (Pair(a, (Pair(b, Nil)))))))
  | (Vector(sexprs)) ->
  let s = expand_qq (List.fold_right (fun a b -> Pair(a, b)) sexprs Nil) in
  (Pair((Symbol("list->vector")), (Pair(s, Nil))))
  | Nil | Symbol _ -> (Pair((Symbol("quote")), (Pair(sexpr, Nil))))
  | expr -> expr;;

(*(Printf.printf "#%s#%s" (Sexpr.sexpr_to_string procedure) (Sexpr.sexpr_to_string sexprs)) *)
  let tag_parse sexpr = 
  let rec tpHelper sexpr = 
  match sexpr with 
  | Bool _ | Number _ | Nil | Void | Char _ | String _ | Vector _ -> Const sexpr
  | (Pair ((Symbol "let"), (Pair (ribs, sexprs)))) -> (letHelper ribs sexprs)
  | (Pair ((Symbol "let*"), (Pair (ribs, sexprs)))) ->  (tpHelper (expand_let_star ribs sexprs))
  | (Pair ((Symbol "letrec"), (Pair (ribs, sexprs)))) -> (tpHelper (expand_letrec ribs sexprs))
  | (Pair ((Symbol "define"), (Pair ((Pair (def_name, def_argl), def_value))))) ->
  (tpHelper (Pair ((Symbol "define"), (Pair (def_name, (Pair ((Pair ((Symbol "lambda"), (Pair(def_argl, def_value)))), Nil)))))))
  | (Pair ((Symbol "quote"), Pair ((q, Nil)))) -> (Const q)
  | (Pair ((Symbol "quasiquote"), Pair ((q, Nil)))) -> (tpHelper (expand_qq q))
  | (Pair ((Symbol "cond"), args)) -> (cndHelper args)
  | (Pair ((Symbol "and"), pr)) -> (andHelper pr) 
  | (Pair ((Symbol "if"), (Pair (pred, Pair(do_if_true, Nil))))) -> (If ((tpHelper pred), (tpHelper do_if_true), (Const Void)))
  | (Pair ((Symbol "if"), (Pair (pred, Pair(do_if_true, (Pair (do_if_false, Nil))))))) -> (If ((tpHelper pred), (tpHelper do_if_true), (tpHelper do_if_false)))
  | (Pair ((Symbol "define"), Pair (def_name, Pair (def_value, Nil)))) -> (dfnHelper def_name def_value)
  | (Pair ((Symbol "set!"),  Pair (set_name, Pair (set_value, Nil)))) -> (Set ((tpHelper set_name), (tpHelper set_value)))
  | (Pair ((Symbol "lambda"),Pair (args, sexprs))) -> (chooseLambda (lambdaArgHandler args) (lambdaBodyHandler sexprs))
  | (Pair ((Symbol "begin"), sexprs)) -> bgnHelper sexprs
  | (Pair ((Symbol("or")), sexprs))-> orHelper sexprs
  | (Pair (procedure,sexprs)) ->  (Applic ((tpHelper procedure), (List.map tpHelper (scheme_list_to_ocaml_list sexprs))))
  | (Symbol sym) -> (symHelper sym reserved_word_list)


  and letHelper ribs sexprs = 
  let ribs = scheme_list_to_ocaml_list ribs in
  let params = List.map (function
    | (Pair(name, (Pair(expr, Nil)))) -> name
  | _ -> raise X_this_should_not_happen) ribs in
  let args = List.map
  (function
   | (Pair(name, (Pair(expr, Nil)))) -> expr
 | _ -> raise X_this_should_not_happen) ribs in
  (Applic ((chooseLambda (lambdaArgHandler (ocaml_list_to_scheme_list params)) (bgnHelper sexprs)) , (List.map tpHelper args)))

  and andHelper pr = 
  match pr with
  | Nil -> (Const (Bool true))
  | (Pair (arg, Nil)) -> tpHelper arg
  | (Pair (arg, Pair (do_if_true, Nil))) -> If (tpHelper arg, tpHelper do_if_true, (Const (Bool false)))
  | (Pair (arg, rest)) -> let s = tpHelper arg in If  (s ,tpHelper (Pair ((Symbol "and"), rest)), (Const (Bool false)) )
  | _ -> raise X_no_match

  and cndHelper args =
  match args with
  | Pair (Pair (Symbol "else", Nil), Nil) -> (Const Void)
  | Pair (Pair (Symbol "else", rest), Nil) -> (tpHelper rest)
  | Pair ((Pair (t, e)), Nil) -> If ( tpHelper t, bgnHelper e, (Const Void))
  | Pair ((Pair (t, e)), rest) -> If (tpHelper t, bgnHelper e, tpHelper (Pair ((Symbol "cond"), rest)))
  | Nil -> Const Void
  | _ -> raise X_syntax_error

  and dfnHelper name value = 
  match name with
  | (Symbol sym) ->  (Def ((tpHelper name), (tpHelper value)))
  | _ -> raise X_no_match

  and lambdaBodyHandler sexprs = 
  let sList = (Pair ((Symbol "begin"), sexprs)) in
  let lst = (flattenBegin (scheme_list_to_ocaml_list sList)) in
(*  let ([dfn_lst ; bdy_lst]) = (build_letrec lst [] []) in*)
  let (dfn_lst, bdy_lst) = (build_letrec lst [] []) in
(*  let lst = (build_letrec lst [] []) in
  let dfn_lst = if lst = [] then [] else List.hd lst in
  let bdy_lst = if lst = [] then [] else List.hd (List.tl lst) in*)
  let ribs = ocaml_list_to_scheme_list dfn_lst in
  let body = ocaml_list_to_scheme_list bdy_lst in
  let letrec_exp = if dfn_lst == [] then (Pair ((Symbol "begin"), body)) else (Pair ((Symbol "letrec"), (Pair (ribs, body)))) in
  tpHelper letrec_exp

and build_letrec lst dfn_lst bdy_lst = 
  if lst == [] then raise X_this_should_not_happen else
    let car = List.hd lst in
    let cdr = List.tl lst in
    if (Sexpr.sexpr_to_string car) = "begin" then (build_letrec cdr dfn_lst bdy_lst) else
      match car with
      | (Pair ((Symbol "define"), (Pair ((Pair (def_name, def_argl), def_value))))) ->
	 let reg_define = (Pair ((Symbol "define"), (Pair (def_name, (Pair ((Pair ((Symbol "lambda"), (Pair(def_argl, def_value)))), Nil)))))) in
	 (build_letrec (reg_define::cdr) dfn_lst bdy_lst)
      | (Pair ((Symbol "define"), Pair (def_name, Pair (def_value, Nil)))) -> let rib = Pair (def_name, Pair (def_value, Nil)) in
									      if cdr == [] then raise X_this_should_not_happen else (build_letrec cdr (dfn_lst@[rib]) cdr)
      | _ -> (dfn_lst , lst)

and flattenBegin sexprs = 
  match sexprs with
  | Pair ((Symbol "begin"), bgnSexprs)::restSexprs -> flattenBegin ((scheme_list_to_ocaml_list bgnSexprs)@(flattenBegin restSexprs))
  | [] -> []
  | notBegin::bgn -> notBegin::(flattenBegin bgn)
				 
and bgnHelper sexprs = 
  process_scheme_list sexprs
		      (fun () -> Const Void)
		      (fun sex -> tpHelper sex)
		      (fun sexes -> Seq (List.map tpHelper sexes))
		      
and orHelper sexprs =
  process_scheme_list sexprs
		      (fun () -> Const (Bool false))
		      (fun sex -> tpHelper sex)
		      (fun sexes -> Or (List.map tpHelper sexes))
		      
and chooseLambda args sexprs =
  let  (a,b)= args in
  match b with
  | Nil -> (LambdaSimple (a,sexprs))
  | (Symbol b) -> (LambdaOpt (a,b,sexprs))
  | _ -> raise X_this_should_not_happen
	       
and lambdaArgHandler args = 
  match args with 
  | Nil -> ([],Nil)
  | Pair((Symbol frst),rest) -> 
     let (frstOfRest,restofRest) = (lambdaArgHandler rest) in
     (frst::frstOfRest,restofRest)
  | _ -> ([],args)
	   
  in tpHelper sexpr;;
    

let read_expression string = tag_parse (Parser.read_sexpr string);;

let read_expressions string = List.map tag_parse (Parser.read_sexprs string);;

let string_of_bool b =
  match b with
  |false -> "#f"
  |true -> "#t"
;;

let rec expression_to_string expr = 
  match expr with
  | Const (String str) -> "\"" ^ str ^ "\""
  | Const (Char c) ->  char_expttostr c 
  | Const (Bool b) -> string_of_bool b
  | Const (Number (Int num)) -> string_of_int num
  | Const Nil -> "'()"
  | Const Void -> ""
  | Const b -> "'" ^ Sexpr.sexpr_to_string b
  | Var v -> v
  | (If (pred, do_if_true, (Const Void))) -> "(if " ^ (expression_to_string pred) ^ " " ^ (expression_to_string do_if_true) ^ ")"
  | (If (pred, do_if_true, do_if_false)) -> "(if " ^ (expression_to_string pred) ^ " " ^ (expression_to_string do_if_true) ^ " " ^ (expression_to_string do_if_false) ^ ")"
  | (Def (name, value)) -> "(define " ^ (expression_to_string name) ^ " " ^ (expression_to_string value) ^ ")"
  | (Set (name, value)) -> "(set! " ^ (expression_to_string name) ^ " " ^ (expression_to_string value) ^ ")"
  | (Seq lst) -> "(begin " ^ String.concat " " (List.map expression_to_string lst) ^ ")"
  | (Or lst) -> "(or " ^ String.concat " "(List.map expression_to_string lst) ^ ")"
  | (LambdaSimple (a,sexprs)) -> "(lambda (" ^  (String.concat " " a) ^ ") " ^ (expression_to_string sexprs) ^ ")"
  | (LambdaOpt ([], a, b)) -> "(lambda "  ^ a ^ " " ^ expression_to_string b ^ ")"
  | (LambdaOpt (l, a, b)) -> "(lambda (" ^ (String.concat " " l) ^ " . " ^ a ^ ") " ^ expression_to_string b ^ ")"
  | (Applic (procedure, sexprs)) -> "(" ^ (expression_to_string procedure)  ^ " " ^ (String.concat " " (List.map expression_to_string sexprs)) ^ ")"

  and char_expttostr c = 
  match c with
  | '\012' ->  "#\\page"
  | '\r' -> "#\\return"
  | '\t' -> "#\\tab"
  | '\n' ->  "#\\newline"
  | ' ' ->  "#\\space"
  | _ ->  "#\\" ^ Char.escaped c
;;

end;; (* struct Tag_Parser *)

let test_parser string =
  let expr = Tag_Parser.read_expression string in
  let string' = (Tag_Parser.expression_to_string expr) in
  Printf.printf "%s\n" string';;

  type var = 
  | VarFree' of string
  | VarParam' of string * int
  | VarBound' of string * int * int;;

  type expr' =
  | Const' of sexpr
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of expr' * expr'
  | Def' of expr' * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

  module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

let rec p_helper expr = 
  match expr with
  | Const' (String str) -> "\"" ^ str ^ "\""
  | Const' (Char c) ->  char_expttostr c 
  | Const' (Bool b) -> string_of_bool b
  | Const' (Number (Int num)) -> string_of_int num
  | Const' Nil -> "'()"
  | Const' Void -> ""
  | Const' b -> "'" ^ Sexpr.sexpr_to_string b
  | BoxGet' (VarFree' s) -> s
  | BoxGet' (VarParam' (s, _)) -> s
  | BoxGet' (VarBound' (s ,_ ,_)) -> s
  | Box' (VarFree' s) -> s
  | Box' (VarParam' (s, _)) -> s
  | Box' (VarBound' (s ,_ ,_)) -> s
  | BoxSet' ((VarFree' s), exp )-> "(BoxSet' " ^ " " ^ s ^ " " ^ p_helper exp ^ ")"
  | BoxSet' ((VarParam' (s ,_)), exp) -> "(BoxSet' " ^  s ^ " " ^ p_helper exp ^ ")"
  | BoxSet' ((VarBound' (s ,_ ,_)), exp) -> "(BoxSet' " ^  s ^ " " ^ p_helper exp ^ ")"
  | Var' (VarFree' s) -> s
  | Var' (VarParam' (s, _)) -> s
  | Var' (VarBound' (s ,_ ,_)) -> s
  | (If' (pred, do_if_true, (Const' Void))) -> "(if " ^ (p_helper pred) ^ " " ^ (p_helper do_if_true) ^ ")"
  | (If' (pred, do_if_true, do_if_false)) -> "(if " ^ (p_helper pred) ^ " " ^ (p_helper do_if_true) ^ " " ^ (p_helper do_if_false) ^ ")"
  | (Def' (name, value)) -> "(define " ^ (p_helper name) ^ " " ^ (p_helper value) ^ ")"
  | (Set' (name, value)) -> "(set! " ^ (p_helper name) ^ " " ^ (p_helper value) ^ ")"
  | (Seq' lst) -> "(begin " ^ String.concat " " (List.map p_helper lst) ^ ")"
  | (Or' lst) -> "(or " ^ String.concat " "(List.map p_helper lst) ^ ")"
  | (LambdaSimple' (a,sexprs)) -> "(lambda (" ^  (String.concat " " a) ^ ") " ^ (p_helper sexprs) ^ ")"
  | (LambdaOpt' ([], a, b)) -> "(lambda "  ^ a ^ " " ^ p_helper b ^ ")"
  | (LambdaOpt' (l, a, b)) -> "(lambda (" ^ (String.concat " " l) ^ " . " ^ a ^ ") " ^ p_helper b ^ ")"
  | (Applic' (procedure, sexprs)) -> "(" ^ (p_helper procedure)  ^ " " ^ (String.concat " " (List.map p_helper sexprs)) ^ ")"
  | (ApplicTP' (procedure, sexprs)) -> "(" ^ (p_helper procedure)  ^ " " ^ (String.concat " " (List.map p_helper sexprs)) ^ ")"


  and char_expttostr c = 
  match c with
  | '\012' ->  "#\\page"
  | '\r' -> "#\\return"
  | '\t' -> "#\\tab"
  | '\n' ->  "#\\newline"
  | ' ' ->  "#\\space"
  | _ ->  "#\\" ^ Char.escaped c


;;

    let p p1 p2 = Printf.printf "!!%s!! %s\n" p1 p2;;
    let pl p1 p2 = List.map (fun a -> Printf.printf "!!%s!! %s\n" p1 a) p2;;
    let plt p1 p2 = List.map (fun a -> Printf.printf "!!%s!! %s\n" p1 (p_helper a)) p2;;

module Semantics : SEMANTICS = struct

(*------------------------------------------start of  4.2 --------------------------------------------------*)
let annotate_lexical_addresses e =
  let rec run_ala e env scope = 
    match e with
    | Var v -> 
        (if inScope v scope then let minor = (findMinor v scope) in Var' (VarParam' (v,minor))
        else if inEnv v env then let (major,minor) = (findMajorAndMin v (List.rev env)) in Var' (VarBound' (v, major, minor))
        else Var' (VarFree' v))
    | (Const b) -> Const' b
    | (If (pred, dit, dif)) -> (If' (run_ala pred env scope, run_ala dit env scope, run_ala dif env scope))
    | (Def (name, value)) -> Def' (run_ala name env scope, run_ala value env scope)
    | (Set (name, value)) -> Set' (run_ala name env scope, run_ala value env scope)
    | (Seq lst) -> Seq' (List.map (fun exp -> run_ala exp env scope) lst)
    | (Or lst) -> Or' (List.map (fun exp -> run_ala exp env scope) lst)
    | (LambdaSimple (a,sexprs)) -> (LambdaSimple' (a,run_ala sexprs (env@[scope]) a))
    | (LambdaOpt ([], a, b)) -> (LambdaOpt' ([], a, run_ala b (env@[scope]) [a]))
    | (LambdaOpt (l, a, b)) -> (LambdaOpt' (l, a, run_ala b (env@[scope]) (l@[a])))
    | (Applic (procedure, sexprs)) -> Applic' (run_ala procedure env scope, List.map (fun exp -> run_ala exp env scope) sexprs)


(*checks if v is in scope*)
and inScope v scope = ormap (fun vInList -> v = vInList) scope 
(*checks if v is in env*)
and inEnv v env = ormap (fun scope -> (inScope v scope)) env 

(*finds and returns the index of v in scope scp*)
and countVar_scp scp v i=
  match scp with
  | [] -> i
  | car::cdr -> if car = v then i else (countVar_scp cdr v (i + 1))

and findMinor v scope = countVar_scp scope v 0

and countVar_env v env maj min =
  (*let print = Printf.printf "!!countVar!! %s" env in *)
  match env with
  | [] -> (maj,min)
  | car::cdr -> if inScope v car then (maj,(countVar_scp car v min)) else countVar_env v cdr (maj + 1) 0

and findMajorAndMin v env = countVar_env v env 0 0

  in run_ala e [] [];;

(*------------------------------------------end of  4.2/start of 4.3 ---------------------------------------------*)

let rdc_rac exprs =
  let rev_exprs = List.rev exprs in
  let last = List.hd rev_exprs in
  let rest_rev = List.tl rev_exprs in
  let rest = List.rev rest_rev in
  (rest,last);;


let annotate_tail_calls e = 
  let rec run e in_tail =
match e with
| Const' _ | Var' _ -> e
| Box' _ | BoxGet' _ | BoxSet' _ -> raise X_this_should_not_happen
| If'(test, dit, dif) -> (If' (run test false, run dit in_tail, run dif in_tail))
| Seq' expers -> let (all_but_last, last) = (rdc_rac expers) in Seq' ((List.map (fun e -> run e false) all_but_last) @ [(run last in_tail)])
| Set' (Var' v, exp) -> Set' (Var' v, run exp false)
| Def' (Var' v, exp) -> Def' (Var' v, run exp false)
| Or' expers -> let  (all_but_last, last) = (rdc_rac expers) in Or' ((List.map (fun e -> run e false) all_but_last) @ [(run last in_tail)])
| LambdaSimple' (pars, exp) -> LambdaSimple' (pars, run exp true)
| LambdaOpt' (pars, opt,exp) -> LambdaOpt' (pars, opt,run exp true)
| Applic' (proc, args) -> if in_tail then ApplicTP' ((run proc false), (List.map (fun e -> run e false ) args) )
else Applic' ( (run proc false), (List.map (fun e -> run e false ) args) )
| _ -> raise X_no_match
   in run e false;;

(*------------------------------------------end of  4.3/start of 4.4 --------------------------------------------------*)

(* box any procedure parameter that meets the needed criteria *)
(* val box_set : expr' -> expr' *)
let box_set e = 
  let rec run_box_set e = 
    match e with 
    | BoxSet' (var, exp) -> BoxSet' (var, run_box_set exp)
    | If'(test, dit, dif)-> If'(run_box_set test, run_box_set dit, run_box_set dif) 
    | Seq' expLst -> Seq' (List.map (fun exp -> run_box_set exp) expLst)
    | Set' (Var' v, exp) -> Set' (Var' v, run_box_set exp)
    | Def' (Var' v, exp) -> Def' (Var' v, run_box_set exp)
    | Or' expLst -> Or' (List.map (fun exp -> run_box_set exp) expLst)
    | LambdaSimple' (params, expr)  -> LambdaSimple'(params, rmvSeq (run_box_set (run_box_helper params expr [])))
    | LambdaOpt' (params, optPar, expr) -> LambdaOpt'(params,optPar, rmvSeq (run_box_set (run_box_helper (params@[optPar]) expr [])))
    | Applic' (proc, args) -> Applic' (run_box_set proc, List.map (fun arg -> run_box_set arg) args)
    | ApplicTP' (proc, args) -> ApplicTP' (run_box_set proc, List.map (fun arg -> run_box_set arg) args)
    | _ -> e
	     
  and rmvSeq exp = 
  match exp with
	| Seq' [e] -> e 
	|  _ -> exp  
		  
and boxAllParams expr paramsToBox = 
  let new_expr = replaceGetAndSetForAllParams paramsToBox expr in
let setsList = List.map (fun param -> addSet param expr) paramsToBox in
Seq' (setsList@[run_box_set new_expr])
     
and replaceGetAndSetForAllParams paramsToBox expr  = 
  match paramsToBox with
| [] -> expr
| car::cdr -> let expr_new = replaceGetAndSet car expr in replaceGetAndSetForAllParams cdr expr_new 
										       
  and run_box_helper params expr paramsToBox = 
    match params with
    | [] -> (boxAllParams expr paramsToBox) 
    | car::cdr -> let boolBoxParam = shouldBeBoxed car expr in run_box_helper cdr expr (if boolBoxParam  then (paramsToBox@[car]) else paramsToBox) 
									      
  (* if need to box - box. else return expr as is. *)
  and shouldBeBoxed param expr = let (con1,con2,con3) = scan param expr (false,false,false) in
				 (con1 && con2 && con3)

  (* returns true if need to box. else returns false. *)
  (*begin Printf.printf "$$FALSE$$"; true; end*)
  and scan param e (con1,con2,con3) = 
    match e with
    | Set' (Var' (VarBound' (par, _, _)), exp) -> if par = param then scan param exp (true,true,con3) else scan param exp (con1,con2,con3)(*bound and set*) 
    | Set' (Var' (VarParam' (par, _)), exp) -> if par = param then scan param exp (con1,true,con3) else scan param exp (con1,con2,con3)(* set*)
    | Var' (VarBound' (par, _, _)) -> if par = param then (true,con2,true) else (con1,con2,con3) (*bound and get*)
    | Var' (VarParam' (par, _)) -> if par = param then (con1,con2,true) else (con1,con2,con3)  (*get*)
    | BoxSet' (_,exp) -> scan param exp (con1,con2,con3)
    | If'(test, dit, dif)-> checkCons param (test::dit::dif::[]) (con1,con2,con3)
    | Seq' seqLst -> checkCons param seqLst (con1,con2,con3)
    | Set' (Var' v, exp) -> scan param exp (con1,con2,con3)
    | Def' (Var' v, exp) -> scan param exp (con1,con2,con3)
    | Or' expLst -> checkCons param expLst (con1,con2,con3)
    | LambdaSimple' (params, exp)  ->  if List.mem param params then (false,false,false) else scan param exp (con1,con2,con3)
    | LambdaOpt' (params, optPar,exp) -> if List.mem param params || List.mem param [optPar] then (false,false,false) else scan param exp (con1,con2,con3)
    | Applic' (proc, args) -> checkCons param (proc::args) (con1,con2,con3)
    | ApplicTP' (proc, args) ->  checkCons param (proc::args) (con1,con2,con3)
    | _ -> (con1,con2,con3)
	     
  (* checks if there param apears in params of lambda*)
  (*and checkParam params param= andmap (fun par -> par = param) params*)
	     
  and checkCons param expLst (con1,con2,con3) = 
    match expLst with
    | [] -> (con1,con2,con3)
    | (car::cdr) -> let (newCon1,newCon2,newCon3) = scan param car (con1,con2,con3) in checkCons param cdr (newCon1,newCon2,newCon3)

  (*
  and box param expr = 
  let print = p "box START" (p_helper expr)in
  let print = p "box param" param in
  let expr1 = (replaceGetAndSet param expr) in 
  let expr2 = (addSet param expr1) in 
  let expr3 = orderSeqs expr2 in 
  begin p "box END" (p_helper expr3) ; expr3 ; end
  (* let expr3 = orderSeqs expr2 in 
  begin p "box END" (p_helper expr2) ; expr2 ; end*)

 *)
											       
  and  maxFromList l m = (*l for list and m for max*)
  match l with
  | [] -> m
  | car::cdr -> let new_m = Pervasives.max m car in maxFromList cdr new_m
								
and findMinorForSet param e = 
    match e with
    | Set' (Var' (VarBound' (par, _, mi)), _) -> if par = param then mi else (-1)
    | Set' (Var' (VarParam' (par, mi)), _) -> if par = param then mi else (-1)
    | Var' (VarBound' (par, _, mi)) -> if par = param then mi else (-1)
    | Var' (VarParam' (par, mi)) ->  if par = param then mi else (-1)
    | Box' (VarParam' (par, mi)) -> if par = param then mi else (-1)
    | BoxGet' bGet -> findMinorForSet param (Var' bGet)
    | BoxSet' (bSet,exp) -> maxFromList [(findMinorForSet param (Var' bSet)) ; (findMinorForSet param exp)] (-1) 
    | If'(test, dit, dif)-> maxFromList [(findMinorForSet param test) ; (findMinorForSet param dit) ; (findMinorForSet param dif)] (-1)
    | Seq' seqLst -> maxFromList (List.map (fun exp -> findMinorForSet param exp) seqLst) (-1)
    | Set' (v, exp) -> maxFromList [(findMinorForSet param v) ; (findMinorForSet param exp)] (-1)
    | Def' (v, exp) -> maxFromList [(findMinorForSet param v) ; (findMinorForSet param exp)] (-1)
    | Or' expLst -> maxFromList (List.map (fun exp -> findMinorForSet param exp) expLst) (-1)
    | LambdaSimple' (params, exp)  -> findMinorForSet param exp
    | LambdaOpt' (params, optPar,exp) -> findMinorForSet param exp
    | Applic' (proc, args) |ApplicTP' (proc, args) -> maxFromList ((findMinorForSet param proc)::(List.map (fun arg -> findMinorForSet param arg) args)) (-1)
    | _ -> -1
	      
  and addSet param expr= 
    let minor = findMinorForSet param expr in
    let varParam = (VarParam' (param, minor)) in
    let add = Set' (Var' varParam, (Box' varParam)) in
    add


  and replaceGetAndSet param expr =  
    match expr with 
    | Set' (Var' (VarBound' (par, mj, mi)), exp) -> if par = param then BoxSet' ((VarBound' (par, mj, mi)), (replaceGetAndSet param exp)) else Set' (Var' (VarBound' (par, mj, mi)), (replaceGetAndSet param exp))
    | Set' (Var' (VarParam' (par, mi)), exp) ->  if par = param then BoxSet' ((VarParam' (par, mi)), (replaceGetAndSet param exp)) else Set' (Var' (VarParam' (par, mi)), (replaceGetAndSet param exp))
    | Var' (VarBound' (par, mj, mi)) -> if par = param then BoxGet' (VarBound' (par, mj, mi)) else expr
    | Var' (VarParam' (par, mi)) -> if par = param then BoxGet' (VarParam' (par, mi)) else expr
    | BoxSet' (v,exp) -> BoxSet' (v ,(replaceGetAndSet param exp)) 
    | If'(test, dit, dif)-> If' ((replaceGetAndSet param test), (replaceGetAndSet param dit), (replaceGetAndSet param dif))
    | Seq' seqLst -> Seq' (List.map (fun exp -> (replaceGetAndSet param exp)) seqLst)
    | Def' (v, exp) -> Def' (v, (replaceGetAndSet param exp))
    | Or' expLst -> Or' (List.map (fun exp -> replaceGetAndSet param exp) expLst)
    | LambdaSimple' (params, exps)  -> if List.mem param params then LambdaSimple' (params, exps) else LambdaSimple' (params, replaceGetAndSet param exps)
    | LambdaOpt' (params, optPar,exps) -> if List.mem param params || List.mem param [optPar] then LambdaOpt' (params, optPar, exps) else LambdaOpt' (params, optPar, replaceGetAndSet param exps)
    | Applic' (proc, args) -> Applic' ((replaceGetAndSet param proc) , List.map (fun arg -> (replaceGetAndSet param arg)) args)
    | ApplicTP' (proc, args) -> ApplicTP' ((replaceGetAndSet param proc) , List.map (fun arg -> (replaceGetAndSet param arg)) args)
    | _ -> expr
	     
  in run_box_set e;;
  
(*let run_s expr = annotate_tail_calls (annotate_lexical_addresses expr);;*)
  
let run_semantics expr = 
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;

end;; (* struct Semantics *)
