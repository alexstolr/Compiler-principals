(* compiler.ml
 * A compiler from Scheme to CISC
 *
 * Programmer: Mayer Goldberg, 2015
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
  | _ -> raise X_no_match

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

let tag_parse sexpr = 
  let rec tpHelper sexpr = 
    match sexpr with 
    | (Pair ((Symbol "let"), (Pair (ribs, sexprs)))) -> (letHelper ribs sexprs)
    | (Pair ((Symbol "let*"), (Pair (ribs, sexprs)))) ->  (tpHelper (expand_let_star ribs sexprs))
    | (Pair ((Symbol "letrec"), (Pair (ribs, sexprs)))) -> (tpHelper (expand_letrec ribs sexprs))
    | (Pair ((Symbol "define"), (Pair ((Pair (def_name, def_argl), def_value))))) ->
      (tpHelper (Pair ((Symbol "define"), (Pair (def_name, (Pair ((Pair ((Symbol "lambda"), (Pair(def_argl, def_value)))), Nil)))))))
    | (Pair ((Symbol "quote"), Pair ((q, Nil)))) -> (Const q)
    | (Pair ((Symbol "quasiquote"), Pair ((q, Nil)))) -> (tpHelper (expand_qq q))
    | (Pair ((Symbol "cond"), args)) -> (cndHelper args)
    | Bool _ | Number _ | Nil | Void | Char _ | String _ | Vector _ -> Const sexpr
    | (Pair ((Symbol "and"), pr)) -> (andHelper pr) 
    | (Pair ((Symbol "if"), (Pair (pred, Pair(do_if_true, Nil))))) -> (If ((tpHelper pred), (tpHelper do_if_true), (Const Void)))
    | (Pair ((Symbol "if"), (Pair (pred, Pair(do_if_true, (Pair (do_if_false, Nil))))))) -> (If ((tpHelper pred), (tpHelper do_if_true), (tpHelper do_if_false)))
    | (Pair ((Symbol "define"), Pair (def_name, Pair (def_value, Nil)))) -> (dfnHelper def_name def_value)
    | (Pair ((Symbol "set!"),  Pair (set_name, Pair (set_value, Nil)))) -> (Set ((tpHelper set_name), (tpHelper set_value)))
    | (Pair ((Symbol "lambda"),(Pair (args, sexprs)))) -> (chooseLambda (lambdaArgHandler args) (bgnHelper sexprs)) 
    | (Pair ((Symbol "begin"), sexprs)) -> bgnHelper sexprs
    | (Pair((Symbol("or")), sexprs))-> orHelper sexprs
    | (Pair(procedure,sexprs)) -> (Applic ((tpHelper procedure), (List.map tpHelper (scheme_list_to_ocaml_list sexprs))))
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
   (Applic ((chooseLambda (lambdaArgHandler (ocaml_list_to_scheme_list params)) (bgnHelper sexprs)) 
   , (List.map tpHelper args)))

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
  | _ -> raise X_no_match

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
