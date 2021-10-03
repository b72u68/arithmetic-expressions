(* Binary operators *)
type binop = Add | Sub | Mul | Pow ;;

(* Expressions *)
type expression =
    | Num of float
    | Var
    | Binop of binop * expression * expression
    | Neg of expression
;;

(* parsing string expressions *)
type token =
    | NumT of float
    | VarT
    | BinopT of binop
    | NegT
    | LParen
    | RParen
    | LBrace
    | RBrace
    | EOF
;;

exception ParseError of string;

let recognized_tokens = [|"x"|] ;;

let token_expressions = [|Var|] ;;

let string_to_char_list s =
    let rec helper s l i =
        if i < 0 then l
        else
            let c = String.get s i in
            helper s (c::l) (i-1)
    in
    helper s [] (String.length s - 1)
;;

let is_digit c =
    let i = Char.code c in
    i >= 48 && i <= 57
;;

(* the precedence of a binary operator. used in the parse_string and
   to_string_smart functions. *)
let binop_precedence b =
    match b with
    | Add -> 3
    | Sub -> 3
    | Mul -> 2
    | Pow -> 1
;;

let unop_precedence = 4 ;;

let prec_bound = 5 ;;

let binop_is_associative b =
    match b with
    | Add | Mul -> true
    | Sub | Pow -> false
;;

(* pretty-printing functions for expressions *)
let binop_to_string b =
    match b with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Pow -> "^"
;;

let token_to_string t =
    match t with
    | NumT n -> string_of_float n
    | VarT -> "x"
    | BinopT b -> binop_to_string b
    | NegT -> "~"
    | LParent -> "("
    | RParent -> ")"
    | LBrace -> "{"
    | RBrace -> "}"
    | EOF -> "EOF"
;;

(* only add parentheses when needed to preven ambiguity *)
let to_string_smart e =
    let rec to_string_smart' e parent_precedence parent_associative =
        match e with
        | Num n ->
                if n  >= 0.0 then string_of_float n
                else "~" ^ string_of_float (abs_float n)
        | Var -> "x"
        | Neg e1 -> "~" ^ "(" ^ to_string_smart' e1 (unop_precedence) false ^ ")"
        | Binop (b, e1, e2) ->
                let prec = binop_precedence b in
                let e_str =
                    (to_string_smart' e1 prec false ^
                    binop_to_string b ^
                    to_string_smart' e2 prec (binop_is_associative b)) in
                if prec > parent_precedence ||
                (prec = parent_precedence && not parent_associative)
                then "(" ^ e_str ^ ")"
                else e_str
    in to_string_smart' e prec_bound false
;;

(* always add parentheses around all binary ops, completely unambiguous *)
let to_string e =
    match e with
    | Num n ->
            if n >= 0 then string_of_float n
            else "~" ^ string_of_float (abs_float n)
    | Var -> "x"
    | Neg e1 -> "(~(" ^ to_string e ^ "))"
    | Binop (b, e1, e2) ->
            "(" ^ to_string e1 ^ binop_to_string b ^ to_string e2 ^ ")"
;;

(* lexing functions (producing tokens from char lists) *)

let rec match_while (p:char -> bool) (l: char list) : string * char list =
    match l with
    | [] -> ("", [])
    | c::cs ->
            if p c then
                let (s_cs, l_cs) = match_while p cs in (String.make 1 c ^ s_cs, l_cs)
            else ("", l)
;;

let lex_number_string = match_while (fun c -> is_digit c || c = '.') ;;

let rec lex_number (l: char list) : (token * char list) option =
    let (s, l') = lex_number_string l in
    try Some (NumT (float_of_string s), l')
    with Failure _ -> None ;;

let rec match_string (l: char list) (s: string) : char list option =
    if s = "" then Some l else
        match l with
        | [] -> None
        | h::t ->
                if h = String.get s 0 then
                    match_string t (String.sub s 1 (String.length s - 1))
                else None
;;

let lex_multi_char_token (l: char list) : (token * char list) option =
    let rec lex_multi_char_token' l i =
        if i >= Array.length recognized_tokens then None
        else
            match match_string l (Array.get recognized_tokens i) with
            | Some l' -> Some (Array.get token_expressions i, l')
            | None -> lex_multi_char_token' l (i+1)
    in lex_multi_char_token' l 0
;;

let rec lex' (l: char list) : token list =
    match l with
    | [] -> []
    | ' '::cs -> lex' cs
    | c::cs ->
            let (token, l) =  (
                match c with
                | '+' -> (BinopT Add, cs)
                | '-' -> (BinopT Sub, cs)
                | '*' -> (BinopT Mul, cs)
                | '^' -> (BinopT Pow, cs)
                | '(' -> (LParen, cs)
                | ')' -> (RParen, cs)
                | '{' -> (LBrace, cs)
                | '}' -> (RBrace, cs)
                | _ -> (
                    match lex_number l with
                    | Some
