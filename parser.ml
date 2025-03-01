open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)
let rec parse_expr toks = 
let rec parse_recbod toks = match toks with
|Tok_ID(x)::Tok_Equal::xs -> let (rest1, expr1) = parse_expr xs in
                              (match rest1 with
                              |Tok_Semi::xs -> let (rest2, expr2) = parse_recbod (match_token rest1 Tok_Semi) in
                                                (rest2, (Lab(x), expr1)::expr2)
                              |_ -> (rest1, [(Lab(x), expr1)])
                              )
|_ -> (toks, []) in
  
let rec parse_rec toks = match toks with
|Tok_LCurly::xs -> let (rest, expr) = parse_recbod xs in 
(match_token rest Tok_RCurly, Record (expr))
in

let rec parse_prim toks = match toks with
|Tok_Int(x)::xs -> (xs, Int(x))
|Tok_Bool(x)::xs -> (xs, Bool(x))
|Tok_String(x)::xs -> (xs, String(x))
|Tok_ID(x)::xs -> (xs, ID(x))
|Tok_LParen::xs -> let (rest, expr) = parse_expr xs in
                    (match_token rest Tok_RParen, expr)
|Tok_LCurly::xs -> parse_rec toks 
|_ -> raise (InvalidInputException(string_of_list string_of_token toks)) in
  
let rec parse_sel toks = let (rest1, expr1) = parse_prim toks in match rest1 with
|Tok_Dot::Tok_ID(x)::xs -> (xs, Select(Lab (x), expr1))
|_ -> (rest1, expr1) in
  
let rec parse_app toks = let (rest1, expr1) = parse_sel toks in match rest1 with
|[] -> (rest1, expr1)
|Tok_Int(x)::xs -> let (rest2, expr2) = parse_prim rest1 in
(rest2, App (expr1, expr2))
|Tok_Bool(x)::xs -> let (rest2, expr2) = parse_prim rest1 in
(rest2, App (expr1, expr2))
|Tok_String(x)::xs -> let (rest2, expr2) = parse_prim rest1 in
(rest2, App (expr1, expr2))
|Tok_ID(x)::xs -> let (rest2, expr2) = parse_prim rest1 in
(rest2, App (expr1, expr2))
|Tok_LParen::xs -> let (rest2, expr2) = parse_prim rest1 in
(rest2, App (expr1, expr2))
|Tok_LCurly::xs -> let (rest2, expr2) = parse_prim rest1 in
(rest2, App (expr1, expr2))
|_ -> (rest1, expr1) in
  
let rec parse_unary toks = match toks with
|Tok_Not::xs ->let (rest1, expr1) = parse_unary xs in
                (rest1, Not (expr1))
|_ -> parse_app toks in
  
let rec parse_concat toks = let (rest1, expr1) = parse_unary toks in match rest1 with
|Tok_Concat::xs -> let (rest2, expr2) = parse_concat xs in
                    (rest2, Binop(Concat, expr1, expr2))
|_ -> (rest1, expr1) in
  
let rec parse_mult toks = let (rest1, expr1) = parse_concat toks in match rest1 with
|Tok_Mult::xs -> let (rest2, expr2) = parse_mult xs in 
              (rest2, Binop(Mult, expr1, expr2))
|Tok_Div::xs-> let (rest2, expr2) = parse_mult xs in 
              (rest2, Binop(Div, expr1, expr2))
|_ -> (rest1, expr1) in
  
let rec parse_add toks = let (rest1, expr1) = parse_mult toks in match rest1 with
|Tok_Add::xs -> let (rest2, expr2) = parse_add xs in 
              (rest2, Binop(Add, expr1, expr2))
|Tok_Sub::xs-> let (rest2, expr2) = parse_add xs in 
              (rest2, Binop(Sub, expr1, expr2))
|_ -> (rest1, expr1) in
  
let rec parse_rel toks = let (rest1, expr1) = parse_add toks in match rest1 with
|Tok_Greater::xs -> let (rest2, expr2) = parse_rel xs in 
                    (rest2, Binop(Greater, expr1, expr2))
|Tok_GreaterEqual::xs -> let (rest2, expr2) = parse_rel xs in 
                    (rest2, Binop(GreaterEqual, expr1, expr2))
|Tok_Less::xs-> let (rest2, expr2) = parse_rel xs in 
              (rest2, Binop(Less, expr1, expr2))
|Tok_LessEqual::xs-> let (rest2, expr2) = parse_rel xs in 
              (rest2, Binop(LessEqual, expr1, expr2))
|_ -> (rest1, expr1) in
  
let rec parse_eq toks = let (rest1, expr1) = parse_rel toks in match rest1 with
|Tok_Equal::xs -> let (rest2, expr2) = parse_eq xs in 
              (rest2, Binop(Equal, expr1, expr2))
|Tok_NotEqual::xs-> let (rest2, expr2) = parse_eq xs in 
              (rest2, Binop(NotEqual, expr1, expr2))
|_ -> (rest1, expr1) in
  
let rec parse_and toks = let (rest1, expr1) = parse_eq toks in match rest1 with
|Tok_And::xs -> let (rest2, expr2) = parse_and xs in
                (rest2, Binop(And, expr1, expr2))
|_ -> (rest1, expr1) in

let rec parse_or toks = let (rest1, expr1) = parse_and toks in match rest1 with
|Tok_Or::xs -> let (rest2, expr2) = parse_or xs in (rest2, Binop (Or, expr1, expr2))
|_ -> (rest1, expr1) in
  
match toks with
|Tok_Let::Tok_Rec::Tok_ID(x)::Tok_Equal::xs -> let (rest1, expr1) = parse_expr xs in 
                                    let (rest2, expr2) = parse_expr (match_token rest1 Tok_In) in
                                    (rest2, Let (x, true, expr1, expr2))
|Tok_Let::Tok_ID(x)::Tok_Equal::xs -> let (rest1, expr1) = parse_expr xs in 
                            let (rest2, expr2) = parse_expr (match_token rest1 Tok_In) in
                            (rest2, Let (x, false, expr1, expr2))
|Tok_Fun::Tok_ID(x)::Tok_Arrow::xs -> let (rest, expr) = parse_expr xs in
                                      (rest, Fun (x, expr))
|Tok_If::xs -> let (rest1, expr1) = parse_expr xs in
                let (rest2, expr2) = parse_expr (match_token rest1 Tok_Then) in
                let (rest3, expr3) = parse_expr (match_token rest2 Tok_Else) in
                (rest3, If (expr1, expr2, expr3))
|_ -> parse_or toks

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = match toks with
|Tok_Def::Tok_ID(x)::Tok_Equal::xs -> let (rest, expr) = parse_expr xs in (match rest with
                                      |Tok_DoubleSemi::[] -> ([], Def (x, expr)
                                      ))
|Tok_DoubleSemi::xs -> ([], NoOp) 
|_ -> let (rest, expr) = parse_expr toks in (match rest with
|Tok_DoubleSemi::[] -> ([], Expr (expr)
))
