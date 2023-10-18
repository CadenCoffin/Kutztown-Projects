(* CSC 310, Spring 2021 *)
(************************************************************)
(* Author: Caden Coffin *)
(* Major: Computer Science *)
(* Due Date: April 21, 2023 *)
(* Course: CSC310 020 *)
(* Professor Name: Dr. Schwesinger *)
(* Assignment: #6 *)
(* Filename: parser.ml *)                             
(********************************************************************)

open AstTypes
open Utils
open TokenTypes

(* Parsing helpers (you do not need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is
   empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match
   the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =

parse_or_expr toks

and parse_or_expr toks =
  let (t1, a1) = parse_and_expr toks in
  match lookahead t1 with
  | Tok_Or ->
      let m1 = match_token t1 Tok_Or in
      let (t2, a2) = parse_or_expr m1 in
      (t2, Or(a1, a2))
  | _ -> (t1, a1)

and parse_and_expr toks  =
  let (t1, a1) = parse_equality_expr toks in
  match lookahead t1 with
  | Tok_And ->
      let m1 = match_token t1 Tok_And in
      let (t2, a2) = parse_and_expr m1 in
      (t2, And(a1, a2))
  | _ -> (t1, a1)


and parse_equality_expr toks =
  let (t1, a1) = parse_relational_expr toks in
  match lookahead t1 with
  | Tok_Equal ->
      let m1 = match_token t1 Tok_Equal in
      let (t2, a2) = parse_equality_expr m1 in
      (t2, Equal(a1, a2))
  | Tok_NotEqual ->
      let m1 = match_token t1 Tok_NotEqual in
      let (t2, a2) = parse_equality_expr m1 in
      (t2, NotEqual(a1, a2))
  | _ -> (t1, a1)



and parse_relational_expr toks =
  let (t1, a1) = parse_additive_expr toks in
  match lookahead t1 with
  | Tok_Less ->
      let m1 = match_token t1 Tok_Less in
      let (t2, a2) = parse_relational_expr m1 in
      (t2, Less(a1, a2))
  | Tok_Greater ->
      let m1 = match_token t1 Tok_Greater in
      let (t2, a2) = parse_relational_expr m1 in
      (t2, Greater(a1, a2))
  | Tok_LessEqual ->
      let m1 = match_token t1 Tok_LessEqual in
      let (t2, a2) = parse_relational_expr m1 in
      (t2, LessEqual(a1, a2))
  | Tok_GreaterEqual ->
      let m1 = match_token t1 Tok_GreaterEqual in   
      let (t2, a2) = parse_relational_expr m1 in
      (t2, GreaterEqual(a1, a2))
  | Tok_LParen ->
      let m1 = match_token t1 Tok_LParen in
      let (t2, a2) = parse_expr m1 in
      let m2 = match_token t2 Tok_RParen in
      parse_relational_expr m2
  | _ -> (t1, a1)


and parse_additive_expr toks  =
  let (t1, a1) = parse_multiplicative_expr toks in
  match lookahead t1 with
  | Tok_Add ->
      let m1 = match_token t1 Tok_Add in
      let (t2, a2) = parse_additive_expr m1 in
      (t2, Add(a1, a2))
  | Tok_Sub ->
      let m1 = match_token t1 Tok_Sub in
      let (t2, a2) = parse_additive_expr m1 in
      (t2, Sub(a1, a2))
  | Tok_LParen ->
      let m1 = match_token t1 Tok_LParen in
      let (t2, a2) = parse_expr m1 in
      let m2 = match_token t2 Tok_RParen in
      parse_relational_expr m2
  | _ -> (t1, a1)

  and parse_multiplicative_expr toks =
  let (t1, a1) = parse_power_expr toks in
  match lookahead t1 with
  | Tok_Mult ->
      let m1 = match_token t1 Tok_Mult in
      let (t2, a2) = parse_multiplicative_expr m1 in
      (t2, Mult(a1, a2))
  | Tok_Div ->
      let m1 = match_token t1 Tok_Div in
      let (t2, a2) = parse_multiplicative_expr m1 in
      (t2, Div(a1, a2))
  | Tok_LParen ->
      let m1 = match_token t1 Tok_LParen in
      let (t2, a2) = parse_expr m1 in
      let m2 = match_token t2 Tok_RParen in
      parse_relational_expr m2
  | _ -> (t1, a1)


  and parse_power_expr toks =
  let (t1, a1) = parse_unary_expr toks in
  match lookahead t1 with
  | Tok_Pow ->
      let m1 = match_token t1 Tok_Pow in
      let (t2, a2) = parse_power_expr m1 in
      (t2, Pow(a1, a2))
  | Tok_LParen ->
      let m1 = match_token t1 Tok_LParen in
      let (t2, a2) = parse_expr m1 in
      let m2 = match_token t2 Tok_RParen in
      parse_relational_expr m2
  | _ -> (t1, a1)


and parse_unary_expr toks =
  match lookahead toks with
  | Tok_Not ->
      let t1 = match_token toks Tok_Not in
      let (t2, a2) = parse_unary_expr t1 in
      (t2, Not(a2))
  | _ -> parse_primary_expr toks
  
  and parse_primary_expr toks =
  match lookahead toks with
  | Tok_Int n -> (List.tl toks, Int n)
  | Tok_Bool b -> (List.tl toks, Bool b)
  | Tok_ID x -> (List.tl toks, ID x)
  | Tok_LParen ->
      let (t1, e) = parse_expr (List.tl toks) in
      let t2 = match_token t1 Tok_RParen in
      (t2, e)
  | _ -> raise (InvalidInputException "Invalid")

let rec parse_stmt toks : stmt_result =
  let parse_declare toks =
  let (m1, a1) = match lookahead toks with
  | Tok_Int_Type ->
      let m1 = match_token toks (Tok_Int_Type) in
      (m1, Int_Type)
  | Tok_Bool_Type ->
      let m1 = match_token toks (Tok_Bool_Type) in
      (m1, Bool_Type)
  | _ -> raise (InvalidInputException "Invalid")
  in
  let curr = match lookahead m1 with
    | Tok_ID curr -> curr
    | _ -> raise (InvalidInputException "Invalid")
  in
  let m2 = match_token m1 (Tok_ID curr) in
  let m3 = match_token m2 Tok_Semi in
  (m3, Declare(a1, curr))

 and parse_assign toks =
  match lookahead toks with
  | Tok_ID id ->
    let m1 = match_token toks (Tok_ID id) in
    let m2 = match_token m1 Tok_Assign in
    let (m3, e) = parse_expr m2 in
    let m4 = match_token m3 Tok_Semi in
    (m4, Assign(id, e))
  | _ -> raise (InvalidInputException "Invalid")

  and parse_if toks = 
    match lookahead toks with
    | Tok_If ->
      let m1 = match_token toks Tok_If in
      let m2 = match_token m1 Tok_LParen in 
      let (t2, e) = parse_expr m2 in
      let m3 = match_token t2 Tok_RParen in 
      let t3 = match_token m3 Tok_LBrace in
      let (t4, s1) = parse_stmt t3 in
      let t5 = match_token t4 Tok_RBrace in
      (match lookahead t5 with
      | Tok_Else ->
      let m2 = match_token t5 Tok_Else in
      let t6 = match_token m2 Tok_LBrace in
      let (t7, s2) = parse_stmt t6 in
      let t8 = match_token t7 Tok_RBrace in
      (t8, If(e, s1, s2))
      | _ -> (t5, If(e, s1, NoOp)))
    | _ -> raise (InvalidInputException "Invalid")

and parse_for toks =
      let m1 = match_token toks Tok_For in
      let m2 = match_token m1 Tok_LParen in
      let id = match lookahead m2 with
    | Tok_ID id -> id
    | _ -> raise (InvalidInputException "Invalid")
    in
      let m3 = match_token m2 (Tok_ID id) in
      let m4 = match_token m3 Tok_From in
      let (m5, a1) = parse_expr m4 in
      let m6 = match_token m5 Tok_To in
      let (m7, a2) = parse_expr m6 in
      let m8 = match_token m7 Tok_RParen in
      let m9 = match_token m8 Tok_LBrace in
      let (m10, a3) = parse_stmt m9 in
      let m11 = match_token m10 Tok_RBrace in
      (m11, For(id, a1, a2, a3)) in
      
  match lookahead toks with 
    | Tok_Int_Type ->
      let (t1, a1) = parse_declare toks in
      let (t2, a2) = parse_stmt t1 in
      (t2, Seq(a1, a2))
    | Tok_Bool_Type ->
      let (t1, a1) = parse_declare toks in
      let (t2, a2) = parse_stmt t1 in
      (t2, Seq(a1, a2))
    | Tok_ID(id) ->
      let (t1, a1) = parse_assign toks in
      let (t2, a2) = parse_stmt t1 in
      (t2, Seq(a1, a2))
    | Tok_Print ->
      let (t1, a1) =     
    let m1 = match_token toks Tok_Print in
    let m2 = match_token m1 Tok_LParen in
    let (m3, e) = parse_expr m2 in
    let m4 = match_token m3 Tok_RParen in
    let m5 = match_token m4 Tok_Semi in
    (m5, Print(e)) in
      let (t2, a2) = parse_stmt t1 in
      (t2, Seq(a1, a2))
    | Tok_If ->
      let (t1, a1) = parse_if toks in
      let (t2, a2) = parse_stmt t1 in
      (t2, Seq(a1, a2))
    | Tok_For ->
      let (t1, a1) = parse_for toks in
      let (t2, a2) = parse_stmt t1 in
      (t2, Seq(a1, a2))
    | Tok_While ->
      let (t1, a1) =      
      let m1 = match_token toks Tok_While in
      let (t2, e) = parse_expr m1 in
      let t3 = match_token t2 Tok_LBrace in
      let (t4, s) = parse_stmt t3 in
      let t5 = match_token t4 Tok_RBrace in
      (t5, While(e, s))
      in
      let (t2, a2) = parse_stmt t1 in
      (t2, Seq(a1, a2))
    | _ -> (toks, NoOp) 

let parse_main toks : stmt =
  let m1 = match_token toks Tok_Int_Type in
  let m2 = match_token m1 Tok_Main in
  let m3 = match_token m2 Tok_LParen in
  let m4 = match_token m3 Tok_RParen in
  let m5 = match_token m4 Tok_LBrace in
  let (m6, s) = parse_stmt m5 in
  match m6 with
  | [Tok_RBrace; EOF] -> s
  | _ -> raise (InvalidInputException "Invalid")
