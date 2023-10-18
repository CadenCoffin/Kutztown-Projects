(* CSC 310, Spring 2021 *)
(************************************************************)
(* Author: Caden Coffin *)
(* Major: Computer Science *)
(* Due Date: April 21, 2023 *)
(* Course: CSC310 020 *)
(* Professor Name: Dr. Schwesinger *)
(* Assignment: #6 *)
(* Filename: lexer.ml *)                             
(********************************************************************)

open TokenTypes

let tokenize (input : string) : token list =
  let regWhitespace = Str.regexp "[ \t\n\r]" in 
  let regId = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
  let regInt = Str.regexp "-?[0-9]+" in 
  let regBool = Str.regexp "true|false" in
  let regLparen = Str.regexp "(" in
  let regRparen = Str.regexp ")" in
  let regLbrace = Str.regexp "{" in
  let regRbrace = Str.regexp "}" in 
  let regEqual = Str.regexp "==" in 
  let regNotEqual = Str.regexp "!=" in 
  let regAssign = Str.regexp "=" in 
  let regGreater = Str.regexp ">" in 
  let regLesser = Str.regexp "<" in 
  let regGreaterEq = Str.regexp ">=" in 
  let regLesserEq = Str.regexp "<=" in 
  let regOr = Str.regexp "||" in 
  let regAnd = Str.regexp "&&" in 
  let regNot = Str.regexp "!" in
  let regSemi = Str.regexp ";" in 
  let regAdd = Str.regexp "+" in
  let regSub = Str.regexp "-" in
  let regMult = Str.regexp "*" in 
  let regDiv = Str.regexp "/" in
  let regPow = Str.regexp "\\^" in 

  
  let rec tok pos s =
    if pos >= String.length s then [EOF]

    else if Str.string_match regWhitespace s pos then
        tok (pos + 1) s

    else if Str.string_match regInt s pos then
      let token = Str.matched_string s in
      Tok_Int (int_of_string token) :: tok (pos + (String.length token)) s

    else if Str.string_match regBool s pos then 
      let token = Str.matched_string s in
      match token with
      | "true" -> Tok_Bool true :: tok (pos + (String.length token)) s
      | "false" -> Tok_Bool false :: tok (pos + (String.length token)) s
      | _ -> Tok_ID token :: tok (pos + (String.length token)) s

    else if Str.string_match regId s pos then
      let token = Str.matched_string s in
      match token with
      | "int" -> Tok_Int_Type :: tok (pos + (String.length token)) s
      | "bool" -> Tok_Bool_Type   :: tok (pos + (String.length token)) s
      | "print" -> Tok_Print :: tok (pos + (String.length token)) s
      | "main" -> Tok_Main :: tok (pos + (String.length token)) s
      | "if" -> Tok_If :: tok (pos + (String.length token)) s
      | "else" -> Tok_Else :: tok (pos + (String.length token)) s
      | "for" -> Tok_For :: tok (pos + (String.length token)) s
      | "from" -> Tok_From :: tok (pos + (String.length token)) s
      | "to" -> Tok_To :: tok (pos + (String.length token)) s
      | "while" -> Tok_While :: tok (pos + (String.length token)) s
      | _ -> Tok_ID token :: tok (pos + (String.length token)) s

    else if Str.string_match regLparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match regRparen s pos then Tok_RParen :: tok (pos + 1) s
    else if Str.string_match regMult s pos then Tok_Mult :: tok (pos + 1) s
    else if Str.string_match regAdd s pos then Tok_Add :: tok (pos + 1) s
    else if Str.string_match regEqual s pos then Tok_Equal :: tok (pos + 2) s
    else if Str.string_match regLbrace s pos then Tok_LBrace :: tok (pos + 1) s
    else if Str.string_match regRbrace s pos then Tok_RBrace :: tok (pos + 1) s
    else if Str.string_match regNotEqual s pos then Tok_NotEqual :: tok (pos + 2) s
    else if Str.string_match regAssign s pos then Tok_Assign :: tok (pos + 1) s
    else if Str.string_match regGreater s pos then Tok_Greater :: tok (pos + 1) s
    else if Str.string_match regLesser s pos then Tok_Less :: tok (pos + 1) s
    else if Str.string_match regGreaterEq s pos then Tok_GreaterEqual :: tok (pos + 2) s
    else if Str.string_match regLesserEq s pos then Tok_LessEqual :: tok (pos + 2) s
    else if Str.string_match regNot s pos then Tok_Not :: tok (pos + 1) s
    else if Str.string_match regAnd s pos then Tok_And :: tok (pos + 2) s
    else if Str.string_match regOr s pos then Tok_Or :: tok (pos + 2) s
    else if Str.string_match regSemi s pos then Tok_Semi :: tok (pos + 1) s
    else if Str.string_match regSub s pos then Tok_Sub :: tok (pos + 1) s
    else if Str.string_match regDiv s pos then Tok_Div :: tok (pos + 1) s
    else if Str.string_match regPow s pos then Tok_Pow :: tok (pos + 1) s
    else raise (InvalidInputException ("tokenize: " ^ (String.sub s pos ((String.length s)-pos) ^ "asdfasdf")))
  in
  tok 0 input
