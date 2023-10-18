(* CSC 310, Spring 2021 *)
(************************************************************)
(* Author: Caden Coffin *)
(* Major: Computer Science *)
(* Due Date: May 5, 2023 *)
(* Course: CSC310 020 *)
(* Professor Name: Dr. Schwesinger *)
(* Assignment: #7 *)
(* Filename: eval.ml *)                             
(********************************************************************)


open AstTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let extend env x y = (x, y)::env

let rec lookup env x =
  match env with
  | [] -> failwith "undefined"
  | (k,v)::t -> if x = k then v
                else lookup t x


let rec eval_expr env t =
match t with
  | Int i -> Int_Val i
  | Bool b -> Bool_Val b

| ID id ->
    let rec lookupid env id =
      match env with
      | [] -> raise (DeclareError("ID not found"))
      | (key, value)::tl -> if key = id then value else lookupid tl id
    in lookupid env id

| Add (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with 
  | Int_Val i -> (match value2 with
    | Int_Val j -> (Int_Val (i+j))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| Sub (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with 
  | Int_Val i -> (match value2 with
    | Int_Val j -> (Int_Val (i-j))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| Mult (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with 
  | Int_Val i -> (match value2 with
    | Int_Val j -> (Int_Val (i*j))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| Div (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with 
  | Int_Val i -> (match value2 with
    | Int_Val j -> 
      if j == 0 then raise (DivByZeroError) 
      else (Int_Val (i/j))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| Pow (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with 
  | Int_Val i -> (match value2 with
    | Int_Val j -> 
      let result = (float_of_int i) ** (float_of_int j) 
      in (Int_Val (int_of_float result))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| Or (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with 
  | Bool_Val b1 -> (match value2 with
    | Bool_Val b2 -> (Bool_Val (b1 || b2))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| And (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with 
  | Bool_Val b1 -> (match value2 with
    | Bool_Val b2 -> (Bool_Val (b1 && b2))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| Not expr ->
  let value1 = (eval_expr env expr) in
  (match value1 with 
  | Bool_Val b -> (Bool_Val (not b))
  | _ -> raise (TypeError("Invalid argument")))

| Greater (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with
  | Int_Val i -> (match value2 with
    | Int_Val j -> (Bool_Val (i > j))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| Less (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with
  | Int_Val i -> (match value2 with
    | Int_Val j -> (Bool_Val (i < j))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| GreaterEqual (expr1, expr2) ->
  let value1 = eval_expr env expr1 in
  let value2 = eval_expr env expr2 in
  (match value1 with
  | Int_Val i -> (match value2 with
    | Int_Val j -> (Bool_Val (i >= j))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| LessEqual (expr1, expr2) ->
  let value1 = eval_expr env expr1 in
  let value2 = eval_expr env expr2 in
  (match value1 with
  | Int_Val i -> (match value2 with
    | Int_Val j -> (Bool_Val (i <= j))
    | _ -> raise (TypeError("Invalid argument")))
  | _ -> raise (TypeError("Invalid argument")))

| Equal (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with
  | Int_Val i -> (match value2 with
    | Int_Val j -> (Bool_Val (i == j))
    | _ -> raise (TypeError("Invalid argument")))
  | Bool_Val b1 -> (match value2 with
    | Bool_Val b2 -> (Bool_Val (b1 == b2))
    | _ -> raise (TypeError("Invalid argument"))))


| NotEqual (expr1, expr2) ->
  let value1 = (eval_expr env expr1) in
  let value2 = (eval_expr env expr2) in
  (match value1 with
  | Int_Val i -> (match value2 with
    | Int_Val j -> (Bool_Val (i != j))
    | _ -> raise (TypeError("Invalid argument")))
  | Bool_Val b1 -> (match value2 with
    | Bool_Val b2 -> (Bool_Val (b1 != b2))
    | _ -> raise (TypeError("Invalid argument"))))



let rec eval_stmt env s = 
match s with
	
| NoOp -> env

| Seq (stmt1, stmt2) ->
let env1 = eval_stmt env stmt1 in
eval_stmt env1 stmt2

| Declare (t, id) ->
  if List.mem_assoc id env then
  raise (DeclareError "Variable already declared")
  else
  let value = match t with
    | Int_Type -> Int_Val 0
    | Bool_Type -> Bool_Val false
  in (id, value) :: env

| Assign (id, expr) ->
  let value = eval_expr env expr in
  let rec update_env env = match env with
  | [] -> raise (DeclareError "Variable not declared")
  | (x, _) :: env' when x = id ->
  (match (lookup env id, value) with
  | (Int_Val _, Int_Val _) -> (id, value) :: env'
    | (Bool_Val _, Bool_Val _) -> (id, value) :: env'
    | _ -> raise (TypeError "Type error"))
  | pair :: env' -> pair :: (update_env env')
  in update_env env

| If (expr, stmt1, stmt2) -> 
  let value = (eval_expr env expr) in
  (match value with
    | Bool_Val b ->
      let result_env = (if b then (eval_stmt env stmt1) else (eval_stmt env stmt2)) in
      result_env
    | Int_Val i ->
      raise (TypeError("Invalid Type")))

| While (expr,stmt) -> 
let value = (eval_expr env expr) in
		(match value with
			| Bool_Val b -> 
      (match b with
			| true -> (eval_stmt (eval_stmt env stmt) (While(expr,stmt)))
			| false -> env)
			| Int_Val i -> raise (TypeError("Invalid Type"))) 

| Print (expr) -> 
let value = (eval_expr env expr) in
		(match value with
		| Int_Val i -> 
			let () = print_output_int i in
      let () = print_output_newline () in
      env
		| Bool_Val b -> 
			let () = print_output_bool b in
      let () = print_output_newline () in
      env)
      
| For (value, starts, ends, body) ->
  let start_val = eval_expr env starts in
  let end_val = eval_expr env ends in
  let rec loop i env_acc =
    let current_val =
      match end_val with
      | Int_Val n -> Int_Val i
      | Bool_Val b -> if b then Bool_Val (i < 1) else Bool_Val (i < 0)
    in
    let env' = eval_stmt env_acc body in
    let env'' = (value, current_val) :: env' in
    if i >= match end_val with Int_Val n -> n | Bool_Val _ -> 1 then env''
    else loop (i+1) env''
  in
  match start_val with 
  | Int_Val start -> loop start ((value, start_val) :: env)
  | Bool_Val _ -> loop 0 ((value, start_val) :: env)
