open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e = match e with
|ID (x) -> lookup env x
|Not (x) -> let eval = eval_expr env x in (match eval with
                                          |Bool (x) -> Bool (not x)
                                          |_ -> raise (TypeError "Expected type bool"))
|Binop(op, expr1, expr2) -> let eval1 = eval_expr env expr1 in let eval2 = eval_expr env expr2 in 
  (match op with
  |Add -> (match eval1 with 
          |Int (x) -> (match eval2 with
                      |Int (y) -> Int (x + y)
                      |_ -> raise (TypeError "Expected type int")
                      )
          |_ -> raise (TypeError "Expected type int")
          )
  |Sub -> (match eval1 with 
          |Int (x) -> (match eval2 with
                      |Int (y) -> Int (x - y)
                      |_ -> raise (TypeError "Expected type int")
                      )
          |_ -> raise (TypeError "Expected type int")
          ) 
  |Mult -> (match eval1 with 
          |Int (x) -> (match eval2 with
                      |Int (y) -> Int (x * y)
                      |_ -> raise (TypeError "Expected type int")
                      )
          |_ -> raise (TypeError "Expected type int")
          )
  |Div -> (match eval1 with 
          |Int (x) -> (match eval2 with
                      |Int (0) -> raise (DivByZeroError)
                      |Int (y) -> Int (x / y)
                      |_ -> raise (TypeError "Expected type int")
                      )
          |_ -> raise (TypeError "Expected type int")
          )
  |Greater -> (match eval1 with 
          |Int (x) -> (match eval2 with
                      |Int (y) -> Bool (x > y)
                      |_ -> raise (TypeError "Expected type int")
                      )
          |_ -> raise (TypeError "Expected type int")
          )  
  |GreaterEqual -> (match eval1 with 
          |Int (x) -> (match eval2 with
                      |Int (y) -> Bool (x >= y)
                      |_ -> raise (TypeError "Expected type int")
                      )
          |_ -> raise (TypeError "Expected type int")
          )  
  |Less -> (match eval1 with 
          |Int (x) -> (match eval2 with
                      |Int (y) -> Bool (x < y)
                      |_ -> raise (TypeError "Expected type int")
                      )
          |_ -> raise (TypeError "Expected type int")
          ) 
  |LessEqual -> (match eval1 with 
          |Int (x) -> (match eval2 with
                      |Int (y) -> Bool (x <= y)
                      |_ -> raise (TypeError "Expected type int")
                      )
          |_ -> raise (TypeError "Expected type int")
          )
  |Concat -> (match eval1 with 
          |String (x) -> (match eval2 with
                      |String (y) -> String (x ^ y)
                      |_ -> raise (TypeError "Expected type string")
                      )
          |_ -> raise (TypeError "Expected type string")
          ) 
  |Equal -> (match eval1 with
          |Closure (env1, x1, expr1) -> raise (TypeError "Cannot compare type closure") 
          |_ -> (match eval2 with
                  |Closure (env1, x1, expr1) -> raise (TypeError "Cannot compare type closure") 
                  |_ -> if eval1 = eval2 then Bool (true) else Bool (false)
                )
          )
  |NotEqual -> (match eval1 with 
          |Closure (env1, x1, expr1) -> raise (TypeError "Cannot compare type closure") 
          |_ -> (match eval2 with
                  |Closure (env1, x1, expr1) -> raise (TypeError "Cannot compare type closure") 
                  |_ -> if eval1 <> eval2 then Bool (true) else Bool (false)
                )
          )     
  |Or -> (match eval1 with 
          |Bool (x) -> (match eval2 with
                      |Bool (y) -> Bool (x || y)
                      |_ -> raise (TypeError "Expected type bool")
                      )
          |_ -> raise (TypeError "Expected type bool")
          )
  |And -> (match eval1 with 
          |Bool (x) -> (match eval2 with
                      |Bool (y) -> Bool (x && y)
                      |_ -> raise (TypeError "Expected type bool")
                      )
          |_ -> raise (TypeError "Expected type bool")
          )
  )
|If(expr1, expr2, expr3) -> let eval1 = eval_expr env expr1 in 
  (match eval1 with
  |Bool(true) -> eval_expr env expr2
  |Bool(false) -> eval_expr env expr3
  |_ -> raise (TypeError "Expected type bool")
  ) 
|Let(x, true, expr1, expr2) -> let temp1 = extend_tmp env x in 
                               let v = (update temp1 x (eval_expr temp1 expr1)) in 
                               eval_expr temp1 expr2
|Let(x, false, expr1, expr2) -> let env1 = extend env x (eval_expr env expr1) in 
                                eval_expr env1 expr2
|Fun(x, expr) -> Closure (env, x, expr)
|App(expr1, expr2) -> let clos = eval_expr env expr1 in let eval2 = eval_expr env expr2 in
  (match clos with
  | Closure (env1, x, e) -> eval_expr (extend env1 x eval2) e
  |_ -> raise (TypeError "Expected type closure")
  )
|Record (lst) -> Record (lst)
|Select (label, expr) -> let eval1 = eval_expr env expr in 
  (match eval1 with
  |Record(lst) -> (match List.fold_left (fun (found, value) (k, v) -> if not found then if k = label then (true, v) else (false, value) else (found, value)) (false, Record([])) lst with
                  | (true, v1) -> v1
                  | (false, v2) -> raise (SelectError "label not found")
                  )
  |_ -> raise (TypeError "Expected type record")
  )
|_ -> e

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
|Def (x, expr) -> let temp = extend_tmp env x in let res = eval_expr temp expr in let y = update temp x res in 
                  (temp, Some(res))
|Expr (expr) -> (env, Some(eval_expr env expr))
|NoOp -> (env, None)
