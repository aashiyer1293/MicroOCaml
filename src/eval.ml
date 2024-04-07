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
let rec eval_expr env e = 
  match e with
  | ID var_name -> lookup env var_name
  | Binop (op, (e1), (e2)) ->
    let eval_e1 = eval_expr env e1 in
    let eval_e2 = eval_expr env e2 in
    (match (op, eval_e1, eval_e2) with 
    | (Add, Int i, Int j) -> Int (i + j)
    | (Sub, Int i, Int j) -> Int (i - j)
    | (Mult, Int i, Int j) -> Int (i * j)
    | (Div, Int i, Int j) -> 
         if j = 0 then raise DivByZeroError else Int (i / j) 
    | (Greater, Int i, Int j) -> Bool(i > j)
    | (Less, Int i, Int j) -> Bool(i < j)
    | (GreaterEqual, Int i, Int j) -> Bool(i >= j)
    | (LessEqual, Int i, Int j) -> Bool(i <= j)
    | (Concat, String i, String j) -> String(i^j)
    | (Equal, Int i, Int j) -> Bool(i = j)
    | (NotEqual, Int i, Int j) -> Bool(i <> j)
    | (Equal, Bool i, Bool j) -> Bool(i = j)
    | (NotEqual, Bool i, Bool j) -> Bool(i <> j)
    | (Equal, String i, String j) -> Bool(i = j)
    | (NotEqual, String i, String j) -> Bool(i <> j)
    | (Or, Bool b1, Bool b2) -> Bool (b1 || b2)
    | (And, Bool b1, Bool b2) -> Bool (b1 && b2)
    | (_, Closure _, _) | (_, _, Closure _) -> raise (TypeError "Cannot compare closures")
    | (Equal, _, _) | (NotEqual, _, _) -> raise (TypeError "Cannot compare different types")
    | (_, _, _) -> raise (TypeError "Invalid type for operator"))
  | If (exp, ex_t, ex_f) ->
    (match eval_expr env exp with 
    | Bool true -> eval_expr env ex_t
    | Bool false -> eval_expr env ex_f
    | _ -> raise (TypeError "Expression don't work")) 
  | Let (name, is_rec, exp1, exp2) ->
    (if is_rec then 
      let env_tmp = extend_tmp env name in
      let new_val = eval_expr env_tmp exp1 in 
      update env_tmp name new_val; 
      eval_expr env_tmp exp2
    else 
      let new_env = extend env name (eval_expr env exp1) in 
      eval_expr new_env exp2)
  | Not b -> 
    let not_res = eval_expr env b in 
    (match not_res with 
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "we want a bool for this one"))
  | Fun (p, b) -> Closure(env, p, b)
  | App (exp1, exp2) -> 
    let clos = eval_expr env exp1 in 
    let arg = eval_expr env exp2 in
    (match clos with 
    | Closure (clos_env, p, b) -> eval_expr (extend clos_env p arg) b 
    | _ -> raise (TypeError "exp1 should be a closure"))
  | Record fields -> 
    let f = List.map (fun (Lab label, expr) -> (Lab label, eval_expr env expr)) fields in
    Record f
  | Select (Lab label, e) -> 
    let expr = eval_expr env e in
    (match expr with 
    | Record fields -> find_label label fields
    | _ -> raise (TypeError "not a record"))
  | Bool t -> 
    (match t with 
    |true -> Bool true
    |false -> Bool false)
  | Int i -> Int i
  | String s -> String s
  | _ -> failwith "What are you doing here"
  

  and find_label label fields =
    (match fields with 
    | [] -> raise (SelectError "error, select didn't work out for you")
    | (Lab l, value) :: ys -> if l = label then value else find_label label ys)

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with 
  | Def (e1, e2) -> 
    let x = extend_tmp env e1 in 
    let y = eval_expr x e2 in
    update x e1 y;
    (x, Some y)
  | Expr e -> 
    let x = eval_expr env e in
    (env, Some x)
  | NoOp -> 
    (env, None)
