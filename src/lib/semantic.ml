(* semantic.ml *)

module A = Absyn
module S = Symbol
module T = Type

type entry = [%import: Env.entry]
type env = [%import: Env.env]

(* Obtain the location of an ast *)

let loc = Location.loc

(* Reporting errors *)

let undefined loc category id =
  Error.error loc "undefined %s %s" category (S.name id)

let misdefined loc category id =
  Error.error loc "%s is not a %s" (S.name id) category

let type_mismatch loc expected found =
  Error.error loc "type mismatch: found %s, expecting %s"
    (T.show_ty found)
    (String.concat ", " (List.map T.show_ty expected))

(* Searhing in symbol tables *)

let look env category id pos =
  match S.look id env with
  | Some x -> x
  | None -> undefined pos category id

let tylook tenv id pos =
  look tenv "type" id pos

let varlook venv id pos =
  match look venv "variable" id pos with
  | VarEntry t -> t
  | FunEntry _ -> misdefined pos "variable" id

let funlook venv id pos =
  match look venv "function" id pos with
  | VarEntry _ -> misdefined pos "function" id
  | FunEntry (params, result) -> (params, result)

(* Type compatibility *)

let compatible ty1 ty2 pos =
  if not (T.coerceable ty1 ty2) then
    type_mismatch pos [ty2] ty1

let compatible2 ty1 ty2 pos =
  if T.coerceable ty2 ty1 then ty1
  else if T.coerceable ty1 ty2 then ty2
  else Error.error pos "type mismatch: %s and %s" (T.show_ty ty1) (T.show_ty ty2)

let check_bool ty pos = compatible ty T.BOOL pos

let check_int ty pos = compatible ty T.INT pos

let check_real ty pos = compatible ty T.REAL pos

let check_string ty pos = compatible ty T.STRING pos

let check_void ty pos = compatible ty T.VOID pos

let check_numeric t u pos1 pos2 =
  if T.coerceable t T.INT then
    if T.coerceable u T.INT then
      T.INT
    else if T.coerceable u T.REAL then
      T.REAL
    else
      type_mismatch pos2 [T.INT; T.REAL] u
  else if T.coerceable t T.REAL then
    if T.coerceable u T.INT then
      T.REAL
    else if T.coerceable u T.REAL then
      T.REAL
    else
      type_mismatch pos2 [T.INT; T.REAL] u
  else
    type_mismatch pos1 [T.INT; T.REAL] t

let check_equal t u pos =
  if T.coerceable t T.INT || T.coerceable t T.REAL then
    (if not (T.coerceable u T.INT || T.coerceable u T.REAL) then
       type_mismatch pos [T.INT; T.REAL] u
    )
  else if not (T.coerceable t u) || not (T.coerceable u t) then
    Error.error pos "type mismatch: %s and %s" (T.show_ty t) (T.show_ty u);
  T.BOOL

let check_relational t u pos1 pos2 =
  if T.coerceable t T.INT || T.coerceable t T.REAL then
    (if not (T.coerceable u T.INT || T.coerceable u T.REAL) then
       type_mismatch pos2 [T.INT; T.REAL] u
    )
  else if T.coerceable t T.STRING then
    (if not (T.coerceable u T.STRING) then
       type_mismatch pos2 [T.STRING] u
    )
  else
    type_mismatch pos1 [T.INT; T.REAL; T.STRING] t;
  T.BOOL

(* Set the value in a reference of optional *)

let set reference value =
  reference := Some value;
  value

(* Checking expressions *)

let rec check_exp env (pos, (exp, tref)) =
  match exp with
  | A.BoolExp _ -> set tref T.BOOL
  | A.IntExp  _ -> set tref T.INT
  | A.RealExp _ -> set tref T.REAL
  | A.StringExp _ -> set tref T.STRING
  | A.LetExp (decs, body) -> set tref (check_let_exp env pos decs body)
  | A.VarExp v -> set tref (check_var env v)
  | A.AssignExp (var, exp) -> set tref (check_assign_exp env pos var exp)
  | A.BinaryExp (left, op, right) -> set tref (check_binary_exp env pos op left right)
  | A.NegativeExp exp -> set tref (check_neg_exp env pos exp)
  | A.CallExp (f, args) -> set tref (check_call_exp env pos f args)
  | A.SeqExp exp_list -> set tref (check_seq_exp env pos exp_list)
  | A.IfExp (test, e1, e2) -> set tref (check_if_exp env pos test e1 e2)
  | A.WhileExp (test, body) -> set tref (check_while_exp env pos test body)
  | A.BreakExp -> set tref (check_break_exp env pos)
  | A.IftExp (x, y, z) -> check_ift env pos (x, y, z) tref
  | _ -> Error.fatal "unimplemented"

and check_ift env pos (x, y, z) tref =
  let aux_type = check_exp env (pos, (x, tref)) in
  match aux_type with
  | T.BOOL -> let aux_expressao = (check_exp env y) in
              compatible aux_expressao (check_exp env z) pos;
              set tref aux_expressao
  | _ -> Error.fatal "Expected Boolean"

and check_let_exp env _pos decs body =
  let env' = List.fold_left check_dec env decs in
  check_exp env' body

and check_assign_exp env pos var exp =
  compatible (check_exp env exp) (check_var env var) pos;
  T.VOID

and check_binary_exp env pos op left right =
  let ltype = check_exp env left in
  let rtype = check_exp env right in
  match op with
  | A.Plus | A.Minus | A.Times | A.Div | A.Mod | A.Power ->
     check_numeric ltype rtype (loc left) (loc right)
  | A.Equal | A.NotEqual ->
     check_equal ltype rtype pos
  | A.GreaterThan | A.GreaterEqual | A.LowerThan | A.LowerEqual ->
     check_relational ltype rtype (loc left) (loc right)
  | A.And | A.Or ->
     check_bool ltype (loc left);
     check_bool rtype (loc right);
     T.BOOL

and check_neg_exp env pos exp =
  let t = check_exp env exp in
  if not (T.coerceable t T.INT || T.coerceable t T.REAL) then
    type_mismatch pos [T.INT; T.REAL] t;
  t

and check_call_exp env pos fname args =
  let (tparams, tresult) = funlook env.venv fname pos in
  try
    List.iter2
      (fun arg tparam ->
        ignore (compatible (check_exp env arg) tparam (loc arg)))
      args
      tparams;
    tresult
  with
  | Invalid_argument _ ->
     Error.error pos "function expected %i arguments, but was called with %i"
       (List.length tparams)
       (List.length args);

and check_seq_exp env _pos exp_list =
  let rec check_sequence = function
    | [] -> T.VOID
    | [e] -> check_exp env e
    | e::rest ->
       ignore (check_exp env e);
       check_sequence rest
  in check_sequence exp_list

and check_if_exp env pos test e1 e2opt =
  check_bool (check_exp env test) (loc test);
  let t1 = check_exp env e1 in
  match e2opt with
  | Some e2 ->
     compatible2 t1 (check_exp env e2) pos
  | None ->
     T.VOID

and check_while_exp env _pos test body =
  check_bool (check_exp env test) (loc test);
  ignore (check_exp {env with inloop = true} body);
  T.VOID

and check_break_exp env pos =
  if env.inloop then
    T.VOID
  else
    Error.error pos "break outside while loop"

(* Checking variables *)

and check_var env (pos, var) =
  match var with
  | A.SimpleVar v -> varlook env.venv v pos
  | _ -> Error.fatal "unimplemented"

(* Checking declarations *)

and check_dec_var env pos ((name, type_opt, init), tref) =
  let tinit = check_exp env init in
  let tvar =
    match type_opt with
    | Some tname ->
       let t = tylook env.tenv tname pos in
       compatible tinit t (loc init);
       t
    | None -> tinit
  in
  ignore (set tref tvar);
  let venv' = S.enter name (VarEntry tvar) env.venv in
  {env with venv = venv'}

and check_dec env (pos, dec) =
  match dec with
  | A.VarDec x -> check_dec_var env pos x
  | A.FunDecGroup ds -> check_fun_dec_group env pos ds
  | _ -> Error.fatal "unimplemented"

and check_fun_dec_group env pos ds =
  let venv' = List.fold_left (check_function_signature env.tenv) env.venv ds in
  let env' = {env with venv=venv'} in
  List.iter (check_function_body env') ds;
  env'

and check_function_signature tenv venv (pos, (fname, params, (rpos, rtype), body, sig_ref)) =
  ignore
    (List.fold_left
       (fun known_params (pos, (pname, _)) ->
         if List.mem pname known_params then
           Error.error pos "parameter name must be unique";
         pname :: known_params)
       []
       params);
  let ptypes = List.map (fun (pos,(_,ptype)) -> tylook tenv ptype pos) params in
  let presult = tylook tenv rtype rpos in
  sig_ref := Some (ptypes, presult);
  S.enter fname (FunEntry (ptypes, presult)) venv

and check_function_body env (pos, (fname, params, (rpos, rtype), body, sig_ref)) =
  match !sig_ref with
  | None -> Error.fatal "function signature should already be known"
  | Some (tparams, tresult) ->
     let venv' =
       List.fold_left2
         (fun venv (_, (pname, _)) ptype ->
           S.enter pname (VarEntry ptype) venv)
         env.venv
         params
         tparams
     in
     let tbody = check_exp {env with venv=venv'; inloop=false} body in
     compatible tbody tresult (loc body)

(* main semantic analysis function *)

let semantic program =
  check_exp Env.initial program
