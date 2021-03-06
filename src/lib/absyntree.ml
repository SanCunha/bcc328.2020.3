(* Convert abstract syntax trees to generic trees of list of string *)

open Absyn

(* Helper functions *)

let name = Symbol.name
let map  = List.map


(* Format arguments according to a format string resulting in a string *)
let sprintf = Format.sprintf

(* Concatenate the lines of text in the node *)
let flat_nodes tree =
  Tree.map (String.concat "\n") tree

(* Build a singleton tree from a string *)
let mktr s = Tree.mkt [s]

(* Convert a symbol to a general tree *)
let tree_of_symbol s = mktr (Symbol.name s) []

(* Convert an option to a general tree *)
let tree_of_option conversion_function opt =
  match opt with
  | None   -> Tree.empty
  | Some v -> conversion_function v

(* Convert a binary operator to a string *)
let stringfy_op op =
  match op with
  | Plus          -> "+"
  | Minus         -> "-"
  | Times         -> "*"
  | Div           -> "/"
  | Mod           -> "%"
  | Power         -> "^"
  | Equal         -> "="
  | NotEqual      -> "<>"
  | GreaterThan   -> ">"
  | GreaterEqual  -> ">="
  | LowerThan     -> "<"
  | LowerEqual    -> "<="
  | And           -> "&&"
  | Or            -> "||"

(* Convert an ast with an optional annotation to a generic tree *)
let tree_of_ann_opt conversion_function annotation_to_string (ast, ty_opt) =
  let tree = conversion_function ast in
  match !ty_opt with
  | None -> tree
  | Some ty ->
     let s = annotation_to_string ty in
     match tree with
     | Tree.Empty -> Tree.Node ([""; s], [])
     | Tree.Node (x, children) ->
        Tree.Node (List.append x [s], children)

let tree_of_typed conversion_function x =
  tree_of_ann_opt conversion_function Type.show_ty x

(* Convert an expression to a generic tree *)
let rec tree_of_exp exp =
  tree_of_typed tree_of_exp_basic exp

and tree_of_exp_basic exp =
  match exp with
  | BoolExp x                 -> mktr (sprintf "BoolExp %b" x) []
  | IntExp x                  -> mktr (sprintf "IntExp %i" x) []
  | StringExp x               -> mktr (sprintf "StringExp %s" x) []
  | RealExp x                 -> mktr (sprintf "RealExp %f" x) []
  | NegativeExp e             -> mktr "NegativeExp" [tree_of_lexp e]
  | BinaryExp (l, op, r)      -> mktr (sprintf "BinaryOp %s" (stringfy_op op)) [tree_of_lexp l; tree_of_lexp r]
  | IfExp (t,b,c)             -> mktr "IfExp" [tree_of_lexp t; tree_of_lexp b; tree_of_option tree_of_lexp c]
  | WhileExp (t, b)           -> mktr "WhileExp" [tree_of_lexp t; tree_of_lexp b]
  | BreakExp                  -> mktr "BreakExp" []
  | SeqExp seq                -> mktr "SeqExp" (List.map tree_of_lexp seq)
  | CallExp (f, xs)           -> mktr "CallExp" [mktr (name f) []; mktr "Args" (List.map tree_of_lexp xs)]
  | VarExp x                  -> mktr "VarExp" [tree_of_lvar x]
  | LetExp (d, e)             -> mktr "LetExp" [mktr "Decs" (List.map tree_of_ldec d); tree_of_lexp e]
  | AssignExp (x, e)          -> mktr "AssignExp" [tree_of_lvar x; tree_of_lexp e]
  | IftExp (x, y, z)          -> mktr "IftExp" [tree_of_lexp x; tree_of_lexp y; tree_of_lexp z]

and tree_of_var var =
  match var with
  | SimpleVar x -> mktr (sprintf "SimpleVar %s" (Symbol.name x)) []

  
and tree_of_dec dec =
  match dec with
  | VarDec vardec -> tree_of_typed tree_of_vardec vardec
  | FunDecGroup decs -> mktr "FunDecGroup" (map tree_of_lfundec decs)

and tree_of_vardec (v, t, i) =
  mktr "VarDec" [ tree_of_symbol v;
                  tree_of_option tree_of_symbol t;
                  tree_of_lexp i ]

and tree_of_fundec (f, p, r, b, sig_ref) =
  Tree.mkt
    ("FunDec" :: Option.to_list (Option.map string_of_signature !sig_ref))
    [ tree_of_symbol f;
      mktr "Parameters" (map tree_of_lparameter p);
      tree_of_lsymbol r;
      tree_of_lexp b ]

and string_of_signature (tparams, tresult) =
  String.concat "->" (List.map Type.show_ty (List.append tparams [tresult]))

and tree_of_parameter (pname, ptype) =
  mktr (sprintf "%s:%s" (name pname) (name ptype)) []

(* Convert an anotated expression to a generic tree *)
and tree_of_lexp (_, x) = tree_of_exp x

and tree_of_lvar (_, x) = tree_of_var x

and tree_of_ldec (_, x) = tree_of_dec x

and tree_of_lfundec (_, x) = tree_of_fundec x

and tree_of_lparameter (_, x) = tree_of_parameter x

and tree_of_lsymbol (_, x) = tree_of_symbol x
