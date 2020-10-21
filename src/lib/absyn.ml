(* absyn.ml *)

type symbol = Symbol.symbol
[@@deriving show]

type 'a loc = 'a Location.loc
[@@deriving show]

(* AST annotated with its type *)
type 'a typed = 'a * Type.ty option ref
[@@deriving show]

type exp = exp_basic typed
[@@deriving show]

and exp_basic =
  | BoolExp       of bool
  | IntExp        of int
  | StringExp     of string
  | RealExp       of float
  | NegativeExp   of lexp
  | BinaryExp     of (lexp * binary_op * lexp)
  | IfExp         of (lexp * lexp * lexp option)
  | WhileExp      of (lexp * lexp)
  | BreakExp
  | SeqExp        of lexp list
  | CallExp       of symbol * lexp list
  | VarExp        of lvar
  | LetExp        of ldec list * lexp
  | AssignExp     of lvar * lexp
  | IftExp        of (lexp * lexp * lexp)
  [@@deriving show]

and binary_op =
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | Power
  | Equal
  | NotEqual
  | GreaterThan
  | GreaterEqual
  | LowerThan
  | LowerEqual
  | And
  | Or
  [@@deriving show]

and var =
  | SimpleVar     of symbol
  [@@deriving show]

and dec =
  | VarDec of vardec typed
  | FunDecGroup of fundec loc list
  [@@deriving show]

and vardec = symbol * symbol option * lexp
  [@@deriving show]

and fundec = symbol * parameter loc list * lsymbol * lexp * (Type.ty list * Type.ty) option ref
  [@@deeriving show]

and parameter = symbol * symbol
  [@@deriving show]

and lexp = exp loc  (* exp anotated with a location *)
  [@@deriving show]

and lvar = var loc
  [@@deriving show]

and ldec = dec loc
  [@@deriving show]

and lsymbol = symbol loc
  [@@deriving show]

(* Annotate an ast with a dummy type representation *)
let dummyt ast = (ast, ref None)
