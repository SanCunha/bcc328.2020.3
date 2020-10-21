// parser.mly

%{
    open Absyn

    (* Annotate an ast with a location and a dummy type representation *)
    let (%) loc ast = (loc, dummyt ast)
%}

%token                 EOF
%token <bool>          LITBOOL
%token <string>        LITSTRING
%token <int>           LITINT
%token <float>         LITREAL
%token <Symbol.symbol> ID
%token                 PLUS
%token                 MINUS
%token                 TIMES
%token                 DIV
%token                 MOD
%token                 POW
%token                 EQ
%token                 NE
%token                 GT
%token                 GE
%token                 LT
%token                 LE
%token                 AND
%token                 OR
%token                 ASSIGN
%token                 IFT 
%token                 LPAREN
%token                 RPAREN
%token                 COMMA
%token                 SEMI
%token                 COLON
%token                 BREAK
%token                 DO
%token                 ELSE
%token                 END
%token                 IF
%token                 IN
%token                 LET
%token                 THEN
%token                 VAR
%token                 WHILE
%token                 UMINUS
%token                 FUNCTION

%right                 OR
%right                 AND
%left                  EQ NE GT GE LT LE
%left                  PLUS MINUS
%left                  TIMES DIV MOD
%right                 POW
%nonassoc              UMINUS

%start <Absyn.lexp> program

%%

program:
| e=exp EOF                                    {e}

exp:
| x=LITBOOL                                    {$loc % BoolExp x}
| x=LITINT                                     {$loc % IntExp x}
| x=LITREAL                                    {$loc % RealExp x}
| x=LITSTRING                                  {$loc % StringExp x}
| MINUS e=exp             %prec UMINUS         {$loc % NegativeExp e}
| l=exp PLUS r=exp                             {$loc % BinaryExp (l, Plus, r)}
| l=exp MINUS r=exp                            {$loc % BinaryExp (l, Minus, r)}
| l=exp TIMES r=exp                            {$loc % BinaryExp (l, Times, r)}
| l=exp DIV r=exp                              {$loc % BinaryExp (l, Div, r)}
| l=exp MOD r=exp                              {$loc % BinaryExp (l, Mod, r)}
| l=exp POW r=exp                              {$loc % BinaryExp (l, Power, r)}
| l=exp EQ r=exp                               {$loc % BinaryExp (l, Equal, r)}
| l=exp NE r=exp                               {$loc % BinaryExp (l, NotEqual, r)}
| l=exp GT r=exp                               {$loc % BinaryExp (l, GreaterThan, r)}
| l=exp GE r=exp                               {$loc % BinaryExp (l, GreaterEqual, r)}
| l=exp LT r=exp                               {$loc % BinaryExp (l, LowerThan, r)}
| l=exp LE r=exp                               {$loc % BinaryExp (l, LowerEqual, r)}
| l=exp AND r=exp                              {$loc % BinaryExp (l, And, r)}
| l=exp OR r=exp                               {$loc % BinaryExp (l, Or, r)}
| WHILE t=exp DO b=exp                         {$loc % WhileExp (t, b)}
| BREAK                                        {$loc % BreakExp}
| IF t=exp THEN b=exp v=option(ELSE c=exp {c}) {$loc % IfExp (t,b,v)}
| f=ID LPAREN p=exp_list RPAREN                {$loc % CallExp (f, p)}
| LPAREN es=exp_seq RPAREN                     {$loc % SeqExp es}
| x=var                                        {$loc % VarExp x}
| LET d=list(dec) IN e=exp                     {$loc % LetExp (d, e)}
| x=var ASSIGN e=exp                           {$loc % AssignExp (x, e)}
| x=exp IFT y=exp COLON z=exp                  {$loc % IftExp (x, y, z)}

(* semicolon separated sequence of expressions *)
exp_seq:
| es=separated_list(SEMI, exp)                 {es}

(* function call arguments *)
exp_list:
| opt=separated_list(COMMA, exp)               {opt}

(* variables *)
var:
| x=ID                                         {$loc, SimpleVar x}

(* declarations *)
dec:
| d=var_dec                                    {d}
| ds=nonempty_list(fun_dec)                    {$loc, FunDecGroup ds}

var_dec:
| VAR x=ID t=optional_type EQ e=exp            {$loc, VarDec (dummyt (x, t, e))}

fun_dec:
| FUNCTION f=ID LPAREN p=separated_list(COMMA, parameter) RPAREN COLON t=lid EQ e=exp  {$loc, (f, p, t, e, ref None)}

parameter:
| name=ID COLON t=ID                           {$loc, (name, t)}

optional_type:
| ot=option(COLON t=ID {t})                    {ot}

lid:
| id=ID                                        {$loc, id}
