tokens SYM_EOF SYM_IDENTIFIER<string> SYM_INTEGER<int> SYM_PLUS SYM_MINUS SYM_ASTERISK SYM_DIV SYM_MOD
tokens SYM_LPARENTHESIS SYM_RPARENTHESIS SYM_LBRACE SYM_RBRACE
tokens SYM_ASSIGN SYM_SEMICOLON SYM_RETURN SYM_IF SYM_WHILE SYM_ELSE SYM_COMMA SYM_PRINT
tokens SYM_EQUALITY SYM_NOTEQ SYM_LT SYM_LEQ SYM_GT SYM_GEQ
non-terminals S INSTR INSTRS ELSE EXPR FACTOR
non-terminals LPARAMS REST_PARAMS
non-terminals IDENTIFIER INTEGER
non-terminals FUNDEF FUNDEFS
non-terminals ADD_EXPRS ADD_EXPR
non-terminals MUL_EXPRS MUL_EXPR
non-terminals CMP_EXPRS CMP_EXPR
non-terminals EQ_EXPRS EQ_EXPR
non-terminals BLOC
non-terminals FUN_OR_ASSIGN FUN_OR_VAR
non-terminals LARGS REST_ARGS
axiom S
{

  open Symbols
  open Ast
  open BatPrintf
  open BatBuffer
  open Batteries
  open Utils

  let resolve_associativity term other = List.fold_left (fun acc elt -> match elt with Node(x,y) -> Node(x,acc::y) | _ -> elt ) term other

  let funcall_or_assign id args_or_expr = match args_or_expr with
    | Node(Targs, tree_l) -> [Node(Tcall, [id ; args_or_expr])]
    | _ -> [Node(Tassign, [Node(Tassignvar,[id; args_or_expr])])]

  let funcall_or_var id args_or_expr = match args_or_expr with
    | Node(Targs, tree_l) -> Node(Tcall, [id ; args_or_expr])
    | _ -> id
}

rules
IDENTIFIER -> SYM_IDENTIFIER { StringLeaf($1) }

INTEGER -> SYM_INTEGER { IntLeaf($1) }

S -> FUNDEFS SYM_EOF {  Node (Tlistglobdef, $1) }

FUNDEFS -> FUNDEF FUNDEFS {  Node(Tfundef,$1)::$2  }
FUNDEFS -> {  []  }

FUNDEF -> IDENTIFIER SYM_LPARENTHESIS LPARAMS SYM_RPARENTHESIS INSTR {  [$1; Node(Tfunargs,$3)]@$5  }

LPARAMS -> IDENTIFIER REST_PARAMS {  Node(Targ,[$1])::$2  }
LPARAMS ->{ [] }

REST_PARAMS -> SYM_COMMA IDENTIFIER REST_PARAMS {  Node(Targ,[$2]) :: $3  }
REST_PARAMS -> { [] }

LARGS -> EXPR REST_ARGS { $1:: $2}
LARGS -> {[]}

REST_ARGS -> SYM_COMMA EXPR REST_ARGS{ $2::$3 }
REST_ARGS -> {[]}

INSTR -> IDENTIFIER FUN_OR_ASSIGN {funcall_or_assign $1 $2 }
INSTR -> SYM_IF SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS BLOC ELSE {  [Node(Tif,[$3] @ $5 @ $6)]  }
INSTR -> SYM_WHILE SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS INSTR {  [Node(Twhile,[$3] @ $5)]  }
INSTR -> SYM_RETURN EXPR SYM_SEMICOLON {  [Node(Treturn,[$2])]  }
INSTR -> SYM_PRINT EXPR SYM_SEMICOLON {  [Node(Tprint,[$2])]  }
INSTR -> BLOC {  $1  }

FUN_OR_ASSIGN -> SYM_ASSIGN EXPR SYM_SEMICOLON {$2}
FUN_OR_ASSIGN -> SYM_LPARENTHESIS LARGS SYM_RPARENTHESIS SYM_SEMICOLON {Node(Targs, $2)} 

EXPR -> EQ_EXPR EQ_EXPRS {  resolve_associativity $1 $2  }

EQ_EXPR -> CMP_EXPR CMP_EXPRS {  resolve_associativity $1 $2  }

CMP_EXPR-> ADD_EXPR ADD_EXPRS {  resolve_associativity $1 $2  }

ADD_EXPR -> MUL_EXPR MUL_EXPRS {  resolve_associativity $1 $2  }
ADD_EXPR -> SYM_MINUS MUL_EXPR MUL_EXPRS {  resolve_associativity (Node(Tneg,[$2])) $3  }

MUL_EXPR -> FACTOR {  $1  }
FACTOR -> INTEGER {  Node(Tint,[$1])  }
FACTOR -> IDENTIFIER FUN_OR_VAR { funcall_or_var $1 $2 }
FACTOR -> SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS {  $2  }

FUN_OR_VAR -> SYM_LPARENTHESIS LARGS SYM_RPARENTHESIS {  Node(Targs,$2)  }
FUN_OR_VAR -> { NullLeaf }

MUL_EXPRS -> SYM_ASTERISK MUL_EXPR MUL_EXPRS {  Node(Tmul,[$2])::$3  }
MUL_EXPRS -> SYM_DIV MUL_EXPR MUL_EXPRS {  Node(Tdiv,[$2])::$3  }
MUL_EXPRS -> SYM_MOD MUL_EXPR MUL_EXPRS {  Node(Tmod,[$2])::$3  }
MUL_EXPRS -> {  []  }

ADD_EXPRS -> SYM_PLUS ADD_EXPR ADD_EXPRS {  Node(Tadd,[$2])::$3  }
ADD_EXPRS -> SYM_MINUS ADD_EXPR ADD_EXPRS {  Node(Tsub,[$2])::$3  }
ADD_EXPRS -> {  []  }

EQ_EXPRS -> SYM_EQUALITY EQ_EXPR EQ_EXPRS {  Node(Tceq,[$2])::$3  }
EQ_EXPRS -> SYM_NOTEQ EQ_EXPR EQ_EXPRS {  Node(Tne,[$2])::$3  }
EQ_EXPRS -> {  []  }

CMP_EXPRS -> SYM_GEQ CMP_EXPR CMP_EXPRS {  Node(Tcge,[$2])::$3  }
CMP_EXPRS -> SYM_LEQ CMP_EXPR CMP_EXPRS {  Node(Tcle,[$2])::$3  }
CMP_EXPRS -> SYM_GT CMP_EXPR CMP_EXPRS {  Node(Tcgt,[$2])::$3  }
CMP_EXPRS -> SYM_LT CMP_EXPR CMP_EXPRS {  Node(Tclt,[$2])::$3  }
CMP_EXPRS -> {  []  }

ELSE -> SYM_ELSE BLOC {  $2  }
ELSE -> {  [Node(Tblock,[])]  }

BLOC -> SYM_LBRACE INSTRS SYM_RBRACE {  [Node(Tblock,$2)]  }

INSTRS -> INSTR INSTRS {  $1 @ $2  } 
INSTRS -> {  []  }
