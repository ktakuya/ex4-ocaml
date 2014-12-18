%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT LOGAND LOGOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token RARROW FUN

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LET ID EQ Expr SEMISEMI { Decl ($2, $4) }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | LOGExpr { $1 }
  | FunExpr { $1 }
  | { ErrorExp ("Syntax Error") }

FunExpr :
    FUN ID RARROW Expr { FunExp($2, $4) }

LetExpr :
    LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }

LOGExpr :
    LOGExpr LOGAND LOGExpr { BinOp (LogAnd, $1, $3) }
  | LOGExpr LOGOR LOGExpr { BinOp (LogOr, $1, $3) }
  | LTExpr { $1 }

LTExpr : 
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }
  | { ErrorExp ("Unbound Error") }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }
  | {ErrorExp ("Unbound Error") }

MExpr : 
    MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | AppExpr { $1 }
  | { ErrorExp ("Unbound Error") }

AppExpr :
    AppExpr AExpr { AppExp($1, $2) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }
  | { ErrorExp ("Unbound Error") }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }
  | { ErrorExp ("Unbound Error") }
   
