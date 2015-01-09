%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT LOGAND LOGOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token RARROW FUN
%token REC

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LET ID LetExprArgumentList SEMISEMI { Decl ($2, $3) }
  | LET REC ID EQ FUN ID RARROW Expr SEMISEMI { RecDecl ($3, $6, $8) }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | LOGExpr { $1 }
  | FunExpr { $1 }
  | LetRecExpr { $1 }
  | { ErrorExp ("Syntax Error") }

LetRecExpr :
    LET REC ID EQ FUN ID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }

FunExpr :
    FUN FunExprArgumentList { $2 }

FunExprArgumentList :
    ID RARROW Expr { FunExp ($1, $3) }
  | ID FunExprArgumentList { FunExp ($1, $2) } 

LetExpr :
    LET ID LetExprArgumentList IN Expr { LetExp($2, $3, $5) }

LetExprArgumentList :

    EQ Expr { $2 }
  | ID LetExprArgumentList { FunExp($1, $2) }

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
  | InfixExpr { $1 }
  | LPAREN Expr RPAREN { $2 }
  | { ErrorExp ("Unbound Error") }

InfixExpr :
    LPAREN PLUS RPAREN { InfixExp (Plus) }
  | LPAREN MULT RPAREN { InfixExp (Mult) }
  | LPAREN LT RPAREN { InfixExp (Lt) }
  | LPAREN LOGAND RPAREN { InfixExp (LogAnd) }
  | LPAREN LOGOR RPAREN { InfixExp (LogOr) }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }
  | { ErrorExp ("Unbound Error") }
   
