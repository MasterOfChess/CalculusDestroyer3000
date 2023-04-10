%{
open Ast
%}

%token <int> INT
%token <string> ID
%token FACT
%token POWER
%token TIMES
%token DIV  
%token PLUS
%token MINUS
%token LN
%token EXP
%token SIN
%token COS
%token TAN
%token ARCSIN
%token ARCCOS
%token ARCTAN
%token LPAREN
%token RPAREN
%token DRVT
%token OF
%token WITH
%token SIMPL
%token REPR
%token INTGR
%token EXIT
%token EOF

%nonassoc INTGR
%nonassoc SIMPL
%nonassoc REPR
%nonassoc OF
%nonassoc WITH
%nonassoc DRVT
%left PLUS
%left MINUS
%left TIMES
%left DIV
%right LN
%right EXP
%right SIN
%right COS
%right TAN
%right ARCSIN
%right ARCCOS
%right ARCTAN
%right POWER
%nonassoc FACT

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| i = INT { Int i }
	| x = ID { Var x }
	| e = expr; FACT { Fact e }
	| e1 = expr; POWER; e2 = expr { Binop (Pow, e1, e2) }
	| LN; e = expr; { Fun ("ln", e) }
	| EXP; e = expr; { Fun ("exp", e) }
	| SIN; e = expr; { Fun ("sin", e) }
	| COS; e = expr; { Fun ("cos", e) }
	| TAN; e = expr; { Fun ("tan", e) }
	| ARCSIN; e = expr; { Fun ("arcsin", e) }
	| ARCCOS; e = expr; { Fun ("arccos", e) }
	| ARCTAN; e = expr; { Fun ("arctan", e) }
	| e1 = expr; DIV; e2 = expr { Binop (Div, e1, e2) }
	| e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
	| e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) }
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| MINUS; e = expr; { Neg e }
	| DRVT; WITH; e1 = expr; OF; e2 = expr; { Com (Derive(e1, e2)) }
	| INTGR; WITH; e1 = expr; OF; e2 = expr; { Com (Integrate(e1, e2)) }
	| SIMPL; e = expr; { Com (Simpl e) }
	| REPR; e = expr; { Com (Repr e) }
	| EXIT; { Com(Ext) }
	| LPAREN; e = expr; RPAREN {e} 
	;
	
