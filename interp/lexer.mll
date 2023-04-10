{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse
  | white { read lexbuf }
  | "/" { DIV }
  | "*" { TIMES }
  | "-" { MINUS }
  | "+" { PLUS }
  | "^" { POWER }
  | "!" { FACT }
  | "ln" { LN }
  | "exp" { EXP }
  | "sin" { SIN }
  | "cos" { COS }
  | "tan" { TAN }
  | "arcsin" { ARCSIN }
  | "arccos" { ARCCOS }
  | "arctan" { ARCTAN }
  | "derivative" { DRVT }
  | "integral" { INTGR }
  | "with" { WITH }
  | "of" { OF }
  | "simplify" { SIMPL }
  | "repr" { REPR }
  | "exit" { EXIT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }