type token =
  | Num of (int)
  | Id of (string)
  | EOF
  | LET
  | EQ
  | IN
  | FUN
  | PLUS
  | MUL
  | AND
  | OR
  | LPAREN
  | RPAREN
  | SEMI
  | COLONCOLON
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | REC
  | ARROW
  | MINUS
  | DIV
  | LT
  | LE
  | NE
  | LBRAC
  | RBRAC

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
