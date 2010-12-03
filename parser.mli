type token =
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | LBRACKET
  | RBRACKET
  | EOF
  | CASE
  | CLOCK
  | CONCAT
  | ELSE
  | FOR
  | IF
  | INPUT
  | MODULE
  | NEGEDGE
  | OUTPUT
  | PARAMETER
  | POSEDGE
  | REG
  | RESET
  | RETURN
  | WIRE
  | ASSIGN
  | NOT
  | OR
  | XOR
  | AND
  | NOR
  | XNOR
  | NAND
  | EQ
  | NE
  | GT
  | GE
  | LT
  | LE
  | LSHIFT
  | RSHIFT
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | MODULUS
  | SIGEXT
  | NOELSE
  | ID of (string)
  | DLIT of (int)
  | DECLIT of (int)
  | BLIT of (string)
  | XLIT of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
