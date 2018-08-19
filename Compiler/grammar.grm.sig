signature Compiler_TOKENS =
sig
type ('a,'b) token
type svalue
val MOD:  'a * 'a -> (svalue,'a) token
val PIPE:  'a * 'a -> (svalue,'a) token
val AMP:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val RECOP:  'a * 'a -> (svalue,'a) token
val ISNULL:  'a * 'a -> (svalue,'a) token
val R2I:  'a * 'a -> (svalue,'a) token
val I2R:  'a * 'a -> (svalue,'a) token
val BANG:  'a * 'a -> (svalue,'a) token
val OTHERWISE:  'a * 'a -> (svalue,'a) token
val CASE:  'a * 'a -> (svalue,'a) token
val SWITCH:  'a * 'a -> (svalue,'a) token
val RELEASE:  'a * 'a -> (svalue,'a) token
val RESERVE:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val FUNCTION:  'a * 'a -> (svalue,'a) token
val T_STRING:  'a * 'a -> (svalue,'a) token
val T_CHAR:  'a * 'a -> (svalue,'a) token
val T_REAL:  'a * 'a -> (svalue,'a) token
val T_INTEGER:  'a * 'a -> (svalue,'a) token
val T_BOOLEAN:  'a * 'a -> (svalue,'a) token
val R_PAREN:  'a * 'a -> (svalue,'a) token
val L_PAREN:  'a * 'a -> (svalue,'a) token
val R_BRACE:  'a * 'a -> (svalue,'a) token
val L_BRACE:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val SUB_OR_NEG:  'a * 'a -> (svalue,'a) token
val ADD:  'a * 'a -> (svalue,'a) token
val R_BRACK:  'a * 'a -> (svalue,'a) token
val L_BRACK:  'a * 'a -> (svalue,'a) token
val C_REAL: (string*int*int) *  'a * 'a -> (svalue,'a) token
val C_CHAR: (string*int*int) *  'a * 'a -> (svalue,'a) token
val FALSE: (int*int) *  'a * 'a -> (svalue,'a) token
val TRUE: (int*int) *  'a * 'a -> (svalue,'a) token
val C_INTEGER: (int*int*int) *  'a * 'a -> (svalue,'a) token
val C_STRING: (string*int*int) *  'a * 'a -> (svalue,'a) token
val NULL: (int*int) *  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val TYPE:  'a * 'a -> (svalue,'a) token
val ID: (string*int*int) *  'a * 'a -> (svalue,'a) token
end
signature Compiler_LRVALS=
sig
structure Tokens : Compiler_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
