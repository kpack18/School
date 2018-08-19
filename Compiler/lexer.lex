structure Tokens = Tokens
val str = ref "";
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val line = ref 1
val col =  ref 1
val oldcol = ref 1
fun incline() = (line:=(!line)+1; col:=1; oldcol:=1)
fun inccol(text) = (col:=(!oldcol);oldcol := (!oldcol) + (size text))

val eof = fn () => Tokens.EOF(!line,!col);
val error = fn (e,l : int,c:int) =>
             TextIO.output(TextIO.stdOut,"LINE " ^ (Int.toString l) ^
                          ":" ^ (Int.toString c) ^ e ^ "\n")
fun clear() = str := "";
fun append(a) = str := (!str) ^  a;

%%
%s CMNT;
%header (functor CompilerLexFun(structure Tokens: Compiler_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
integer = (\+|-)?{digit}+;
realnum =  (\+|-)?{digit}+\.{digit}+([e|E][-+]?{digit}+)?;
id = [a-zA-Z_][a-zA-Z0-9_]*;
ws = [\ \t];
escaped = \\(\"|n|t|escape|\\);
ch    = '\\?.';
str = \"(\\.|[^"\n\\])*\";

%%
<INITIAL>\n       => (incline(); continue());
<INITIAL>{ws}+    => (inccol(yytext);continue());
<INITIAL>"integer" => (inccol(yytext);Tokens.T_INTEGER(!line,!col));
<INITIAL>"real" => (inccol(yytext);Tokens.T_REAL(!line,!col));
<INITIAL>"Boolean" => (inccol(yytext);Tokens.T_BOOLEAN(!line,!col));
<INITIAL>"character" => (inccol(yytext);Tokens.T_CHAR(!line,!col));
<INITIAL>"string" => (inccol(yytext);Tokens.T_STRING(!line,!col));

<INITIAL>"(*" => (inccol(yytext);YYBEGIN CMNT; clear();append("(*"); continue());
<CMNT>{ws} => (inccol(yytext);append(yytext); continue());
<CMNT>{id} => (inccol(yytext);append(yytext); continue());
<CMNT>. => (inccol(yytext);append(yytext); continue());
<CMNT>\n => (incline();append(yytext);continue());
<CMNT>"*" => (inccol(yytext);append(yytext); continue());
<CMNT>"*)" => (inccol(yytext);YYBEGIN INITIAL; append("*)");
               continue());

<INITIAL>"["     => (inccol(yytext);Tokens.L_BRACK(!line,!col));
<INITIAL>"]"     => (inccol(yytext);Tokens.R_BRACK(!line,!col));
<INITIAL>"{"     => (inccol(yytext);Tokens.L_BRACE(!line,!col));
<INITIAL>"}"     => (inccol(yytext);Tokens.R_BRACE(!line,!col));
<INITIAL>"("     => (inccol(yytext);Tokens.L_PAREN(!line,!col));
<INITIAL>")"     => (inccol(yytext);Tokens.R_PAREN(!line,!col));
<INITIAL>","     => (inccol(yytext);Tokens.COMMA(!line,!col));
<INITIAL>";"     => (inccol(yytext);Tokens.SEMI(!line,!col));
<INITIAL>"+"     => (inccol(yytext);Tokens.ADD(!line,!col));
<INITIAL>"-"     => (inccol(yytext);Tokens.SUB_OR_NEG(!line,!col));
<INITIAL>"*"     => (inccol(yytext);Tokens.MUL(!line,!col));
<INITIAL>"/"     => (inccol(yytext);Tokens.DIV(!line,!col));
<INITIAL>":"     => (inccol(yytext);Tokens.COLON(!line,!col));
<INITIAL>":="    => (inccol(yytext);Tokens.ASSIGN(!line,!col));
<INITIAL>"="     => (inccol(yytext);Tokens.EQ(!line,!col));
<INITIAL>"<"     => (inccol(yytext);Tokens.LT(!line,!col));
<INITIAL>"&"     => (inccol(yytext);Tokens.AMP(!line,!col));
<INITIAL>"|"     => (inccol(yytext);Tokens.PIPE(!line,!col));
<INITIAL>"%"     => (inccol(yytext);Tokens.MOD(!line,!col));
<INITIAL>"->"    => (inccol(yytext);Tokens.ARROW(!line,!col));
<INITIAL>"!"     => (inccol(yytext);Tokens.BANG(!line,!col));
<INITIAL>"."     => (inccol(yytext);Tokens.RECOP(!line,!col));
<INITIAL>"i2r"   => (inccol(yytext);Tokens.I2R(!line,!col));
<INITIAL>"r2i"   => (inccol(yytext);Tokens.R2I(!line,!col));
<INITIAL>"for"   => (inccol(yytext);Tokens.FOR(!line,!col));
<INITIAL>"while" => (inccol(yytext);Tokens.WHILE(!line,!col));
<INITIAL>"if"    => (inccol(yytext);Tokens.IF(!line,!col));
<INITIAL>"then"  => (inccol(yytext);Tokens.THEN(!line,!col));
<INITIAL>"else"  => (inccol(yytext);Tokens.ELSE(!line,!col));
<INITIAL>"type"   => (inccol(yytext);Tokens.TYPE(!line,!col));
<INITIAL>"reserve" => (inccol(yytext);Tokens.RESERVE(!line,!col));
<INITIAL>"release" => (inccol(yytext);Tokens.RELEASE(!line,!col));
<INITIAL>"null" => (inccol(yytext);Tokens.NULL((!line,!col),!line,!col));
<INITIAL>"isNull" => (inccol(yytext);Tokens.ISNULL(!line,!col));
<INITIAL>"switch" => (inccol(yytext);Tokens.SWITCH(!line,!col));
<INITIAL>"case" => (inccol(yytext);Tokens.CASE(!line,!col));
<INITIAL>"otherwise" => (inccol(yytext);Tokens.OTHERWISE(!line,!col));
<INITIAL>"function" => (inccol(yytext);Tokens.FUNCTION(!line,!col));
<INITIAL>"true" => (inccol(yytext);Tokens.TRUE((!line,!col),!line,!col));
<INITIAL>"false" => (inccol(yytext);Tokens.FALSE((!line,!col),!line,!col));

<INITIAL>{integer} => (inccol(yytext);Tokens.C_INTEGER((valOf(Int.fromString yytext),!line,!col),!line,!col+1));
<INITIAL>{id} => (inccol(yytext);Tokens.ID((if hd(explode(yytext)) = #"[" then implode(tl(explode(yytext))) else yytext,!line,!col),!line,!col));
<INITIAL>{ch}     => (inccol(yytext);Tokens.C_CHAR((yytext,!line,!col),!line,!col));
<INITIAL>{realnum} => (inccol(yytext);Tokens.C_REAL((yytext,!line,!col),!line,!col));
<INITIAL>{str} => (inccol(yytext);Tokens.C_STRING((yytext,!line,!col),!line,!col));

<INITIAL>.      => (inccol(yytext);
                    error(" ** ERROR: Illegal token : "^yytext,!line,!col);
                    errorMsgs.push_Entry("** ERROR:"^Int.toString(!line)
                    ^":"^Int.toString(!col)^": Illegal token : "^yytext^"\n", !line);
                    lex());
