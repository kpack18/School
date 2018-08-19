functor CompilerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Compiler_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)open Quad;
val name1 = ref "";
val type1 = ref "";
val type1_list = ref "";
val extra1 = ref "";
val scope = ref 0;
val new_scope = ref 0;
val old_scope = ref 0;
val pblockstr = ref "";
val dblockstr = ref "";
val dblockstr_rec = ref "";
fun printf() =
  let
    val str = !name1 ^ " : " ^ Int.toString(!scope) ^ " : " ^ !type1 ^ " : " ^ !extra1 ^ "\n"
  in (TextIO.output(TextIO.stdOut, str)) end

val print_grm_error : string * int * int -> unit =
  fn (msg, line, col) => (
  print ("LINE "^Int.toString(line)^":"^Int.toString(col)^
         " ** ERROR: "^msg^"\n");
         errorMsgs.push_Entry("** ERROR:"^Int.toString(line)^":"^Int.toString(col)^
         ": "^msg^"\n", line));

val stack = ref [0];
fun push() = stack := (!scope)::(!stack);
fun pop() = stack := tl (!stack);

fun newscope() = (new_scope:=(!new_scope)+1)
fun incscope() = (scope:=(!new_scope) + 1;push())
fun decscope() = ()
   (*(scope:= hd (!stack);pop())
   handle Empty => print "Error: decscope()\n"*)
fun oldscope() = if length(!stack) <= 2 then 0 else List.nth(!stack,1)

fun print_stack(s) = case s of
  (h::t) => (print((Int.toString h) ^ "\n");print_stack(t))
  | z => ()

fun add_Table_Entry(lin,col) = (symbol_Table.push_Entry(!name1,!scope,!type1,!extra1);
                         create_Scope.add_table_value(!name1,lin,col,!scope))

                         (* NOTE I CHANGED %left PIPE AMP TO BE ABOVE EQ BELOW *)


(*#line 55.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\009\000\000\000\
\\001\000\001\000\013\000\000\000\
\\001\000\001\000\027\000\000\000\
\\001\000\001\000\027\000\005\000\078\000\006\000\077\000\007\000\076\000\
\\008\000\075\000\009\000\074\000\010\000\073\000\011\000\072\000\
\\015\000\071\000\022\000\070\000\042\000\069\000\043\000\068\000\
\\044\000\067\000\000\000\
\\001\000\001\000\027\000\020\000\007\000\021\000\046\000\032\000\026\000\
\\033\000\025\000\034\000\024\000\037\000\023\000\038\000\022\000\
\\039\000\021\000\000\000\
\\001\000\001\000\027\000\020\000\007\000\032\000\026\000\033\000\025\000\
\\034\000\024\000\037\000\023\000\038\000\022\000\039\000\021\000\000\000\
\\001\000\001\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\
\\027\000\032\000\028\000\031\000\000\000\
\\001\000\001\000\060\000\000\000\
\\001\000\001\000\085\000\000\000\
\\001\000\001\000\139\000\000\000\
\\001\000\001\000\142\000\000\000\
\\001\000\002\000\008\000\020\000\007\000\029\000\006\000\000\000\
\\001\000\003\000\000\000\000\000\
\\001\000\004\000\044\000\000\000\
\\001\000\004\000\053\000\013\000\052\000\000\000\
\\001\000\004\000\059\000\022\000\042\000\046\000\041\000\000\000\
\\001\000\004\000\113\000\000\000\
\\001\000\004\000\149\000\014\000\102\000\015\000\101\000\016\000\100\000\
\\017\000\099\000\045\000\098\000\047\000\097\000\048\000\096\000\
\\049\000\095\000\050\000\094\000\051\000\093\000\000\000\
\\001\000\005\000\078\000\006\000\077\000\007\000\076\000\008\000\075\000\
\\009\000\074\000\010\000\073\000\011\000\072\000\000\000\
\\001\000\007\000\057\000\012\000\012\000\022\000\056\000\000\000\
\\001\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\023\000\110\000\045\000\098\000\047\000\097\000\048\000\096\000\
\\049\000\095\000\050\000\094\000\051\000\093\000\000\000\
\\001\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\023\000\111\000\045\000\098\000\047\000\097\000\048\000\096\000\
\\049\000\095\000\050\000\094\000\051\000\093\000\000\000\
\\001\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\023\000\112\000\045\000\098\000\047\000\097\000\048\000\096\000\
\\049\000\095\000\050\000\094\000\051\000\093\000\000\000\
\\001\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\023\000\132\000\045\000\098\000\047\000\097\000\048\000\096\000\
\\049\000\095\000\050\000\094\000\051\000\093\000\000\000\
\\001\000\018\000\043\000\022\000\042\000\046\000\041\000\000\000\
\\001\000\019\000\014\000\000\000\
\\001\000\019\000\037\000\000\000\
\\001\000\019\000\051\000\000\000\
\\001\000\019\000\120\000\000\000\
\\001\000\019\000\152\000\000\000\
\\001\000\019\000\153\000\000\000\
\\001\000\020\000\007\000\000\000\
\\001\000\022\000\047\000\000\000\
\\001\000\022\000\048\000\000\000\
\\001\000\022\000\049\000\000\000\
\\001\000\022\000\050\000\000\000\
\\001\000\022\000\151\000\000\000\
\\001\000\023\000\104\000\000\000\
\\001\000\023\000\119\000\000\000\
\\001\000\023\000\160\000\000\000\
\\001\000\023\000\161\000\000\000\
\\001\000\030\000\087\000\000\000\
\\001\000\030\000\092\000\000\000\
\\001\000\035\000\136\000\000\000\
\\001\000\036\000\154\000\000\000\
\\001\000\040\000\135\000\000\000\
\\001\000\040\000\135\000\041\000\146\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\019\000\144\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\012\000\012\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\001\000\027\000\005\000\078\000\006\000\077\000\007\000\076\000\
\\008\000\075\000\009\000\074\000\010\000\073\000\011\000\072\000\
\\015\000\071\000\022\000\070\000\042\000\069\000\043\000\068\000\
\\044\000\067\000\000\000\
\\180\000\031\000\103\000\000\000\
\\181\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\045\000\098\000\047\000\097\000\048\000\096\000\049\000\095\000\
\\050\000\094\000\051\000\093\000\000\000\
\\182\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\045\000\098\000\047\000\097\000\048\000\096\000\049\000\095\000\
\\050\000\094\000\051\000\093\000\000\000\
\\183\000\001\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\
\\027\000\032\000\028\000\031\000\000\000\
\\184\000\031\000\118\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\031\000\114\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\018\000\116\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\045\000\098\000\047\000\097\000\048\000\096\000\049\000\095\000\
\\050\000\094\000\051\000\093\000\000\000\
\\205\000\000\000\
\\206\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\000\000\
\\211\000\000\000\
\\212\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\022\000\042\000\046\000\041\000\000\000\
\\216\000\000\000\
\\217\000\000\000\
\\218\000\000\000\
\\219\000\000\000\
\\220\000\016\000\100\000\017\000\099\000\045\000\098\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\000\000\
\\224\000\000\000\
\\225\000\000\000\
\\226\000\000\000\
\\227\000\000\000\
\\228\000\000\000\
\\229\000\000\000\
\\230\000\000\000\
\\231\000\000\000\
\\232\000\000\000\
\\233\000\000\000\
\\234\000\000\000\
\\235\000\016\000\100\000\017\000\099\000\045\000\098\000\000\000\
\\236\000\016\000\100\000\017\000\099\000\045\000\098\000\000\000\
\\237\000\045\000\098\000\000\000\
\\238\000\045\000\098\000\000\000\
\\239\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\045\000\098\000\047\000\097\000\048\000\096\000\051\000\093\000\000\000\
\\240\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\045\000\098\000\047\000\097\000\048\000\096\000\051\000\093\000\000\000\
\\241\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\045\000\098\000\000\000\
\\242\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\045\000\098\000\051\000\093\000\000\000\
\\243\000\014\000\102\000\015\000\101\000\016\000\100\000\017\000\099\000\
\\045\000\098\000\048\000\096\000\051\000\093\000\000\000\
\"
val actionRowNumbers =
"\048\000\011\000\049\000\047\000\
\\000\000\058\000\001\000\025\000\
\\005\000\059\000\006\000\026\000\
\\006\000\002\000\024\000\013\000\
\\078\000\004\000\080\000\032\000\
\\117\000\116\000\033\000\034\000\
\\035\000\113\000\027\000\072\000\
\\014\000\094\000\093\000\095\000\
\\091\000\092\000\096\000\019\000\
\\031\000\015\000\114\000\007\000\
\\062\000\003\000\081\000\079\000\
\\056\000\003\000\003\000\003\000\
\\005\000\008\000\057\000\006\000\
\\041\000\050\000\066\000\042\000\
\\053\000\086\000\115\000\099\000\
\\098\000\097\000\065\000\063\000\
\\037\000\003\000\003\000\003\000\
\\003\000\003\000\108\000\107\000\
\\111\000\110\000\106\000\109\000\
\\112\000\087\000\020\000\021\000\
\\022\000\016\000\073\000\076\000\
\\071\000\006\000\069\000\067\000\
\\038\000\028\000\006\000\003\000\
\\003\000\003\000\003\000\003\000\
\\104\000\003\000\003\000\003\000\
\\003\000\003\000\061\000\102\000\
\\101\000\100\000\023\000\103\000\
\\045\000\043\000\031\000\003\000\
\\009\000\075\000\018\000\052\000\
\\006\000\060\000\010\000\054\000\
\\124\000\122\000\123\000\125\000\
\\126\000\121\000\120\000\119\000\
\\118\000\064\000\105\000\046\000\
\\088\000\018\000\031\000\082\000\
\\017\000\076\000\077\000\068\000\
\\070\000\051\000\036\000\089\000\
\\029\000\030\000\044\000\005\000\
\\074\000\018\000\031\000\031\000\
\\031\000\039\000\040\000\085\000\
\\090\000\083\000\031\000\055\000\
\\084\000\012\000"
val gotoT =
"\
\\001\000\161\000\002\000\001\000\000\000\
\\003\000\003\000\004\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\009\000\006\000\008\000\000\000\
\\000\000\
\\000\000\
\\003\000\018\000\022\000\017\000\023\000\016\000\024\000\015\000\
\\028\000\014\000\029\000\013\000\000\000\
\\000\000\
\\007\000\028\000\008\000\027\000\013\000\026\000\000\000\
\\000\000\
\\013\000\036\000\000\000\
\\028\000\037\000\000\000\
\\009\000\038\000\000\000\
\\000\000\
\\000\000\
\\003\000\018\000\023\000\043\000\024\000\015\000\028\000\014\000\
\\029\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\053\000\017\000\052\000\000\000\
\\003\000\056\000\000\000\
\\009\000\038\000\000\000\
\\000\000\
\\000\000\
\\010\000\064\000\011\000\063\000\012\000\062\000\027\000\061\000\
\\028\000\060\000\032\000\059\000\000\000\
\\012\000\077\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\078\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\079\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\080\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\003\000\018\000\023\000\081\000\024\000\015\000\028\000\014\000\
\\029\000\013\000\000\000\
\\015\000\082\000\000\000\
\\000\000\
\\008\000\084\000\013\000\026\000\000\000\
\\000\000\
\\000\000\
\\013\000\089\000\018\000\088\000\019\000\087\000\020\000\086\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\038\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\103\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\104\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\105\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\106\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\107\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\113\000\000\000\
\\000\000\
\\013\000\115\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\119\000\000\000\
\\012\000\120\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\121\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\122\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\123\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\124\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\000\000\
\\012\000\125\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\126\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\127\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\128\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\012\000\129\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\025\000\132\000\026\000\131\000\000\000\
\\000\000\
\\003\000\135\000\000\000\
\\012\000\136\000\027\000\061\000\028\000\060\000\032\000\059\000\000\000\
\\000\000\
\\000\000\
\\027\000\138\000\000\000\
\\000\000\
\\013\000\089\000\020\000\139\000\000\000\
\\000\000\
\\000\000\
\\021\000\141\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\025\000\143\000\000\000\
\\000\000\
\\027\000\145\000\000\000\
\\003\000\146\000\000\000\
\\000\000\
\\000\000\
\\016\000\148\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\018\000\023\000\153\000\024\000\015\000\028\000\014\000\
\\029\000\013\000\000\000\
\\000\000\
\\027\000\154\000\000\000\
\\003\000\155\000\000\000\
\\003\000\156\000\000\000\
\\003\000\157\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\160\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 162
val numrules = 80
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit | C_REAL of unit ->  (string*int*int) | C_CHAR of unit ->  (string*int*int) | FALSE of unit ->  (int*int) | TRUE of unit ->  (int*int) | C_INTEGER of unit ->  (int*int*int) | C_STRING of unit ->  (string*int*int) | NULL of unit ->  (int*int) | ID of unit ->  (string*int*int) | program of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = string
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 1) => true | (T 28) => true | (T 31) => true | (T 32) => true | (T 33) => true | (T 34) => true | (T 35) => true | (T 36) => true | (T 37) => true | (T 38) => true | (T 39) => true | (T 7) => true | (T 8) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 2) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "TYPE"
  | (T 2) => "EOF"
  | (T 3) => "SEMI"
  | (T 4) => "NULL"
  | (T 5) => "C_STRING"
  | (T 6) => "C_INTEGER"
  | (T 7) => "TRUE"
  | (T 8) => "FALSE"
  | (T 9) => "C_CHAR"
  | (T 10) => "C_REAL"
  | (T 11) => "L_BRACK"
  | (T 12) => "R_BRACK"
  | (T 13) => "ADD"
  | (T 14) => "SUB_OR_NEG"
  | (T 15) => "MUL"
  | (T 16) => "DIV"
  | (T 17) => "ASSIGN"
  | (T 18) => "COLON"
  | (T 19) => "L_BRACE"
  | (T 20) => "R_BRACE"
  | (T 21) => "L_PAREN"
  | (T 22) => "R_PAREN"
  | (T 23) => "T_BOOLEAN"
  | (T 24) => "T_INTEGER"
  | (T 25) => "T_REAL"
  | (T 26) => "T_CHAR"
  | (T 27) => "T_STRING"
  | (T 28) => "FUNCTION"
  | (T 29) => "ARROW"
  | (T 30) => "COMMA"
  | (T 31) => "FOR"
  | (T 32) => "WHILE"
  | (T 33) => "IF"
  | (T 34) => "THEN"
  | (T 35) => "ELSE"
  | (T 36) => "RESERVE"
  | (T 37) => "RELEASE"
  | (T 38) => "SWITCH"
  | (T 39) => "CASE"
  | (T 40) => "OTHERWISE"
  | (T 41) => "BANG"
  | (T 42) => "I2R"
  | (T 43) => "R2I"
  | (T 44) => "ISNULL"
  | (T 45) => "RECOP"
  | (T 46) => "EQ"
  | (T 47) => "LT"
  | (T 48) => "AMP"
  | (T 49) => "PIPE"
  | (T 50) => "MOD"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 50) $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID sblock1, _, sblock1right)) :: ( _, ( MlyValue.ntVOID def_list1, def_list1left, _)) :: rest671)) => let val  result = MlyValue.program (fn _ => let val  def_list1 = def_list1 ()
 val  sblock1 = sblock1 ()
 in ((*#line 116.24 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)addMainLabel();
        parse_Tree.add_Node("program","");""(*#line 581.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, def_list1left, sblock1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 120.8 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([("e",""),("def_list","")])(*#line 589.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID def1, _, def1right)) :: ( _, ( MlyValue.ntVOID def_list1, def_list1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (def_list as def_list1) = def_list1 ()
 val  def1 = def1 ()
 in ((*#line 121.21 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node("def_list","")(*#line 593.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 1, ( result, def_list1left, def1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ntVOID dblock1, _, dblock1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  dblock1 = dblock1 ()
 in ((*#line 123.29 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)extra1:="rtype";name1:=(#1(ID));
                            type1:=(!dblockstr_rec);
                            add_Table_Entry(#2ID,#3ID);
                            scope_Table.add_scopeID(!name1,#2ID,#3ID,!scope);
                            parse_Tree.add_Leaf_List([("TYPE","",~1,~1),("ID",#1(ID),#2ID,#3ID),(":","",~1,~1),("def","",~1,~1)])(*#line 600.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 3, ( result, TYPE1left, dblock1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ntVOID optionalVal1, _, optionalVal1right)) :: ( _, ( MlyValue.ntVOID t_type1, _, _)) :: _ :: ( _, ( MlyValue.C_INTEGER C_INTEGER1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (C_INTEGER as C_INTEGER1) = C_INTEGER1 ()
 val  t_type1 = t_type1 ()
 val  optionalVal1 = optionalVal1 ()
 in ((*#line 128.57 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)name1:=(#1(ID));
                                                        type1:=(Int.toString(#1(C_INTEGER)) ^ " -> "^(!type1));
                                                        extra1:="atype";
                                                        add_Table_Entry(#2ID,#3ID);
                                                        scope_Table.add_scopeID(!name1,#2ID,#3ID,!scope);
                                                        parse_Tree.add_Leaf_List([("TYPE","",~1,~1),("ID",#1(ID),#2ID,#2ID),(":","",~1,~1),("C_INTEGER",Int.toString(#1C_INTEGER),#2C_INTEGER,#3C_INTEGER),("->","",~1,~1),("def","",~1,~1)])(*#line 611.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 3, ( result, TYPE1left, optionalVal1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID t_type1, _, t_type1right)) :: _ :: ( _, ( MlyValue.ntVOID pblock1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  pblock1 = pblock1 ()
 val  t_type1 = t_type1 ()
 in ((*#line 134.42 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)extra1:="ftype";
                                         name1:=(#1(ID));
                                         type1:=(!pblockstr^" -> "^(!type1));
                                         add_Table_Entry(#2ID,#3ID);
                                         scope_Table.add_scopeID(!name1,#2ID,#3ID,!scope);
                                         parse_Tree.add_Leaf_List([("TYPE","",~1,~1),("ID",#1(ID),#2ID,#3ID),(":","",~1,~1),("->","",~1,~1),("def","",~1,~1)])(*#line 625.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 3, ( result, TYPE1left, t_type1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID sblock1, _, sblock1right)) :: ( _, ( MlyValue.ntVOID t_type1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  t_type1 = t_type1 ()
 val  sblock1 = sblock1 ()
 in ((*#line 140.40 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Id(#1ID,[]));mkTreeFunc();
                                  extra1:="function"; name1:=(#1(ID));
                                   add_Table_Entry(#2ID,#3ID);
                                   scope_Table.add_scopeID(!name1,#2ID,#3ID,!scope);
                                   parse_Tree.add_Leaf_List([("FUNCTION","",~1,~1),("ID",#1(ID),#2ID,#3ID),(":","",~1,~1),("def","",~1,~1)])(*#line 638.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 3, ( result, FUNCTION1left, sblock1right), rest671)
end
|  ( 7, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 147.8 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List[("e",""),("optionalVal","")](*#line 650.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 20, ( result, defaultPos, defaultPos), rest671)
end
|  ( 8, ( ( _, ( _, _, R_PAREN1right)) :: ( _, ( MlyValue.ntVOID constant1, _, _)) :: _ :: ( _, ( _, COLON1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  constant1 = constant1 ()
 in ((*#line 148.39 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([(":",""),("(",""),(")",""),("optionalVal","")])(*#line 654.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 20, ( result, COLON1left, R_PAREN1right), rest671)
end
|  ( 9, ( ( _, ( _, _, R_BRACE1right)) :: ( _, ( MlyValue.ntVOID stmt_list1, _, _)) :: ( _, ( MlyValue.ntVOID dblock_empty1, _, _)) :: ( _, ( _, L_BRACE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  dblock_empty1 = dblock_empty1 ()
 val  stmt_list1 = stmt_list1 ()
 in ((*#line 151.47 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeSblock();
      parse_Tree.add_Node_List([("{",""),("}",""),("sblock","")])(*#line 660.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 2, ( result, L_BRACE1left, R_BRACE1right), rest671)
end
|  ( 10, ( ( _, ( _, _, R_BRACK1right)) :: ( _, ( MlyValue.ntVOID dec_list1, _, _)) :: ( _, ( _, L_BRACK1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  dec_list1 = dec_list1 ()
 in ((*#line 154.33 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([("[",""),("]",""),("dblock","")]);newscope();dblockstr:="["^(!dblockstr)^"]";dblockstr_rec:="["^(!dblockstr_rec)^"]"(*#line 668.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 4, ( result, L_BRACK1left, R_BRACK1right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 156.8 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeDBlockEmpty();
        (parse_Tree.add_Node_List([("e",""),("dblock_empty","")]);newscope())(*#line 674.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID dblock1, dblock1left, dblock1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  dblock1 = dblock1 ()
 in ((*#line 158.15 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node("dblock_empty","")(*#line 679.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 5, ( result, dblock1left, dblock1right), rest671)
end
|  ( 13, ( ( _, ( _, _, R_PAREN1right)) :: ( _, ( MlyValue.ntVOID param_list1, _, _)) :: ( _, ( _, L_PAREN1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  param_list1 = param_list1 ()
 in ((*#line 160.35 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)newscope();pblockstr:="("^(!pblockstr)^")";
                                  parse_Tree.add_Node_List([("(",""),(")",""),("pblock","")])(*#line 685.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 16, ( result, L_PAREN1left, R_PAREN1right), rest671)
end
|  ( 14, ( ( _, ( _, _, R_PAREN1right)) :: ( _, ( MlyValue.ntVOID arg_list1, _, _)) :: ( _, ( _, L_PAREN1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  arg_list1 = arg_list1 ()
 in ((*#line 163.33 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)
        parse_Tree.add_Node_List([("(",""),(")",""),("ablock","")])(*#line 692.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 8, ( result, L_PAREN1left, R_PAREN1right), rest671)
end
|  ( 15, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 167.8 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeAssArgEmpty();
        parse_Tree.add_Node_List([("e",""),("arg_list","")])(*#line 699.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID arg_list_nonempty1, arg_list_nonempty1left, arg_list_nonempty1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  arg_list_nonempty1 = arg_list_nonempty1 ()
 in ((*#line 169.26 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node("arg_list","")(*#line 704.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 9, ( result, arg_list_nonempty1left, arg_list_nonempty1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ntVOID arg_list_nonempty1, arg_list_nonempty1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (arg_list_nonempty as arg_list_nonempty1) = arg_list_nonempty1 ()
 val  exp1 = exp1 ()
 in ((*#line 171.36 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)addTreeAssArg();
        parse_Tree.add_Node_List([(",",""),("arg_list_nonempty","")])(*#line 710.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 10, ( result, arg_list_nonempty1left, exp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 in ((*#line 173.12 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeAssArg();
        parse_Tree.add_Node("arg_list_nonempty","")(*#line 718.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 10, ( result, exp1left, exp1right), rest671)
end
|  ( 19, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 177.8 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([("e",""),("param_list","")])(*#line 725.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ntVOID param_list_nonempty1, param_list_nonempty1left, param_list_nonempty1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  param_list_nonempty1 = param_list_nonempty1 ()
 in ((*#line 178.28 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node("param_list","")(*#line 729.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 17, ( result, param_list_nonempty1left, param_list_nonempty1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ntVOID param_dec1, _, param_dec1right)) :: _ :: ( _, ( MlyValue.ntVOID param_list_nonempty1, param_list_nonempty1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (param_list_nonempty as param_list_nonempty1) = param_list_nonempty1 ()
 val  param_dec1 = param_dec1 ()
 in ((*#line 181.44 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)pblockstr:=(!pblockstr)^", "^(!type1);
                                        parse_Tree.add_Node_List([(",",""),("param_list_nonempty","")])(*#line 735.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 18, ( result, param_list_nonempty1left, param_dec1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID param_dec1, param_dec1left, param_dec1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  param_dec1 = param_dec1 ()
 in ((*#line 183.18 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)pblockstr:=(!type1);parse_Tree.add_Node("param_list_nonempty","")(*#line 743.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 18, ( result, param_dec1left, param_dec1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( MlyValue.ntVOID t_type1, t_type1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  t_type1 = t_type1 ()
 val  (ID as ID1) = ID1 ()
 in ((*#line 186.24 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)incscope();name1:=(#1(ID));extra1:="parameter";
                       add_Table_Entry(#2ID,#3ID);
                       parse_Tree.add_Leaf_List([(":","",~1,~1),("ID",#1(ID),#2ID,#3ID),("param_dec","",~1,~1)]);
                       decscope()(*#line 749.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 19, ( result, t_type1left, ID1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ntVOID dec1, _, dec1right)) :: _ :: ( _, ( MlyValue.ntVOID dec_list1, dec_list1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (dec_list as dec_list1) = dec_list1 ()
 val  dec1 = dec1 ()
 in ((*#line 192.26 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([(";",""),("dec_list","")]);dblockstr:=(!dblockstr)^", "^(!type1);dblockstr_rec:=(!dblockstr_rec)^", "^(!type1_list)(*#line 759.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 6, ( result, dec_list1left, dec1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ntVOID dec1, dec1left, dec1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  dec1 = dec1 ()
 in ((*#line 193.12 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node("dec_list","");dblockstr:=(!type1);dblockstr_rec:=(!type1_list)(*#line 766.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 6, ( result, dec1left, dec1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID id_list1, _, id_list1right)) :: _ :: ( _, ( MlyValue.ntVOID t_type1, t_type1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  t_type1 = t_type1 ()
 val  id_list1 = id_list1 ()
 in ((*#line 196.29 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([(":",""),("dec","")])(*#line 772.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 7, ( result, t_type1left, id_list1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ntVOID assignOp1, _, assignOp1right)) :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID id_list1, id_list1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (id_list as id_list1) = id_list1 ()
 val  (ID as ID1) = ID1 ()
 val  assignOp1 = assignOp1 ()
 in ((*#line 199.34 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Id(#1ID,[]));mkTreeStmtAssign2();incscope();
                                parse_Tree.add_Leaf_List([(",","",~1,~1),("ID",#1(ID),#2ID,#3ID),("id_list","",~1,~1)]);name1:=(#1(ID));extra1:="local";
                                add_Table_Entry(#2ID,#3ID);type1_list := (!type1_list)^", "^(!type1)(*#line 779.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 14, ( result, id_list1left, assignOp1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ntVOID assignOp1, _, assignOp1right)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  assignOp1 = assignOp1 ()
 in ((*#line 202.20 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Id(#1ID,[])); mkTreeStmtAssign2();incscope();
                  parse_Tree.add_Leaf_List([("ID",#1(ID),#2ID,#3ID),("id_list","",~1,~1)]);name1:=(#1(ID));extra1:="local";
                  add_Table_Entry(#2ID,#3ID);type1_list := (!type1)(*#line 789.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 14, ( result, ID1left, assignOp1right), rest671)
end
|  ( 29, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 206.8 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(EmptyExp(1));
       parse_Tree.add_Node_List([("e",""),("assignOp","")])(*#line 798.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ntVOID constant1, _, constant1right)) :: ( _, ( _, ASSIGN1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  constant1 = constant1 ()
 in ((*#line 208.24 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([(":=",""),("assignOp","")])(*#line 803.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 15, ( result, ASSIGN1left, constant1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.ntVOID stmt1, stmt1left, stmt1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  stmt1 = stmt1 ()
 in ((*#line 211.13 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeStmtlist();
        parse_Tree.add_Node("stmt_list","")(*#line 809.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 21, ( result, stmt1left, stmt1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ntVOID stmt1, _, stmt1right)) :: ( _, ( MlyValue.ntVOID stmt_list1, stmt_list1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (stmt_list as stmt_list1) = stmt_list1 ()
 val  stmt1 = stmt1 ()
 in ((*#line 213.23 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)addTreeStmtlist();
        parse_Tree.add_Node("stmt_list","")(*#line 816.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 21, ( result, stmt_list1left, stmt1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ntVOID sblock1, sblock1left, sblock1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  sblock1 = sblock1 ()
 in ((*#line 216.15 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node("stmt","")(*#line 824.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 22, ( result, sblock1left, sblock1right), rest671)
end
|  ( 34, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.ntVOID stmt_assign1, stmt_assign1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  stmt_assign1 = stmt_assign1 ()
 in ((*#line 217.25 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([(";",""),("stmt","")])(*#line 830.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 22, ( result, stmt_assign1left, SEMI1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ntVOID sblock1, _, sblock1right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  sblock1 = sblock1 ()
 in ((*#line 218.41 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeWhile();
        parse_Tree.add_Node_List([("WHILE",""),("(",""),(")",""),("stmt","")])(*#line 836.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 22, ( result, WHILE1left, sblock1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ntVOID sblock2, _, sblock2right)) :: _ :: ( _, ( MlyValue.ntVOID sblock1, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  sblock1 = sblock1 ()
 val  sblock2 = sblock2 ()
 in ((*#line 220.55 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeIf();
        parse_Tree.add_Node_List([("IF",""),("(",""),(")",""),("THEN",""),("ELSE",""),("stmt","")])(*#line 844.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 22, ( result, IF1left, sblock2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ntVOID sblock1, _, sblock1right)) :: _ :: ( _, ( MlyValue.ntVOID stmt2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID stmt1, _, _)) :: _ :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (stmt as stmt1) = stmt1 ()
 val  exp1 = exp1 ()
 val  stmt2 = stmt2 ()
 val  sblock1 = sblock1 ()
 in ((*#line 222.59 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeFor();
        parse_Tree.add_Node_List([("FOR",""),("(",""),(";",""),(";",""),(")",""),("stmt","")])(*#line 853.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 22, ( result, FOR1left, sblock1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ntVOID sblock1, _, sblock1right)) :: _ :: _ :: ( _, ( MlyValue.ntVOID stmt_case_list1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( _, SWITCH1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  stmt_case_list1 = stmt_case_list1 ()
 val  sblock1 = sblock1 ()
 in ((*#line 224.73 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)addTreeOtherwise();
        parse_Tree.add_Node_List([("SWITCH",""),("(",""),(")",""),("OTHERWISE",""),(":",""),("stmt","")])(*#line 863.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 22, ( result, SWITCH1left, sblock1right), rest671)
end
|  ( 39, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.ntVOID assignable1, _, _)) :: ( _, ( MlyValue.ntVOID memOp1, memOp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  memOp1 = memOp1 ()
 val  assignable1 = assignable1 ()
 in ((*#line 226.30 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeMemOp();
        parse_Tree.add_Node_List([(";",""),("stmt","")])(*#line 872.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 22, ( result, memOp1left, SEMI1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ntVOID assignable1, assignable1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  assignable1 = assignable1 ()
 val  exp1 = exp1 ()
 in ((*#line 229.30 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeStmtAssign();
      parse_Tree.add_Node_List([(":=",""),("stmt_assign","")])(*#line 880.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 23, ( result, assignable1left, exp1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ntVOID stmt_case1, stmt_case1left, stmt_case1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  stmt_case1 = stmt_case1 ()
 in ((*#line 232.18 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeSwitch();
        parse_Tree.add_Node("stmt_case_list","")(*#line 888.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 25, ( result, stmt_case1left, stmt_case1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.ntVOID stmt_case1, _, stmt_case1right)) :: ( _, ( MlyValue.ntVOID stmt_case_list1, stmt_case_list1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (stmt_case_list as stmt_case_list1) = stmt_case_list1 ()
 val  stmt_case1 = stmt_case1 ()
 in ((*#line 234.33 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)addTreeSwitch();
        parse_Tree.add_Node("stmt_case_list","")(*#line 895.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 25, ( result, stmt_case_list1left, stmt_case1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.ntVOID sblock1, _, sblock1right)) :: _ :: ( _, ( MlyValue.ntVOID constant1, _, _)) :: ( _, ( _, CASE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  constant1 = constant1 ()
 val  sblock1 = sblock1 ()
 in ((*#line 237.35 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeCase();
        parse_Tree.add_Node_List([("CASE",""),(":",""),("stmt_case","")])(*#line 903.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 24, ( result, CASE1left, sblock1right), rest671)
end
|  ( 44, ( ( _, ( _, T_INTEGER1left, T_INTEGER1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 241.18 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([("T_INTEGER",""),("t_type","")]);type1 := "integer";type1_list := "integer"(*#line 911.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 12, ( result, T_INTEGER1left, T_INTEGER1right), rest671)
end
|  ( 45, ( ( _, ( _, T_BOOLEAN1left, T_BOOLEAN1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 242.18 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([("T_BOOLEAN",""),("t_type","")]);type1 := "Boolean";type1_list := "Boolean"(*#line 915.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 12, ( result, T_BOOLEAN1left, T_BOOLEAN1right), rest671)
end
|  ( 46, ( ( _, ( _, T_CHAR1left, T_CHAR1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 243.15 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([("T_CHAR",""),("t_type","")]);type1 := "char";type1_list := "char"(*#line 919.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 12, ( result, T_CHAR1left, T_CHAR1right), rest671)
end
|  ( 47, ( ( _, ( _, T_STRING1left, T_STRING1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 244.17 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([("T_STRING",""),("t_type","")]);type1 := "string";type1_list := "string"(*#line 923.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 12, ( result, T_STRING1left, T_STRING1right), rest671)
end
|  ( 48, ( ( _, ( _, T_REAL1left, T_REAL1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 245.15 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([("T_REAL",""),("t_type","")]);type1 := "real";type1_list := "real"(*#line 927.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 12, ( result, T_REAL1left, T_REAL1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 in ((*#line 246.11 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Leaf_List([("ID",#1(ID),#2ID,#3ID),("t_type","",~1,~1)]);type1 := (#1(ID));type1_list := #1 ID(*#line 931.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 12, ( result, ID1left, ID1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ntVOID constant1, constant1left, constant1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  constant1 = constant1 ()
 in ((*#line 248.17 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node("exp","")(*#line 937.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, constant1left, constant1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.ntVOID assignable1, assignable1left, assignable1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  assignable1 = assignable1 ()
 in ((*#line 249.19 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node("exp","")(*#line 943.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, assignable1left, assignable1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ntVOID binaryOp1, binaryOp1left, binaryOp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  binaryOp1 = binaryOp1 ()
 in ((*#line 250.17 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node("exp","")(*#line 949.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, binaryOp1left, binaryOp1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, BANG1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 251.17 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeUnOp(Bang("!"));
        parse_Tree.add_Node_List([("!",""),("exp","")])(*#line 955.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, BANG1left, exp1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, I2R1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 253.16 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeUnOp(I2r("(real "));
        parse_Tree.add_Node_List([("I2R",""),("exp","")])(*#line 962.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, I2R1left, exp1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, R2I1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 255.16 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeUnOp(R2i("(integer) "));
        parse_Tree.add_Node_List([("R2I",""),("exp","")])(*#line 969.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, R2I1left, exp1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, SUB_OR_NEG1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 257.23 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeUnOp(Neg("-"));
        parse_Tree.add_Node_List([("-",""),("exp","")])(*#line 976.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, SUB_OR_NEG1left, exp1right), rest671)
end
|  ( 57, ( ( _, ( _, _, ISNULL1right)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 259.19 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeIsNull();
        parse_Tree.add_Node_List([("ISNULL",""),("exp","")])(*#line 983.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, exp1left, ISNULL1right), rest671)
end
|  ( 58, ( ( _, ( _, _, R_PAREN1right)) :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, L_PAREN1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 261.28 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Node_List([("(",""),(")",""),("exp","")])(*#line 990.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, L_PAREN1left, R_PAREN1right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.C_INTEGER C_INTEGER1, C_INTEGER1left, C_INTEGER1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (C_INTEGER as C_INTEGER1) = C_INTEGER1 ()
 in ((*#line 264.18 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Const(Int.toString(#1C_INTEGER),[]));
        parse_Tree.add_Leaf_List([("C_INTEGER",Int.toString(#1(C_INTEGER)),#2C_INTEGER,#3C_INTEGER),("constant","",~1,~1)])(*#line 996.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 26, ( result, C_INTEGER1left, C_INTEGER1right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.C_CHAR C_CHAR1, C_CHAR1left, C_CHAR1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (C_CHAR as C_CHAR1) = C_CHAR1 ()
 in ((*#line 266.15 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Const(#1C_CHAR,[]));
      parse_Tree.add_Leaf_List([("C_CHAR",#1C_CHAR,#2C_CHAR,#3C_CHAR),("constant","",~1,~1)])(*#line 1003.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 26, ( result, C_CHAR1left, C_CHAR1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.C_REAL C_REAL1, C_REAL1left, C_REAL1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (C_REAL as C_REAL1) = C_REAL1 ()
 in ((*#line 268.15 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Const(#1C_REAL,[]));
      parse_Tree.add_Leaf_List([("C_REAL",#1C_REAL,#2C_REAL,#3C_REAL),("constant","",~1,~1)])(*#line 1010.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 26, ( result, C_REAL1left, C_REAL1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.C_STRING C_STRING1, C_STRING1left, C_STRING1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (C_STRING as C_STRING1) = C_STRING1 ()
 in ((*#line 270.17 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Const(#1C_STRING,[]));
      parse_Tree.add_Leaf_List([("C_STRING",#1C_STRING,#2C_STRING,#3C_STRING),("constant","",~1,~1)])(*#line 1017.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 26, ( result, C_STRING1left, C_STRING1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.TRUE TRUE1, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (TRUE as TRUE1) = TRUE1 ()
 in ((*#line 272.13 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Const("true",[]));
      parse_Tree.add_Leaf_List([("true","true",#1TRUE,#2TRUE),("constant","",~1,~1)])(*#line 1024.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 26, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.FALSE FALSE1, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (FALSE as FALSE1) = FALSE1 ()
 in ((*#line 274.14 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Const("false",[]));
      parse_Tree.add_Leaf_List([("false","false",#1FALSE,#2FALSE),("constant","",~1,~1)])(*#line 1031.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 26, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.NULL NULL1, NULL1left, NULL1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (NULL as NULL1) = NULL1 ()
 in ((*#line 276.13 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Const("null",[]));
      parse_Tree.add_Leaf_List([("null","null",#1NULL,#2NULL),("constant","",~1,~1)])(*#line 1038.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 26, ( result, NULL1left, NULL1right), rest671)
end
|  ( 66, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 in ((*#line 279.11 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(Id(#1ID,[]));
      parse_Tree.add_Leaf_List([("ID",#1(ID),#2(ID),#3(ID)),("assignable","",~1,~1)]);name1 := #1(ID)(*#line 1045.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 27, ( result, ID1left, ID1right), rest671)
end
|  ( 67, ( ( _, ( MlyValue.ntVOID ablock1, _, ablock1right)) :: ( _, ( MlyValue.ntVOID assignable1, assignable1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (assignable as assignable1) = assignable1 ()
 val  ablock1 = ablock1 ()
 in ((*#line 281.26 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)
        parse_Tree.add_Node("assignable","")(*#line 1052.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 27, ( result, assignable1left, ablock1right), rest671)
end
|  ( 68, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( MlyValue.ntVOID assignable1, assignable1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (assignable as assignable1) = assignable1 ()
 val  (ID as ID1) = ID1 ()
 in ((*#line 283.28 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)parse_Tree.add_Leaf_List([(".","",~1,~1),("ID",#1(ID),#2(ID),#3(ID)),("assignable","",~1,~1)])(*#line 1060.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 27, ( result, assignable1left, ID1right), rest671)
end
|  ( 69, ( ( _, ( _, RESERVE1left, RESERVE1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 285.16 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(ResExp("reserve"));
        parse_Tree.add_Node_List([("reserve",""),("memOp","")])(*#line 1067.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 28, ( result, RESERVE1left, RESERVE1right), rest671)
end
|  ( 70, ( ( _, ( _, RELEASE1left, RELEASE1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 287.16 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)qpush(RelExp("release"));
        parse_Tree.add_Node_List([("release",""),("memOp","")])(*#line 1072.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
))
 in ( LrTable.NT 28, ( result, RELEASE1left, RELEASE1right), rest671)
end
|  ( 71, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 290.20 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeBinOp(Plus("+"));
        parse_Tree.add_Node_List([("+",""),("binaryOp","")])(*#line 1077.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 31, ( result, exp1left, exp2right), rest671)
end
|  ( 72, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 292.27 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeBinOp(Sub("-"));
        parse_Tree.add_Node_List([("-",""),("binaryOp","")])(*#line 1085.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 31, ( result, exp1left, exp2right), rest671)
end
|  ( 73, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 294.20 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeBinOp(Mul("*"));
        parse_Tree.add_Node_List([("*",""),("binaryOp","")])(*#line 1093.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 31, ( result, exp1left, exp2right), rest671)
end
|  ( 74, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 296.20 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeBinOp(Div("/"));
        parse_Tree.add_Node_List([("/",""),("binaryOp","")])(*#line 1101.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 31, ( result, exp1left, exp2right), rest671)
end
|  ( 75, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 298.21 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeBoolOp(Pipe("|"));
        parse_Tree.add_Node_List([("|",""),("binaryOp","")])(*#line 1109.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 31, ( result, exp1left, exp2right), rest671)
end
|  ( 76, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 300.20 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeBoolOp(Amp("&"));
        parse_Tree.add_Node_List([("&",""),("binaryOp","")])(*#line 1117.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 31, ( result, exp1left, exp2right), rest671)
end
|  ( 77, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 302.20 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeBinOp(Mod("%"));
        parse_Tree.add_Node_List([("%",""),("binaryOp","")])(*#line 1125.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 31, ( result, exp1left, exp2right), rest671)
end
|  ( 78, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 304.19 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeRelOp(Lt("<"));
        parse_Tree.add_Node_List([("<",""),("binaryOp","")])(*#line 1133.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 31, ( result, exp1left, exp2right), rest671)
end
|  ( 79, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 306.19 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm"*)mkTreeRelOp(Eq("=="));
        parse_Tree.add_Node_List([("=",""),("binaryOp","")])(*#line 1141.1 "C:\Users\kylek\Desktop\Filing Cabinet\IDE\SML\443\Take 6\grammar.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 31, ( result, exp1left, exp2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Compiler_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun NULL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.NULL (fn () => i),p1,p2))
fun C_STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.C_STRING (fn () => i),p1,p2))
fun C_INTEGER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.C_INTEGER (fn () => i),p1,p2))
fun TRUE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.TRUE (fn () => i),p1,p2))
fun FALSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.FALSE (fn () => i),p1,p2))
fun C_CHAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.C_CHAR (fn () => i),p1,p2))
fun C_REAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.C_REAL (fn () => i),p1,p2))
fun L_BRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun R_BRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun SUB_OR_NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun L_BRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun R_BRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun L_PAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun R_PAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun T_BOOLEAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun T_INTEGER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun T_REAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun T_CHAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun T_STRING (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
fun RESERVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID,p1,p2))
fun RELEASE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.VOID,p1,p2))
fun SWITCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID,p1,p2))
fun CASE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID,p1,p2))
fun OTHERWISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(ParserData.MlyValue.VOID,p1,p2))
fun BANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(ParserData.MlyValue.VOID,p1,p2))
fun I2R (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(ParserData.MlyValue.VOID,p1,p2))
fun R2I (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(ParserData.MlyValue.VOID,p1,p2))
fun ISNULL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(ParserData.MlyValue.VOID,p1,p2))
fun RECOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(ParserData.MlyValue.VOID,p1,p2))
fun AMP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(ParserData.MlyValue.VOID,p1,p2))
end
end
