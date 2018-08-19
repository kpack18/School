structure parse_Tree = struct

  datatype node = Node of (node ref * string * string * node ref list ref)
                 | Leaf of (node ref * string * string * int * int)
                 | None

  val token_list: node list ref = ref []

  fun add_Node(token: string,value: string) = token_list := rev(Node(ref None,token,value,ref [])::rev(!token_list))

  fun add_Node_List(list: (string * string) list) = case list of
        ((k,v)::t) => (token_list := rev(Node(ref None,k,v,ref [])::rev(!token_list));add_Node_List(t))
        | t => ();

  fun add_Leaf(token: string,value: string,line: int,col: int) = token_list := rev(Leaf(ref None,token,value,line,col)::rev(!token_list))

  fun add_Leaf_List(list: (string * string * int * int) list) = case list of
        ((k,v,l,c)::t) => if l = ~1 andalso c = ~1 then (token_list := rev(Node(ref None,k,v,ref [])::rev(!token_list));add_Leaf_List(t))
                          else (token_list := rev(Leaf(ref None,k,v,l,c)::rev(!token_list));add_Leaf_List(t))
        | t => ()

  val tree: node ref = ref None

  fun get_Parent(input_node) = case input_node of
     Node(a,b,c,d) => a
     |Leaf(a,b,c,d,e) => a
     | z => ref None

  fun get_token(input_node) = case input_node of
     Node(a,b,c,d) => b
     |Leaf(a,b,c,d,e) => b
     | z => "~1"

  fun get_value(input_node) = case input_node of
     Node(a,b,c,d) => c
     |Leaf(a,b,c,d,e) => c
     | z => "null"

  fun get_Leaf_Pos(input_node) = case input_node of
    Leaf(a,b,c,d,e) => (d,e)
    | z => (~1,~1)

  fun get_Child(input_node) = case input_node of
     Node(a,b,c,d) => d
     | z => ref []

  fun createTree() =
    let
       val stack: node list ref = ref []
       fun node_To_String(node_list) = case node_list of
         (h::t) => get_token(h)::node_To_String(t)
         |z => []
       fun Child(parent) =
          let
           fun identify(child_list: string list list) = case child_list of
            (h::t) => if length(h) > length(!stack) then identify(t) else if h = List.take(node_To_String(!stack),length(h)) then h else identify(t)
            |z => []
           fun child_List(parent) = case parent of
            "program" => [["sblock","def_list"]]
            |"def_list" => [["e"],["def","def_list"]]
            |"def" => [[":","ID","TYPE","dblock"],
                       ["->","C_INTEGER",":","ID","TYPE","optionalVal","t_type"],
                       ["->",":","ID","TYPE","t_type","pblock"],
                       [":","ID","FUNCTION","sblock","t_type"],["COMMENT"]]
            |"optionalVal" => [["e"],[")","(",":","constant"]]
            |"sblock" => [["}","{","stmt_list","dblock_empty"]]
            |"dblock" => [["]","[","dec_list"]]
            |"dblock_empty" => [["e"],["dblock"]]
            |"pblock" => [[")","(","param_list"]]
            |"ablock" => [[")","(","arg_list"]]
            |"arg_list" => [["e"],["arg_list_nonempty"]]
            |"arg_list_nonempty" => [[",","exp","arg_list_nonempty"],["exp"]]
            |"param_list" => [["e"],["param_list_nonempty"]]
            |"param_list_nonempty" => [[",","param_dec","param_list_nonempty"],["param_dec"]]
            |"param_dec" => [["ID",":","t_type"]]
            |"dec_list" => [[";","dec","dec_list"],["dec"]]
            |"dec" => [[":","id_list","t_type"]]
            |"id_list" => [["ID",",","assignOp","id_list"],["ID","assignOp"]]
            |"assignOp" => [["e"],[":=","constant"]]
            |"stmt_list" => [["stmt","stmt_list"],["stmt"]]
            |"stmt" => [["sblock"],[";","stmt_assign"],
                        [")","(","WHILE","sblock","exp"],
                        ["ELSE","THEN",")","(","IF","sblock","sblock","exp"],
                        [")",";",";","(","FOR","sblock","stmt","exp","stmt"],
                        [":","OTHERWISE",")","(","SWITCH","sblock","stmt_case_list","exp"],
                        [";","assignable","memOp"]]
            |"stmt_assign" => [[":=","exp","assignable"]]
            |"stmt_case_list" => [["stmt_case","stmt_case_list"],["stmt_case"]]
            |"stmt_case" => [[":","CASE","sblock","constant"]]
            |"t_type" => [["T_INTEGER"],["T_BOOLEAN"],["T_CHAR"],
                          ["T_STRING"],["T_REAL"],["ID"]]
            |"exp" => [["constant"],["assignable"],["binaryOp"],["!","exp"],
                       ["I2R","exp"],["R2I","exp"],["-","exp"],["isNull","exp"],[")","(","exp"]]
            |"constant" => [["C_INTEGER"],["C_CHAR"],["C_REAL"],["C_STRING"],
                            ["true"],["false"],["null"]]
            |"assignable" => [["ID",".","assignable"],["ID"],["ablock","assignable"]]
            |"memOp" => [["reserve"],["release"]]
            |"binaryOp" => [["+","exp","exp"],["-","exp","exp"],["*","exp","exp"],
                            ["/","exp","exp"],["|","exp","exp"],["&","exp","exp"],
                            ["%","exp","exp"],["<","exp","exp"],["=","exp","exp"]]
            | z => [[]]
          in
            identify(child_List(parent))
          end

       fun set_Children(parent) =
          let
             val c_type = Child(get_token(parent))
             val c_length = length(c_type)

            fun ref_list(c_type) = case c_type of
  (*program*) ("sblock"::"def_list"::t) => (case !stack of (h::i::t) => (ref i)::(ref h)::[] | z => [])
 (*def_list*) |("e"::t) => (ref (hd(!stack)))::[]
              |("def"::"def_list"::t) => (case !stack of (h::i::t) => (ref i)::(ref h)::[] | z => [])
      (*def*) |(":"::"ID"::"TYPE"::"dblock"::t) => (case !stack of (h::i::j::k::t) => (ref j)::(ref i)::(ref h)::(ref k)::[] | z => [])
              |("->"::"C_INTEGER"::":"::"ID"::"TYPE"::"optionalVal"::"t_type"::t) => (case !stack of (h::i::j::k::l::m::n::t) =>
                                                                                     (ref l)::(ref k)::(ref j)::(ref i)::(ref h)::(ref n)::(ref m)::[] | z=> [])
              |("->"::":"::"ID"::"TYPE"::"t_type"::"pblock"::t) => (case !stack of (h::i::j::k::l::m::t) => (ref k)::(ref j)::(ref i)::(ref m)::(ref h)::(ref l)::[] | z => [])
              |(":"::"ID"::"FUNCTION"::"sblock"::"t_type"::t) => (case !stack of (h::i::j::k::l::t) => (ref j)::(ref i)::(ref h)::(ref l)::(ref k)::[] | z => [])
              |("COMMENT"::t) => (ref (hd(!stack )))::[]
(*option-Val*)|(")"::"("::":"::"constant"::t) => (case !stack of (h::i::j::k::t) => (ref j)::(ref i)::(ref k)::(ref h)::[] | z => [])
    (*sblock*)|("}"::"{"::"stmt_list"::"dblock_empty"::t) => (case !stack of (h::i::j::k::t) => (ref i)::(ref k)::(ref j)::(ref h)::[] | z => [])
    (*dblock*)|("]"::"["::"dec_list"::t) => (case !stack of (h::i::j::t) => (ref i)::(ref j)::(ref h)::[] | z => [])
(*dblock_emp*)|("dblock"::t) => (ref (hd(!stack)))::[]
(*pblock*)    |(")"::"("::"param_list"::t) => (case !stack of (h::i::j::t) => (ref i)::(ref j)::(ref h)::[] | z => [])
(*ablock*)    |(")"::"("::"arg_list"::t) => (case !stack of (h::i::j::t) => (ref i)::(ref j)::(ref h)::[] | z => [])
(*arg_list*)  |("arg_list_nonempty"::t) => (ref (hd(!stack)))::[]
(*argl_noemp*)|(","::"exp"::"arg_list_nonempty"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              |("exp"::t) => (ref (hd(!stack)))::[]
(*param_list*)|("param_list_nonempty"::t) => (ref (hd(!stack)))::[]
(*para_nonem*)|(","::"param_dec"::"param_list_nonempty"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              |("param_dec"::t) => (ref (hd(!stack)))::[]
(*param_dec*) |("ID"::":"::"t_type"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref i)::(ref h)::[] | z => [])
(*dec_list*)  |(";"::"dec"::"dec_list"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              |("dec"::t) => (ref (hd(!stack)))::[]
    (*dec*)   |(":"::"id_list"::"t_type"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
(*id_list*)   |("ID"::","::"assignOp"::"id_list"::t) => (case !stack of (h::i::j::k::t) => (ref k)::(ref i)::(ref h)::(ref j)::[] | z => [])
              |("ID"::"assignOp"::t) => (case !stack of (h::i::t) => (ref h)::(ref i)::[] | z => [])
(*assignOp*)  |(":="::"constant"::t) => (case !stack of (h::i::t) => (ref h)::(ref i)::[] | z => [])
(*stmt_list*) |("stmt"::"stmt_list"::t) => (case !stack of (h::i::t) => (ref i)::(ref h)::[] | z => [])
              |("stmt"::t) => (ref (hd(!stack)))::[]
(*stmt*)      |("sblock"::t) => (ref (hd(!stack)))::[]
              |(";"::"stmt_assign"::t) => (case !stack of (h::i::t) => (ref i)::(ref h)::[] | z => [])
              |(")"::"("::"WHILE"::"sblock"::"exp"::t) => (case !stack of (h::i::j::k::l::t) => (ref j)::(ref i)::(ref l)::(ref h)::(ref k)::[] | z => [])
              |("ELSE"::"THEN"::")"::"("::"IF"::"sblock"::"sblock"::"exp"::t) => (case !stack of (h::i::j::k::l::m::n::q::t) => (ref l)::(ref k)::(ref q)::(ref j)::(ref i)::(ref n)::(ref h)::(ref m)::[] | z => [])
              |(")"::";"::";"::"("::"FOR"::"sblock"::"stmt"::"exp"::"stmt"::t) => (case !stack of (h::i::j::k::l::m::n::q::p::t) => (ref l)::(ref k)::(ref p)::(ref j)::(ref q)::(ref i)::(ref n)::(ref h)::(ref m)::[] | z => [])
              |(":"::"OTHERWISE"::")"::"("::"SWITCH"::"sblock"::"stmt_case_list"::"exp"::t) => (case !stack of (h::i::j::k::l::m::n::q::t) => (ref l)::(ref k)::(ref q)::(ref j)::(ref n)::(ref i)::(ref h)::(ref m)::[] | z => [])
              |(";"::"assignable"::"memOp"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref i)::(ref h)::[] | z => [])
(*stmtassign*)|(":="::"exp"::"assignable"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
(*stmt_cas_l*)|("stmt_case"::"stmt_case_list"::t) => (case !stack of (h::i::t) => (ref i)::(ref h)::[] | z => [])
              |("stmt_case"::t) => (ref (hd(!stack)))::[]
(*stmt_case*) |(":"::"CASE"::"sblock"::"constant"::t) => (case !stack of (h::i::j::k::t) => (ref i)::(ref k)::(ref h)::(ref j)::[] | z => [])
   (*t_type*) |("T_INTEGER"::t) => (ref (hd(!stack)))::[]
              |("T_BOOLEAN"::t) => (ref (hd(!stack)))::[]
              |("T_CHAR"::t) => (ref (hd(!stack)))::[]
              |("T_STRING"::t) => (ref (hd(!stack)))::[]
              |("T_REAL"::t) => (ref (hd(!stack)))::[]
(*assignable*)|("ID"::"."::"assignable"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref i)::(ref h)::[] | z => [])
              |("ablock"::"assignable"::t) => (case !stack of (h::i::t) => (ref i)::(ref h)::[] | z => [])
(*exp*)       |("constant"::t) => (ref (hd(!stack)))::[]
              |("assignable"::t) => (ref (hd(!stack)))::[]
              |("binaryOp"::t) => (ref (hd(!stack)))::[]
              |("!"::"exp"::t) => (case !stack of (h::i::t) => (ref h)::(ref i)::[] | z => [])
              |("I2R"::"exp"::t) => (case !stack of (h::i::t) => (ref h)::(ref i)::[] | z => [])
              |("R2I"::"exp"::t) => (case !stack of (h::i::t) => (ref h)::(ref i)::[] | z => [])
              |("isNull"::"exp"::t) => (case !stack of (h::i::t) => (ref i)::(ref h)::[] | z => [])
              |(")"::"("::"exp"::t) => (case !stack of (h::i::j::t) => (ref i)::(ref j)::(ref h)::[] | z => [])
(*constant*)  |("C_INTEGER"::t) => (ref (hd(!stack)))::[]
              |("C_CHAR"::t) => (ref (hd(!stack)))::[]
              |("C_REAL"::t) => (ref (hd(!stack)))::[]
              |("C_STRING"::t) => (ref (hd(!stack)))::[]
              |("true"::t) => (ref (hd(!stack)))::[]
              |("false"::t) => (ref (hd(!stack)))::[]
              |("null"::t) => (ref (hd(!stack)))::[]
(*t_type*)    |("ID"::t) => (ref (hd(!stack)))::[]
(*memOp*)     |("reserve"::t) => (ref (hd(!stack)))::[]
              |("release"::t) => (ref (hd(!stack)))::[]
(*binaryOp*)  |("+"::"exp"::"exp"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              |("-"::"exp"::"exp"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
(*exp*)       |("-"::"exp"::t) => (case !stack of (h::i::t) => (ref h)::(ref i)::[] | z => [])
              |("*"::"exp"::"exp"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              |("/"::"exp"::"exp"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              |("|"::"exp"::"exp"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              |("&"::"exp"::"exp"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              |("%"::"exp"::"exp"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              |("<"::"exp"::"exp"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              |("="::"exp"::"exp"::t) => (case !stack of (h::i::j::t) => (ref j)::(ref h)::(ref i)::[] | z => [])
              | x => []
            (* val stack_children = ref_list(List.take(!stack,c_length)) *)
             val stack_children = ref_list(c_type)
             fun set_Parent(child_list,parent) = case child_list of
                (h::t) => (get_Parent(!h) := parent;set_Parent(t,parent))
                | z => ()
             fun add_Children(parent) = case parent of
               Node(p,k,v,l) => (get_Child(parent) := stack_children;stack := List.drop(!stack,c_length);stack := parent::(!stack);set_Parent(stack_children,parent))
               |Leaf(p,k,v,l,c) => (stack := parent::(!stack))
               |z => ()
          in
             add_Children(parent)
          end
       fun print_Stack(stack) = case stack of
         (h::t) => (print(get_token(h) ^ "\n");print_Stack(t))
         |z => ()
       fun create_Struct(list) = case list of
          (Node(p,k,v,l)::t) => (set_Children(Node(p,k,v,l));create_Struct(t))
          |(Leaf(p,k,v,l,c)::t) => (set_Children(Leaf(p,k,v,l,c));create_Struct(t))
          | z => hd(!stack)
    in
       tree := create_Struct(!token_list)
    end

  (* Prints out a visualized Parse Tree*)
    fun print_Tree2() =
      let
       val i = ref 0
       val _ = print("\n----------Parse Tree---------- \n\n")
       fun get_indent(v) = if v > 0 then "  " ^ get_indent(v-1) else ""
       fun pn(tree) = case tree of
        (ref (Node(p,k,v,l))::t) => (print(get_indent(!i + 1) ^ Int.toString(!i) ^ " " ^ k ^ "\n");if not(v = "") then print(get_indent(!i+1) ^ Int.toString(!i + 1) ^ " " ^ k ^ " " ^ v ^ "\n") else ();i := !i + 1;pn(!l);pn(t))
        |(ref (Leaf(p,k,v,l,c))::t) => (print(get_indent(!i + 1) ^ Int.toString(!i) ^ " " ^ k ^ " :" ^ v ^ " " ^ Int.toString(l) ^ " " ^ Int.toString(c) ^ "\n");pn(t))
        | z => (i := !i - 1)
      in pn([tree]);print("\n--------------END--------------\n\n") end

  (* Prints out a list of each node in the parse tree with it's children listed *)
  (* p= k=type, v=lexeme, l=line, c=column *)
      fun print_Tree() =
        let
         val _ = print("\n----------Parse Tree---------- \n\n")
         fun print_children(list) = case list of
            (h::t) => (print("    " ^ get_token(!h) ^ "\n");print_children(t))
           | z => ()
         fun pn(token_list) = case token_list of
          (Node(p,k,v,l)::t) => (print("Node: " ^ k);if not(v = "") then print(": " ^ v ^ "\n") else print("\n");print_children(!l);print("\n");pn(t))
          |(Leaf(p,k,v,l,c)::t) => (print("Node: " ^ k);if not(v = "") then print(": " ^ v ^ " " ^ Int.toString(l) ^ " " ^ Int.toString(c) ^ "\n") else print("\n");print("\n");pn(t))
          | z => ()
        in pn(rev(!token_list));print("\n--------------END--------------\n\n") end

end

structure error_checking = struct

fun add_Syntax_Element(n,l,c) = syntax_Error.add_token(symbol_Table.get_Type(symbol_Table.lookup(n,l,c)),n,l,c)
fun add_Syntax_Const(tp,n,l,c) = syntax_Error.add_token(tp,n,l,c)

val print_grm_error : string * int * int -> unit =
  fn (msg, line, col) => (
  print ("LINE "^Int.toString(line)^":"^Int.toString(col)^
         " ** ERROR: "^msg^"\n");
         errorMsgs.push_Entry("** ERROR:"^Int.toString(line)^":"^Int.toString(col)^
         ": "^msg^"\n", line));

fun check_ID_Error(node,oper) =
  let
    val name = syntax_Error.get_name(node)
    val lin = syntax_Error.get_line(node)
    val col = syntax_Error.get_col(node)
    val msg = name ^ " is not defined"
    fun search_return_Type(name_list) = case name_list of
      (#">"::t) => implode(tl(t))
      |(h::[]) => ""
      |(h::t) => search_return_Type(t)
      |z => ""
    fun look(name,line,coll) = case symbol_Table.lookup(name,line,coll) of
      symbol_Table.Entry(n,s,t,ea) => ((t,s))
      |symbol_Table.End => ("@undef",~1)
    fun get_Input_Type(name,line,coll) =
      let
        val name1 = if hd(explode(name)) = #"(" then name else #1(look(name,line,coll))
        val name_list = explode(name1)
        fun find_Arrow(name_list,i) = case name_list of
          (#">"::t) => i
         |(h::t) => find_Arrow(t,i+1)
         |z => ~1
        val arrow_loc = find_Arrow(name_list,0)
      in
        if arrow_loc = ~1 then "" else implode(List.take(name_list,arrow_loc - 2))
      end
    fun return_Type(name,line,coll) = case look(name,line,coll) of
      ("integer",s) => if oper = "func" then "(integer)" else "integer"
      |("Boolean",s) => if oper = "func" then "(Boolean)" else "Boolean"
      |("real",s) => if oper = "func" then "(real)" else"real"
      |("char",s) => if oper = "func" then "(char)" else"char"
      |("string",s) => if oper = "func" then "(string)" else"string"
      |("@undef",s) => "@undef"
      |(z,s) => if oper = "func" then if get_Input_Type(name,#1(scope_Table.get_Scope_Pos(name,s)),#2(scope_Table.get_Scope_Pos(name,s))) = "" then return_Type(z,#1(scope_Table.get_Scope_Pos(z,s)),#2(scope_Table.get_Scope_Pos(z,s))) else get_Input_Type(name,#1(scope_Table.get_Scope_Pos(name,s)),#2(scope_Table.get_Scope_Pos(name,s)))
            else if search_return_Type(explode(z)) = "" then return_Type(z,#1(scope_Table.get_Scope_Pos(z,s)),#2(scope_Table.get_Scope_Pos(z,s))) else search_return_Type(explode(z))
      val id = return_Type(name,lin,col)
  in
    if id = "@undef" then if oper = "fs" then "@undef" else (print_grm_error(msg,lin,col);"@undef") else id
  end

fun convert_Type(exp,oper) = case syntax_Error.get_type(exp) of
  "C_INTEGER" => "integer"
  |"C_REAL" => "real"
  |"C_CHAR" => "char"
  |"C_STRING" => "string"
  |"true" => "Boolean"
  |"false" => "Boolean"
  |"null" => "null"
  |"ID" => check_ID_Error(exp,oper)
  |"integer" => "integer"
  |"real" => "real"
  |"char" => "char"
  |"string" => "string"
  |"Boolean" => "Boolean"
  |"unit" => "unit"
  |"@undef" => "@undef"
  | z => "@null"

fun check_Function_Undef(type_list) = case type_list of
  ((#"@")::(#"u")::(#"n")::(#"d")::(#"e")::(#"f")::t) => true
  |(h::t) => false orelse check_Function_Undef(t)
  | z => false

fun is_integer(string_list) = case string_list of
  (h::t) => if h < #":" andalso h > #"/" then true andalso is_integer(t) else false
  |z => true

fun create_Array_Input(i) =
  let
  fun create_input(i) = case i of
    1 => "integer)"
   |z => "integer, " ^ create_input(i-1)
  in
    "(" ^ create_input(i)
  end

fun check_binaryOp_error(oper) =
  let
     val exp1 = List.nth(!syntax_Error.stack,1)
     val exp2 = hd(!syntax_Error.stack)
     val sname1 = syntax_Error.get_name(exp1)
     val slin1 = syntax_Error.get_line(exp1)
     val scol1 = syntax_Error.get_col(exp1)
     val sname2 = syntax_Error.get_name(exp2)
     val exp_type1 = if oper = "." then "" else convert_Type(exp1,oper)
     val exp_type2 = if oper = "func" then syntax_Error.get_type(exp2) else convert_Type(exp2,oper)
     val valid_exp1 = not(exp_type1 = "@null")
     val valid_exp2 = not(exp_type2 = "@null")
     val undefined = (exp_type1 = "@undef") orelse if oper = "func" then check_Function_Undef(explode(exp_type2)) else (exp_type2 = "@undef")
     val add_R2I = ref 0 (*0 if no R2I needed, 1 if left term needs R2I, 2 if right term needs R2I*)
     val is_array = is_integer(explode(exp_type1))
     val exp_type1 = if is_array then if not (oper = ".") then create_Array_Input(valOf(Int.fromString(exp_type1))) else exp_type1 else exp_type1
     fun operator(oper) = case oper of
       "+" => "binary"
      |"-" => "binary"
      |"*" => "binary"
      |"/" => "binary"
      |"%" => "mod"
      |"|" => "logic"
      |"&" => "logic"
      |"<" => "inequality"
      |"=" => "equality"
      |":=" => "assign"
      |"func" => "func"
      |"." => "recOp"
      | z => ""
     val error_type = if oper = "func"
                      then syntax_Error.function_Syntax(exp_type1,exp_type2,exp1,check_ID_Error)
                      else if oper = "." then syntax_Error.recOp_Syntax(sname1,sname2,exp_type2,slin1,scol1)
                      else syntax_Error.binary_Syntax(exp_type1,exp_type2,operator(oper))
       fun get_msg(error_int) = case error_int of
         1 => "\"" ^ sname1 ^ "\" should be of type: int/real but is type: " ^ exp_type1
       | 2 => "\"" ^ sname2 ^ "\" should be of type: int/real but is type: " ^ exp_type2
       | 3 => (case (valid_exp1,valid_exp2) of
                    (false,false) => ""
                    |(true,false) => get_msg(1)
                    |(false,true) => get_msg(2)
                    |(true,true) => "neither \"" ^ sname1 ^ "\" or \"" ^ sname2 ^ "\" are of type int/real they are of type: " ^ exp_type1 ^ " and " ^ exp_type2 )
       | 4 => (add_R2I := 1;"")
       | 5 => (add_R2I := 2;"")
       | 7 => "\"" ^ sname1 ^ "\" should be of type: Boolean but is type: " ^ exp_type1
       | 8 => "\"" ^ sname2 ^ "\" should be of type: Boolean but is type: " ^ exp_type2
       | 9 => (case (valid_exp1,valid_exp2) of
                    (false,false) => ""
                    |(true,false) => get_msg(7)
                    |(false,true) => get_msg(8)
                    |(true,true) => "neither \"" ^ sname1 ^ "\" or \"" ^ sname2 ^ "\" are of type: Boolean they are of type: " ^ exp_type2 ^ " and " ^ exp_type1 )
       | 10 => sname1 ^ " and " ^ sname2 ^ " should be of the same type but are of type: " ^ exp_type1  ^ " and " ^ exp_type2
       | 11 => "\"" ^ sname1 ^ "\" should be of type: integer but is type: " ^ exp_type1
       | 12 => "\"" ^ sname2 ^ "\" should be of type: integer but is type: " ^ exp_type2
       | 13 => (case (valid_exp1,valid_exp2) of
                    (false,false) => ""
                    |(true,false) => get_msg(11)
                    |(false,true) => get_msg(12)
                    |(true,true) => "neither \"" ^ sname1 ^ "\" or \"" ^ sname2 ^ "\" are of type: integer they are of type: " ^ exp_type2 ^ " and " ^ exp_type1 )
       | 14 => sname2 ^ " should be of type function: " ^ sname1 ^ ": " ^ exp_type1 ^ " but is type: " ^ exp_type2
       | 15 => sname1 ^ " is not a valid record type"
       | 16 => sname2 ^ " is not a member of " ^ sname1
       | z => ""
     val eq = if oper = "func" then sname1 ^ sname2 else if oper = "." then sname1 ^ oper ^ sname2 else sname1 ^ " " ^ oper ^ " " ^ sname2
     val msg = get_msg(#1 error_type)
     val thin_stack = syntax_Error.stack := List.drop(!syntax_Error.stack,2)
     val exp = add_Syntax_Const(#2 error_type,eq,syntax_Error.get_line(exp1),syntax_Error.get_col(exp1))
  in
     ((if not(msg = "") andalso not undefined then print_grm_error(msg ^ " in: " ^ eq,syntax_Error.get_line(exp1),syntax_Error.get_col(exp1)) else ())(*;print("sname1: " ^ sname1 ^ " " ^ exp_type1 ^ " sname2: " ^ sname2 ^ " " ^ exp_type2 ^ " oper: " ^ oper ^ " \n");syntax_Error.print_Stack();print("\n-----------------------------\n")*);!add_R2I)
  end

fun check_unaryOp_error(oper) =
   let
     fun extra_Type(string,exp,oper) = case string of
      "atype" => "Array"
      |"rtype" => "Record"
      |"ftype" => "Function"
      | z => convert_Type(exp,oper)
     val exp = hd(!syntax_Error.stack)
     val sname = syntax_Error.get_name(exp)
     val slin = syntax_Error.get_line(exp)
     val scol = syntax_Error.get_col(exp)
     val exp_type = extra_Type(symbol_Table.get_Extra(symbol_Table.lookup(sname,slin,scol)),exp,oper)
     val valid_exp = not(exp_type = "@null")
     val undefined = (exp_type = "@undef")
     fun get_msg(error_int) = case error_int of
       1 => "int/real"
      |2 => "integer"
      |3 => "real"
      |4 => "Boolean"
      |5 => "integer"
      |6 => "Array or Record"
      |7 => sname ^ ": dimensions are required when allocating array's in memory"
      |z => ""
      val eq = if oper = "isNull" then sname ^ " " ^ oper else if oper = "sc" then sname else oper ^ " " ^ sname
      val error_int = if oper = "isNull" then syntax_Error.rec_array_Syntax(sname,oper,slin,scol) else if oper = "reserve" orelse oper = "release" then syntax_Error.memOp_Syntax(sname,slin,scol) else syntax_Error.unary_Syntax(exp_type,oper)
      val thin_stack = syntax_Error.stack := tl(!syntax_Error.stack)
      val exp_new = add_Syntax_Const(#2 error_int,eq,syntax_Error.get_line(exp),syntax_Error.get_col(exp))
      val msg = if #1 error_int = 7 then get_msg(7) else
                if #1 error_int = 0 then "" else "\"" ^ sname ^ "\" should be of type: " ^ get_msg(#1 error_int) ^ " but is type: " ^ exp_type
   in
      ((if not(msg = "") andalso not undefined then print_grm_error(msg ^ " in: " ^ eq,syntax_Error.get_line(exp),syntax_Error.get_col(exp)) else ())(*;print("sname: " ^ sname ^ " oper: !" ^ oper ^ "! \n");syntax_Error.print_Stack();print("\n-----------------------------\n")*);0)
   end

fun add_Paren_Stack() =
   let
     val token = hd(!syntax_Error.stack)
     val tp = syntax_Error.get_type(token)
     val name = "(" ^ syntax_Error.get_name(token) ^ ")"
     val l = syntax_Error.get_line(token)
     val c = syntax_Error.get_col(token)
   in
      syntax_Error.stack := syntax_Error.parse_Token(tp,name,l,c)::(tl(!syntax_Error.stack))
   end

fun I2R_Left(node) =
  let
    val l = parse_Tree.get_Child(node)
    val exp1 = hd(!l)
    val exp_new = parse_Tree.Node(ref node,"exp","",ref [])
    val set_exp1_p = parse_Tree.get_Parent(!exp1) := exp_new
    val I2R = ref (parse_Tree.Node(ref exp_new,"I2R","",ref []))
    val set_new_children = parse_Tree.get_Child(exp_new) := [I2R, exp1]
  in
    l := (ref exp_new)::tl(!l)
  end

fun I2R_Right(node) =
  let
    val l = parse_Tree.get_Child(node)
    val exp1 = List.last(!l)
    val exp_new = parse_Tree.Node(ref node,"exp","",ref [])
    val set_exp1_p = parse_Tree.get_Parent(!exp1) := exp_new
    val I2R = ref (parse_Tree.Node(ref exp_new,"I2R","",ref []))
    val set_new_children = parse_Tree.get_Child(exp_new) := [I2R, exp1]
  in
    l := rev((ref exp_new)::tl(rev(!l)))
  end

fun insert_I2R(i: int,node) = case node of
  parse_Tree.Node(p,k,v,l) => ( case i of
                                  0 => ((*print("i: " ^ Int.toString(i) ^ " ")*))
                                  |1 => I2R_Left(parse_Tree.Node(p,k,v,l))
                                  |2 => I2R_Right(parse_Tree.Node(p,k,v,l))
                                  |z => () )
  |z => ()

fun add_Param_List() =
  let
    val stack = !syntax_Error.stack
    fun find_Paren(stack_list) = case stack_list of
        (h::t) => if syntax_Error.get_type(h) = "(" then 0 else 1 + find_Paren(t)
       | z => 0
    val open_arg = find_Paren(stack)
    val range = open_arg
    val arg_list = rev(List.take(stack,range))
    fun trav_list(list: syntax_Error.parse_Token list, f) = case list of
      (h::[]) => f(h,"+")
     |(h::t) => f(h,"+") ^ ", " ^ trav_list(t,f)
     | z => ""
    fun trav_list_name(list: syntax_Error.parse_Token list, f) = case list of
      (h::[]) => f(h)
     |(h::t) => f(h) ^ ", " ^ trav_list_name(t,f)
     | z => ""
    val list_type = "(" ^ trav_list(arg_list,convert_Type) ^ ")"
    val name = "(" ^ trav_list_name(arg_list,syntax_Error.get_name) ^ ")"
    val line = syntax_Error.get_line(List.nth(stack,range))
    val col = syntax_Error.get_col(List.nth(stack,range))
    val entry = syntax_Error.parse_Token(list_type,name,line,col)
  in
    syntax_Error.stack := entry::List.drop(!syntax_Error.stack,range+1)
  end

fun error_check() =
   let
      val stack = [parse_Tree.tree]
      fun ref_Node_To_String(node_list) = case node_list of
        (h::t) => parse_Tree.get_token(!h)::ref_Node_To_String(t)
        |z => []
      fun token_Function(node) = case ((*print("node: " ^ parse_Tree.get_token(node) ^ "\n");*)parse_Tree.get_token(node)) of
        "binaryOp" => check_binaryOp_error(parse_Tree.get_token(!(List.nth(!(parse_Tree.get_Child(node)),1))))
        |"exp" => if parse_Tree.get_token(!(parse_Tree.get_Parent(node))) = "stmt"
                  then if  parse_Tree.get_token( !(hd( ( !(parse_Tree.get_Child(!(parse_Tree.get_Parent(node) ) ) ) ) ) )  ) = "SWITCH" then check_unaryOp_error("sc") else check_unaryOp_error("")
                  else (case ref_Node_To_String(!(parse_Tree.get_Child(node))) of
                        ("-"::"exp"::t) => check_unaryOp_error("-")
                        |("!"::"exp"::t) => check_unaryOp_error("!")
                        |("I2R"::"exp"::t) => check_unaryOp_error("i2r")
                        |("R2I"::"exp"::t) => check_unaryOp_error("r2i")
                        |("("::"exp"::")"::t) => (add_Paren_Stack();0)
                        |("exp"::"isNull"::t) => check_unaryOp_error("isNull")
                        |z => 0 )
        |"stmt_assign" => check_binaryOp_error(":=")
        |"id_list" => ( case ref_Node_To_String(!(parse_Tree.get_Child(!(List.last(!(parse_Tree.get_Child(node))))))) of
                                             ("e"::t) => 0
                                             |z => check_binaryOp_error(":=") )
        |"constant" => if parse_Tree.get_token(!(parse_Tree.get_Parent(node))) = "stmt_case" then check_unaryOp_error("sc") else 0
        |"stmt" => (case ref_Node_To_String(!(parse_Tree.get_Child(node))) of
                    ("memOp"::"assignable"::";"::t) => check_unaryOp_error( parse_Tree.get_token((!(hd((!(parse_Tree.get_Child(!(hd((!(parse_Tree.get_Child(node)))))))))))) )
                    | z => 0)
        |"assignable" => (case ref_Node_To_String(!(parse_Tree.get_Child(node))) of
              ("assignable"::"."::t) => check_binaryOp_error(".") (*RESERVE RELASE*)
              |z => 0)
        |"(" => (if parse_Tree.get_token(!(parse_Tree.get_Parent(node))) = "ablock" then syntax_Error.stack := syntax_Error.parse_Token("(","(",~1,~1)::(!syntax_Error.stack) else ();0)
        |")" => if parse_Tree.get_token(!(parse_Tree.get_Parent(node))) = "ablock" then (add_Param_List();0) else 0
        |"ablock" => check_binaryOp_error("func")
        |z => 0
      fun depth_search(tree) = case tree of
        (ref (parse_Tree.Node(p,k,v,l))::t) => (depth_search(!l);insert_I2R(token_Function(parse_Tree.Node(p,k,v,l)),parse_Tree.Node(p,k,v,l));depth_search(t))
        |(ref (parse_Tree.Leaf(p,k,v,l,c))::t) => (syntax_Error.stack := syntax_Error.parse_Token(k,v,l,c)::(!syntax_Error.stack);(*print("node: " ^ k ^ " " ^ v ^ "\n");*)depth_search(t))
        |z => ()
   in
      (depth_search(stack))
   end

end

structure create_Scope = struct

val table_list: (string * int * int * int) list ref = ref []

fun add_table_value(n,l,c,s) = table_list := (n,l,c,s)::(!table_list)

val scope = ref 0
val new_scope = ref 1;
val old_scope = ref 0;

val stack: int list ref = ref [];
fun push() = stack := (!scope)::(!stack);
fun pop() = stack := tl (!stack);

fun print_stack() =
  let
   fun pstack(s) = case s of
     (h::t) => (print((Int.toString h) ^ "\n");pstack(t))
     | z => ()
   val _ = print("\n-----Stack-----\n")
   val _ = print("Current: " ^ Int.toString(!scope) ^ "\n")
   val _ = pstack(!stack)
   val _ = print("\n-----End-----\n")
  in
   ()
  end

fun oldscope() = if length(!stack) <= 1 then 0 else hd(!stack)

fun newscope() = (new_scope:=(!new_scope)+1)
fun incscope() = (push();scope:=(!new_scope);newscope())
fun decscope() = (scope:= hd (!stack);pop())

fun search_table(name,lin,col,list) = case list of
  ((n,l,c,s)::t) => if name = n andalso lin = l andalso col = c then s else search_table(name,lin,col,t)
  | z => ~1

fun edit_Scope(name,line,col) = if search_table(name,line,col,!table_list) = ~1 then () else symbol_Table.update_Scope(name,search_table(name,line,col,!table_list),!scope)

fun add_Scope_Entry(name,line,col) = (edit_Scope(name,line,col);
                         scope_Table.create_Scope_Elem(!scope,name,line,col))

fun is_Def(p) = (parse_Tree.get_token(!p) = "def" orelse parse_Tree.get_token(!p) = "param_dec" orelse parse_Tree.get_token(!p) = "id_list")

fun set_Scopes(tree) = case tree of
  (ref (parse_Tree.Node(p,k,v,l))::t) => (set_Scopes(!l);
      (case k of
        "{" => (incscope();scope_Table.new_Scope(oldscope(),!scope))
        |"}" => decscope()
        |"(" => if is_Def(p) then (incscope();scope_Table.new_Scope(oldscope(),!scope)) else ()
        | ")" => if is_Def(p) then decscope() else ()
        | z => ()
        );set_Scopes(t))
  |(ref (parse_Tree.Leaf(p,k,v,l,c))::t) => (if k = "ID" andalso is_Def(p) then add_Scope_Entry(v,l,c) else ();scope_Table.add_scopeID(v,l,c,!scope);set_Scopes(t))
  |z => ()

  fun create_Scopes() = ((*parse_Tree.print_Tree2();*)set_Scopes([parse_Tree.tree]))
end




  (*

    fun toList(char_list) = case char_list of
      (h::t) => h::toString(t)
      | z => []

    This function doesnt do much, it just takes in a list and outputs the same
    list but illistrates recursivly building a list.

    (h::t) means that the char_list is of the form (some char)::(rest of the list).
    Note at the end of your list this will equal (last char)::[]

    If your list begins with a char it will take the first char and append it to the recursive call
    of toString on the rest of the list_type

    z is the default case meaning "anything but the above cases". Since it returns [] your code will return here.

    so say your input list is [1,2,3]

    and you do

    val ins = toList([1,2,3])

    the code will do:
       (1::t) => 1::toString([2,3])
       (2::t) => 1::2::toString([3])
       (3::t) => 1::2::3::toString([])
       ([]::t) => 1::2::3::4::[]

       = [1,2,3,4]

    This is the idea you should use when creating your list of lists of strings

  *)
