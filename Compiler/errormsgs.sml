structure errorMsgs = struct
  datatype element = Entry of (string * int) (* we could replace these so we can just put the tokens right in the types if we want*)
                  | End
  val errors = ref [End]
  val length = ref 0

  fun push_Entry(s,i) = (length := !length + 1; errors := Entry(s,i)::(!errors))

  fun get_String_List() =
    let
      val error_list = !errors
      fun get_String_Pairs(errors) = case errors of
        (Entry(a,b)::t) => (a,b)::get_String_Pairs(t)
        | z => []
    in
      get_String_Pairs(error_list)
    end

  fun print_Table() =
    let
      val y = print("\n")
      val temp_table = tl(List.rev(!errors))
      fun print_entries(errors) = case errors of
        (Entry(s,i)::t) => (TextIO.output(TextIO.stdOut,s ^ " : " ^ Int.toString(i) ^
                            "\n");print_entries(t))
      | z => ()
    in
      (print_entries(temp_table);TextIO.output(TextIO.stdOut,"\n"))
    end
end

structure syntax_Error = struct
                                      (*Type    Name     Line  Col *)
  datatype parse_Token = parse_Token of (string * string * int * int)

  val stack: parse_Token list ref = ref [];

  fun add_token(tp,name,line,col) = stack := parse_Token(tp,name,line,col)::(!stack)

  fun print_Stack() = let
   fun prints(stack) = case stack of
    (parse_Token(tp,n,l,c)::t) => (print(n ^ "\n");prints(t))
    | z => ()

    in
      prints(!stack)
    end

  fun get_token(exp) = case exp of parse_Token(t,n,l,c) => (t,n,l,c)
  fun get_type(exp) = #1(get_token(exp))
  fun get_name(exp) = #2(get_token(exp))
  fun get_line(exp) = #3(get_token(exp))
  fun get_col(exp) = #4(get_token(exp))

  fun count_Params(string_list) = case string_list of
    (#","::t) => 1 + count_Params(t)
    |(#"]"::t) => 1
    |(#")"::t) => 1
    |(h::t) => 0 + count_Params(t)
    | z => 0

  fun get_fun(string) = let
    val has_paren = ref false
    fun get_fun(string_list) = case string_list of
     (#"("::t) => (has_paren := true;[])
     |(h::t) => h::get_fun(t)
     |z => []
    in (implode(get_fun(explode(string))),!has_paren) end

 (*Returns:
    BINARY: 0 = no error  1 = left term is incorrect; 2 = right term is incorrect; 3 = both terms are incorrect; 4 = I2R left; 5 = I2R right
    LOGIC: 6 = no error; 7 = right term is incorrect; 8 = right term is incorrect; 9 = both terms are incorrect
    INEQUALITY: 10 = types are not equal *)
  fun binary_Syntax(exp1,exp2,oper) = case oper of
    "binary" => (case (exp1,exp2) of
               ("integer","integer") => (0,"integer")
              |("integer","real") => (4,"real")
              |("real","integer") => (5,"real")
              |("real","real") => (0,"real")
              |("integer",z) => (2,"integer")
              |("real",z) => (2,"real")
              |(z,"real") => (1,"real")
              |(z,"integer") => (1,"integer")
              |(z,x) => (3,"integer") )
    |"logic" => (case (exp1,exp2) of
               ("Boolean","Boolean") => (6,"Boolean")
              |(z,"Boolean") => (7,"Boolean")
              |("Boolean",z) => (8,"Boolean")
              |(z,x) => (9,"Boolean") )
    |"inequality" => if exp1 = exp2 then (0,"Boolean") else (case (exp1,exp2) of
                    ("integer","real") => (4,"Boolean")
                    |("real","integer") => (5,"Boolean")
                    | z => (10,"Boolean") )
    |"equality" => if exp1 = exp2 then (0,"Boolean") else (case (exp1,exp2) of
                    ("integer","real") => (4,"Boolean")
                    |("real","integer") => (5,"Boolean")
                    |("null",z) => (13,"null")
                    |(z,"null") => (14,"null")
                    | z => (10,"@null") )
    |"assign" => if exp1 = exp2 then (0,"unit") else (10,"unit")
    |"mod" => (case (exp1,exp2) of
              ("integer","integer") => (0,"integer")
              |(z,"integer") => (11,"integer")
              |("integer",z) => (12,"integer")
              |z => (13,"integer") )
    |z => (~1,"")

    fun recOp_Syntax(exp1,exp2,exp2_type,lin,col) =
    let
      val exp1_type = symbol_Table.get_Type(symbol_Table.lookup(exp1,lin,col))
      val exp1_types = count_Params(explode(exp1_type))
      val entry_index = symbol_Table.entry_Pos(exp1)
      fun compare_Types(num) = if not(num > exp1_types) then (exp2 = symbol_Table.get_Name(List.nth((!symbol_Table.table),entry_index + num))) orelse compare_Types(num+1) else false
    in
      if exp1_types = 0 then (15,"@undef") else if compare_Types(0) then (0,exp2_type) else (16,"@undef")
    end

    fun function_Syntax(exp1,exp2,f,func) = if exp1 = exp2 then (0,func(f,"fs")) else (14,func(f,"fs"))

    fun unary_Syntax(exp,oper) = case oper of
      "-" => (case exp of
                       "integer" => (0,"integer")
                       | "real" => (0,"real")
                       | z => (1,"integer") )
      |"i2r" => if exp = "integer" then (0,"real") else (2,"real")
      |"r2i" => if exp = "real" then (0,"integer") else (3,"integer")
      |"!" => if exp = "Boolean" then (0,"Boolean") else (4,"Boolean")
      |"" => if exp = "Boolean" then (0,"Boolean") else (4,"Boolean") (* for while/if/for loop *)
      |"sc" => if exp = "integer" then (0,"integer") else (5,"integer")
      | z => (~1,"")

    fun rec_array_Syntax(exp1,oper,lin,col) = if symbol_Table.get_Extra(symbol_Table.lookup(exp1,lin,col)) = "rtype" orelse symbol_Table.get_Extra(symbol_Table.lookup(exp1,lin,col)) = "atype" then (0,"Boolean") else (6,"Boolean")

    fun memOp_Syntax(exp1,lin,col) = case symbol_Table.get_Extra(symbol_Table.lookup(symbol_Table.get_Type(symbol_Table.lookup((#1 (get_fun(exp1))),lin,col)) , #1(scope_Table.get_Scope_Pos(symbol_Table.get_Type(symbol_Table.lookup((#1 (get_fun(exp1))),lin,col)),0)) , #2(scope_Table.get_Scope_Pos(symbol_Table.get_Type(symbol_Table.lookup((#1 (get_fun(exp1))),lin,col)),0))))
 of
      "rtype" => (0,"unit")
      |"atype" => if not(#2(get_fun(exp1))) then (7,"unit") else (0,"unit")
      |z => (6,"unit")

end
