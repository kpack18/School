structure Assembly =
struct
                                 (*OPER*) (*STORE*) (*OP1*)  (*OP2*)
datatype line_Types = Binary of (string * string * string * string)
                                (*OPER*) (*STORE*) (*OP1*)
                     |Unary of (string * string * string)
                              (*Label: Note could also be a branch*)
                     |Jump of (string)
                         (*Conditional*) (*Label*)
                     |Branch of (string *string)
                           (*OPER*)(*store reg temp*) (*OP1*)  (*OP2*)
                     |Bool of (string * string * string * string)
                                      (*OPER*) (*STORE*) (*OP1*) (*OP2*)
                     |Bool_Assign of (string * string * string * string)
                             (*store reg*) (*OP1*)
                     |Malloc of (string * string)
                               (*Name*)
                     |Label of string
                     |Main
                     |Function of string
                     |If
                        (*REG TO SAVE*) (*Offset from SP*)
                     |Save of string * string
                    (*REG TO LOAD INTO*) (*OFFSET FROM SP (S5)*)
                     |Load of string * string
                            (*store reg*) (*op1*)
                     |Assign of string * string
                               (*return val*)
                     |Return of string
                               (*Param var*)
                     |Param of string
                     |Branch_Return
                    (*function name*) (*Number of Args*)
                     |Call of string * string
                            (*Store reg*) (*Name*)  (*Param Reg*)
                     |Array of string * string * string
                                    (*Param reg*) (*Name*) (*Val*)
                     |Array_Assign of string * string * string
                     |NOP

val int_Code: line_Types list ref = ref []

fun add_Code(code) = int_Code := rev(code::(rev(!int_Code)))

val a_Code: line_Types list ref = ref []

fun add_Assembly(code) = a_Code := rev(code::(rev(!a_Code)))
fun add_Assembly_LR() = a_Code := Binary("ADD","S5","0","SP")::(!a_Code)

fun print_Code(code) = case code of
   Binary(oper,st,op1,op2)::t => (print("Binary: " ^ oper ^ ", " ^ st ^ ", " ^ op1 ^ ", " ^ op2 ^ "\n");print_Code(t))
  |Unary(oper,st,op1)::t => (print("Unary: " ^ oper ^ ", " ^ st ^ ", " ^ op1 ^ "\n");print_Code(t))
  |Jump(label)::t => (print("Jump: " ^ label ^ "\n");print_Code(t))
  |Branch(C,label)::t => (print("Branch: " ^ C ^ ", " ^ label ^ "\n");print_Code(t))
  |Branch_Return::t => (print("Branch_Return\n");print_Code(t))
  |Bool(oper,st,op1,op2)::t => (if st = "" then print("Bool: " ^ oper ^ ", " ^ op1 ^ ", " ^ op2 ^ "\n") else print("Bool: " ^ oper ^ ", " ^ st ^ ", " ^ op1 ^ ", " ^ op2 ^ "\n");print_Code(t))
  |Bool_Assign(oper,st,op1,op2)::t => (print("Bool_Assign: " ^ oper ^ ", " ^ st ^ ", " ^ op1 ^ ", " ^ op2 ^ "\n");print_Code(t))
  |Malloc(st,op1)::t => (print("Malloc " ^ st ^ ", " ^ op1 ^ "\n");print_Code(t))
  |Label(name)::t => (print("Label: " ^ name ^ "\n");print_Code(t))
  |Function(name)::t => (print("Function: " ^ name ^ "\n");print_Code(t))
  |Return(value)::t => (print("Return: " ^ value ^ "\n");print_Code(t))
  |Param(var)::t => (print("Param: " ^ var ^ "\n");print_Code(t))
  |If::t => (print("If\n");print_Code(t))
  |NOP::t => (print("NOP" ^ "\n");print_Code(t))
  |Save(reg,offset)::t => (print("Save: " ^ reg ^ ", " ^ offset ^ "\n");print_Code(t))
  |Load(reg,off)::t => (print("Load: " ^ reg ^ ", #" ^ off ^ "\n");print_Code(t))
  |Assign(st,op1)::t => (print("Assign: " ^ st ^ ", " ^ op1 ^ "\n");print_Code(t))
  |Call(func,params)::t => (print("Call: " ^ func ^ ", " ^ params ^ "\n");print_Code(t))
  |Array(st,name,params)::t => (print("Array: " ^ st ^ ", " ^ name ^ ", " ^ params ^ "\n");print_Code(t))
  |Array_Assign(param,name,value)::t => (print("Array_Assign: " ^ param ^ ", " ^ name ^ ", " ^ value ^ "\n");print_Code(t))
  |Main::t => (print("Main\n");print_Code(t))
  |z => (print("\n"))

val next_label = ref 0
fun get_Label() = (next_label := !next_label + 1;Int.toString(!next_label - 1))

val R0 = ref true    val T3 = ref false   val S3 = ref false  val FP = ref true
val A0 = ref false   val T4 = ref false   val S4 = ref false  val SP = ref true
val A1 = ref false   val T5 = ref false  val S5 = ref false  val LR0 = ref true
val A2 = ref false   val T6 = ref false  val Posit = ref true  val LR1 = ref true
val A3 = ref false   val T7 = ref false  val Neg = ref true  val LR2 = ref true
val T0 = ref false   val S0 = ref false  val CP = ref true  val LR3 = ref true
val T1 = ref false   val S1 = ref false  val CM = ref true  val PC = ref true
val T2 = ref false   val S2 = ref false  val HP = ref true  val CPSR = ref true

fun bools(bool) = if bool then "1" else "0"
fun print_Reg() = print(
      "\nR0: " ^ bools(!R0) ^ " T3: " ^ bools(!T3) ^ " S3: " ^ bools(!S3) ^ " FP: " ^ bools(!FP) ^
      "\nA0: " ^ bools(!A0) ^ " T4: " ^ bools(!T4) ^ " S4: " ^ bools(!S4) ^ " SP: " ^ bools(!SP) ^
      "\nA1: " ^ bools(!A1) ^ " T5: " ^ bools(!T5) ^ " S5: " ^ bools(!S5) ^ " LR0: " ^ bools(!LR0) ^
      "\nA2: " ^ bools(!A2) ^ " T6: " ^ bools(!T6) ^ " Posit: " ^ bools(!Posit) ^ " LR1: " ^ bools(!LR1) ^
      "\nA3: " ^ bools(!A3) ^ " T7: " ^ bools(!T7) ^ " Neg: " ^ bools(!Neg) ^ " LR2: " ^ bools(!LR2) ^
      "\nT0: " ^ bools(!T0) ^ " S0: " ^ bools(!S0) ^ " CP: " ^ bools(!CP) ^ " LR3: " ^ bools(!LR3) ^
      "\nT1: " ^ bools(!T1) ^ " S1: " ^ bools(!S1) ^ " CM: " ^ bools(!CM) ^ " PC: " ^ bools(!PC) ^
      "\nT2: " ^ bools(!T2) ^ " S2: " ^ bools(!S2) ^ " HP: " ^ bools(!HP) ^ " CPSR: " ^ bools(!CPSR) ^ "\n\n"
  )

fun access_Reg(string) = case string of
   "0" => true
   |"A0" => !A0
   |"A1" => !A1
   |"A2" => !A2
   |"A3" => !A3
   |"T0" => !T0
   |"T1" => !T1
   |"T2" => !T2
   |"T3" => !T3
   |"T4" => !T4
   |"T5" => !T5
   |"T6" => !T6
   |"T7" => !T7
   |"S0" => !S0
   |"S1" => !S1
   |"S2" => !S2
   |"S3" => !S3
   |"S4" => !S4
   |"S5" => !S5
   | z => true

fun num_To_Reg(string) = case string of
    0 => "0"
   |1 => "A0"
   |2 => "A1"
   |3 => "A2"
   |4 => "A3"
   |5 => "T0"
   |6 => "T1"
   |7 => "T2"
   |8 => "T3"
   |9 => "T4"
   |10 => "T5"
   |11 => "T6"
   |12 => "T7"
   |13 => "S0"
   |14 => "S1"
   |15 => "S2"
   |16 => "S3"
   |17 => "S4"
   |18 => "S5"
   |z => ""

fun find_Open_Reg() =
  let
   fun oR(int) = if int > 18 then ~1 else if access_Reg(num_To_Reg(int)) then oR(int+1) else int
  in
    oR(0)
  end

fun set_True(string) = case string of
     "0" => ()
     |"A0" => A0 := true
     |"A1" => A1 := true
     |"A2" => A2 := true
     |"A3" => A3 := true
     |"T0" => T0 := true
     |"T1" => T1 := true
     |"T2" => T2 := true
     |"T3" => T3 := true
     |"T4" => T4 := true
     |"T5" => T5 := true
     |"T6" => T6 := true
     |"T7" => T7 := true
     |"S0" => S0 := true
     |"S1" => S1 := true
     |"S2" => S2 := true
     |"S3" => S3 := true
     |"S4" => S4 := true
     |"S5" => S5 := true
     | z => ()
fun set_False(string) = case string of
      "0" => ()
      |"A0" => A0 := false
      |"A1" => A1 := false
      |"A2" => A2 := false
      |"A3" => A3 := false
      |"T0" => T0 := false
      |"T1" => T1 := false
      |"T2" => T2 := false
      |"T3" => T3 := false
      |"T4" => T4 := false
      |"T5" => T5 := false
      |"T6" => T6 := false
      |"T7" => T7 := false
      |"S0" => S0 := false
      |"S1" => S1 := false
      |"S2" => S2 := false
      |"S3" => S3 := false
      |"S4" => S4 := false
      |"S5" => S5 := false
      | z => ()

val reg_Table: (string * string) list ref = ref []
fun add_Pair(tempvar,reg) = reg_Table := rev((tempvar,reg)::(rev(!reg_Table)))
fun get_Paired_Reg(var) = let
  fun iterate_reg(table) = case table of
     (temp,reg)::t => if temp = var then reg else iterate_reg(t)
    | z => ""
  in  iterate_reg(!reg_Table) end
fun search_Pair_Pos(var) =
  let
  fun PP(reg_list,int) = case reg_list of
    ((h,i)::t) => if h = var then int + 1 else PP(t,int+1)
    |z => ~1
  in
   PP(!reg_Table,0)
  end
fun delete_Pair(var) =
  let
    val var_pos = search_Pair_Pos(var)
  in
    if var_pos = ~1 then () else reg_Table := List.concat([List.take(!reg_Table,var_pos-1),List.drop(!reg_Table,var_pos)])
  end
fun print_RT() =
  let
    val _ = print("RT: ")
    fun prt(reg_list) = case reg_list of
      (a,b)::t => (print("(" ^ a ^ ", " ^ b ^ ") ");prt(t))
      | z => print("\n")
  in
    prt(!reg_Table)
  end

val code: char list ref = ref []

val memory_Table: string list ref = ref []
fun search_Memory_Pos(var) =
  let
    fun sM(memory_list,int) = case memory_list of
      (h::t) => if h = var then int + 1 else sM(t,int+1)
      |z => ~1
  in
    sM(!memory_Table,0)
  end
fun add_Memory(var) =
  let
    val null_Pos = search_Memory_Pos("@null")
  in
    if null_Pos = ~1 then memory_Table := rev((var)::(rev(!memory_Table))) else memory_Table := List.concat([List.take(!memory_Table,null_Pos-1),[var],List.drop(!memory_Table,null_Pos)])
  end
fun delete_Memory(var) =
  let
    val var_pos = search_Memory_Pos(var)
  in
    if var_pos = ~1 then () else memory_Table := List.concat([List.take(!memory_Table,var_pos-1),["@null"],List.drop(!memory_Table,var_pos)])
  end
fun print_Memory(var) =
  let
    val _ = print("Memory: [")
    fun pm(mem_list) = case mem_list of
      (h::t) => (print(h ^ ", ");pm(t))
     | z => print("]\n")
  in
    pm(!memory_Table)
  end

val param_list: string list ref = ref []
fun print_Params() =
  let
    val _ = print("Param List: [")
    fun ppar(string_list) = case string_list of
      (h::t) => (print(h ^ ", ");ppar(t))
      |z => print("]\n")
  in
    ppar(!param_list)
  end

val heap_list: string list ref = ref []
fun search_Heap_Pos(var) =
  let
    fun sH(heap_list,int) = case heap_list of
      (h::t) => if h = var then int + 1 else sH(t,int+1)
      |z => ~1
  in
    sH(!heap_list,0)
  end
fun add_Heap(var) =
  let
    val null_Pos = search_Heap_Pos("@null")
  in
    if null_Pos = ~1 then heap_list := rev((var)::(rev(!heap_list))) else heap_list := List.concat([List.take(!heap_list,null_Pos-1),[var],List.drop(!heap_list,null_Pos)])
  end
fun delete_Heap(var) =
  let
    val var_pos = search_Heap_Pos(var)
  in
    if var_pos = ~1 then () else heap_list := List.concat([List.take(!heap_list,var_pos-1),["@null"],List.drop(!heap_list,var_pos)])
  end
fun print_Heap(var) =
  let
    val _ = print("Heap: [")
    fun pH(h_list) = case h_list of
      (h::t) => (print(h ^ ", ");pH(t))
      | z => print("]\n")
  in
    (pH(!heap_list))
  end
               (* TYPE    NAME     VAL *)
val data_list: (string * string * string) list ref = ref []
fun search_Data_Pos(var) =
  let
    fun sD(data_list,int) = case data_list of
     ((tp,nm,vl)::t) => if nm = var then int + 1 else sD(t,int+1)
     | z => ~1
  in
    sD(!data_list,0)
  end
fun delete_Data(var) =
  let
    val var_pos = search_Data_Pos(var)
  in
    if var_pos = ~1 then () else data_list := List.concat([List.take(!data_list,var_pos-1),List.drop(!data_list,var_pos)])
  end
fun add_Data(tp,nm,vl) = (if not(search_Data_Pos(nm) = ~1) then delete_Data(nm) else ();data_list := rev((tp,nm,vl)::(rev(!data_list))))
fun get_Data_Val(var) =
  let
    fun sDV(data_list) = case data_list of
      ((tp,nm,vl)::t) => if nm = var then (tp,nm,vl) else sDV(t)
      | z => ("","","")
  in
    sDV(!data_list)
  end
fun print_Data(var) =
  let
    val _ = print("Data: [")
    fun pRD(D_list) = case D_list of
      ((ty,n,v)::t) => (print("(" ^ ty ^ ", " ^ n ^ ", " ^ v ^ ") , ");pRD(t))
      | z => print("]\n")
  in
    (pRD(!data_list))
  end

fun clear_Memory(return_reg) =
  let
    fun cr(int) = if int > 18 then () else if num_To_Reg(int) = return_reg then cr(int+1) else (set_False(num_To_Reg(int));cr(int+1))
    val _ = reg_Table := []
    val _ = memory_Table := []
  in
    cr(0)
  end

fun get_Len(string_list) = case string_list of
  (#"\n"::t) => 1
  |(h::[]) => 0
  |(h::t) => 1 + get_Len(t)
  | z => 0
fun take_Line(pos) = List.take(!code,pos)

fun find_Char_Pos(Tchar: char,line_list) = case line_list of
   (h::t) => if h = Tchar then 1 else 1 + find_Char_Pos(Tchar,t)
  | z => 1


fun is_Imm(string_list) =
  let
    fun check_Imm(string_list) = case string_list of
      (#"0"::t) => true andalso check_Imm(t)
      |(#"1"::t) => true andalso check_Imm(t)
      |(#"2"::t) => true andalso check_Imm(t)
      |(#"3"::t) => true andalso check_Imm(t)
      |(#"4"::t) => true andalso check_Imm(t)
      |(#"5"::t) => true andalso check_Imm(t)
      |(#"6"::t) => true andalso check_Imm(t)
      |(#"7"::t) => true andalso check_Imm(t)
      |(#"8"::t) => true andalso check_Imm(t)
      |(#"9"::t) => true andalso check_Imm(t)
      | [] => true
      | z => false
   in
     if implode(string_list) = "true" orelse implode(string_list) = "false"then true else
     if implode(string_list) = "0" then false else if hd(string_list) = #"~" then true else
     check_Imm(string_list)
   end

fun is_String(string_list) = hd(string_list) = #"\"" andalso List.last(string_list) = #"\""
fun is_Char(string_list: char list) = implode [hd(string_list),List.last(string_list)] = "''"
fun is_Real(string_list) =
  let
    fun check_Real(string_list) = case string_list of
      (#"."::t) => true
      |(h::t) => false orelse is_String(t)
      | z => false
  in
    not(is_String(string_list) orelse is_Char(string_list)) andalso check_Real(string_list)
  end

fun is_Neg(string_list) = hd(string_list) = #"~"

fun to_Imm(string) = if string = "true" then "1" else if string = "false" then "0" else if hd(explode string) = #"~" then "#-" ^ implode(tl(explode string)) else "#" ^ string

fun bin_Oper(operator) = case operator of
      "+" => "ADD"
      |"-" => "SUB"
      |"&" => "AND"
      |"|" => "OR"
      |"+i" => "ADDI"
      |"&i" => "ANDI"
      |"|i" => "ORI"
      |"-i" => "SUBI"
      |"!" => "NOT"
      |z => ""

val reached_Main = ref false
fun create_Line(line) =
  let
   val input_line = if hd(rev(line)) = #"\n" then List.take(line,length(line)-1) else line
   fun is_Malloc(line_list) = case line_list of
     ((#"m")::(#"a")::(#"l")::(#"l")::(#"o")::(#"c")::t) => true
     |(h::t) => false orelse is_Malloc(t)
     |z => false
   fun is_Array_Acc(line_list) = case line_list of
     (#"["::t) => true
     |(h::t) => false orelse is_Array_Acc(t)
     |z => false
   fun has_Oper(line_list) = case line_list of
     (#"*"::t) => true
     |(#"+"::t) => true
     |(#"-"::t) => true
     |(#"/"::t) => true
     |(h::t) => false orelse has_Oper(t)
     |z => false
   fun is_Int(string_list) = case string_list of
      (#"0"::t) => true andalso is_Int(t)
     |(#"1"::t) => true andalso is_Int(t)
     |(#"2"::t) => true andalso is_Int(t)
     |(#"3"::t) => true andalso is_Int(t)
     |(#"4"::t) => true andalso is_Int(t)
     |(#"5"::t) => true andalso is_Int(t)
     |(#"6"::t) => true andalso is_Int(t)
     |(#"7"::t) => true andalso is_Int(t)
     |(#"8"::t) => true andalso is_Int(t)
     |(#"9"::t) => true andalso is_Int(t)
     |[] => true
     | z => false
   fun get_Binary(oper) =
     let
      val endl_pos = find_Char_Pos(#"\n",!code)
      val rest_of_code = List.drop(!code,endl_pos)
      val leng_rest = get_Len(rest_of_code)
      val next_line = List.take(rest_of_code,leng_rest)

      val eq_pos_next = find_Char_Pos(#"=",next_line)

      val eq_pos = find_Char_Pos(#"=",input_line)

      val store_reg = if eq_pos_next >= length(next_line) orelse is_Malloc(next_line) orelse is_Array_Acc(next_line) orelse has_Oper(next_line) then implode(List.take(input_line,eq_pos-2)) else implode(List.take(next_line,eq_pos_next-2))

      val op1_list = List.drop(input_line,eq_pos+1)
      val newln_pos1 = find_Char_Pos(#" ",op1_list)
      val op1 = implode(List.take(op1_list,newln_pos1-1))

      val op2_list = List.drop(op1_list,newln_pos1)
      val newln_pos2 = find_Char_Pos(#" ",op2_list)
      val op2 = implode(List.drop(op2_list,newln_pos2))

     in
       if oper = "&" orelse oper = "|" then add_Code(Bool_Assign(oper,store_reg,op1,op2)) else add_Code(Binary(oper,store_reg,op1,op2))
     end
   fun get_Unary(oper) =
     let
     val endl_pos = find_Char_Pos(#"\n",!code)
     val rest_of_code = List.drop(!code,endl_pos)
     val leng_rest = get_Len(rest_of_code)
     val next_line = List.take(rest_of_code,leng_rest)

     val eq_pos_next = find_Char_Pos(#"=",next_line)

     val eq_pos = find_Char_Pos(#"=",input_line)

     val store_reg = if eq_pos_next >= length(next_line) then implode(List.take(input_line,eq_pos-2)) else implode(List.take(next_line,eq_pos_next-2))

     val oper_pos = find_Char_Pos(hd(explode(oper)),input_line)
     val op1 = implode(List.drop(input_line,oper_pos))

     in
       add_Code(Unary(oper,store_reg,op1))
     end
   fun get_Bool(oper) =
     let
       fun find_EQ_Bool_Pos(line_list) = case line_list of
         (#"=")::(#"=")::t => 2 + find_EQ_Bool_Pos(t)
        |(#"=")::t => 1
        | (h::t) => 1 + find_EQ_Bool_Pos(t)
        | z => 1
       val eq_pos = find_EQ_Bool_Pos(input_line)
       fun assign_Bool() =
         let
           val endl_pos = find_Char_Pos(#"\n",!code)
           val rest_of_code = List.drop(!code,endl_pos)
           val leng_rest = get_Len(rest_of_code)
           val next_line = List.take(rest_of_code,leng_rest)

           val eq_pos_next = find_Char_Pos(#"=",next_line)

           val store_reg = if eq_pos_next >= length(next_line) then implode(List.take(input_line,eq_pos-2)) else implode(List.take(next_line,eq_pos_next-2))

           val bool_Line = List.drop(input_line,eq_pos+1)

           val inc = if oper = "==" then 2 else 1
           val oper_pos = find_Char_Pos(hd(explode(oper)),bool_Line)
           val op1 = implode(List.take(bool_Line,oper_pos-2))
           val op2 = implode(List.drop(bool_Line,oper_pos+inc))
         in
           add_Code(Bool_Assign(oper,store_reg,op1,op2))
         end

       fun cond_Bool() =
         let
           val inc = if oper = "==" then 2 else 1
           val oper_pos = find_Char_Pos(hd(explode(oper)),input_line)
           val op1 = implode(List.take(input_line,oper_pos-2))
           val op2 = implode(List.drop(input_line,oper_pos+inc))
         in
           add_Code(Bool(oper,"",op1,op2))
         end
     in
       if eq_pos <= length(input_line) then assign_Bool() else cond_Bool()
     end
   fun get_If(rest_of_line) =
     let
     fun find_Goto_Pos(line_list) = case line_list of
        (#"g")::(#"o")::(#"t")::(#"o")::t => 1
       | (h::t) => 1 + find_Goto_Pos(t)
       | z => 1
       val rest = tl(explode(implode(rest_of_line)))
       val goto_pos = find_Goto_Pos(rest)
       val jump = List.drop(rest,goto_pos-1)
       val bool = List.take(rest,goto_pos-2)
     in
       (add_Code(If);create_Line(bool);create_Line(jump))
     end
   fun get_Jump(l) =
     let
       val label = implode(tl(l))
     in
       add_Code(Jump(label))
     end
   fun get_Label(rest_of_line) =
     let
       val col_pos = find_Char_Pos(#":",input_line)
       val label = implode(List.take(input_line,col_pos-1))
       val rest = tl(explode(implode(rest_of_line) ^ "\n"))

       val is_Function = !reached_Main
       val is_label = hd(explode label) = #"L" andalso is_Int(tl(explode label))
       val _ = if label = "Main" then reached_Main := true else ()
     in
       (if label = "Main" then add_Code(Main) else if not is_Function andalso not is_label then add_Code(Function(label)) else add_Code(Label(label));if rest = [] then () else create_Line(rest))
     end
   fun get_Malloc(rest_of_list) =
     let
       val eq_pos = find_Char_Pos(#"=",input_line)
       val op1 = implode(List.take(input_line,eq_pos-2))
       val op2 = implode(rev(tl(rev(tl(rest_of_list)))))
     in
       add_Code(Malloc(op1,op2))
     end
   fun get_Assign() =
     let
       fun is_Inter(op1) = (hd(explode(op1)) = #"t" andalso is_Int(tl(explode(op1))))

       val eq_pos = find_Char_Pos(#"=",input_line)

       val op1 = implode(List.take(input_line,eq_pos-2))
       val op2 = implode(List.drop(input_line,eq_pos+1))

       val is_Inter1 = is_Inter(op1)
       val is_Inter2 = is_Inter(op2)
     in
       if is_Inter1 orelse is_Inter2 then () else add_Code(Assign(op1,op2))
     end
   fun get_Return(var) = add_Code(Return(implode var))
   fun get_Param(var) = add_Code(Param(implode var))
   fun get_Call(parameters) =
     let
       val endl_pos = find_Char_Pos(#"\n",!code)
       val rest_of_code = List.drop(!code,endl_pos)
       val leng_rest = get_Len(rest_of_code)
       val next_line = List.take(rest_of_code,leng_rest)

       val eq_pos_next = find_Char_Pos(#"=",next_line)

       val eq_pos = find_Char_Pos(#"=",input_line)

       val store_reg = if eq_pos_next >= length(next_line) then implode(List.take(input_line,eq_pos-2)) else implode(List.take(next_line,eq_pos_next-2))

       val rest_of_line = List.drop(input_line,eq_pos+6)
       val comma_pos = find_Char_Pos(#",",rest_of_line)
       val function_name = implode(List.take(rest_of_line,comma_pos-1))

       val number_Args = List.drop(rest_of_line,comma_pos)
       val number_Args = implode(rev(tl(rev(number_Args))))
     in
        (add_Code(Call(function_name,number_Args));add_Code(Assign(store_reg,"*S5")))
     end
   fun get_Array() =
     let
       val eq_pos = find_Char_Pos(#"=",input_line)
       val bracket_pos_1 = find_Char_Pos(#"[",input_line)
       fun array_get() =
         let
           val eq_pos = find_Char_Pos(#"=",input_line)
           val store_reg = implode(List.take(input_line,eq_pos-2))

           val rest_of_list = List.drop(input_line,eq_pos+1)

           val bracket_pos = find_Char_Pos(#"[",rest_of_list)

           val array_name = implode(List.take(rest_of_list,bracket_pos-1))

           val op1 = implode(rev(tl(rev(List.drop(rest_of_list,bracket_pos)))))

         in
           add_Code(Array(store_reg,array_name,op1))
         end
       fun array_assign() =
         let
           val val_reg = implode(List.drop(input_line,eq_pos+1))
           val array_name = implode(List.take(input_line,bracket_pos_1-1))

           val rest_of_list = List.drop(input_line,bracket_pos_1)

           val right_brack_pos = find_Char_Pos(#"]",rest_of_list)

           val op1 = implode(List.take(rest_of_list,right_brack_pos-1))
         in
           add_Code(Array_Assign(op1,array_name,val_reg))
         end
     in
       if eq_pos < bracket_pos_1 then array_get() else array_assign()
     end
   fun by_Type(ll) = case ll of
    (#"+"::t) => get_Binary("+")
    |(#"*"::t) => get_Binary("*")
    |(#"/"::t) => get_Binary("/")
    |(#"%"::t) => get_Binary("%")
    |((#"=")::(#" ")::(#"-")::t) => get_Unary("-")
    |((#"i")::(#"f")::t) => get_If(t)
    |((#"g")::(#"o")::(#"t")::(#"o")::t) => get_Jump(t)
    |(#"<"::t) => get_Bool("<")
    |((#"=")::(#"=")::t) => get_Bool("==")
    |(#"-"::t) => get_Binary("-")
    |(#"&"::t) => get_Binary("&")
    |(#"|"::t) => get_Binary("|")
    |(#"!"::t) => get_Unary("!")
    |(#":"::t) => get_Label(t)
    |((#"m")::(#"a")::(#"l")::(#"l")::(#"o")::(#"c")::t) => get_Malloc(t)
    |((#"r")::(#"e")::(#"t")::(#"u")::(#"r")::(#"n")::(#" ")::t) => get_Return(t)
    |((#"p")::(#"a")::(#"r")::(#"a")::(#"m")::(#" ")::t) => get_Param(t)
    |((#"c")::(#"a")::(#"l")::(#"l")::(#"(")::t) => get_Call(t)
    |((#"[")::t) => get_Array()
    |(h::t) => by_Type(t)
    |z => if find_Char_Pos(#"=",input_line) < length(input_line) then get_Assign() else ()
  in
    by_Type(input_line)
  end
fun gen_Code_List() =
  let
    val _ = code := (explode(!Quad.output))
    (* val _ = print(implode(!code)) *)
    fun code_Empty(string_list) = (!code = [#"\n"] orelse !code = [#" "] orelse !code = [])
    fun iterate_Lines() = if code_Empty(!code) then () else (create_Line(take_Line(get_Len(!code)));code := List.drop(!code,get_Len(!code));iterate_Lines())
  in
    (iterate_Lines();add_Code(NOP))
  end

fun gen_Assembly() =
  let
    val conditional = ref ""
    fun set_Conditional(string) = case string of
       "<" => conditional := "LT"
      |"==" => conditional := "EQ"
      | z => ()
    fun replace_Oldest_Reg(new_var,avoid_list) =
      let
        fun compare_Avoid_List(var,var_list) = case var_list of
          (h::t) => if h = var then true else false andalso compare_Avoid_List(var,t)
          |z => false
        fun get_Oldest(reg_list) = case reg_list of
          (a,b)::t => if compare_Avoid_List(a,avoid_list) then get_Oldest(t) else (a,b)
          |z => ("","")
        val oldest_Reg = get_Oldest(!reg_Table)
        val _ = delete_Pair(#1oldest_Reg)
        val _ = add_Pair(new_var,#2oldest_Reg)

        val ld_Pos = search_Memory_Pos(new_var)
        val need_load = if ld_Pos = ~1 then false else (delete_Memory(new_var);true)
        val _ = if hd(explode(#1oldest_Reg)) = #"@" then () else add_Memory(#1oldest_Reg)
        val saving_offset = if hd(explode(#1oldest_Reg)) = #"@" then 4 * search_Memory_Pos(#1oldest_Reg) else 4 * (length(!memory_Table) - 1)
      in
        (if need_load then add_Assembly(Load(#2oldest_Reg,Int.toString(4 * ld_Pos))) else ();add_Assembly(Save(#2oldest_Reg,"#" ^ Int.toString(saving_offset))))
      end
    fun get_Reg(var) =
      let
        val register = get_Paired_Reg(var)
        (* val _ = print("var: !" ^ var ^ "! reg: !" ^ register ^ "!") *)
        val open_register = find_Open_Reg();
        val open_register = if not(register = "") then var else num_To_Reg(open_register)
        (* val _ = print(" open reg: !" ^ open_register ^ "!\n") *)
      in
        if register = "" then open_register else register
      end
    fun to_Reg(st,avoid_list,is_operand) =
      let
        val register = get_Reg(st)
        (* val _ = print("reg: " ^ register ^ "\n") *)
        val load_Store_Code_St = if register = "" (* or st?*) then replace_Oldest_Reg(st,avoid_list) else if is_operand then (delete_Pair(st);add_Pair(st,register);set_True(register)) else ()
      in
        if register = "" then get_Reg(st) else register
      end
    fun gen_Binary(oper,st: string,op1: string,op2: string) =
      let
        fun int_Binary() =
          let
             val is_Imm1 = is_Imm(explode op1)
             val is_Imm2 = is_Imm(explode op2)

             val new_op1 = if is_Imm1 then to_Imm(op1) else op1
             val new_op2 = if is_Imm2 then to_Imm(op2) else op2

             val new_oper = oper ^ (if is_Imm1 orelse is_Imm2 then "i" else "")
             val new_oper = bin_Oper(new_oper) ^ !conditional

             (* If op1 is saved in memory load it into open register*)
             val new_op1 = if not(is_Imm1) andalso not(op1 = "0") then to_Reg(op1,[st,op2],false) else new_op1
             (* If op2 is saved in memory load it into open register*)
             val new_op2 = if not(is_Imm2) andalso not(op2 = "0") then to_Reg(op2,[st,op1],false) else new_op2
             (* If there is no open register save oldest reg to memeory and overwrite it*)
             val register = to_Reg(st,[op1,op2],true)

             val solution = if is_Imm1 andalso is_Imm2 then "#" ^ (if oper = "+" then Int.toString (valOf (Int.fromString op2) + valOf (Int.fromString op1)) else Int.toString (valOf (Int.fromString op2) - valOf (Int.fromString op1))) else ""
          in
             if is_Imm1 andalso is_Imm2 then add_Assembly(Binary("ADDI",register,"0",solution)) else
             if is_Imm1 then add_Assembly(Binary(new_oper,register,new_op2,new_op1)) else
             add_Assembly(Binary(new_oper,register,new_op1,new_op2))
          end
        fun mult_Binary() =
          let
            fun Imm_Mul(store,op1,op2_val,int) =
               if int = 0 then (add_Assembly(Binary("ADD",store,"0",op1));Imm_Mul(store,op1,op2_val,int+1)) else
               if int = op2_val then ()
               else (add_Assembly(Binary("ADD",store,store,op1));Imm_Mul(store,op1,op2_val,int+1))

            val is_Imm1 = is_Imm(explode op1)
            val is_Imm2 = is_Imm(explode op2)
            val new_op1 = if is_Imm1 then to_Imm(op1) else op1
            val new_op2 = if is_Imm2 then to_Imm(op2) else op2

            (* If op1 is saved in memory load it into open register*)
            val new_op1 = if not(is_Imm1) andalso not(op1 = "0") then to_Reg(op1,[st,op2],false) else new_op1
            (* If op2 is saved in memory load it into open register*)
            val new_op2 = if not(is_Imm2) andalso not(op2 = "0") then to_Reg(op2,[st,op1],false) else new_op2
            (* If there is no open register save oldest reg to memeory and overwrite it*)
            val temp1 = if not(is_Imm1 orelse is_Imm2) andalso not(op1 = "0" orelse op2 = "0") then to_Reg("@temp" ^ get_Label(),[op1,op2],true) else new_op1

            val label1 = get_Label()
            val label2 = get_Label()
            val label3 = get_Label()

            val register = to_Reg(st,[op1,op2],true)

            val solution = if is_Imm1 andalso is_Imm2 then "#" ^ Int.toString (valOf (Int.fromString op2) * valOf (Int.fromString op1)) else ""
          in
            if op1 = "0" orelse op2 = "0" then add_Assembly(Binary("ADD",register,"0","0")) else
            if is_Imm1 andalso is_Imm2 then add_Assembly(Binary("ADDI",register,"0",solution)) else
            if is_Imm1 then Imm_Mul(register,new_op2,valOf (Int.fromString op1),0) else
            if is_Imm2 then Imm_Mul(register,new_op1,valOf (Int.fromString op2),0)
            else
            (add_Assembly(Binary("ADD",temp1,new_op2,"0"));
            add_Assembly(Label("MUL" ^ label1));
            add_Assembly(Binary("SUBS",temp1,temp1,"0"));
            add_Assembly(Branch("NE","MUL" ^ label2));
            add_Assembly(Jump("MUL" ^ label3));
            add_Assembly(Label("MUL" ^ label2));
            add_Assembly(Binary("ADD",register,register,new_op1));
            add_Assembly(Binary("SUBI",temp1,temp1,"#1"));
            add_Assembly(Jump("MUL" ^ label1));
            add_Assembly(Label("MUL" ^ label3)))
          end
        fun div_Binary(modulus) =
          let
            val is_Imm1 = is_Imm(explode op1)
            val is_Imm2 = is_Imm(explode op2)

            val new_op1 = if is_Imm1 then to_Imm(op1) else op1
            val new_op2 = if is_Imm2 then to_Imm(op2) else op2

            (* If op1 is saved in memory load it into open register*)
            val new_op1 = if not(is_Imm1) andalso not(op1 = "0") then to_Reg(op1,[st,op2],false) else new_op1
            (* If op2 is saved in memory load it into open register*)
            val new_op2 = if not(is_Imm2) andalso not(op2 = "0") then to_Reg(op2,[st,op1],false) else new_op2
            (* If there is no open register save oldest reg to memeory and overwrite it*)
            val temp1 = if not(op1 = "0" orelse op2 = "0") then to_Reg("@temp" ^ get_Label(),[op1,op2],true) else ""
            val temp2 = if not(op1 = "0" orelse op2 = "0") then to_Reg("@temp" ^ get_Label(),[op1,op2],true) else ""
            val temp_Imm2 = if not(op1 = "0" orelse op2 = "0") andalso is_Imm2 then to_Reg("@tempIMM" ^ get_Label(),[op1,op2],true) else new_op2
            val imm_op2 = new_op2
            val new_op2 = temp_Imm2

            val label1 = get_Label()
            val label2 = get_Label()

            val register = to_Reg(st,[op1,op2],true)
          in
           if op1 = "0" orelse op2 = "0" then add_Assembly(Binary("ADD",register,"0","0")) else
          (add_Assembly(Binary("ADD",register,"0","0"));
           if is_Imm2 then add_Assembly(Binary("ADDI",temp_Imm2,"0",imm_op2)) else ();
           if is_Imm1 then add_Assembly(Binary("ADDI",temp1,"0",new_op1)) else add_Assembly(Binary("ADD",temp1,"0",new_op1));
           add_Assembly(Label("DIV" ^ label1));
           add_Assembly(Binary("SUBS",temp2,temp1,new_op2));
           add_Assembly(Branch("LT","DIV" ^ label2));
           add_Assembly(Binary("SUB",temp1,temp1,new_op2));
           add_Assembly(Binary("ADDI",register,register,"#1"));
           add_Assembly(Jump("DIV" ^ label1));
           add_Assembly(Label("DIV" ^ label2));
           if modulus then add_Assembly(Binary("ADD",register,"0",temp1)) else ())
          end
        fun check_Oper() = case oper of
          "+" => int_Binary()
          |"-" => int_Binary()
          |"*" => mult_Binary()
          |"/" => div_Binary(false)
          |"%" => div_Binary(true)
          |z => ()
      in
        check_Oper()
      end
    fun gen_Bool_Assign(oper,st,op1,op2) =
      let
        fun And_Or_Bool() =
          let
            fun to_Imm_Bool() = case (op1,op2) of
               ("true","true") => "#1"
               |("true","false") => if oper = "&" then "0" else "#1"
               |("false","true") => if oper = "&" then "0" else "#1"
               |("false","false") => "0"
               | z => ""
            val is_Imm1 = is_Imm(explode op1)
            val is_Imm2 = is_Imm(explode op2)

            val new_op1 = if is_Imm1 then to_Imm(op1) else op1
            val new_op2 = if is_Imm2 then to_Imm(op2) else op2

            val new_oper = oper ^ (if is_Imm1 orelse is_Imm2 then "i" else "")
            val new_oper = bin_Oper(new_oper) ^ !conditional

            (* If op1 is saved in memory load it into open register*)
            val new_op1 = if not(is_Imm1) andalso not(op1 = "true" orelse op1 = "false") then to_Reg(op1,[st,op2],false) else new_op1
            (* If op2 is saved in memory load it into open register*)
            val new_op2 = if not(is_Imm2) andalso not(op2 = "true" orelse op2 = "false") then to_Reg(op2,[st,op1],false) else new_op2
            (* If there is no open register save oldest reg to memeory and overwrite it*)
            val register = to_Reg(st,[op1,op2],true)
          in
            if is_Imm1 andalso is_Imm2 then if to_Imm_Bool() = "0" then add_Assembly(Bool_Assign("ADD",register,"0","0")) else add_Assembly(Bool_Assign("ADDI",register,"0","#1")) else
            if is_Imm1 then add_Assembly(Bool_Assign(new_oper,register,new_op2,"#1")) else
            add_Assembly(Bool_Assign(new_oper,register,new_op1,new_op2))
          end
        fun Conditional_Bool() =
          let
            val is_Imm1 = is_Imm(explode op1)
            val is_Imm2 = is_Imm(explode op2)

            val val_Pos1 = search_Data_Pos(op1)
            val val_Pos2 = search_Data_Pos(op2)

            val isstring = is_String(explode op2) orelse not(val_Pos2 = ~1) orelse is_String(explode op1) orelse not(val_Pos1 = ~1)
            val ischar = is_Char(explode op2) orelse not(val_Pos2 = ~1) orelse is_Char(explode op1) orelse not(val_Pos1 = ~1)
            val _ = print("ischar: " ^ op1 ^ " " ^ Bool.toString(ischar) ^ "\n")

            val new_op1 = if is_Imm1 then to_Imm(op1) else op1
            val new_op2 = if is_Imm2 then to_Imm(op2) else op2

            val new_oper = oper ^ (if is_Imm1 orelse is_Imm2 then "i" else "")
            val new_oper = bin_Oper(new_oper) ^ !conditional

            (* If op1 is saved in memory load it into open register*)
            val new_op1 = if not(is_Imm1 orelse isstring orelse ischar) andalso not(op1 = "true" orelse op1 = "false") then to_Reg(op1,[st,op2],false) else new_op1
            (* If op2 is saved in memory load it into open register*)
            val new_op2 = if not(is_Imm2 orelse isstring orelse ischar) andalso not(op2 = "true" orelse op2 = "false") then to_Reg(op2,[st,op1],false) else new_op2
            (* If there is no open register save oldest reg to memeory and overwrite it*)
            val temp1 = if not(is_Imm1 orelse is_Imm2 orelse isstring orelse ischar) andalso not(op1 = "0" orelse op2 = "0") then to_Reg("@temp" ^ get_Label(),[op1,op2],true) else new_op1
            val temp_Imm2 = if not(op1 = "0" orelse op2 = "0" orelse isstring orelse ischar) andalso is_Imm2 then to_Reg("@tempIMM" ^ get_Label(),[op1,op2],true) else new_op2
            val imm_op2 = new_op2
            val new_op2 = temp_Imm2

            val temp_Imm1 = if not(op1 = "0" orelse op2 = "0" orelse isstring orelse ischar) andalso is_Imm1 then to_Reg("@tempIMM" ^ get_Label(),[op1,op2],true) else new_op1
            val imm_op1 = new_op1
            val new_op1 = temp_Imm1

            val label1 = get_Label()
            val label2 = get_Label()

            val register = to_Reg(st,[op1,op2],true)

            val solution = if is_Imm1 andalso is_Imm2 then (if oper = "<" then valOf (Int.fromString op2) < valOf (Int.fromString op1) else valOf (Int.fromString op2) = valOf (Int.fromString op1)) else false
            val solution_Imm = if is_Imm1 andalso is_Imm2 then (if solution then "#1" else "0") else ""

            val temp_control = if oper = "<" then "LT" else "EQ"
            val label_names = if oper = "<" then "L" else "E"

            val heap_truth = op2 = "null"
            val heap_index = if op2 = "null" then search_Heap_Pos(op1) else ~1

            val dataop1 = if isstring orelse ischar then if search_Data_Pos(op1) = ~1 then op1 else #3(get_Data_Val(op1)) else ""
            val dataop2 = if isstring orelse ischar then if search_Data_Pos(op2) = ~1 then op2 else #3(get_Data_Val(op2)) else ""
          in
            if isstring orelse ischar then if dataop1 = dataop2 then add_Assembly(Binary("ADDI",register,"0","#1")) else add_Assembly(Binary("ADD",register,"0","0")) else
            if heap_truth then if heap_index = ~1 then add_Assembly(Binary("ADDI",st,"0","#1")) else add_Assembly(Binary("ADD",st,"0","0")) else
            if is_Imm1 andalso is_Imm2 then if solution_Imm = "#1" then add_Assembly(Binary("ADDI",register,"0","#1")) else add_Assembly(Binary("ADD",register,"0","0"))  else
            (add_Assembly(Binary("ADD",register,"0","0"));
             if is_Imm1 then add_Assembly(Binary("ADD",temp_Imm1,"0",imm_op1)) else ();
             if is_Imm2 then add_Assembly(Binary("ADD",temp_Imm2,"0",imm_op2)) else ();
             add_Assembly(Binary("SUBS",temp1,new_op1,new_op2));
             add_Assembly(Branch(temp_control,label_names ^ label1));
             add_Assembly(Binary("ADDI",register,"0","#1"));
             add_Assembly(Jump(label_names ^ label2));
             add_Assembly(Label(label_names ^ label1));
             add_Assembly(Binary("ADD",register,"0","0"));
             add_Assembly(Label(label_names ^ label2)) )
          end
        fun check_Oper_Bool() = case oper of
          "&" => And_Or_Bool()
          |"|" => And_Or_Bool()
          |"<" => Conditional_Bool()
          |"==" => Conditional_Bool()
          |z => ()
      in
        check_Oper_Bool()
      end
    fun gen_Bool(oper,op1,op2) =
      let
        val is_Imm1 = is_Imm(explode op1)
        val is_Imm2 = is_Imm(explode op2)

        val val_Pos1 = search_Data_Pos(op1)
        val val_Pos2 = search_Data_Pos(op2)

        val isstring = is_String(explode op2) orelse not(val_Pos2 = ~1) orelse is_String(explode op1) orelse not(val_Pos1 = ~1)
        val ischar = is_Char(explode op1) orelse not(val_Pos2 = ~1) orelse is_Char(explode op1) orelse not(val_Pos1 = ~1)

        val new_op1 = if is_Imm1 then to_Imm(op1) else op1
        val new_op2 = if is_Imm2 then to_Imm(op2) else op2

        (* If op1 is saved in memory load it into open register*)
        val new_op1 = if not(is_Imm1 orelse isstring orelse ischar) andalso not(op1 = "0") then to_Reg(op1,[op2],false) else new_op1
        (* If op2 is saved in memory load it into open register*)
        val new_op2 = if not(is_Imm2 orelse isstring orelse ischar) andalso not(op2 = "0") then to_Reg(op2,[op1],false) else new_op2
        (* If there is no open register save oldest reg to memeory and overwrite it*)
        val temp_Imm1 = if is_Imm1 andalso not(op1 = "0" orelse isstring orelse ischar) then to_Reg("@temp" ^ get_Label(),[op1,op2],true) else new_op1
        val temp_Imm2 = if is_Imm2 andalso not(op2 = "0" orelse isstring orelse ischar) then to_Reg("@temp" ^ get_Label(),[op1,op2,"@temp1"],true) else new_op2

        val register = to_Reg("",[op1,op2],false)

        val cc = !conditional
        val _ = if not(!conditional = "") then set_Conditional(oper) else ()

        val dataop1 = if isstring orelse ischar then if search_Data_Pos(op1) = ~1 then op1 else #3(get_Data_Val(op1)) else ""
        val dataop2 = if isstring orelse ischar then if search_Data_Pos(op2) = ~1 then op2 else #3(get_Data_Val(op2)) else ""

        val out1 = if is_Imm1 orelse not(isstring orelse ischar) then add_Assembly(Binary("ADDI",temp_Imm1,"0",new_op1)) else ()
        val ou2 = if is_Imm2 orelse not(isstring orelse ischar) then add_Assembly(Binary("ADDI",temp_Imm2,"0",new_op2)) else ()
      in
        if isstring orelse ischar then if dataop1 = dataop2 then add_Assembly(Binary("SUB" ^ cc,register,"0","0")) else add_Assembly(Binary("SUB",register,"0","SP")) else
        if is_Imm1 orelse is_Imm2 then add_Assembly(Bool("SUB" ^ cc,register,temp_Imm1,temp_Imm2)) else add_Assembly(Bool("SUB" ^ cc,register,new_op1,new_op2))
      end
    fun gen_Unary(oper,st,op1) =
      let
        val is_Imm1 = is_Imm(explode op1)

        val new_op1 = if is_Imm1 then to_Imm(op1) else op1

        val new_op1 = if not(is_Imm1) andalso not(op1 = "0") then to_Reg(op1,[st],false) else new_op1

        val register = to_Reg(st,[op1],true)
        fun to_Not() =
          let
            val op1_imm = if op1 = "true" then "0" else if op1 = "false" then "1" else new_op1

            val temp1 = if not is_Imm1 andalso not(op1 = "0") then to_Reg("@temp" ^ get_Label(),[op1],true) else new_op1
            val label1 = if not is_Imm1 then get_Label() else ""
            val label2 = if not is_Imm1 then get_Label() else ""
          in
            if is_Imm1 then add_Assembly(Binary("ADDI",register,"0",op1_imm)) else
            ( add_Assembly(Binary("SUBS",temp1,new_op1,"0"));
              add_Assembly(Branch("EQ","NOT" ^ label1));
              add_Assembly(Binary("ADD",register,"0","0"));
              add_Assembly(Jump("NOT" ^ label2));
              add_Assembly(Label("NOT" ^ label1));
              add_Assembly(Binary("ADDI",register,"0","#1"));
              add_Assembly(Label("NOT" ^ label2)) )
          end
      in
        if oper = "-" then add_Assembly(Binary("SUB",register,"0",new_op1)) else if oper = "!" then to_Not() else ()
      end
    fun gen_Jump(label) =
      let
        val cc = !conditional
        val _ = if not(!conditional = "") then conditional := "" else ()
      in
        if cc = "" then add_Assembly(Jump(label)) else add_Assembly(Branch(cc,label))
      end
    fun gen_If() = conditional := "S"
    fun gen_Label(name) = add_Assembly(Label(name))
    fun gen_Function(name) =
      let
        val table_val = List.nth(!symbol_Table.table,symbol_Table.entry_Pos(name))
        val function_Type = symbol_Table.get_Type(table_val)
        val function_Pos = symbol_Table.entry_Pos(function_Type)
        val type_val = List.nth(!symbol_Table.table,function_Pos)
        val type_type = symbol_Table.get_Type(type_val)
        fun num_Of_Args(list) = case list of
          ((#",")::t) => 1 + num_Of_Args(t)
          |(h::t) => 0 + num_Of_Args(t)
          |z => 0
        val number_Args = num_Of_Args(explode(type_type)) + 1
        fun assign_Args(int) = if int > 0 then (add_Pair(symbol_Table.get_Name(List.nth(!symbol_Table.table,function_Pos+int)),num_To_Reg(number_Args-int+1));assign_Args(int-1)) else ()
        val _ = add_Assembly(Function(name))
      in
        assign_Args(number_Args)
      end
    fun gen_Return(op1) =
      let
        val is_Imm1 = is_Imm(explode op1)
        val new_op1 = if is_Imm1 then to_Imm(op1) else op1
        val new_op1 = if not(is_Imm1) andalso not(op1 = "0") then to_Reg(op1,[],false) else new_op1
        val new_op1 = if op1 = "true" then "0" else if op1 = "false" then "1" else new_op1

        val _ = add_Assembly(Binary("ADD","S5","0",new_op1))
        val _ = clear_Memory("S5")
      in
        add_Assembly(Branch_Return)
      end
    fun gen_Param(var) = (param_list := rev(var::rev(!param_list));add_Assembly(Param(var)))
    fun gen_Main() = add_Assembly(Main)
    fun gen_Assign(st,op1) =
      let
        val val_Pos = search_Data_Pos(op1)

        val isstring = is_String(explode op1) orelse not(val_Pos = ~1)
        val ischar = is_Char(explode op1) orelse not(val_Pos = ~1)


        val is_Imm1 = is_Imm(explode op1)

        val new_op1 = if is_Imm1 then to_Imm(op1) else op1

        val new_op1 = if not(is_Imm1 orelse isstring) andalso not(op1 = "0" orelse op1 = "*S5") then to_Reg(op1,[st],false) else new_op1
        val new_op1 = if op1 = "true" then "#1" else if op1 = "*S5" then "S5" else new_op1
        val register = if not(isstring) then to_Reg(st,[op1],true) else ""
      in
        if isstring then if val_Pos = ~1 then add_Data("string",st,op1) else add_Data("string",st,#3(get_Data_Val(op1))) else
        if ischar then if val_Pos = ~1 then add_Data("char",st,op1) else add_Data("char",st,#3(get_Data_Val(op1))) else
        if is_Imm1 then add_Assembly(Binary("ADDI",register,"0",new_op1)) else add_Assembly(Binary("ADD",register,"0",new_op1))
      end
    fun gen_Call(func,params) =
      let
        val table_val = List.nth(!symbol_Table.table,symbol_Table.entry_Pos(func))
        val function_Type = symbol_Table.get_Type(table_val)
        val function_Pos = symbol_Table.entry_Pos(function_Type)
        val arg_list: string list ref = ref []
        fun assign_Param(op1,int) =
          let
            val argument = symbol_Table.get_Name(List.nth(!symbol_Table.table,function_Pos+int))
            val pair_reg = get_Paired_Reg(op1)
            val next_reg = num_To_Reg(int)
            val _ = arg_list := pair_reg::(!arg_list)
          in
            add_Assembly(Binary("ADD",next_reg,"0",pair_reg))
          end
        fun set_Params(list,int) = case list of
          (h::t) => (assign_Param(h,int);set_Params(t,int+1))
          | z => ()
        fun check_param_list(var,param_list) = case param_list of
          (h::t) => (h = var) andalso check_param_list(var,t)
          | z => true
        fun clear_Registers(reg_list,int) = case reg_list of
          ((var,reg)::t) => (if not(check_param_list(reg,!arg_list)) then (delete_Pair(var);add_Memory(var);add_Assembly(Save(reg,Int.toString(4 * int)));set_False(reg)) else ();clear_Registers(t,int+1))
          | z => ()
        val _ = set_Params(!param_list,1)
        val _ = clear_Registers(!reg_Table,1)
        val _ = param_list := []
      in
        add_Assembly(Branch("",func))
      end
    fun gen_Malloc(st,op1) =
      let
        val is_Imm1 = is_Imm(explode op1)
        val new_op1 = if is_Imm1 then to_Imm(op1) else op1
        val new_op1 = if not(is_Imm1) andalso not(op1 = "0") then to_Reg(op1,[],false) else new_op1

        val register = to_Reg(st,[op1],true)

        val _ = add_Assembly(Binary("ADD",register,"0","HP"))
        val _ = add_Assembly(Binary("ADD","HP","0",new_op1))
      in
        add_Heap(st)
      end
    fun gen_Array(st,name,param) =
      let
        val st_register = to_Reg(st,[name,param],true)
        val name_reg = to_Reg(name,[st,param],true)
        val param_reg = to_Reg(param,[name,st],true)
        val temp_reg = to_Reg("@temp" ^ get_Label(),[name,st,param],true)

        val _ = add_Assembly(Binary("ADD",temp_reg,name_reg,param_reg))
      in
        add_Assembly(Binary("LDR",st_register,temp_reg,"#0"))
      end
    fun gen_Array_Assign(param,name,value) =
      let
        val name_reg = to_Reg(name,[param,value],true)

        val is_Immval = is_Imm(explode value)
        val new_val = if is_Immval then to_Imm(value) else value
        val new_val = if not(is_Immval) andalso not(value = "0") then to_Reg(value,[param,name],true) else value

        val is_Immparam = is_Imm(explode param)
        val new_param = if is_Immparam then to_Imm(param) else param
        val new_param = if not(is_Immparam) andalso not(param = "0") then to_Reg(param,[value,name],true) else param
        val temp_reg = to_Reg("@temp" ^ get_Label(),[name,value,param],true)
        val temp_reg2 = if is_Immparam then to_Reg("@temp" ^ get_Label(),[name,value,param],true) else ""

        val _ = if is_Immval then add_Assembly(Binary("ADDI",temp_reg2,"0","#" ^ new_val)) else ()
        val _ = if is_Immparam then add_Assembly(Binary("ADDI",temp_reg,name_reg,"#" ^ new_param)) else add_Assembly(Binary("ADD",temp_reg,name_reg,new_param))
      in
        if is_Immval then add_Assembly(Binary("STR",temp_reg2,temp_reg,"#0"))
        else add_Assembly(Binary("STR",new_val,temp_reg,"#0"))
      end
    fun iterate_Code(code_list) = case code_list of
      Binary(oper,st,op1,op2)::t => (gen_Binary(oper,st,op1,op2);iterate_Code(t))
     |Unary(oper,st,op1)::t => (gen_Unary(oper,st,op1);iterate_Code(t))
     |Jump(label)::t => (gen_Jump(label);iterate_Code(t))
     |Bool(oper,st,op1,op2)::t => (gen_Bool(oper,op1,op2);iterate_Code(t))
     |Bool_Assign(oper,st,op1,op2)::t => (gen_Bool_Assign(oper,st,op1,op2);iterate_Code(t))
     |Malloc(st,op1)::t => (gen_Malloc(st,op1);iterate_Code(t))
     |Label(name)::t => (gen_Label(name);iterate_Code(t))
     |Function(name)::t => (gen_Function(name);iterate_Code(t))
     |If::t => (gen_If();iterate_Code(t))
     |NOP::t => (();iterate_Code(t))
     |Assign(st,op1)::t => (gen_Assign(st,op1);iterate_Code(t))
     |Return(var)::t => (gen_Return(var);iterate_Code(t))
     |Param(var)::t => (gen_Param(var);iterate_Code(t))
     |Call(func,param)::t => (gen_Call(func,param);iterate_Code(t))
     |Array(st,name,param)::t => (gen_Array(st,name,param);iterate_Code(t))
     |Array_Assign(param,name,value)::t => (gen_Array_Assign(param,name,value);iterate_Code(t))
     |Main::t => (gen_Main();iterate_Code(t))
     |z => ()
  in
    iterate_Code(!int_Code)
  end

val output_string = ref ""
fun get_output_string() =
  let
    fun gos(assembly_list) = case assembly_list of
      Binary(oper,st,op1,op2)::t => oper ^ " " ^ st ^ " " ^ op1 ^ " " ^ op2 ^ "\n" ^ gos(t)
      |Unary(oper,st,op1)::t => oper ^ " " ^ st ^ " " ^ op1 ^ "\n" ^ gos(t)
      |Jump(label)::t => "J " ^ label ^ "\n" ^ gos(t)
      |Branch(C,label)::t => "BL" ^ C ^ " " ^ label ^ "\n" ^ gos(t)
      |Branch_Return::t => "BR\n" ^ gos(t)
      |Bool(oper,st,op1,op2)::t => oper ^ " " ^ st ^ " " ^ op1 ^ " " ^ op2 ^ "\n" ^ gos(t)
      |Bool_Assign(oper,st,op1,op2)::t => oper ^ " " ^ st ^ " " ^ op1 ^ " " ^ op2 ^ "\n" ^ gos(t)
      (*|Malloc(st,op1)::t => *)
      |Label(name)::t => "\n" ^ name ^ " " ^ gos(t)
      |Function(name)::t => "\n" ^ name ^ " " ^ gos(t)
      |Return(value)::t => gos(t)
      |Param(var)::t => gos(t)
      |If::t => gos(t)
      |NOP::t => "NOP\n" ^ gos(t)
      |Save(reg,offset)::t => "STR " ^ reg ^ " SP #" ^ offset ^ "\n" ^ gos(t)
      |Load(reg,offset)::t => "LDR " ^ reg ^ " SP #" ^ offset ^ "\n" ^ gos(t)
      |Assign(st,op1)::t => "ADD " ^ st ^ " 0 " ^ op1 ^ "\n" ^ gos(t)
      |Call(func,params)::t => gos(t)
      |Main::t => "\nMain " ^ gos(t)
      |(h::t) => "" ^ gos(t)
      |z => ""
    in
      gos(!a_Code)
    end
fun print_All() =
    let
      val _ = print("\n\nIntermediate:\n")
      val _ = print(!Quad.output)
      val _ = print("\n\nAssembly Pre-Step:\n")
      val _ = print_Code(!int_Code)
      val _ = print("\nAssembly:\n")
      val _ = print_Code(!a_Code)
      val _ = print_RT()
      val _ = print_Memory()
      val _ = print_Params()
      val _ = print_Heap()
      val _ = print_Data()
      val _ = print_Reg()
      val output_Assembly = get_output_string()
      val _ = print("\nCode:\n")
    in
      print(output_Assembly)
    end

fun Assembly_Code(filename) =
    let
      val assembly = gen_Code_List()
      val assembly = gen_Assembly()
      val add0 = add_Assembly(Binary("ADD","0","0","0"))
      val nop = add_Assembly(NOP)
      val _ = output_string := get_output_string()

      val file_stream = TextIO.openOut (filename ^ ".s")
    in
      (TextIO.output(file_stream,!output_string);TextIO.closeOut(file_stream))
    end

end

(* SAVE CODE: STR T1 SP #0     T1 = register to save
              ADDI SP SP #32 *)

(* LOAD CODE: LDR T2 S5 #i        i = 32 * index in memory_Table *)
