(* syntax: Converts the inputted Stream of instructions into a readable list (datatype found in datatype.sml) *)
structure syntax =
struct

   (* Used to store the set of Instructions outputted by the analyzer *)
   val instructions: step list ref = ref []

   (* check_Push: Input: A line within the input file
                  Output: A command in the form of a Push Step (See datatype.sml for datatype)
                  Function: Compares the string after the keyword push to categorize it into one
                            of the defined primitive types. If it's an integer it then converts each digit
                            from a string to int through multiplying each * 10^(decimal place). ex: 145 = (5 * 1) + (4 * 10) + (1 * 100). *)
   fun check_Push(step_string) =
     let
       val var = List.take(List.drop(explode(step_string),5),length(explode step_string)-6)
       val first_char = hd(var)
       fun check_String() = hd(var) = #"\"" andalso List.last(var) = #"\""
       fun iterate_list(val_list,func) = case val_list of
          h::t => func(h) andalso iterate_list(t,func)
        | z => true
       fun check_name() = if Char.isAlpha(first_char) then iterate_list(tl(var),Char.isAlphaNum) else false
       fun check_int() = if Char.isDigit(first_char) orelse first_char = #"-" then iterate_list(tl(var),Char.isDigit) else false
       fun string_to_int(var) =
         let
           fun char_convert(digit) = case digit of
             #"0" => 0 | #"1" => 1 | #"2" => 2 | #"3" => 3 | #"4" => 4 | #"5" => 5 | #"6" => 6 | #"7" => 7 | #"8" => 8| #"9" => 9| z => 0
           fun to_int(var,dec) = case var of
              h::t => char_convert(h) * dec + to_int(t,10 * dec)
            | z => 0
         in
           (if hd(var) = #"-" then ~1 else 1) * to_int(rev var,1)
         end
       val elem = if check_String() then STRING(implode var) else
                  if check_name() then NAME(implode var) else
                  if check_int() then INT(string_to_int var)
                  else ERROR
     in
       Push(elem)
     end

   (* check_fun: Input: The line beginning with either fun or inOutFun
                 Output: A command in the form of a Fun or InOutFun Step (See datatype.sml for datatype)
                 Function: Seperates the line into the name and parameter used in the function.
                           Then based on the io argument it returns an Fun of InOutFun*)
   fun check_fun(step_string,io) =
     let
       fun find_char(step_string,char,pos) = case step_string of
         h::t => if h = char then pos else find_char(t,char,pos+1)
         |z => ~1
       val substring = List.drop(explode(step_string),if io then 9 else 4)
       val name_end_pos = find_char(substring,#" ",1)
       val name = List.take(substring,name_end_pos-1)
       val parameter = List.take(List.drop(substring,name_end_pos),length(List.drop(substring,name_end_pos))-1)
     in
       if io then InOutFun(implode name,NAME(implode parameter)) else Fun(implode name,NAME(implode parameter))
     end

   (* analyze_Step: Input: A line within the input file
                    Output: A command in the form of a Step (See datatype.sml for datatype)
                    Function:  Matches the inputted line with the corresponding Command. If the command doesn't
                               exist it is substituted for "push ERROR
                               Note push,fun, and inOutFun need additional filtering to determine the name/parameters" *)
   fun analyze_Step(step_string) = case step_string of
     "pop\n" => Pop
     |"add\n" => Add
     |"sub\n" => Sub
     |"mul\n" => Mul
     |"div\n" => Div
     |"rem\n" => Rem
     |"neg\n" => Neg
     |"swap\n" => Swap
     |"quit\n" => Quit
     |":true:\n" => Push(BOOL(true))
     |":false:\n" => Push(BOOL(false))
     |":error:\n" => Push(ERROR)
     |"and\n" => And
     |"or\n" => Or
     |"not\n" => Not
     |"equal\n" => Equal
     |"lessThan\n" => LessThan
     |"if\n" => If
     |"bind\n" => Bind
     |"let\n" => Let
     |"end\n" => End
     |"funEnd\n" => FunEnd
     |"call\n" => Call
     |"return\n" => Return
     | z => if implode(List.take(explode(step_string),4)) = "push" then check_Push(step_string) else
            if implode(List.take(explode(step_string),3)) = "fun" then check_fun(step_string,false) else check_fun(step_string,true)

   (* get_Instructions: Input: The input file
                Output: Unit
                Function: Recursivly Reads a Line from the given file and coverts it to a command and appends it to the instruction list.
                          Terminates by appending a [] to the instruction list when the file ends *)
   fun get_instructions(input_file) = case TextIO.inputLine input_file of
       SOME(h) => analyze_Step(h) :: get_instructions(input_file)
     | None => []

end
