structure source_Code = struct
  fun generate_Asc(filename,print_condition,errors) =
     let
     fun string_From_File(filename) =
       let
         val file = TextIO.openIn filename
         fun read_file(file) = case TextIO.inputLine(file) of
            SOME text => text^read_file(file)
           |NONE => ""
       in
         read_file(file) before TextIO.closeIn file
       end
       val text = string_From_File(filename)
        val ref_text = ref (explode(text))
        val line_num = ref 1
        val prev_newln = ref 0
        val position = ref 1
        val close_Scope = ref false
        val in_comment = ref false
        val scope = ref [0]
        val next_scope = ref 1
        fun nums() = if !line_num < 10 then "0" ^ Int.toString(!line_num) ^ ":0:" ^ Int.toString(hd(!scope)) ^ ": " else Int.toString(!line_num) ^ ":0:" ^ Int.toString(hd(!scope)) ^ ": "
        fun length_nums() = size(nums())
        fun sub_To_Endln() = List.take(!ref_text,!prev_newln)
        fun size_To_Endln() = length(sub_To_Endln())
        fun inc_scope() = (scope := (!next_scope)::(!scope);next_scope := !next_scope + 1)
        fun dec_scope() = scope := tl(!scope)
        fun get_Error_List(line_number,error_list) =
          let
            fun get_Error_Set(line_number,error_list: (string * int) list) = case error_list of
                ((a,b)::t) => if b = line_number then explode(a)::get_Error_Set(line_number,t) else get_Error_Set(line_number,t)
                | z => []
            val e_list = List.concat(get_Error_Set(line_number,error_list))
          in
             (e_list,length(e_list))
          end
        fun traverse_text(text) = case text of
            ((#"(")::(#"*")::t) => ((position := !position + 2);in_comment := true;traverse_text(t))
           |((#"*")::(#")")::t) => ((position := !position + 2);in_comment := false;traverse_text(t))
           |((#"{")::t) => ((if !in_comment then () else inc_scope());position := !position + 1;traverse_text(t))
           |((#"}")::t) => ((if !in_comment then () else close_Scope := true);position := !position + 1;traverse_text(t))
           |((#"t")::(#"y")::(#"p")::(#"e")::t) => ((if !in_comment then () else (close_Scope := true;inc_scope()));position := !position + 4;traverse_text(t))
           |((#"\n")::t) => (ref_text := List.concat
      ([sub_To_Endln(),
      (if length(errors) > 0 then (position := !position + #2(get_Error_List(!line_num-1,errors));#1(get_Error_List(!line_num-1,errors))) else []),

      explode(nums()),List.drop(!ref_text,size_To_Endln())]);
                        line_num := !line_num + 1;
                        prev_newln := !position + size(nums());
                        position := !position + 1 + size(nums());
                        (if !close_Scope then (close_Scope := false;dec_scope()) else ());
                        traverse_text(t))
           |(h::t) => (position := !position + 1;traverse_text(t))
           | z => implode(!ref_text);
        fun print_asc(new_file) = TextIO.output(TextIO.stdOut,"\n" ^ new_file ^ "\n")
        fun write_asc(file_name,new_file) = let val out = TextIO.openOut(filename ^ ".asc") in TextIO.output(out,new_file) before TextIO.closeOut(out) end
        val new_file = traverse_text(explode(text ^ "\n"))
     in
        (if print_condition then print_asc(new_file) else write_asc(filename,new_file);print("\n"))
     end
end
