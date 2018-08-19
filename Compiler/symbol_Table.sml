
structure scope_Table = struct
                           (*     id      lin   col   scope  *)
  datatype scope_id = ScopeID of string * int * int * int

  val id_stack: scope_id list ref = ref []

  fun add_scopeID(id,l,c,s) = id_stack := ScopeID(id,l,c,s)::(!id_stack)

  fun get_ScopeID(name,line,col) =
    let
      fun gsi(idstack) = case idstack of
        (ScopeID(id,l,c,s)::t) => if id = name andalso l = line andalso c = col then s else gsi(t)
        | z => ~1
    in gsi(!id_stack) end

  fun get_Scope_Pos(name,scope) =
    let
      fun gsp(idstack) = case idstack of
        (ScopeID(id,l,c,s)::t) => if id = name andalso s = scope then (l,c) else gsp(t)
        | z => (~1,~1)
    in gsp(!id_stack) end

  fun print_ScopeID() =
    let
      fun psi(stack_list) = case stack_list of
        (ScopeID(id,l,c,s)::t) => (print(id ^ " " ^ (Int.toString(l)) ^ " " ^ (Int.toString(c)) ^ " " ^ (Int.toString(s)) ^ "\n");psi(t))
        | z => ()
    in
      psi(!id_stack)
    end

                            (* id      line  col *)
  datatype var_entry = Var of string * int * int | None
                                 (* parent      value       child list *)
  datatype scope_elem = Scope of scope_elem ref * int * var_entry list ref * scope_elem ref list ref | None

  val scope_stack: scope_elem ref list ref  = ref [ref (Scope(ref None,0,ref [],ref []))]

  fun get_Scope_Parent(scope_elem) = case scope_elem of Scope(p,id,v,c) => !p | z => None
  fun get_Scope_Value(scope_elem) = case scope_elem of Scope(p,id,v,c) => id | z => ~1
  fun get_Scope_Children(scope_elem) = case scope_elem of Scope(p,id,v,c) => !c | z => []
  fun get_Scope_VList(scope_elem) = case scope_elem of Scope(p,id,v,c) => !v | z => []

  fun add_Scope_Elem(scope_elem) = (scope_stack := (ref scope_elem)::(!scope_stack))
  fun add_Scope_Child(scope_elem,child) = case scope_elem of Scope(p,id,v,c) => (c := (rev((ref child)::rev(!c)))) | z => ()
  fun add_Scope_Var(scope_elem,var,lin,col) = case scope_elem of Scope(p,id,v,c) => (v := Var(var,lin,col)::(!v)) | z => ()

  fun get_Scope_By_Val(scope) =
    let
      fun gsbv(stack) = case stack of
        (ref (Scope(p,id,v,c))::t) => if id = scope then Scope(p,id,v,c) else gsbv(t)
        | z => None
    in
      gsbv(!scope_stack)
    end

  fun lookup_Scope(scope) =
    let
     fun ls(scope_stack) = case scope_stack of
      (ref (Scope(p,id,v,c))::t) => if id = scope then Scope(p,id,v,c) else ls(t)
      | z => None
    in
     ls(!scope_stack)
    end

  fun new_Scope(parent,new_scope) =
    let
      val scope = lookup_Scope(new_scope)
      val p = lookup_Scope(parent)
      val new_elem = Scope(ref p,new_scope, ref [], ref [])
    in
      if scope = None then (add_Scope_Elem(new_elem);add_Scope_Child(p,new_elem)) else ()
    end

  fun create_Scope_Elem(new_scope,var,lin,col) =
    let
      val scope = lookup_Scope(new_scope)
    in
      add_Scope_Var(scope,var,lin,col)
    end

  fun print_Scope() =
    let
     val i = ref 0
     fun get_indent(v) = if v > 0 then "  " ^ get_indent(v-1) else ""
     fun pv(v_list) = case v_list of
       ((Var(id,l,c))::t) => "(" ^ id ^ ", " ^ (Int.toString l) ^ ", " ^ (Int.toString c) ^ ") " ^ pv(t)
       | z => ""
     fun ps(scope_elem: scope_elem ref list) = case scope_elem of
      (ref (Scope(p,id,v,c))::t) => (print(get_indent(!i + 1) ^ (Int.toString(id)) ^ " [ " ^ pv(!v) ^ "]" ^ "\n");i := !i + 1;ps(!c);ps(t))
      | z => (i := !i - 1)
    in
      (print("\n---------START--------\n");ps([List.last((!scope_stack))]);print("\n-------------End------------\n"))
    end

end

structure symbol_Table = struct
   datatype element = Entry of (string * int * string * string) (* we could replace these so we can just put the tokens right in the types if we want*)
                  | End
   (* a reference to the table stored in memory. End is used to declare the type of the list as element and to notify the end of the list *)
   val table = ref [End]
   (* stores the number of entries in the table (Does not include the "End" entry) *)
   val length = ref 0
   (* Increments the length of the table by 1 and creates an entry from the given arguments and pushes it to the front of the table *)
   fun push_Entry(n,s,t,ea) = (length := !length + 1;table := Entry(n,s,t,ea)::(!table))
   (* Increments the length of the table by 1 and pushes an Entry to the front of the table *)
   fun push(some_entry) = (length := !length + 1;table := some_entry::(!table))
   (* Returns the stored table as a list *)
   fun get_Table() = !table
   (* Returns the length of the array *)
   fun size() = !length
   (* Returns if the table is empty *)
   fun isEmpty() = !length = 0
   fun get_Type(entry) = case entry of Entry(n,s,tp,ea) => tp | End => ""

   fun get_Name(entry) = case entry of Entry(n,s,tp,ea) => n | End => ""

   fun get_Extra(entry) = case entry of Entry(n,s,tp,ea) => ea | End => ""
   (* If an entry with the name "name" exists it returns that entry, otherwise it returns End (the datatype element) *)
   fun lookup(name,lin,col) =
     let
      val sc = scope_Table.get_ScopeID(name,lin,col)
      val scope_entry = scope_Table.get_Scope_By_Val(sc)
      fun search_Var_List(var_list) = case var_list of
        (scope_Table.Var(id,l,c)::t) => if id = name then true else false orelse search_Var_List(t)
        | z => false
      fun search_Valid_Scopes(scope_elem) = case scope_elem of scope_Table.Scope(p,id,v,c) => if search_Var_List(!v) then id else search_Valid_Scopes(!p) | z => ~1
      val valid_scope = search_Valid_Scopes(scope_entry)
    (*  val _ = print("looking for: " ^ name ^ " " ^ Int.toString(lin) ^ " " ^ Int.toString(col) ^ " " ^ Int.toString(valid_scope) ^ "\n") *)

      fun lookup_in_Table(name,valid_scope,table) = case table of
        (Entry(n,s,tp,ea)::t) => (if (name = n andalso s = valid_scope) then Entry(n,s,tp,ea) else lookup_in_Table(name,valid_scope,t))
        | z => End
     in
       lookup_in_Table(name,valid_scope,!table)
     end

   fun entry_Pos(name) =
     let
       fun search_entries(name,table) = case table of
         (Entry(n,s,tp,ea)::t) => if (name = n) then 0 else 1 + search_entries(name,t)
         | z => ~1
       in
         search_entries(name,!table)
       end

   (* returns the return type of an arraytype as a string *)
   fun getArrayType(id) =
    let
      val temp_table = tl(List.rev(!table))
      fun splitStr(str) = 
        let
          val exploded = explode(str)
          fun removeSpaces(newLst,charLst) = 
            case charLst of 
              [] => newLst
            | x::xs => if(x = #" ") then removeSpaces(newLst,xs) else removeSpaces(newLst@[x],xs) 
          fun getReturnType(charLst) = 
            case charLst of
              [] => ""
            | x::xs => if(x = #">") then implode (removeSpaces ([],xs)) else (getReturnType xs)
        in
          getReturnType(exploded)
        end

      fun iterEntries(table) =
        case table of
          (Entry(n,s,tp,ea)::t) => 
            if(n = id) then
              if (Char.contains tp #">") then splitStr(tp) else getArrayType(tp)
            else iterEntries(t)
        | z => "Error"
    in
      iterEntries(temp_table)
    end

    (* returns extra annotation as string *)
    fun getExtra(id) = 
      let
        val temp_table = tl(List.rev(!table))
        fun iterEntries(table) = 
          case table of
            (Entry(n,s,tp,ea)::t) => if(n = id) then (ea) else (iterEntries(t))
          | z => "Error"
      in
        iterEntries(temp_table)
      end
    (* returns type as string *)
    fun getType(id) = 
     let
        val temp_table = tl(List.rev(!table))
        fun iterEntries(table) = 
          case table of
            (Entry(n,s,tp,ea)::t) => if(n = id) then (tp) else (iterEntries(t))
          | z => "Error"
      in
        iterEntries(temp_table)
      end

   (* Writes the Symbol Table to a file with the name name_of_file.st *)
   fun write_Table(name_of_file) =
     let
         val ostream = TextIO.openOut name_of_file
         val header = TextIO.output(ostream,"Name : Scope : Type : Extra annotation \n")
         val temp_table = tl(List.rev(!table))
         fun write_entries(table,ostream) = case table of
           (Entry(n,s,tp,ea)::t) => (TextIO.output(ostream,n ^ " : " ^ Int.toString(s) ^ " : " ^ tp ^ " : " ^ ea ^ "\n");write_entries(t,ostream))
         | z => ()
     in
         (write_entries(temp_table,ostream);TextIO.closeOut(ostream))
     end
     fun print_Table() =
       let
           val header = print("Name : Scope : Type : Extra annotation \n")
           val temp_table = tl(List.rev(!table))
           fun print_entries(table) = case table of
             (Entry(n,s,tp,ea)::t) => (TextIO.output(TextIO.stdOut,n ^ " : " ^ Int.toString(s) ^ " : " ^ tp ^ " : " ^ ea ^ "\n");print_entries(t))
           | z => ()
       in
           (print_entries(temp_table);TextIO.output(TextIO.stdOut,"\n"))
       end

       fun update_Scope(name,oldscope,newscope) =
         let
           fun lookup_in_Table(name,valid_scope,table,i) = case table of
             (Entry(n,s,tp,ea)::t) => if (name = n andalso s = valid_scope) then (i,(n,s,tp,ea)) else lookup_in_Table(name,valid_scope,t,i+1)
             | z => (~1,("",~1,"",""))
           val index = #1(lookup_in_Table(name,oldscope,!table,0))
           val entry = #2(lookup_in_Table(name,oldscope,!table,0))
           val new_entry = Entry(#1entry,newscope,#3entry,#4entry)
         in
           (table := List.concat([List.take(!table,index),[new_entry],List.drop(!table,index+1)]))
         end
end
