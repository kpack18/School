(* Stack: contains the stack data structure itself and the defined commands in the language *)
structure Stack =
struct

  (* Each sublist contains a new stack. When a subtask (or let/end) is entered a new stack is
     appended to the front of it. All commands below are applied to the frontmost stack. *)
  val environments: element list ref list ref = ref [ref []]

  (* Shortcut to access the pointer to the frontmost stack *)
  fun slist() = hd(!environments)
  (* Shortcut to access the data behind the pointer to the frontmost stack *)
  fun ref_slist() = !(hd(!environments))

  (* push: Input: a stack element. Output: Unit
           Function: appends an element to the top of the stack *)
  fun push(e: element) = slist() := e :: ref_slist()

  (* pop: Function: removes the top most element of the stack. If no element exists Error is pushed to the stack *)
  fun pop() = if length(ref_slist()) > 0 then slist() := tl(ref_slist()) else push(ERROR)

  (* bin: Input: a function that is applied to the top two most elements of the stack
           Function: Used to apply a function to two elements on top of the stack. The elements must be of
                     type integer or as a Name bound to an integer. If not then ERROR is pushed to the stack.
                     If the types match then the function is applied to the elements and replaces them on top of the stack *)
  fun bin(func) = case ref_slist() of
     (NAME(y)::INT(x)::t) => if check_Type(lookup_bind(y),"int") then slist() := func(get_INT(lookup_bind(y)),x) :: List.drop(ref_slist(),2) else push ERROR
    |(INT(y)::NAME(x)::t) => if check_Type(lookup_bind(x),"int") then slist() := func(y,get_INT(lookup_bind(x))) :: List.drop(ref_slist(),2) else push ERROR
    |(NAME(y)::NAME(x)::t) => if check_Type(lookup_bind(y),"int") andalso check_Type(lookup_bind(x),"int") then slist() := func(get_INT(lookup_bind(y)),get_INT(lookup_bind(x))) :: List.drop(ref_slist(),2) else push ERROR
    |(INT(y)::INT(x)::t) => slist() := func(y,x) :: List.drop(ref_slist(),2)
    | z => push ERROR

  (* Arithmatic Operations. Inputted to the binary function above. Note divd = divison and rem = modulus.
                            Divison and Modulus push Error if it divides by zero *)
  fun add() = bin( fn(y,x) => INT(x+y) )
  fun sub() = bin( fn(y,x) => INT(x-y) )
  fun mul() = bin( fn(y,x) => INT(x*y) )
  fun divd() = bin( fn(y,x) => if y = 0 then ERROR else INT(x div y) )
  fun rem() = bin( fn(y,x) => if y = 0 then ERROR else INT(x mod y) )
  (* Appends a boolean based on if the elements are identical *)
  fun equal() = bin( fn(y,x) => BOOL(y = x) )
  (* Appends a boolean based on if the top element is greater then the 2nd most *)
  fun lessThan() = bin( fn(y,x) => BOOL(x < y) )

  (* Negates (mult by ~1) the top element of the stack if it's an integer/Name. Otherwise it pushes ERROR*)
  fun neg() = case ref_slist() of
      (NAME(y)::t) => if check_Type(lookup_bind(y),"int") then slist() := INT(~1 * get_INT(lookup_bind(y)) ) :: tl(ref_slist()) else push ERROR
    | (INT(y)::t) => slist() := INT(~y) :: tl(ref_slist())
    | z => push ERROR

  (* Swaps the positions of the top two elements of the stack regardless of type. Pushes Error if two elements don't exist *)
  fun swap() = case ref_slist() of
    (y::x::t) => slist() := x::(y::List.drop(ref_slist(),2))
    | z => push ERROR

  (* Effectivly the same function as bin but is applied to Boolean functions as opposed to integer. *)
  fun logic(func) = case ref_slist() of
     (NAME(y)::BOOL(x)::t) => if check_Type(lookup_bind(y),"bool") then slist() := func(get_BOOL(lookup_bind(y)),x) :: List.drop(ref_slist(),2) else push ERROR
    |(BOOL(y)::NAME(x)::t) => if check_Type(lookup_bind(x),"bool") then slist() := func(y,get_BOOL(lookup_bind(x))) :: List.drop(ref_slist(),2) else push ERROR
    |(NAME(y)::NAME(x)::t) => if check_Type(lookup_bind(y),"bool") andalso check_Type(lookup_bind(x),"bool") then slist() := func(get_BOOL(lookup_bind(y)),get_BOOL(lookup_bind(x))) :: List.drop(ref_slist(),2) else push ERROR
    |(BOOL(y)::BOOL(x)::t) => slist() := func(y,x) :: List.drop(ref_slist(),2)
    | z => push ERROR

  (* Logical Operations *)
  fun And() = logic( fn(y,x) => BOOL(y andalso x))
  fun Or() = logic( fn(y,x) => BOOL(y orelse x))
  fun Not() = case ref_slist() of
    (NAME(y)::t) => if check_Type(lookup_bind(y),"bool") then slist() := BOOL(not (get_BOOL(lookup_bind(y))) ) :: tl(ref_slist()) else push ERROR
    |(BOOL(y)::t) => slist() := BOOL(not y) :: tl(ref_slist())
    | z => push ERROR

  (* Envolves three elements on the stack. The bottom most must be a boolean while the other two can be any type.
     If the boolean is true then the top most element is pushed back on the stack. If false then the 2nd element is pushed.
     If the third element isn't a bool or three elements don't exist then ERROR is pushed. *)
  fun If() = case ref_slist() of
     (x::y::NAME(z)::t) => if check_Type(lookup_bind(z),"bool") then slist() := (if get_BOOL(lookup_bind(z)) then x else y) :: List.drop(ref_slist(),3) else push ERROR
    |(x::y::BOOL(z)::t) => slist() := (if z then x else y) :: List.drop(ref_slist(),3)
    | z => push ERROR

  (* Binds any non ERROR element to a Name as a variable. If two names are used then the top most name must have a value bound to it to copy.
     if the 2nd element isn't a name or two elements don't exist then ERROR is pushed.
     The binding is stored in a list within datatype.sml. Once bound names can be re pushed to the stack and used as variables in computations. *)
  fun bind() = case ref_slist() of
      ERROR::NAME(y)::t => push ERROR
    | NAME(x)::NAME(y)::t => if check_Type(lookup_bind(x),"error") then push ERROR else (add_Bind(y,lookup_bind(x));slist() := UNIT :: List.drop(ref_slist(),2) )
    | x::NAME(y)::t => (add_Bind(y,x);slist() := UNIT :: List.drop(ref_slist(),2) )
    | z => push ERROR


  (* Creates a new environment to control the lifetime of variables and functions. Must be followed by a closing end step.
     Function: A new list is appended to the stack environment and all binding/function storage lists.
     Follows the structure of scopes in most languages (larger scopes are available in smaller ones but not vice versa) *)
  fun stack_Let() = (environments := ref [] :: !environments; bind_env := ref [] :: !bind_env; fun_env := ref [] :: !fun_env)

  (* Input: a boolean determining whether the top most element from the current environ ment should be pushed onto the new one after removing it.
     Function: Ends the current environment by removing the frontmost environment list and binding/function lists. Thus the stored functions and bindings
     are also deleted in the process. If no element exists on the stack when trying to return a value then nothing happens *)
  fun stack_End(return) =
    let
      val peak = if ref_slist() = [] then NULL else hd(ref_slist())
      val _ = environments := tl(!environments)
      val _ = if not(peak = NULL) andalso return then slist() := peak :: ref_slist() else ()
      val _ = bind_env := tl(!bind_env)
      val _ = fun_env := tl(!fun_env)
    in
     ()
    end

  (* Input: the defined parameter of the function and the inputted argument to be bound to it. Additionally the Return Bool (see End above)
     Function: The same function as stack_End except the inputted argument to an InOutFunction must be a name. The value bound to the param in the function definition
               is bound to the argument after execution. If the defined param isn't bound then ERROR is just pushed to the stack *)
  fun stack_EndIO(param,arg,return) =
    let
      val peak = if ref_slist() = [] then NULL else hd(ref_slist())
      val _ = environments := tl(!environments)
      val _ = if not(peak = NULL) andalso return then slist() := peak :: ref_slist() else ()
      val IObind = lookup_bind(get_STR_NAME(param))
      val _ = bind_env := tl(!bind_env)
      val _ = if IObind = ERROR then push ERROR else (push arg;push IObind;bind();pop())
      val _ = fun_env := tl(!fun_env)
    in
     ()
    end

  (* Input: The name of the function.
            The defined parameter of the function.
            Boolean determining if it's a regular or InOutFunction
            A list containing the rest of the main instruction list after the "fun" line is reached.
     Function: Extracts the inner steps of a function definition and creates a binding in memory for it to be called later (in fun_env in datatype.sml).
               Once the function is extracted UNIT is pushed to the stack and the execution continues from the step after the funEnd keyword.
               Functions can be called by pushing a Name element, an argument to input, and the call function.
               Note that it returns the main instruction list after the function definition and the execute function continues from there *)
  fun Func(name,parameter,inOut,list) =
    let
      (* Determines the length of the function definition based on where the corresponding funEnd keyword appears.
         Functions can contain inner functions. Thus they are treated as generic steps and not stored in datatype.sml *)
      fun end_Pos(list,pos,nested_funs) = case list of
        FunEnd::t => if nested_funs = 0 then pos else end_Pos(t,pos+1,nested_funs-1)
        |Fun(s,p)::t => end_Pos(t,pos+1,nested_funs+1)
        |InOutFun(s,p)::t => end_Pos(t,pos+1,nested_funs+1)
        |h::t => end_Pos(t,pos+1,nested_funs)
        | z => ~1
      val list_length = end_Pos(list,1,0)
    in
      (Push UNIT;add_Fun(name,parameter,inOut,List.take(list,list_length-1));List.drop(list,list_length))
    end

  (* Input: argument, name of function, and the execute function (from interpreter.sml).
     Function: The top two elements are popped from the stack (name/argument).

               The function stored in datatype.sml is retrieved an analyzed. To simulate the argument binding and the new environment
                a let end block and Binding steps are appended to the front and end to the list.

               If the function ends in return then like the end command a value is pushed onto the original stack after execution. Otherwise a
                funEnd command will restore the state of the stack/bindings/function data without pushing a value. If it's an IO function
                the value bound to the parameter given in the definition will be bound to the inputted argument given with the call. Thus a ReturnIO or funEndIO will be appended instead.

               Finally once the new step list has been created (func_list) it is run through the execute method.
                After execution the execute method continues from after the call as normal.  *)
  fun callFun(argument,name,execute_func) =
    let
      val _ = (pop();pop())
      val fun_record = lookup_fun(name)

      val parameter = #1 fun_record
      val io = #2 fun_record
      val func_list = #3 fun_record

      val front_list = [Let,Push(parameter),Push(argument),Bind,Pop]

      fun IOList() =
        let
          val End_list = if List.last(func_list) = Return then [ReturnIO(parameter,argument)] else [FunEndIO(parameter,argument)]
          val func_list = if List.last(func_list) = Return then List.take(func_list,length(func_list)-1) else func_list
        in
          List.concat([front_list,func_list,End_list])
        end
      fun Fun_List() =
        let
          val End_list = if List.last(func_list) = Return then [] else [FunEnd]
        in
          List.concat([front_list,func_list,End_list])
        end

      val func_list = if io then IOList() else Fun_List()
    in
      execute_func(func_list)
    end

 (* Calls a function stored in memory with the matching name of the top element. The 2nd element is a parameter passed into the function.
    The name is looked up in memory and if a function exists it is called and run. Otherwise Error is pushed to the stack.
    Similar to let/end blocks a function creates a new environment with the parameter/arg binding stored in it and an empty stack. *)
  fun Call(execute_func) = case ref_slist() of
    NAME(f)::h::t => if #1(lookup_fun(f)) = ERROR then push ERROR else callFun(h,f,execute_func)
    | z => push ERROR

  (* Prints the Contents of the stack environment to the terminal after execution *)
  fun print_stack() = let
    fun print_list(slist) = case slist of
       INT(h)::t => (print("Int: " ^ Int.toString(h) ^ "\n");print_list t)
      | STRING(h)::t => (print("String: " ^ h ^ "\n");print_list t)
      | NAME(h)::t => (print("Name: " ^ h ^ "\n");print_list t)
      | BOOL(b)::t => (print("Bool: " ^ Bool.toString(b) ^ "\n");print_list t)
      | UNIT::t => (print "Unit\n";print_list t)
      | ERROR::t => (print "Error\n";print_list t)
      | z => ()
    fun print_env(stack_list,i) = case stack_list of
      h::t => (print("------------" ^ Int.toString(i) ^ "-----------\n");print_list(!h);print_env(t,i+1))
      | z => ()
    in
      (print "Stack:\n";print_env(!environments,1);print("------------------------\n"))
    end

end
