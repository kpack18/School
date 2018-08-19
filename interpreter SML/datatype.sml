
   (* The primitive types defined in the language. These elements are pushed onto the stack and passed into the function calls. *)
   datatype element = INT of int | STRING of string | NAME of string | BOOL of bool | ERROR | UNIT | NULL

   (* The functions defined in the language. ReturnIO, FunEndIO cannot be inputted by the user. The input file must end in a Quit function to signal
      the termination of the interpreter. *)
   datatype step = Push of element |Pop |Add |Sub |Mul |Div |Rem |Neg |Swap |And |Or |Not |Equal |LessThan |If
                  |Bind |Let |End |Fun of string * element |FunEnd |Call |Return |InOutFun of string * element
                  |ReturnIO of element * element |FunEndIO of element * element |Quit

   (* Function: Prints the inputted Instruction list into the terminal*)
   fun print_i(inst_list,indent) = case (if not (inst_list = []) then print(indent) else ();inst_list) of
      Pop::t => (print("Pop\n");print_i(t,indent))
      | Add::t => (print("Add\n");print_i(t,indent))
      | Sub::t => (print("Sub\n");print_i(t,indent))
      | Mul::t => (print("Mul\n");print_i(t,indent))
      | Div::t => (print("Div\n");print_i(t,indent))
      | Rem::t => (print("Rem\n");print_i(t,indent))
      | Neg::t => (print("Neg\n");print_i(t,indent))
      | Swap::t => (print("Swap\n");print_i(t,indent))
      | And::t => (print("and\n");print_i(t,indent))
      | Or::t => (print("or\n");print_i(t,indent))
      | Not::t => (print("not\n");print_i(t,indent))
      | Equal::t => (print("equal\n");print_i(t,indent))
      | LessThan::t => (print("lessThan\n");print_i(t,indent))
      | If::t => (print("if\n");print_i(t,indent))
      | Bind::t => (print("bind\n");print_i(t,indent))
      | Let::t => (print("let\n");print_i(t,indent))
      | End::t => (print("end\n");print_i(t,indent))
      | Quit::t => print("Quit\n")
      | Fun(s,e)::t => (case e of NAME(a) => print("fun " ^ s ^ " " ^ a ^ "\n") | z => (); print_i(t,indent))
      | InOutFun(s,e)::t => (case e of NAME(a) => print("inOutFun " ^ s ^ " " ^ a ^ "\n") | z => (); print_i(t,indent))
      | FunEnd::t => (print("funEnd\n");print_i(t,indent))
      | Call::t => (print("call\n");print_i(t,indent))
      | Return::t => (print("return\n");print_i(t,indent))
      | ReturnIO(a,b)::t => (print("returnIO\n");print_i(t,indent))
      | FunEndIO(a,b)::t => (print("funEndIO\n");print_i(t,indent))
      | Push(h)::t => (case h of INT(a) => (print("Push INT " ^ Int.toString(a) ^ "\n"))
                              | STRING(a) => (print("Push STRING " ^ a ^ "\n"))
                              | NAME(a) => (print("Push NAME " ^ a ^ "\n"))
                              | BOOL(a) => (print("Push BOOL :" ^ Bool.toString(a) ^ ":\n"))
                              | ERROR => (print("Push ERROR :error:\n"))
                              | UNIT => (print("Push UNIT :error:\n"))
                              | z => ();  print_i(t,indent))
      | z => ()

   (* Similar structure to the stack environments purpose the list stores the name (string) / element bindings.
      The front most list represents the current environment. *)
   val bind_env: (string * element) list ref list ref = ref [ref []]

   (* Input: a string (variable name) and an element to bind to it.
      Function: Stores a element within a variable of the given string. Can be used in place of primitive types for computation.
                Can overwrite existing bindings of the same name. *)
   fun add_Bind(string,elem) = hd(!bind_env) := (string,elem) :: !(hd(!bind_env))

   (* Prints the binding environment to the terminal. *)
   fun print_bindings() =
     let
       fun print_b(bind_list) = case bind_list of
          (s,INT(x))::t => (print(s ^ ": " ^ Int.toString(x) ^ "\n");print_b t)
         |(s,STRING(x))::t => (print(s ^ ": " ^ x ^ "\n");print_b t)
         |(s,BOOL(x))::t => (print(s ^ ": " ^ Bool.toString(x) ^ "\n");print_b t)
         |(s,UNIT)::t => (print(s ^ ": :unit:" ^ "\n");print_b t)
         | z => ()
       fun print_env(env_list,i) = case env_list of
         h::t => (print("------------" ^ Int.toString(i) ^ "-----------\n");print_b(!h);print_env(t,i+1))
         | z => ()
     in
       (print_env(!bind_env,1);print("------------------------\n"))
     end

   (* Iterates through each existing binding list and returns the element bound to the inputted variable name.
      If none exists then ERROR is returned. (Note a name cannot be bound to ERROR). *)
   fun lookup_bind(string) =
     let
       (* Iterates through each sublist comparing the entries to the inputted string (name of binding). *)
       fun search(list) = case list of
         (s,e)::t => if string = s then e else search(t)
         | z => ERROR
       (* Iterates through each list stored in the environment. *)
       fun search_bind(bind_list) = case bind_list of
         h::t => if search(!h) = ERROR then search_bind(t) else search(!h)
         | z => ERROR
     in
       search_bind(!bind_env)
     end

   (* Retrieves the sml primitive type behind an element type *)
   fun get_INT(elem) = case elem of INT(y) => y | z => 0
   fun get_BOOL(elem) = case elem of BOOL(y) => y | z => false
   fun get_STR_NAME(elem) = case elem of STRING(y) => y | NAME(y) => y | z => ""

   (* Input: an element and a string of a primitive type to compare it to.
      Function: returns a boolean based on if an element matches the type of the given string. *)
   fun check_Type(elem,tp) = case elem of
     INT(y) => tp = "int"
     | BOOL(y) => tp = "bool"
     | STRING(y) => tp = "string"
     | UNIT => tp = "unit"
     | ERROR => tp = "error"
     | z => false

                (* Name    Param     step list  *)
                (*Name    Param     In/Out?  Func List *)

   (* Similar to the stack and bindings, this houses the defined functions available. *)
   val fun_env: (string * element * bool * (step list)) list ref list ref = ref [ref []]

   (* Input: string = name of the function
             element = the defined parameter
             inOut = Boolean of whether it's an inOutFun
             step_list = the steps contained within the function definition.
      Function: Appends a new function definition to the current environment. Can overwrite existing functions with the same name. *)
   fun add_Fun(string,element,inOut,step_list) = hd(!fun_env) := (string,element,inOut,step_list) :: !(hd(!fun_env))

   (* Function: Same effect as the lookup_bind function. If an existing function is found matching the given name then
                the parameter, io boolean, and step list is returned. *)
   fun lookup_fun(name) =
     let
       fun search_fun(list) = case list of
         (n,p,io,l)::t => if name = n then (p,io,l) else search_fun(t)
         | z => (ERROR,false,[])
       fun search_fenv(fun_env) = case fun_env of
         h::t => if search_fun(!h) = (ERROR,false,[]) then search_fenv(t) else search_fun(!h)
         | z => (ERROR,false,[])
     in
       search_fenv(!fun_env)
     end

   (* Prints the functions stored in memory to the terminal after execution. *)
   fun print_functions() =
     let
       fun print_f(bind_list) = case bind_list of
         (n,p,io,l)::t => (print( (if io then "inOutFun " else "fun ") ^ n ^ "(" ^ get_STR_NAME(p) ^ "):\n");print_i(l,"  ");print("  funEnd\n");print_f(t))
         | z => ()
       fun print_fenv(env_list,i) = case env_list of
         h::t => (print("------------" ^ Int.toString(i) ^ "-----------\n");print_f(!h);print_fenv(t,i+1))
         | z => ()
     in
       (print_fenv(!fun_env,1);print("------------------------\n"))
     end
