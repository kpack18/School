(* Interpreter: The main class, Opens the input instructions and runs them through a syntax analyzer (syntax.sml)
                Then executes the synthesised instruction set on the stack *)
structure interpreter =
struct

 (* Execute: Input: a list of Instructions correpsonding to a function within the stack structure.
             Output: Unit
             Function: Recursivly Runs each command within the inputted instruction list on the stack structure. Each case pertains to a function
                       defined within the provided stack based language.
    *Note that Fun(n,p) returns a new subtask instruction list (corresponding to the function called) and executes it before continuing
    *Note that the boolean value passed into the End and Return functions controls whether the top of the stack is
      added back into the environment after the let end/function ends.
    *Note that ReturnIO and FunEndIO store the parameter in the function definition and the inputted argument to be bound to the parameter *)
  fun execute(instructions) = case instructions of
     Push(h)::t => (Stack.push(h);execute(t))
   | Pop::t => (Stack.pop();execute(t))
   | Add::t => (Stack.add();execute(t))
   | Sub::t => (Stack.sub();execute(t))
   | Mul::t => (Stack.mul();execute(t))
   | Div::t => (Stack.divd();execute(t))
   | Rem::t => (Stack.rem();execute(t))
   | Neg::t => (Stack.neg();execute(t))
   | Swap::t => (Stack.swap();execute(t))
   | And::t => (Stack.And();execute(t))
   | Or::t => (Stack.Or();execute(t))
   | Not::t => (Stack.Not();execute(t))
   | Equal::t => (Stack.equal();execute(t))
   | LessThan::t => (Stack.lessThan();execute(t))
   | If::t => (Stack.If();execute(t))
   | Bind::t => (Stack.bind();execute(t))
   | Let::t => (Stack.stack_Let();execute(t))
   | End::t => (Stack.stack_End(true);execute(t))
   | Return::t => (Stack.stack_End(true);execute(t))
   | Fun(n,p)::t => execute(Stack.Func(n,p,false,t))
   | InOutFun(n,p)::t => execute(Stack.Func(n,p,true,t))
   | FunEnd::t => (Stack.stack_End(false);execute(t))
   | ReturnIO(param,arg)::t => (Stack.stack_EndIO(param,arg,true);execute(t))
   | FunEndIO(param,arg)::t => (Stack.stack_EndIO(param,arg,false);execute(t))
   | Call::t => (Stack.Call(execute);execute(t))
   | Quit::t => ()
   | z => () (* When the instructions reach the [] state it will terminate here *)

 (* Main: Opens the stream to the input file given by "infile".
    Creates a list of Instructions and feeds it through the execute method to recursivly run each command.
    After Execution completes the state of the stack and it's data are printed to the terminal. *)
  fun main() =
    let
      val infile = "input.txt"
      val instream = TextIO.openIn infile
      val instructions = syntax.get_instructions(instream)
      val _ = execute(instructions)
      val _ = (print("------Instructions------\n");print_i(instructions,""))
      val _ = Stack.print_stack()
      val _ = (print("bindings:\n");print_bindings())
      val _ = (print("functions:\n");print_functions())
    in
      ()
    end

end

val _ = interpreter.main()
