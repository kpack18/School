structure Compiler:
sig
  val parse: string list -> string
end =
struct
exception CompilerError;

structure CompilerLrVals =
  CompilerLrValsFun(structure Token = LrParser.Token)

structure CompilerLex =
  CompilerLexFun(structure Tokens = CompilerLrVals.Tokens);

structure CompilerParser =
  Join(structure LrParser = LrParser
       structure ParserData = CompilerLrVals.ParserData
       structure Lex = CompilerLex)

    fun parse command_Line =
    let val filename = List.last(command_Line)
        val commands = if length(command_Line) > 1 then tl(List.rev(command_Line)) else []
        val file = TextIO.openIn filename
        fun get _ = TextIO.input file
        val print_error : string * int * int -> unit =
          fn (msg, line, col) => (
          print ("LINE "^Int.toString(line)^":"^Int.toString(col)^
                 " ** ERROR: "^msg^"\n");
                errorMsgs.push_Entry("** ERROR:"^Int.toString(line)^":"^Int.toString(col)^
                 ": "^msg^"\n", line));
        val lexer = LrParser.Stream.streamify (CompilerLex.makeLexer get)
        val (result, _) = CompilerParser.parse(30,lexer,print_error,())

        val parsetree = parse_Tree.createTree();
        val setscopes = create_Scope.create_Scopes();
        val _ = error_checking.error_check()

        val errors = errorMsgs.get_String_List()
        val errors = []

        fun optOn() =
          let
            fun iter(lst) =
              case lst of
                [] => false
              | x::xs => if(x="-opt") then true else iter(xs)
          in
            iter(CommandLine.arguments())
          end

          val _ = Quad.ir();
          val _ = Quad.gen_Code_String()
          val assembly = Assembly.Assembly_Code(filename)

        fun check_Commands(commands) = case commands of
           ("-st"::t) => (symbol_Table.write_Table(filename ^ ".st");check_Commands(t))
          |("-asc"::t) => (source_Code.generate_Asc(filename,false,errors);check_Commands(t))
          |("-pasc"::t) => (source_Code.generate_Asc(filename,true,errors);check_Commands(t))
          |("-par=1"::t) => (parse_Tree.print_Tree();check_Commands(t))
          |("-par=2"::t) => (parse_Tree.print_Tree2();check_Commands(t))
          |("-pt"::t) => (symbol_Table.print_Table();check_Commands(t))
          |("-pst"::t) => (scope_Table.print_Scope();check_Commands(t))
          |("-ir"::t) =>
            if(optOn()) then
              check_Commands(t)
             else
              (Quad.writeFile(filename ^ ".ir");check_Commands(t))
          |(x::"-opt"::t) => (Quad.writeFile(filename ^ ".ir");
                Opt.optimize(x,filename ^ ".ir"); check_Commands(t))
          |("-ps"::t) => (Assembly.print_All();check_Commands(t))
          |("-all"::t) => (check_Commands(["-st","-asc","-ir"]);check_Commands(t))
          |(h::t) => check_Commands(t)
          |[] => ()
        val _ = check_Commands(commands)
     in TextIO.closeIn file;
         result
    end handle LrParser.ParseError => "Compiler Error"

end

val _ = print("running...\n")
val _ = Compiler.parse(CommandLine.arguments())
val _ = print("done\n")
