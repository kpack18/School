package interpreter;
/**
 * Class: Main.java
 * 
 * Function: Recieves a stream of Instructions from "syntax" and feeds them
 * to the "stack" and executes the appropriate function based on the Instruction type.
 * After execution the state of the stack and it's data storage are printed to the terminal.
 * 
 * @author Kyle Kolpack
 * 
 */
public class Main {
	
	private static String inputfile = "input.txt";
	private static Syntax syntax = new Syntax(inputfile);
	private static My_Stack stack = new My_Stack();
	
	/**
	 * Execute:
	 * @param i An Instruction to be run on the stack.
	 * 
	 * @function: Given an Instruction, the corresponding method on the stack is run. The type of function is determined by the String command 
	 * within the Instruction class.
	 */
	public static void execute(Instruction i) {
		switch(i.command) {
		case "push": stack.push(i.val); break;
		case "pop": stack.pop(); break;
		case "add": stack.binary("add", "Int", "Int"); break;
		case "sub": stack.binary("sub", "Int", "Int"); break;
		case "mul": stack.binary("mul", "Int", "Int"); break;
		case "div": stack.binary("div", "Int", "Int"); break;
		case "rem": stack.binary("rem", "Int", "Int"); break;
		case "neg": stack.unary("neg", "Int"); break;
		case "swap": stack.binary("swap", "any", "any");  break;
		case "and": stack.binary("and", "Bool", "Bool"); break;
		case "or": stack.binary("or", "Bool", "Bool"); break;
		case "not": stack.unary("not", "Bool"); break;
		case "equal": stack.binary("equal", "Int", "Int"); break;
		case "lessThan": stack.binary("lessThan", "Int", "Int"); break;
		case "bind": stack.binary("bind", "any", "Name"); break;
		case "if": stack.If(); break;
		case "let": stack.Let(); break;
		case "fun": stack.Fun("fun", i.fun_Name, i.val, syntax); break;
		case "inOutFun": stack.Fun("inOutFun", i.fun_Name, i.val, syntax); break;
		case "end": stack.End(true, i.val, i.val2, false); break;
		case "return": stack.End(true, i.val, i.val2, false); break;
		case "funEnd": stack.End(false, i.val, i.val2, false); break;
		case "returnIO": stack.End(true, i.val, i.val2, true); break;
		case "funEndIO": stack.End(false, i.val, i.val2, true); break;
		case "call": stack.binary("call", "any", "any"); break;
		case "quit": break;
		default: System.out.println("Did not recognize command: \"" + i.command + "\"\n"); break;
		}
	}
	
	/**
	 * Main:
	 * @param args No effect. "inputfile" private variable used to store name of input variable.
	 * 
	 * Function: Provided an input file Lines are extracted 1 at a time and converted to Instructions (syntax.java).
	 * 
	 * 			 The returned instructions pertain to a method within the stack and is run through execute till a terminal "quit" instruction is reached.
	 *           The state of the system is then printed to the terminal.  
	 */
	public static void main(String args[]) {
		inputfile = "input.txt";
		syntax = new Syntax(inputfile);
		stack = new My_Stack();
		
		String line;
		while((line = syntax.get_Line()) != "") {
			Instruction in = syntax.get_Instruction(line);
			execute(in);
		}
		String output_String = stack.print_Stack() + stack.print_Data();
		syntax.output_String(output_String);
	}
	
	/**
	 * print_Instruction:
	 * @param in: An Inputed Instruction
	 * 
	 * Function: As instructions are executed they are printed to the terminal (System.out) to show the list of processed instructions.
	 * Given "in", the information stored within the instruction is printed based on the "command" type.
	 * 
	 * -push instructions have an element stored in them which needs to be displayed
	 * -function commands have a name and parameter stored in them which is displayed
	 * -otherwise the command is simply printed
	 * 
	 * Format: Instructions:
	 *         --------------
	 *         fun name1 x
	 *         push 4
	 *         push 8
	 *         add
	 *         funEnd
	 *         push name1
	 *         push 4
	 *         call
	 *         etc...
	 *         quit
	 *         --------------
	 */
	public static void print_Instruction(Instruction in) {
		if(in.command == "push") { System.out.println(in.command + " " + in.val.get_Text()); }
		else if(in.command == "fun") { System.out.println(in.command + " " + in.fun_Name +  " " + in.val.sn); }
		else { System.out.println(in.command); }
	}
}
