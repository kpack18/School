package interpreter;

import java.util.ArrayList;
import java.util.Stack;
/**
 * Class: My_Stack.java
 * 
 * @function Contains all methods that effect the stack and applies them to it's own saved structures.
 * 
 * @author Kyle Kolpack
 * 
 */
public class My_Stack {
	
	private Stack<Stack<Elem>> environment;
	private Stack<Elem> stack; // CHANGE TO STACK<INSERT TYPE>
	private Data d;
	
	/**
	 * My_Stack:
	 * 
	 * @constructor Creates a new stack, environment, and data variables.
	 *              
	 *              environment: a list of stacks in which the top most is the current available environment. Functions and let/end blocks declare new
	 *              environments when used providing an empty stack and variable binding environment. Variable bindings from older stacks can still be used
	 *              in newer ones. (Replicates an inner scope).
	 *              
	 *              stack: a standard java stack in which the defined methods are applied to. The stack is always the top most entry in the environment.
	 *              
	 *              d: an instance of the data.java class. This stores the binding lists in which variable values are stored. Using the bind function a
	 *              Name Elem can be bound to any other valid Elem (including values behind other bindings) as well as functions. Once bound the name can be
	 *              re pushed to the stack to be used in computations.
	 */
	public My_Stack() {
		stack = new Stack<Elem>();
		environment = new Stack<Stack<Elem>>();
		environment.add(stack);
		d = new Data();
	}
	
	/**
	 * push:
	 * @param e An Elem type
	 * 
	 * @function pushes the inputed element onto the stack
	 */
	public void push(Elem e) {
		stack.push(e);
	}
	
	/**
	 * pop:
	 * @function removes the top most element from the stack
	 */
	public void pop() {
		if(stack.isEmpty()){ push(new Elem("Error")); return; }
		stack.pop();
	}
	
	/**
	 * take_Elem:
	 * @return a shortcut method used to return the top most element of the stack and then remove it.
	 */
	public Elem take_Elem() {
		Elem peek = stack.peek();
		stack.pop();
		return peek;
	}
	
	/**
	 * binary:
	 * @param com_Type The type of the command used to calculate the new value pushed onto the stack
	 * @param elem_Type1 The valid type of the topmost element
	 * @param elem_Type2 The valid type of the second most element
	 * 
	 * @function The binary method handles any inputed function to the stack that involves two elements on the stack.
	 * 
	 *           First there must be two elements on the stack otherwise the "Error" is pushed onto the stack
	 *           
	 *           Then the two top most elements are popped off and their types are checked.
	 *           If either element is a Name then it must have a valid binding in d to use in computation. If no value is found then the
	 *           two Elem's are returned and Error is pushed. If a binding is found then that value is used in the rest of the computation.
	 *           
	 *           The types must match the inputed elem_Type1 and 2 else Error will be pushed to the stack (and the Elem's are returned).
	 *           Note: if the certain functions like swap and If don't require certain types so "any" is inputed to ignore the check.
	 *           
	 *           If the types pass the type check then the corresponding computation is performed based on the inputed com_Type within the switch stmt.
	 *            
	 *            | Com_Type:  | Elem Types: |                                          Effect:                                                             |
	 *            | "add"      | Int , Int   | Pushes the sum of the two Elem's                                                                             |
	 *            | "sub"      | Int , Int   | Pushes the difference of the two Elem's                                                                      |
	 *            | "mul"      | Int , Int   | Pushes the product of the two Elem's                                                                         |
	 *            | "div"      | Int , Int   | Pushes the floor of the quotient of the two Elem's. If the top Elem is 0 then Error is pushed (divide by 0)  |
	 *            | "rem"      | Int , Int   | Pushes the modulus of the two Elem's. If the top Elem is 0 then Error is pushed (divide by 0)                |
	 *            | "swap"     | Any , Any   | Swaps the position of the top two Elem's regardless of type ( y , x => x , y )                               |
	 *            | "and"      | Bool , Bool | Pushes the logical and of the two Elem's                                                                     |
	 *            | "or"       | Bool , Bool | Pushes the logical or of the two Elem's                                                                      |
	 *            | "equal"    | Int , Int   | Pushes a Bool true if the two int's are equal and false otherwise                                            |
	 *            | "lessThan" | Int , Int   | Pushes a Bool true if the second most element is less then the Top and false otherwise                       |
	 *            | "bind"     | Any , Name  | Creates a binding for the variable Name (Top Cannot be Error). Pushes Unit afterward to the stack            |
	 *            |            |             | If the Name given is already bound then it will be overwritten. If Any is a Name then it will bind to a      |
	 *            |            |             | value bound to Any if one exists.                                                                            |  
	 *            | "call"     | Name , Any  | Name must be bound to a function. If the function is an "inOutFun" then Any must be a name. Executes the     |
	 *            |            |             | code saved in a function binding with the passed in argument "Any". Note when a function is created the      |
	 *            |            |             | first few lines are reserved for binding the formal parameter to the argument. The bind command within the   |
	 *            |            |             | function list is edited so the argument is pushed and bound here.                                            |
	 */
	public void binary(String com_Type, String elem_Type1, String elem_Type2) {
		if(stack.size() < 2) { push(new Elem("Error")); return; }
		Elem y = take_Elem();
		Elem x = take_Elem();
		
		Elem y_t = y;
		Elem x_t = x;
		
		if(y.type == "Name" && com_Type != "swap") {
			Elem tempy = d.lookup_Bind(y.sn);
			if(tempy != null) {
				y_t = tempy;
			}
			else {
				push(x);
				push(y);
				push(new Elem("Error"));
				return;
			}
		}
		
		if(x.type == "Name" && com_Type != "bind" && com_Type != "swap" ) {
			Elem tempx = d.lookup_Bind(x.sn);
			if(tempx != null) {
				x_t = tempx;
			}
			else {
				push(x);
				push(y);
				push(new Elem("Error"));
				return;
			}
		}
		
		if( (y_t.type != elem_Type1 && elem_Type1 != "any") || (x_t.type != elem_Type2 && elem_Type2 != "any") ) {
			push(x);
			push(y);
			push(new Elem("Error"));
			return;
		}
		
		Elem answer = new Elem();
		switch(com_Type) {
		case "add": answer.type = "Int";
		            answer.i = y_t.i + x_t.i;
		            push(answer);
		            break;
		case "sub": answer.type = "Int";
                    answer.i = x_t.i - y_t.i;
                    push(answer);
                    break;
		case "mul": answer.type = "Int";
                    answer.i = x_t.i * y_t.i;
                    push(answer);
                    break;
		case "div": answer.type = "Int";
		            if(y.i == 0) { push(new Elem("Error")); break; }
                    answer.i = Math.floorDiv(x_t.i,y_t.i);
                    push(answer);
                    break;
		case "rem": answer.type = "Int";
                    if(y.i == 0) { push(new Elem("Error")); break; }
                    answer.i = x_t.i % y_t.i;
                    push(answer);
                    break;
		case "swap": push(y);
		             push(x);
		             break;
		case "and": answer.type = "Bool";
			        answer.b = y_t.b && x_t.b;
		            push(answer);
		            break;
		case "or": answer.type = "Bool";
                    answer.b = y_t.b || x_t.b;
                    push(answer);
                    break;
		case "equal": answer.type = "Bool";
		              answer.b = (y_t.i == x_t.i);
		              push(answer);
		              break;
		case "lessThan": answer.type = "Bool";
		                 answer.b = (x_t.i < y_t.i);
		                 push(answer);
		                 break;
		case "bind": d.add_Binding(x_t.sn, y_t);
		             push(new Elem("Unit"));
		             break;
		case "call": ArrayList<Instruction> templ = y_t.fun_List;
			         templ.get(2).val = x_t;
			         templ.get(templ.size()-1).val2 = x;
			         
			         for(int i = 0; i < templ.size(); ++i) {
			        	 Main.execute(templ.get(i)); 
			         }
			         
			         break;
        default: push(new Elem("Error"));
                 break;
		}
	}
	
	/**
	 * unary:
	 * @param com_Type The type of the command used to calculate the new value pushed onto the stack
	 * @param elem_Type1 The valid type of the topmost element
	 * 
	 * @function handles defined single Elem functions.
	 *  
	 *           First there must be at least 1 Elem on the stack. Otherwise Error is Pushed
	 *           
	 *           Then the elem is type checked. If it is a name it must have a valid binding and the value behind it must then be equivilent to the inputed
	 *           elem_Type. If it is not the correct type then the Elem is returned and Error is pushed onto the stack.
	 *           
	 *           If it is the correct type it then executes the correct function based on the inputed com_Type:
	 *           
	 *                             | Com_Type:  | Elem Type: |                     Effect:                        |
	 *                             | "neg"      |    Int     | Pushes the negation of the Elem onto the stack.    |
	 *                             | "not"      |    Bool    | Pushes the logical not of the Elem onto the stack. |
	 */  
	public void unary(String com_Type, String elem_Type) {
		if(stack.size() < 1) { push(new Elem("Error")); return; }
		Elem y = take_Elem();
		
		if(y.type != elem_Type && elem_Type != "any") { 
			push(y);
			push(new Elem("Error")); 
			return; 
		}
		
		Elem answer = new Elem();
		switch(com_Type) {
		case "neg": answer.type = "Int";
		            answer.i = -y.i;
		            push(answer);
		            break;
		case "not": answer.type = "Bool";
		            answer.b = !y.b;
		            push(answer);
		            break;
		}
	}
	
	/**
	 * If:
	 * @function If is the only three Elem operation defined in the language. The bottom most elem (z) must be of type Boolean and the other two can be any type.
	 *           The Boolean val is evaulated for type checking and if it is true then the top most element is pushed onto the stack. If it's false then the
	 *           second most Elem is pushed onto the stack. If the third Elem is not a Bool then Error is pushed instead.
	 */
	public void If() {
		if(stack.size() < 3) { push(new Elem("Error")); return; }
		Elem y = take_Elem();
		Elem x = take_Elem();
		Elem z = take_Elem();
		
		Elem z_t = z;
		
		if(z.type == "Name") {
			Elem tempz = d.lookup_Bind(z.sn);
			if(tempz != null) {
				z_t = tempz;
			}
			else {
				push(z);
				push(x);
				push(y);
				push(new Elem("Error"));
				return;
			}
		}
		
		if(z_t.type != "Bool") {
			push(z);
			push(x);
			push(y);
			push(new Elem("Error"));
			return;
		}
		
		if(z_t.b) {
			push(y);
		}
		else {
			push(x);
		}
		
	}
	
	/**
	 * Let:
	 * @function a new stack is created on the environment along with a new binding entry. In creating a new Inner scope Bindings from larger ones are
	 *           still accessible however inner bindings to the same Name do not overwrite the outer one, but still supercede it in lookup bindings.
	 */
	public void Let() {
		environment.add(new Stack<Elem>());
		stack = environment.peek();
		
		d.add_Env();
	}
	
	/**
	 * End:
	 * 
	 * @param ret_Peek Boolean, if true then the top Elem of the stack is pushed onto the to of the new one after deleting the current.
	 * @param param the formal parameter of the ran function. Used in inOutFun endings. 
	 * @param arg the argument passed in to the ran function. Used in inOutFun endings.
	 * @param inOut Boolean, if true then it recognizes that an inOutFun was ran, otherwise it treats it as a normal fun or End block.
	 * 
	 * @function Several defined commands utilize the End function:
	 *             
	 *           End: deletes the current scope (current stack and binding) and pushes the top element of the deleted stack onto the new stack. 
	 *           return: same as End but only used in functions.
	 *           funEnd: deletes the current scope without pushing an element afterward.
	 *           returnIO: cannot be inputed by the user, replaces return when an inOutFun is called. In addition to the returns functions it also
	 *                     binds the inputed argument name in the new scope to the value bound to the formal parameter in the function scope.
	 *                     ex: 1.) fun name1 x runs
	 *                         2.) name1 is called with Name "a" argument
	 *                         3.) x is bound to 4 somewhere in the function
	 *                         4.) returnIO is reached and the stack/binding is deleted and the top most value is pushed to the new stack.
	 *                         5.) In the new top most binding a is bound to the value behind x (4).
	 *           funEndIO: the same function as funEnd except the argument passed in is bound to the formal parameters binding (see returnIO).
	 *           
	 *           Note: param and arg are used to fullfill the function of funEndIO and returnIO and aren't utilized in End, return, and funEnd.
	 */
	public void End(boolean ret_Peek, Elem param, Elem arg, boolean inOut) {
		Elem top = null;
		if(!stack.isEmpty()) { top = stack.peek(); }
		environment.pop();
		stack = environment.peek();
		if(ret_Peek && top != null) { stack.push(top); }
		Elem tp = new Elem();
		if(inOut) {
			tp = d.lookup_Bind(param.sn);
		}
		d.remove_Env();
		if(inOut) {
			Instruction i1 = new Instruction("push",arg);
			Instruction i2 = new Instruction("push", tp);
			Instruction i3 = new Instruction("bind");
			Instruction i4 = new Instruction("pop");
			Main.execute(i1);
			Main.execute(i2);
			Main.execute(i3);
			Main.execute(i4);
		}
	}
	
	/**
	 * Fun:
	 * 
	 * @param com_Type: Either "Fun" or "inOutFun".
	 * @param fun_Name: The name of the function being called.
	 * @param param: The formal parameter defined in the function
	 * @param syntax: The instance of the syntax class so the file can be read from.
	 * 
	 * @function When a Fun line is reached an ArrayList is created and bound to a Name in the data class.
	 *           To simulate the passing in a parameter a set of binding commands are initially put into the list. The third command is edited to push
	 *           the passed in argument at call time.
	 *           
	 *           Using the syntax class lines are recursively read from the input, sorted into instructions, and added to the arraylist until a
	 *           "funEnd" line is reached signaling the end of the function. Since return and funEnd are similar to the End function the function
	 *           begins with a let command and will end with return or funEnd (added if return doesn't exist in the input). or their IO equivilents.
	 *           
	 *           Once the list is built a binding is created in the data class and Unit is pushed to the stack.
	 */
	public void Fun(String com_Type, String fun_Name, Elem param, Syntax syntax) {
		ArrayList<Instruction> il = new ArrayList<Instruction>();
		il.add(new Instruction("let"));
		il.add(new Instruction("push",param));
		il.add(new Instruction("push", param)); //The command is edited to the argument is pushed instead of param.
		il.add(new Instruction("bind"));
		il.add(new Instruction("pop"));
		
		String line;
		
		int funcount = 0;
		while((line = syntax.get_Line()) != "" && !(line.equals("funEnd") && funcount < 1) ) {
			Instruction in = syntax.get_Instruction(line);
			if(in.command == "fun") { ++funcount; }
			if(in.command == "funEnd") { --funcount; }
			il.add(in);
		}
		
		if(!(il.get(il.size()-1).command == "return")) {
			if(com_Type.equals("inOutFun")) {
				Instruction tio = new Instruction("funEndIO");
				tio.val = param;
				il.add(tio);
			}
			else {
			    il.add(new Instruction("funEnd"));
			}
		}
		else {
			if(com_Type.equals("inOutFun")) {
				il.get(il.size()-1).command = "returnIO";
				il.get(il.size()-1).val = param;
			}
		}
		
		d.add_Binding(fun_Name, new Elem(il,true));
		push(new Elem("Unit"));
	}
	
	/**
	 * print_List
	 * @param instack a stack from the environment
	 * @param i an index representing the index of the stack within the environment (separates inner scopes).
	 * 
	 * @function given a list it prints the contents of it to the terminal using the Elem's get_Text() function.
	 */
	public String print_List(Stack<Elem> instack, int i) {
		String temp = "";
		System.out.println("------" + i + "-------");
		temp = temp + "------" + i + "-------\n";
		while(!instack.isEmpty()) {
			Elem top = instack.peek();
			instack.pop();
			System.out.println(top.get_Text());
			temp = temp + top.get_Text() + "\n";
		}
		return temp;
	}
	
	/**
	 * print_Stack()
	 * @function iterates through every stack in the environment and prints out it's contents using print_List() above.
	 */
	public String print_Stack() {
		String temp = "";
		int i = 1;
		System.out.println("Stack:");
		temp = temp + "Stack:\n";
		while(!environment.isEmpty()) {
			Stack<Elem> top = environment.peek();
			environment.pop();
		    temp = temp + print_List(top,i);
		    ++i;
		}
		System.out.println("--------------");
		temp = temp + "--------------\n";
		return temp;
	}
	
	/**
	 * print_Data:
	 * @function prints out all bindings saved in the environment.
	 */
	public String print_Data() {
		return d.print_Bindings();
	}

}

