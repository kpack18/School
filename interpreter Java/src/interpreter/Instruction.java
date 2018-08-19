package interpreter;
/**
 * Class: Instruction.java
 * 
 * @function Contains a single Instruction used to run on the stack
 * 
 *           command: contains a command used to pair this Instruction to a function. Possible Commands include:
 *                     Stack: push, pop, bind, quit (stops execution).
 *                     Arithmatic: add, sub, mul, div, rem, neg
 *                     Logical: and, or, not, equal, lessThan
 *                     Order: swap, if
 *                     function: let, end, fun, inOutFun, return, funEnd, returnIO, funEndIO
 *                     Other: Error if the inputed function cannot be determined (does nothing, stops execution).
 *                     
 *                     *See My_Stack.java documentation for more info on each.
 *                     
 *           fun_Name: used in function instructions only, stores the name of the function
 *           
 *           val: used in push and fun operations. In push operations this Elem stores the value that will be pushed onto the stack
 *                                                 In functions this Elem stores the formal parameter as a Name
 *           
 *           val2: Only used in returnIO and funEndIO to store the passed in argument name so it can be bound after function execution.
 *           
 * @constructor simply models the info needed in each Instruction type:
 *  
 *               Instruction(String,Elem) => Push
 *               Instruction(String c) => Non push/fun commands that only need type
 *               Instruction(String,String,Elem) => Functions
 *               Instruction() => Empty Instruction
 *                     
 * @author Kyle Kolpack
 * 
 */
public class Instruction {
	
	public String command;
	public String fun_Name;
	
	public Elem val;
	public Elem val2;
    
    public Instruction(String c,Elem e) {
    	command = c;
    	val = e;
    }
    
    public Instruction(String c) {
    	command = c;
    }
    
    public Instruction(String c, String fn, Elem v) {
    	command = c;
    	fun_Name = fn;
    	val = v;
    }
    
    public Instruction() { }
}
