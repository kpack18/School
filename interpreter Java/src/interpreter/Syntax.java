package interpreter;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
/**
 * Class: Syntax.java
 * 
 * @function Reads individual lines from the input file and converts the string into an Instruction.
 * 
 * @author Kyle Kolpack
 * 
 */
public class Syntax {
	
	
    private BufferedReader br;
	/**
	 * Syntax:
	 * @param inputfile: The name of the input file within the interpreter package (String).
	 * 
	 * @constructor Creates a new Buffered Reader attached to the given input file if it exists.
	 */
	public Syntax(String inputfile) {
		try {
		  br = new BufferedReader(new FileReader(inputfile));
	    } catch(FileNotFoundException ex) {
	    	System.out.println("File Not Found: " + inputfile + " \n");
	    }
	}
	
	public void output_String(String output) {
			try {
				BufferedWriter bw = new BufferedWriter(new FileWriter("output.txt"));
				bw.write(output);
				bw.close();
			} catch (IOException e) {
				System.out.println("Output File \"output.txt\" must be created");
				return;
			}
	}
	
	/**
	 * get_Line:
	 * @return A String containing the next unread line in the input file.
	 * 
	 * Function: retrieves a line from the buffered reader and returns it. (Will return "" if it reaches the end of the file)
	 */
	public String get_Line() {
		String st = "";
        try {
        	if ((st = br.readLine()) != null) { 
        		return st;
		    }
	    } 
        catch(IOException ex) {
        	System.out.println("IOError Reading the File\n");
	    }
		return "";
	}
	
	/**
	 * get_Fun:
	 * @param line: (String) A read line from the input file. This line was determined to begin with "fun" or "inOutFun" by the get_Instruction method below.
	 * @param inOut (Bool) Will be true if the Instruction is classified as an "inOutFun" command. False if it's a "Fun" command.
	 * @return A "inOutFun" or "Fun" Instruction created from the information in the input string.
	 * 
	 * @function A Fun line in the code will look as follows: "fun name param" (Or will start with inOutFun).
	 *   fun/inOutFun: Will set the command value of the Instruction
	 *   name: The name of the function is stored within the fun_Name value. Must be in the same form of a "Name" Elem.
	 *   param: Each function has one formal parameter. This is bound to an inputed argument at call time. The Elem resulted from this will always be a Name.
	 *          Stored within the val value of the Instruction.
	 *   
	 *   Since all fun lines with have 2 spaces between the three sections the string is divided into three sections using the locations of said spaces.
	 *   The line is divided into those three variables ( "fun", "name", and "param") and each member of the Instruction is set before returning it.
	 */
	public Instruction get_Fun(String line, boolean inOut) {
		Instruction i = new Instruction();
		if(inOut) {
			i.command = "inOutFun";
		}
		else {
			i.command = "fun";
		}
		
		int space = 0;
		for(; space < line.length(); ++space) {
			if(line.charAt(space) == ' ') {
				break;
			}
		}
		
		i.fun_Name = line.substring(0,space);
		i.val = new Elem(line.substring(space+1,line.length()),true);
		return i;
	}
	
	/**
	 * get_Instruction:
	 * @param line A read line from the input file. Each line in the file pertains to a single instruction.
	 * @return A Instruction created from the contents of the given line.
	 * 
	 * @function The given line from the input file is compared to all cases in the switch statement and an Instruction is returned with set inner variables
	 *            based on the string.
	 *           
	 *            Unless the given string is a push or function command the string can be directly compared to the name 
	 *            of a stack function and set accordingly if a match is found.
	 *            
	 *            A push/fun command has additional information:
	 *              push: needs to determine the value following the command to be pushed. (ex: push "4" <-- need to determine what type of Elem 4 is).
	 *              fun: needs to determine the name and parameter of the function.
	 *              
	 *            If the command is a function then the remainder of the string is transfered to the get_Fun method for further analysis.
	 *            If the command is a push command then the get_Val method will determine the remainder of the strings type.
	 *            
	 *            If no command can be determined from the string then it is defaulted to a "Error" command and the user will be notified.
	 */
	public Instruction get_Instruction(String line) {
		
		Instruction i = new Instruction();
		
		switch(line) {
		case "pop": i.command = "pop"; break;
		case ":true:": i.command = "push";
		               i.val = new Elem(true);
		               break;
		case ":false:": i.command = "push";
		                i.val = new Elem(false);
		                break;
		case ":error:": i.command = "push";
		                i.val = new Elem("Error");
		                break;
		case "add": i.command = "add"; break;
		case "sub": i.command = "sub"; break;
		case "mul": i.command = "mul"; break;
		case "div": i.command = "div"; break;
		case "rem": i.command = "rem"; break;
 		case "neg": i.command = "neg"; break;
		case "swap": i.command = "swap"; break;
		case "and": i.command = "and"; break;
		case "or": i.command = "or"; break;
		case "not": i.command = "not"; break;
		case "equal": i.command = "equal"; break;
		case "lessThan": i.command = "lessThan"; break;
		case "bind": i.command = "bind"; break;
		case "if": i.command = "if"; break;
		case "let": i.command = "let"; break;
		case "end": i.command = "end"; 
		            i.fun_Name = "end"; break;
		case "return": i.command = "return"; break;
		case "funEnd": i.command = "funEnd"; break;
		case "call": i.command = "call"; break;
		case "returnIO": i.command = "returnIO"; break;
		case "funEndIO": i.command = "funEndIO"; break;
		case "quit": i.command = "quit"; break;
		default: if(line.substring(0,3).equals("fun")) {
			           i = get_Fun(line.substring(4,line.length()),false);     //Note the initial "fun", "push" and "inOutFun" are removed from the string
	             }                                                             // Before calling the appropriate secondary method.
			     else if(line.substring(0,4).equals("push")){
			           i.command = "push";
			           i.val = get_Val(line.substring(5));                     //<---------- See Above
		         }
			     else if(line.substring(0,8).equals("inOutFun")) {
			           i = get_Fun(line.substring(9,line.length()),true);      //<---------- See Above
	             }
		         else { i.command = "Error"; }
		         break;
		}
		return i;
	}
	
	/**
	 * check_Int:
	 * @param s a String taken from a push command representing an Elem
	 * @return True if the string is found to be an integer, false otherwise.
	 * 
	 * @function Checks every character in the string. If a non digit char is found then false is returned. Otherwise it's true.
	 *           Used to check if a push Elem can be classified as an "Int".
	 */
	public boolean check_Int(String s) {
		for(int i = 0; i < s.length(); ++i) {
			if(!Character.isDigit(s.charAt(i))) return false; 
		}
		return true;
	}
	
	/**
	 * check_Name:
	 * @param s a String taken from a push command representing an Elem
	 * @return True if the string is in the format of a Name Elem. False otherwise.
	 * 
	 * @function A Name elem will start with an upper or lower case letter and following that must only contain letters and numbers.
	 *           Each char is checked to see if it follows this pattern. If a char does not then false is immediatly returned. Otherwise it's true.
	 *           Used to check if a push Elem can be classified as a "Name".
	 */
	public boolean check_Name(String s) {
		for(int i = 0; i < s.length(); ++i) {
			if(!Character.isLetterOrDigit(s.charAt(i))) return false; 
		}
		return true;
	}
	
	/**
	 * get_Val:
	 * @param line The substring of a push function that represents the Elem to be pushed.
	 * @return An Elem based on the form of the inputed Elem.
	 * 
	 * @function The possible types of a push Elem are "Int", "Name", "String". (Bools have their own commands :true: and :false: but have the same effect as push true).
	 *           
	 *           String: Is surrounded by "" and can contain any valid keyboard characters.
	 *           Name: must start with a letter and is followed by only letters and numbers.
	 *           Integer: Only numerical characters
	 *           
	 *           Initially the first char of the word is compared. 
	 *                 If it's a " then it can only be invalid or a String. 
	 *                 If it's a letter then it can only be invalid or a Name.
	 *                 If it's a number then it can only be invalid or an Int.
	 *                 
	 *           A temperary empty Elem is created (temp) and assigned "Error" as it's type. If it matches an Int String or Name then the type is altered
	 *           and the appropriate inner value is changed to hold the String or Integer it holds. An Int is converted from string to int using valueOf().
	 *           The Elem is the returned after its values are set. By default it will identify as an "Error" Elem.
	 *           
	 *           temp.i = integer => An Elem of type "Int" will access i as it's value variable
	 *           temp.sn = string => An Elem of type "String" or "Name" will access sn as it's value variable
	 */
	public Elem get_Val(String line) {
		Elem temp = new Elem();
		temp.type = "Error";
		
		if(line.charAt(0) == '\"' && line.charAt(line.length()-1) == '\"') {
			temp.sn = line.substring(1,line.length()-1);
			temp.type = "String";
		}
		else {
			char c = line.charAt(0);
			if(Character.isDigit(c) || c == '-') { 
				if(check_Int(line.substring(1))) {
					temp.type = "Int";
					if(c == '-') { temp.i = -1 * Integer.valueOf(line.substring(1)); }
					else { temp.i = Integer.valueOf(line); }
				}
			}
			else if(Character.isLetter(c)) {
				if(check_Name(line.substring(1))) {
					temp.type = "Name";
					temp.sn = line;
				}
			}
		}
		return temp;
	}
}
