package interpreter;

import java.util.ArrayList;
/**
 * Class: Elem.java
 * 
 * @function Datatype used for basic variables added and computed in the stack
 * 
 *           Types: Int, String, Name, Boolean, Error, Unit, and Fun
 *           
 *           Each type uses a different public variable to store it
 *           
 *           Int => i
 *           String => sn
 *           Name => Name
 *           Error/Unit => None
 *           Fun => fun_List for it's instructions
 *                  
 *           Elem's are also stored in data for variable bindings in (string * Elem) pairs.
 *
 *@constructor simply models the info needed in each Elem type:
 *
 *                        Elem(Int) => Int
 *                        Elem(String,Bool) => String if bool is false, Name if bool is true
 *                        Elem(Bool) => Bool
 *                        Elem(String) => Generic, used mostly for Error and Unit
 *                        Elem(ArrayList<Instruction>, Boolean) => Function, inOut if Boolean is true, regular "fun" if false.
 *                        Elem() => Empty Elem
 * 
 * @author Kyle Kolpack
 * 
 */
public class Elem {

	public String type = "";
			
	public int i;
	public String sn;
	public boolean b;
	public ArrayList<Instruction> fun_List;
			
	public Elem(int val) {
		i = val;
		type = "Int";
	}
	public Elem(String val, boolean name) {
		sn = val;
		if(name) { type = "Name"; }
		else { type = "String"; }
	}
	public Elem(boolean val) {
		b = val;
		type = "Bool";
	}
	public Elem(String eu) {
		type = eu; //eu must be either "Error" or "Unit"
	}
	
	public Elem(ArrayList<Instruction> l, boolean inOut) {
		if(inOut) {
			type = "inOutFun";
		}
		else {
			type = "Fun";
		}
		fun_List = l;
	}
	
	public Elem() { }
	
	/**
	 * get_Text:
	 * @return   Returns a string form of the Elem based on the type and variables stored in it.
	 * @function returns a string form of the information stored in the Elem for printing purposes depending on it's type.
	 *           used only in print_Stack method (my_stack.java).	    
	 */
	public String get_Text() {
		String text = type + " ";
		    	
		switch(type){
		case "Int": text = text + Integer.toString(i);
		    	    break;
		case "String": text = text + "\"" + sn + "\"";
		    	     break;
		case "Name": text = text + sn;
		             break;
		case "Bool": text = text + Boolean.toString(b);
		             break;
		case "inOutFun": //Has same effect as "Fun"
		case "Fun": text = text + "\n";
			        for(int i = 0; i < fun_List.size(); ++i) {
			          Instruction in = fun_List.get(i);
			          text = text + "| ";
			          if(in.command == "push") { text = text + in.command + " " + in.val.get_Text() + "\n"; }
			  		  else if(in.command == "fun") { text = text + in.command + " " + in.fun_Name +  " " + in.val.sn + "\n"; }
			  		  else { text = text + in.command + "\n"; }
		            }
			        text = text.substring(0,text.length()-1);
		            break;
		default: break; //ERROR and UNIT will only need type
		}
		    	
		return text;
	}
}
