package interpreter;

import java.util.ArrayList;
import java.util.HashMap;
/**
 * Class: Data.java
 * 
 * @function contains the binding environment as well as it's supportive functions.
 * 
 *           bind_Env: Same function as the environment variable in the My_Stack.class. 
 *                     It holds each binding scope where the last one in the list is the active scope.
 *                     
 *           binding_list: Always points to the active scope for reference (last in environment).
 *           
 *           key_Env/key_List: lists used to hold the key's of the bound variables for lookup and printing purposes. (Same funcs as their binding counter parts).
 *           
 * @constructor simply initializes the above variables to new states
 *                     
 * @author Kyle Kolpack
 * 
 */
public class Data {
	
    private ArrayList<HashMap<String,Elem>> bind_Env;
	private HashMap<String,Elem> binding_List;
	
	private ArrayList<ArrayList<String>> key_Env;
	private ArrayList<String> key_List;
	
	public Data() {
		binding_List = new HashMap<String,Elem>();
		bind_Env = new ArrayList<HashMap<String,Elem>>();
		bind_Env.add(binding_List);
		
		key_List = new ArrayList<String>();
		key_Env = new ArrayList<ArrayList<String>>();
		key_Env.add(key_List);
	}
	
	/**
	 * add_Binding:
	 * @param n The name of the variable being bound to
	 * @param e The data being bound to the name
	 * 
	 * @function: Looks up the binding in the table and if it already exists in the current scope it's removed.
	 *            Then a new binding is added as a (String * Elem) pair.
	 */
	public void add_Binding(String n,Elem e) {
		if(lookup_Bind(n) != null) {
			key_List.remove(n);
		}
		binding_List.put(n, e);
		key_List.add(n);
	}
	
	/**
	 * lookup_Bind:
	 * @param s The name of the variable being searched for
	 * @return If the string s has a binding associated with it the found Elem is returned. Otherwise null is returned.
	 * 
	 * @function Starting at the current scope the key lists are iterated through searching for an existing key. If they key is found then they pair
	 *           is looked up in the corresponding list in the binding environment and returned. If no key is found then null is returned.
	 */
	public Elem lookup_Bind(String s) {
		for(int i = key_Env.size()-1; i > -1; --i ) {
			if((bind_Env.get(i)).get(s) != null) {
				return bind_Env.get(i).get(s);
			}
		}
		return null;
	}
	
	/**
	 * add_Env:
	 * @function used by the let function in My_Stack.java. Adds a new environment to the key and binding lists.
	 */
	public void add_Env() {
		bind_Env.add(new HashMap<String,Elem>());
		binding_List = bind_Env.get(bind_Env.size()-1);
		
		key_Env.add(new ArrayList<String>());
		key_List = key_Env.get(key_Env.size()-1);
	}
	
	/**
	 * add_Env:
	 * @function removes the current most binding and key list. Used by the end functions in My_Stack.java.
	 */
	public void remove_Env() {
		bind_Env.remove(bind_Env.size()-1);
		binding_List = bind_Env.get(bind_Env.size()-1);
		
		key_Env.remove(key_Env.size()-1);
		key_List = key_Env.get(bind_Env.size()-1);
	}
	
	/**
	 * print_KeyList:
	 * @param l an inputed list from the environment
	 * @param i index representing the index of the list within the environment.
	 * 
	 * @function given a list every element within it is printed using the Elem's get_Text() function.
	 */
	public String print_KeyList(ArrayList<String> l, int i) {
		String temp = "";
		System.out.println("------" + i + "-------");
		temp = temp + "------" + i + "-------\n";
		for(int j = 0; j < l.size(); ++j) {
			String s = l.get(j);
			System.out.println(s + " " + lookup_Bind(s).get_Text());
			temp = temp + s + " " + lookup_Bind(s).get_Text() + "\n";
		}
		return temp;
	}
	
	/**
	 * print_Bindings:
	 * @function iterates through each list in the environment and prints out it's contents using the print_KeyList() function.
	 */
	public String print_Bindings() {
		String temp = "";
		int i = 1;
		System.out.println("Bindings:");
		temp = temp + "Bindings:\n";
		for(int j = key_Env.size()-1; j > -1; --j ) {
			temp = temp + print_KeyList(key_Env.get(j),i);
			++i;
		}
		
		System.out.println("--------------");
		return temp + "--------------\n";
	}
	
}
