package unit;

import java.awt.Color;
import java.util.Stack;

import graphic.Gui;

/**
 * <h1> Player </h1>
 * 
 * <ul> Contains Several Attributes and Functions to Retrieve/Change Them:
 * 
 * 		     <li> player_Number: The Player Number (1-4)
 *           <li> name: Name of the player given by the Command Line Args
 *           <li> color: The players assigned color, used when comparing to the players start space for victory condition
 *           <li> hand: Set of cards given to each player. The top of the stack is their current card and it displayed on the GUI.
 *           <li> score: The players current score (number of cards completed / score_Limit) </br> </br>
 *           
 *           <li> x_pos and y_pos : The coordinates of the player within the 7x7 grid of tiles on the board.
 * 
 * @author Kyle
 *
 */
public class Player {
	
	private int player_Number;
	private String name;
	private Color color; //Red, Blue, Green, or Purple
	private Color back_Color;
	
	private Stack<String> hand; //Set of Cards the player must complete
	public int score = 0; //Number of Cards Completed

	public int x_pos = 0;
	public int y_pos = 0;
	
	/**
	 * <h1> Player </h1>
	 * 
	 * <ul> Assigns the players name, number, and color
	 * 
	 * @param player_name : name given through Command line Args
	 * @param player_num : number based on the order of players inputed.
	 */
	public Player(String player_name, int player_num) {
		name = player_name;
		player_Number = player_num;
		
		switch(player_num) {
		case 1: color = Gui.myRed;
		        back_Color = Gui.RedBack;
		        break;
		case 2: color = Gui.myBlue;
		        back_Color = Gui.BlueBack;
		        break;
		case 3: color = Gui.myGreen;
		        back_Color = Gui.GreenBack;
		        break;
		case 4: color = Gui.myPurple;
		        back_Color = Gui.PurpleBack;
		        break;
		}
		
		hand = new Stack<String>();
	}
	
	public Stack<String> get_Hand(){
		return hand;
	}
	
	/**
	 * <h1> get_Item </h1>
	 * 
	 * <ul> Returns the top element from the players hand. If the hand is 
	 *      empty then "Goal!" is returned signaling that the player must return to the start space.
	 * @return The current Card the player must fullfill
	 */
	public String get_Item() {
		if(hand.isEmpty()) {
			return "Goal!";
		}
		return hand.peek();
	}
	
	/**
	 * <h1> get_Next_Item </h1>
	 * 
	 * <ul> Removes the top element from the players hand and returns the next one. If the hand is 
	 *      empty then "Goal!" is returned signaling that the player must return to the start space.
	 * @return The next Card the player must fullfill
	 */
	public String get_Next_Item() {
		hand.pop();
		if(hand.isEmpty()) {
			return "Goal!";
		}
		return hand.peek();
	}
	
	public int get_X() {
		return x_pos;
	}
	
	public int get_Y() {
		return y_pos;
	}
	
	public void set_Coors(int new_X, int new_Y) {
		x_pos = new_X;
		y_pos = new_Y;
	}
	
	public String get_Name() {
		return name;
	}
	
	public Color get_Color() {
		return color;
	}
	
	public Color get_Back_Color() {
		return back_Color;
	}
	
	public int get_Num() {
		return player_Number;
	}
	
	public int get_Score() {
		return score;
	}
	
	public void inc_Score() {
		++score;
	}

}
