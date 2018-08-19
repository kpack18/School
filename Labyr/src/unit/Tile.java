package unit;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionListener;
import java.util.LinkedList;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.border.Border;

import graphic.ExtraTileBox;
import graphic.Gui;
import listener.Move;
import listener.Shift;

/**
 * The {@code Tile} class is a sub component of the Board.
 * 
 * <ul> Attributes:
 * 		<ul> {@code type}: Either Corner, Straight, or T. </br>
 *           {@code angle}: Either 0,90,180, or 270 degrees. Represents the rotation of the paths. </br>
 *                                                                                                 </br>
 *           {@code Directional Booleans(up, down, left, right)}: Will be true if the tile has a path in that direction.
 *           
 *           {@code x_Coor and y_Coor}: The position within the 7x7 grid of the Board </br>
 *           																		  </br>
 *           {@code item}: The name of the item (if any) that exists on this Tile. </br>
 *           {@code start_Space}: The color of the starting space (if any) that exists on this Tile. </br>
 *           {@code players}: An array with 4 indices each corresponding to a player. If that player is on this space their index will be true. </br>
 *           																																	</br>
 *           {@code move and shift}: The Shift and Move Listener references. Used to add and remove the listeners between Phases of Play. </br>
 *          																															  </br>
 *           {@code image}: The main JLabel that holds the image of the paths. </br>
 *           {@code player_Image}: JLabel that holds the image of the Player(s) currently in the space (See {@link update_players} for more info). </br>
 *           {@code item_Image}: Jlabel that holds the text of the item on this space (if any). </br> </ul>
 *           
 * @author Kyle Kolpack
 *
 */
public class Tile {

	public Border border = BorderFactory.createLineBorder(Color.BLACK, 2);
	public Border border_Light = BorderFactory.createLineBorder(Color.GREEN , 4);
	
	private String type; // Essentially the Name of the Image File ex: Corner Straight or T
	private int angle;
	
	private boolean up;
	private boolean down;
	private boolean left;
	private boolean right;
	
	private int x_Coor;
	private int y_Coor;
	
	public String item; //if theres an item on the place
	public Color start_space; //Starting Space Red, Blue, Green, or Yellow
	public boolean[] players = new boolean[4]; //Index i is true if player i is on space.
	
	Move move;
	Shift shift;
	
	JButton image;
	JLabel player_Image;
	JLabel start_Image;
	public JLabel item_Image;
	
	public Tile(String tp, int ang) {
		type = tp;
		angle = ang;
		change_Paths();
		
		image = new JButton();
		image.setPreferredSize(new Dimension(110,110));
		image.setBorder(border);
		image.setFocusable(false);
		ImageIcon icon = new ImageIcon("images/Tiles/" + tp + "_" + Integer.toString(ang) + ".png");
		image.setIcon(icon);
		image.setPressedIcon(image.getDisabledIcon());
		image.setDisabledIcon(icon);
		image.setEnabled(false);
		
		player_Image = new JLabel();
		image.add(player_Image);
		
		start_Image = new JLabel();
		image.add(start_Image);
		
		item_Image = new JLabel();
		image.add(item_Image);
		
		remove_Players();
	}
	
	public Tile() {
		image = new JButton();
		image.setPreferredSize(new Dimension(110,110));
		image.setBorder(border);
		image.setFocusable(false);
		image.setEnabled(false);
		
		player_Image = new JLabel();
		image.add(player_Image);
		
		start_Image = new JLabel();
		image.add(start_Image);
		
		item_Image = new JLabel();
		image.add(item_Image);
		
		remove_Players();
	}
	
	/**
	 * The {@code copy_Tile} method takes each attribute from the given Tile parameter and copies it to his tile. Used mostly by the Shift Listener to
	 *                       Iteratively shift the row over through copying.
	 * @param other_Tile Another tile in which its attributes will be copied from.
	 */
	public void copy_Tile(Tile other_Tile) {
		type = other_Tile.type;
		angle = other_Tile.angle;
		
		change_Paths();
		
		item = other_Tile.item;
		start_space = other_Tile.start_space;
		players = other_Tile.players;

		update_Players();
		
		item_Image.setText(other_Tile.item_Image.getText());
		item_Image.repaint();
		
		image.setIcon(other_Tile.get_Image().getIcon());
		
		image.setPressedIcon(other_Tile.get_Image().getPressedIcon());
		image.setDisabledIcon(other_Tile.get_Image().getDisabledIcon());
		
		image.repaint();
	}
	
	/**
	 * The {@code set_Victory} Function Removes all icons from the Tile's image and set's it to the winning players color
	 * @param p The Winning Player
	 */
	public void set_Victory(Player p) {
		image.setDisabledIcon(new ImageIcon("images/Victory/Victory_" + Integer.toString(p.get_Num()) + ".png"));
		image.setBorderPainted(false);
		remove_Players();
		item_Image.setText("");
		item_Image.repaint();
		start_Image.setIcon(null);
		image.repaint();
	}
	
	/**
	 * The {@code set_Item} function Replaces and Repaints the Item Image/variable to the given string i
	 * @param i The New Item to appear on this Tile
	 */
	public void set_Item(String i) {
		item = i;
		item_Image.setText(i);
	    item_Image.setMaximumSize(new Dimension(100,100));
		item_Image.setHorizontalAlignment(SwingConstants.CENTER);
		item_Image.setFont(new Font("Arial", Font.BOLD, 16));
		item_Image.repaint();
	}
	
	public Color get_Start_Space() {
		return start_space;
	}
	
	public void set_Start(Color c) {
		if(c.equals(Gui.myRed)) { 
			start_space = Gui.myRed;
			start_Image.setIcon(new ImageIcon("images/Start/Start_1.png"));
		}
		else if(c.equals(Gui.myBlue)) { 
			start_space = Gui.myBlue;
			start_Image.setIcon(new ImageIcon("images/Start/Start_2.png"));
		}
		else if(c.equals(Gui.myGreen)) { 
			start_space = Gui.myGreen;
			start_Image.setIcon(new ImageIcon("images/Start/Start_3.png"));
		}
		else {
			start_space = Gui.myPurple;
			start_Image.setIcon(new ImageIcon("images/Start/Start_4.png"));
		}
		
		start_Image.repaint();
	}
	
	/**
	 * Adds a Shift Listener and Enables the Button of this Tile. See the {@link Shift} Class for info on the parameters.
	 * @param board Instance of the Board Class
	 * @param x The X Coordinate of this Tile
	 * @param y The Y Coordinate of this Tile
	 * @param etb The Instance of the Extra Tile Box
	 * @param p The List of All Player Objects
	 */
	public void add_Shift_Listener(Board board, int x, int y, ExtraTileBox etb, LinkedList<Player> p) {
		shift = new Shift(board, x, y, etb, p);
		image.addActionListener(shift);
	}
	
	/**
	 * Removes the Shift Listener and Disables the JButton of this Tile
	 */
	public void remove_Shift_Listener() {
		set_Highlight(false);
		image.setEnabled(false);
		image.removeActionListener(shift);
		shift = null;
	}
	
	/**
	 * Adds a Move Listener and Enables the JButton of the Object
	 * @param this_Tile An Instance of the Current Tile being activated
	 * @param player The current Player taking their move action 
	 * @param board An Instance of the Board Class
	 */
	public void add_Move_Listener(Tile this_Tile, Player player, Board board) {
		move = new Move(this_Tile,player,board);
		image.addActionListener(move);
	}
	
	/**
	 *  Removes the Move Listener and Disables the JButton
	 */
	public void remove_Move_Listener() {
		set_Highlight(false);
		image.setEnabled(false);
		image.removeActionListener(move);
		move = null;
	}
	
	public int get_X() {
		return x_Coor;
	}
	
	public int get_Y() {
		return y_Coor;
	}
	
	public boolean up() {
		return up;
	}
	public boolean down() {
		return down;
	}
	public boolean left() {
		return left;
	}
	public boolean right() {
		return right;
	}
	
	public void set_Coors(int x, int y) {
		x_Coor = x;
		y_Coor = y;
	}
	
	/**
	 * Set's the given player_num index of the players array to true signaling they are now on this tile.
	 * Call's the {@code update_Players} method to update the JLabel on the board.
	 * @param player_num The number of the player moved to this Tile
	 */
	public void add_Player(int player_num) {
		players[player_num-1] = true;
		update_Players();
	}
	
	/**
	 * Set's the given player_num index of the players array to false signaling they have left the tile.
	 * Calls the {@code udpate_Players} method to update the JLabel on the board.
	 * @param player_num The number of the player moved to this Tile
	 */
	public void remove_Player(int player_num) {
		players[player_num-1] = false;
		update_Players();
	}
	
	/**
	 * Used by the copy_Tile method. Given a Tile it will copy the players on it to this array.
	 * @param other Another Tile wishing to be copied.
	 */
	public void copy_Players(Tile other) {
		players = other.players;
		update_Players();
	}
	
	/**
	 * Removes every player from the Tile and updates it's JLabel
	 */
	public void remove_Players() {
		players[0] = false;
		players[1] = false;
		players[2] = false;
		players[3] = false;
		update_Players();
	}
	
	/**
	 * The {@code update_Players} function changes the Player image based on which players are on the Tile.
	 * 
	 * 		<ul> The Images are formatted as "Player_" + A String of 1,2,3 and 4 depending on who is on the Tile. </br>
	 * 			 <ul> Ex: If players 1 3 and 4 are present then it will retrieve the file "Player_134". </br>
	 * 			      Ex: If player 2 and 3 are present then it will retrieve the file "Player_23" etc... </ul> </ul>
	 * 
	 * 		<ul> Iterating through the players array the number half of the Icon name can be built by appending the number
	 * 		If the array returns true. Then the JLabel is repainted with the new formed image. </ul>
	 */
	private void update_Players() {
		String player_String = "";
		for(int i = 0; i < players.length; ++i) {
			if(players[i]) {
				player_String = player_String + Integer.toString(i+1);
			}
		}
		player_Image.setIcon(new ImageIcon("images/Player/Player_" + player_String + ".png"));
		player_Image.repaint();
		image.revalidate();
		image.repaint();
	}
	
	/**
	 * Set's the Border of the Tile to a Bright Green Signaling that the JButton is Active
	 * @param islight Boolean controlling whether the border is highlighted or not.
	 */
	public void set_Highlight(boolean islight) {
		if(islight) {
			image.setBorder(border_Light);
		}
		else {
			image.setBorder(border);
		}
		image.repaint();
	}
	
	/**
	 * Rotates the Image and Paths of the Object 90 degrees Counter Clockwise. Note if the angle reaches 360 it's resets back to 0. </br>
	 */
	public void rotate_Left() {
		if(type == "Straight") {
			if(angle == 0) { angle = 90; }
			else { angle = 0; }
		}
		else {
			angle = angle + 90;
			if(angle == 360) { angle = 0; }
		}
		
		JButton temp = new JButton();
		ImageIcon new_Icon = new ImageIcon("images/Tiles/" + type + "_" + Integer.toString(angle) + ".png");
		temp.setIcon(new_Icon);
		image.setIcon(new_Icon);
		image.setPressedIcon(temp.getDisabledIcon());
		image.setDisabledIcon(new_Icon);
		image.repaint();
		change_Paths();
	}
	
	/**
	 * Rotates the Image and Paths of the Object 90 degrees Clockwise. Note if the angle reaches -90 it's resets back to 270. </br>
	 */
	public void rotate_Right() {
		if(type == "Straight") {
			if(angle == 0) { angle = 90; }
			else { angle = 0; }
		}
		else {
			angle = angle - 90;
			if(angle == -90) { angle = 270; }
		}
		
		JButton temp = new JButton();
		ImageIcon new_Icon = new ImageIcon("images/Tiles/" + type + "_" + Integer.toString(angle) + ".png");
		temp.setIcon(new_Icon);
		image.setIcon(new_Icon);
		image.setPressedIcon(temp.getDisabledIcon());
		image.setDisabledIcon(new_Icon);
		image.repaint();
		change_Paths();
	}
	/**
	 * Changes the Direction Booleans of the Tile based on the type and angle variables. Then the Image file is updated. </br>
	 * The Image File takes the format of "Type of Tile" + "_" + "Angle" where the type and angle can be referenced from their private variables. </br>
	 * 																																					 </br>
	 *	Note* Straight Paths can Only have 0 or 90 degree angle's since they would mirror the 270 and 180 degree versions.
	 */
	private void change_Paths() {
		if(type == "Corner") {
			switch(angle) {
			case 0: up = true; down = false; left = false; right = true; break;
			case 90: up = true; down = false; left = true; right = false; break;
			case 180: up = false; down = true; left = true; right = false; break;
			case 270: up = false; down = true; left = false; right = true; break;
			}
		}
		else if(type == "Straight") {
			switch(angle) {
			case 0: up = false; down = false; left = true; right = true; break;
			case 90: up = true; down = true; left = false; right = false; break;
			}
		}
		else if(type == "T") {
			switch(angle) {
			case 0: up = true; down = true; left = false; right = true; break;
			case 90: up = true; down = false; left = true; right = true; break;
			case 180: up = true; down = true; left = true; right = false; break;
			case 270: up = false; down = true; left = true; right = true; break;
			}
		}
	}
	
	public void set_Type(String new_type) {
		type = new_type;
	}
	
	public JButton get_Image() {
		return image;
	}
	
	public String get_Type() {
		return type;
	}
	
	public int get_Angle() {
		return angle;
	}
	
	public void set_Listener(ActionListener listener) {
		image.addActionListener(listener);
	}
	
	public void disable_Listener(ActionListener listener) {
		image.removeActionListener(listener);
	}

}
