package unit;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Random;
import javax.swing.JPanel;

import graphic.ExtraTileBox;
import graphic.Gui;
import listener.Move;

/**
 * 		<ul> The {@code Board} Class Holds the 7x7 grid of tiles and populates them with items and players and 
 *           randomizes the layout of the board each execution. </ul>
 *           
 *       <ul>Also Contains the methods to enable/disable the 
 *           Tiles as buttons and the main path finding algorithm to determine available movements. </ul>
 *
 * @author Kyle Kolpack
 *
 */
public class Board {

	public Tile extra_Tile = new Tile();
	
	/** row_List is organized as:  col_list:
	 *                           
	 *                   row_List: -----> 0  1  2  3  4  5  6...
	 *                                 |  1
	 *                                 |  2
	 *                                 V  3...
	 *                               Columns
	 *                   
	 *                   7x7 Grid Meaning 49 total Tiles (And the Extra Tile)     
	 */
	private ArrayList<ArrayList<Tile>> row_List;
	private JPanel board_Panel;
	
	LinkedList<String> items;
	
	private ArrayList<Tile> last_Paths;
	
	/**
	 * <ul>Initializes the JPanel that houses the Tileset. </ul>
	 * 
	 * <ul>  <li>Randomizes the Board of 7x7 Tiles and populates it's container {@code Row_List} </br>
	 *       <li>Randomly Distributes the Item's across the board. </br>
	 *       <li>Set's the Starting Spaces in the four corners.
	 *     
	 */
	public Board() {
		
		row_List = new ArrayList<ArrayList<Tile>>();
		for(int i = 0; i < 7; ++i) {
			row_List.add(new ArrayList<Tile>());
		}
		
		board_Panel = new JPanel();
		board_Panel.setBackground(Color.BLACK);
		GridBagLayout g = new GridBagLayout();
		board_Panel.setLayout(g);
		
		items = get_Item_List();
		
		set_Tiles();
		set_Items();
		
		get_Tile(0,0).set_Start(Gui.myRed);
		get_Tile(0,6).set_Start(Gui.myBlue);
		get_Tile(6,0).set_Start(Gui.myGreen);
		get_Tile(6,6).set_Start(Gui.myPurple);
	}
	
	public LinkedList<String> get_Item_List(){
		LinkedList<String> tempitems = new LinkedList<String>();
		tempitems.add("Skull"); tempitems.add("Key");
		tempitems.add("Ring"); tempitems.add("Rat");
        tempitems.add("Chest"); tempitems.add("Candle");
		tempitems.add("Owl"); tempitems.add("Wand");
		tempitems.add("Sword"); tempitems.add("Dragon");
		tempitems.add("Helmet"); tempitems.add("Cup");
		tempitems.add("Book"); tempitems.add("Spider");
		tempitems.add("Ghost"); tempitems.add("Vase");
		tempitems.add("Crown"); tempitems.add("Coin");
		tempitems.add("Shield"); tempitems.add("Monacle");
		tempitems.add("Goblin"); tempitems.add("Feather");
		tempitems.add("Bat"); tempitems.add("Gem");
		
		return tempitems;
	}
	
	public JPanel get_Board_Panel() {
		return board_Panel;
	}
	
	public Tile get_Extra_Tile() {
		return extra_Tile;
	}
	
	public void set_Extra_Tile(Tile t) {
		extra_Tile = t;
	}
	
	public LinkedList<String> get_Items(){
		return items;
	}
	
	/**
	 * The {@code get_NewTiles} function Creates A List of 49 Randomly Angled Tiles.
	 * 
	 * <ul> <li> Corner: 16
	 *      <li> Straight: 12
	 *      <li> T: 10 </br> </ul>
	 * @return The Randomized List of Generated Tiles
	 */
	private ArrayList<Tile> get_New_Tiles(){
		ArrayList<Tile> tile_List = new ArrayList<Tile>();
		
		Random rand = new Random();
		
		for(int i = 0; i < 16; ++i) {
			int temp_Num = rand.nextInt(4) + 0;
			tile_List.add(new Tile("Corner", 90 * temp_Num));
		}
		for(int i = 0; i < 12; ++i) {
			int temp_Num = rand.nextInt(2) + 0;
			tile_List.add(new Tile("Straight", 90 * temp_Num));
		}
		for(int i = 0; i < 10; ++i) {
			int temp_Num = rand.nextInt(4) + 0;
			tile_List.add(new Tile("T", 90 * temp_Num));
		}
		
		Collections.shuffle(tile_List);
		
		return tile_List;
	}
	
	/**
	 * The {@code set_Tiles} Method populates the row_List container and the Board Panel with the JButtons
	 * Stored within the generated Tile objects from the {@code get_NewTiles} Method.
	 * 
	 * <ul> Two Tile lists are generated since the outer ring of Tiles (Ever other tile) is static between games. The static Tile list
	 * and the randomized dynamic one are pulled from depending on the index within the row_List. </ul>
	 */
	private void set_Tiles(){
		
	    ArrayList<Tile> tile_List = get_New_Tiles();
		ArrayList<Tile> preset_List = new ArrayList<Tile>();
		
		preset_List.add(new Tile("Corner",90));
		preset_List.add(new Tile("T",90));
		preset_List.add(new Tile("T",90));
		preset_List.add(new Tile("Corner",0));
		preset_List.add(new Tile("T",180));
		preset_List.add(new Tile("T",0));
		preset_List.add(new Tile("T",180));
		preset_List.add(new Tile("T",0));
		preset_List.add(new Tile("Corner",180));
		preset_List.add(new Tile("T",270));
		preset_List.add(new Tile("T",270));
		preset_List.add(new Tile("Corner",270));
		
		extra_Tile = tile_List.get(tile_List.size()-1);
	    tile_List.remove(tile_List.size()-1);

	    GridBagConstraints gc = new GridBagConstraints();
	    
	    gc.gridy = 0;
	    for(int i = 0; i < 7; ++i) {
	    	gc.gridx = i;
	    	if(i % 2 == 0) {
	    		Tile temp = preset_List.get(preset_List.size()-1);
	    		temp.set_Coors(i, 0);
	    		row_List.get(0).add(temp);
	    		board_Panel.add(temp.get_Image(),gc);
	    		preset_List.remove(preset_List.size()-1);
	    	}
	    	else {
	    		Tile temp = tile_List.get(tile_List.size()-1);
	    		temp.set_Coors(i, 0);
	    		row_List.get(0).add(tile_List.get(tile_List.size()-1));
	    		board_Panel.add(temp.get_Image(),gc);
	    		tile_List.remove(tile_List.size()-1);
	    	}
	    }
	    for(int i = 1; i < 6; ++i) {
	    	gc.gridy = i;
	    	for(int j = 0; j < 7; ++j) {
	    		gc.gridx = j;
	    		if(i % 2 == 0 && (j == 0 || j == 6)) {
	    			Tile temp = preset_List.get(preset_List.size()-1);
	    			temp.set_Coors(j, i);
		    		row_List.get(i).add(temp);
		    		board_Panel.add(temp.get_Image(),gc);
		    		preset_List.remove(preset_List.size()-1);
	    		}
	    		else {
	    			Tile temp = tile_List.get(tile_List.size()-1);
	    			temp.set_Coors(j, i);
		    		row_List.get(i).add(tile_List.get(tile_List.size()-1));
		    		board_Panel.add(temp.get_Image(),gc);
		    		tile_List.remove(tile_List.size()-1);
	    		}
	    	}
	    }
	    gc.gridy = 6;
	    for(int i = 0; i < 7; ++i) {
	    	gc.gridx = i;
	    	if(i % 2 == 0) {
	    		Tile temp = preset_List.get(preset_List.size()-1);
	    		temp.set_Coors(i, 6);
	    		row_List.get(6).add(temp);
	    		board_Panel.add(temp.get_Image(),gc);
	    		preset_List.remove(preset_List.size()-1);
	    	}
	    	else {
	    		Tile temp = tile_List.get(tile_List.size()-1);
	    		temp.set_Coors(i, 6);
	    		row_List.get(6).add(tile_List.get(tile_List.size()-1));
	    		board_Panel.add(temp.get_Image(),gc);
	    		tile_List.remove(tile_List.size()-1);
	    	}
	    }
	}
	
	/**
	 * The {@code set_Items} function Iterates through Every non start_Space Tile and randomly adds Items to each.
	 * Since there are only 24 Items and 49 Tiles not every one will get an item. </br> </br>
	 * 
	 * This is handled by adding 21 Empty items to a Linked List along with the available items and shuffling the collection.
	 */
	public void set_Items() {
		LinkedList<String> item_Set = items;
		
		
		for(int i = 0; i < 21; ++i) {
			item_Set.add("");
		}

		Collections.shuffle(items);
		
		for(int i = 1; i < 6; ++i) {
			get_Tile(0,i).set_Item(item_Set.getFirst());
			item_Set.removeFirst();
		}
		for(int i = 1; i < 6; ++i) {
			for(int j = 0; j < 7; ++j) {
				get_Tile(i,j).set_Item(item_Set.getFirst());
				item_Set.removeFirst();
			}
		}
		for(int i = 1; i < 6; ++i) {
			get_Tile(6,i).set_Item(item_Set.getFirst());
			item_Set.removeFirst();
		}
	}
	
	public Tile get_Tile(int r, int c) {
		return row_List.get(r).get(c);
	}
	
	public ArrayList<ArrayList<Tile>> get_Tile_List(){
		return row_List;
	}
	
	/**
	 * <ul>The {@code set_Shift_Listeners} Function Enables the Outer Tile JButtons and configures them to wait for the players input.
	 *     see {@link Shift} for info on their function. </ul>
	 *     
	 * @param etb The ExtraTileBox object used to change the Extra Tiles Image after Shifting
	 * @param p The Player List used to move players from tiles if their row is shifted.
	 */
	public void set_Shift_Listeners(ExtraTileBox etb, LinkedList<Player> p) {
		for(int i = 1; i < 6; i = i + 2) {
			get_Tile(i,0).add_Shift_Listener(this, i, 0, etb, p);
		}
		for(int i = 1; i < 6; i = i + 2) {
			get_Tile(0,i).add_Shift_Listener(this,0,i,etb,p);
			get_Tile(6,i).add_Shift_Listener(this,6,i,etb,p);
		}
		for(int i = 1; i < 6; i = i + 2) {
			get_Tile(i,6).add_Shift_Listener(this,i,6,etb,p);
		}
	}
	
	/**
	 * The {@code remove_Shift_Listeners} Function Iterates through the Outer most Tiles and Removes their actionlisteners after
	 * the shift phase ends.
	 */
	public void remove_Shift_Listeners() {
		for(int i = 1; i < 6; i = i + 2) {
			get_Tile(i,0).remove_Shift_Listener();
		}
		for(int i = 1; i < 6; i = i + 2) {
			get_Tile(0,i).remove_Shift_Listener();
			get_Tile(6,i).remove_Shift_Listener();
		}
		for(int i = 1; i < 6; i = i + 2) {
			get_Tile(i,6).remove_Shift_Listener();
		}
	}
	/**
	 * Used by the {@code find_Paths} function to check if a valid tile has been checked by the algorithm yet.
	 * The tile is valid if it's coordinates lie within the 7x7 grid of tiles. </br>
	 * 
	 * @param cp Boolean array of tiles that are true if they have already been visited
	 * @param x_Coor The X Coordinate of the tile being looked up
	 * @param y_Coor The Y Coordinate of the tile being looked up
	 * @return true if the tile has not been visited yet, false otherwise.
	 */
	private boolean check_Path_Array(boolean[][] cp, int x_Coor, int y_Coor) {
		if(x_Coor > 6 || y_Coor > 6 || x_Coor < 0 || y_Coor < 0) { return false; }
		return !cp[x_Coor][y_Coor];
	}
	
	/**
	 * The {@code find_Paths} Function uses a path finding algorithm to determine all the possible paths 
	 * a player can move to from their current position. </br> </br>
	 * 
	 * 		Tiles have four direction booleans: up, down, left, and right which are true if they contain a path in that direction. </br> </br>
	 * 
	 * 		A 7x7 boolean matrix is used to determine if the Tile with matching coordinates as it's indices has been visited by the algorithm.
	 *      This ensures that the while loops will terminate eventually since a tile cannot be visited more then once and there is a finite amount (49 Tiles.)
	 *      
	 *      Note* The starting tile is initially set to true and added to the output.
	 * 
	 * 		<ul> A queue is created with the current tile the player is on in it: </br>
	 *               To begin the Loop the head of the queue is popped out and it's paths are analyzed.
	 *               
	 *          <ul> <li>For each true direction boolean the adjacent tile in that direction is checked for a true boolean in the opposite direction.
	 *               <ul> Ex: Current Tile has an Up direction. For a path to exist the Tile above it must have a connecting Down direction. </ul>
	 *               
	 *               <li>If a connecting path exists to an adjacent tile it is added to the output list, set true in the visited matrix, and added to the queue
	 *               so it's own paths can be examined.
	 *               <li> The loop continues until no more paths are found between adjacent tiles. </ul>
	 *               
	 * @param p The current Player whose turn it is. Used to find their current Tile which is the starting position of the algorithm.
	 * @return A list of all tiles whose paths connect such that you could move from the starting position to them.
	 */
	public ArrayList<Tile> find_Paths(Player p) {
		ArrayList<Tile> paths_List = new ArrayList<Tile>();
		boolean[][] checked_Paths = new boolean[7][7];
		
		for(int i = 0; i < 7; ++i) {
			for(int j = 0; j < 7; ++j) {
				checked_Paths[i][j] = false;
			}
		}
		
		int x_pos = p.get_X();
		int y_pos = p.get_Y();
		
		Tile start = get_Tile(x_pos,y_pos);
		
		LinkedList<Tile> queue = new LinkedList<Tile>();
		queue.addFirst(start);
		checked_Paths[x_pos][y_pos] = true;
		paths_List.add(start);
		
		while(!queue.isEmpty()) {
			Tile current = queue.getFirst();
			queue.removeFirst();
			int x_Coor = current.get_X();
			int y_Coor = current.get_Y();
			
			if(current.up() && check_Path_Array(checked_Paths,y_Coor-1,x_Coor)) {
				Tile new_current = get_Tile(y_Coor-1,x_Coor);
            	if(new_current.down()) {
					queue.addLast(new_current);
					paths_List.add(new_current);
					checked_Paths[y_Coor-1][x_Coor] = true;
				}
			}
			if(current.down() && check_Path_Array(checked_Paths,y_Coor+1,x_Coor)) {
				Tile new_current = get_Tile(y_Coor+1,x_Coor);
				if(new_current.up()) {
					queue.addLast(new_current);
					paths_List.add(new_current);
					checked_Paths[y_Coor+1][x_Coor] = true;
				}
			}
			if(current.left() && check_Path_Array(checked_Paths,y_Coor,x_Coor-1)) {
				Tile new_current = get_Tile(y_Coor,x_Coor-1);
				if(new_current.right()) {
					queue.addLast(new_current);
					paths_List.add(new_current);
					checked_Paths[y_Coor][x_Coor-1] = true;
				}
			}
			if(current.right() && check_Path_Array(checked_Paths,y_Coor,x_Coor+1)) {
				Tile new_current = get_Tile(y_Coor,x_Coor+1);
				if(new_current.left()) {
					queue.addLast(new_current);
					paths_List.add(new_current);
					checked_Paths[y_Coor][x_Coor+1] = true;
				}
			}
		}
		return paths_List;
	}
	
	/**
	 * The {@code set_Move_Listener} function retrieves all Tiles the player is able to move to from the above {@code find_Paths} method
	 * and set's all of their move listeners to true so they player may make their Move Phase decision.
	 * 
	 * @param p The current Player whose turn it is
	 */
	public void set_Move_Listener(Player p) {
		last_Paths = find_Paths(p);
		
		for(int i = 0; i < last_Paths.size(); ++i) {
			new Move(last_Paths.get(i),p,this);
			last_Paths.get(i).add_Move_Listener(last_Paths.get(i), p, this);
		}
		
	}
	
	/**
	 * The {@code remove_Move_Listener} Function iterates through the last_Paths array which saves the set of Tiles last produced by
	 * the {@code find_Paths} algorithm and removes their move listeners after the move Phase has concluded. </br> </br>
	 * 
	 * It then deletes the last_Paths List.
	 */
	public void remove_Move_Listener() {
		for(int i = 0; i < last_Paths.size(); ++i) {
			last_Paths.get(i).remove_Move_Listener();
		}
		last_Paths = null;
	}
	
	public void print_Tile_Info(int x_Coor,int y_Coor) {
		Tile temp = get_Tile(x_Coor,y_Coor);
		System.out.println("Type: " + temp.get_Type() + " Angle: " + Integer.toString(temp.get_Angle()) + " X: " + Integer.toString(temp.get_X()) + " Y: " + Integer.toString(temp.get_Y()));
		System.out.print("   Angles: ");
		if(temp.up()) { System.out.print("up "); }
		if(temp.down()) { System.out.print("down "); }
		if(temp.left()) { System.out.print("left "); }
		if(temp.right()) { System.out.print("right "); }
		System.out.println("");
	}
	
	
	
}
