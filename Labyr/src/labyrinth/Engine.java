package labyrinth;

import java.util.Collections;
import java.util.LinkedList;
import java.util.Stack;

import graphic.ExtraTileBox;
import graphic.Gui;
import graphic.InfoBoard;
import graphic.InfoBox;
import graphic.ScoreBoard;
import unit.Board;
import unit.Player;
import unit.Tile;

/**
 * 		<ul> The {@code Engine} Class Houses methods to create Each Component of the game and executes the main game play Loop. 
 *           (Methods Called in {@link Main} )
 *          <ul> <h2> Order of Execution: </h2>
 * 
 * 			<ul> <li> {@link set_Score_Limit}:  Set's score limit variable based on input args[].
 * 				 <li> {@link create_Board}:  Creates a new Randomized Game board with distributed items.
 * 				 <li> {@link create_GUI}:  Creates the visual components of the application (InfoBoard, InfoBox, ExtraTileBox, 
 * 								     Board, and ScoreBoard).
 * 				 <li> {@link set_Cards}:  Distributes "score_Limit" number of cards to each player.
 *               <li> {@link set_Hands}:  Set's the player's current card (as well as the player's Gui component)
 *               <li> {@link play}:  As players input commands to the board the play method shifts phases of the turn from shift to Move as well as
 *                                   player order. </br></br>
 *  
 * 
 * 		<ul>{@linkplain score_Limit}: The score a player must reach to win. (total items / number of players) </br>
 *          {@linkplain number_Of_Players}: Incremented each time the {@link add_Player} method is called. </br> 
 * 			{@linkplain player_List}: stores each player object created from the {@link add_player} method. </br> </br>
 * 
 *          {@linkplain game_Over}: Signal's true if a Player has Won to End the Game </br>
 *          moved/shifted: Used to determine the phase of the turn. see {@link play} </br> </ul>
 * 
 * @author Kyle Kolpack
 *
 */
public class Engine {
	
	public static int score_Limit;
	public static int number_Of_Players = 0;
	
	private Board b;
	private LinkedList<Player> player_List;
	
	private ScoreBoard sb;
	
	private InfoBoard ib;
	private InfoBox infoBox;
	private ExtraTileBox extraTileBox;
	
	public static boolean game_Over;
	public static boolean moved;
	public static boolean shifted;
	
	public Engine() {
		player_List = new LinkedList<Player>();
		moved = false;
		shifted = false;
		game_Over = false;
	}
	
	/**
	 * <h1> add_Player </h1>
	 * 
	 * 	<ul> <li>If less then four players have been created then a new Player class is Instantiated with the given name. </br>
	 *       <li>It is then added to the player_List LinkedList. </ul> 
	 * 
	 * @param name : The name of the Player to be added
	 */
	public void add_Player(String name) {
		++number_Of_Players;
		if(number_Of_Players == 5) {
			System.out.println("Sorry Only Four Players Can Play."); System.exit(1);
		}
		player_List.add(new Player(name,number_Of_Players));
	}
	
	/**
	 * <h1> set_Score_Limit </h1>
	 * 
	 * <ul> The Score Limit is the score a player must reach to win the game. Given the Total number of item's </br>
	 *      from the Command Line and the number of players from the "number_Of_Players" variable, the Items are </br>
	 *      divided evenly among the players.
	 * @param sl : The Score Limit retrieved from the Command Line (Main).
	 */
	public void set_Score_Limit(int sl) {
		score_Limit = Math.floorDiv(sl,number_Of_Players);
	}
	
	/**
	 * <h1> set_Hands </h1>
	 * 
	 * Distributes Cards to each Player based on the Score Limit variable
	 */
	public void set_Hands() {
		LinkedList<String> items = b.get_Item_List();
		Collections.shuffle(items);
		for(int i = 0; i < Engine.number_Of_Players; ++i) {
			Stack<String> hand = get_Player(i+1).get_Hand();
			for(int j = 0; j < score_Limit; ++j) {
				hand.push(items.peek());
				items.pop();
			}
		}
	}
	
	/**
	 * <h1> set_Cards </h1>
	 * Updates the Player's Gui Components to Display the Top Card in their Hands.
	 */
	public void set_Cards() {
		for(int i = 0; i < Engine.number_Of_Players; ++i) {
			Stack<String> Hand = get_Player(i+1).get_Hand();
			sb.update_Item(get_Player(i+1), Hand.peek());
		}
	}
	
	/**
	 * <h1> create_GUI </h1>
	 * 
	 * Initializes the GUI's Sub Components:
	 * <ul> <li> {@link ScoreBoard}
	 * 		<li> {@link InfoBoard}
	 * 		<li> {@link InfoBox}
	 * 		<li> {@link ExtraTileBox} </ul>
	 * 
	 * Then Initializes the {@link Gui} class.
	 */
	public void create_GUI() {
		sb = new ScoreBoard(player_List);
		ib = new InfoBoard(b,b.get_Extra_Tile());
		
		infoBox = ib.get_InfoBox();
		extraTileBox = ib.get_ExtraTileBox();
		
		new Gui(sb,ib,b);
	}
	
	/**
	 * <h1> create_Board </h1>
	 * 
	 * <ul> Initializes the Board Class (See it's Constructor)</ul>
	 */
	public void create_Board() {
		b = new Board();
	}
	
	/**
	 * <h1> move </h1>
	 * 
	 * <ul> Given The Player Whose Turn it is all possible Tiles JButton's they can Move to are enabled </br>
	 * and they begin listening for the player's input. </ul>
	 * 
	 * @see Board
	 * @param player_num
	 */
	public void move(int player_num) {
		b.set_Move_Listener(get_Player(player_num));
	}
	/**
	 * <h1> shift </h1>
	 * 
	 * <ul> The Tiles that control the shifting of the maze are Enabled and they begin </br>
	 * Listening for the player's input.
	 * 
	 * @see Board
	 */
	public void shift() {
		b.set_Shift_Listeners(extraTileBox,player_List);
	}
	
	/**
	 * <h1> player_num </h1>
	 * 
	 * <ul> Given the Player they are Moved to the Target Coordinates. </ul>
	 * @param player_num : The Target Player's number(1-4)
	 * @param new_X : The X coordinate they wish to Move to.
	 * @param new_Y : The Y coordinate they wish to Move to.
	 */
	public void move_Player(int player_num, int new_X, int new_Y) {
		Player p = get_Player(player_num);
		p.set_Coors(0, 0);
		int x_pos = p.get_X();
		int y_pos = p.get_Y();
		
		b.get_Tile(x_pos, y_pos).remove_Player(player_num);
		b.get_Tile(new_X, new_Y).add_Player(player_num);
		
		p.set_Coors(new_X, new_Y);
	}
	
	
	/**
	 * <h1> get_Player </h1>
	 * 
	 * @param num : Number of the Player (1-4).
	 * @return : the Player Object in the given index of the player_List
	 */
	public Player get_Player(int num) {
	 	return player_List.get(num-1);
	}
	
	/**
	 * <h1> get_Board </h1>
	 * @return : The Board Class Used to Access it's Method's
	 */
	public Board get_Board() {
		return b;
	}
	
	/**
	 * <h1> set_Victory </h1>
	 * 
	 * <ul> When a Player has Reached the Score Limit and Returned to Their Starting Space The Game is Over. </br>
	 *      The Gameplay loop stops and the InfoBox message is Changed to the Victory Message.
	 * @param p : The Winning Player
	 */
	public void set_Victory(Player p) {
		infoBox.change_Header(p.get_Num());
		infoBox.change_Message(2);
		for(int i = 0; i < 7; ++i) {
			for(int j = 0; j < 7; ++j) {
				b.get_Tile(i, j).set_Victory(p);
			}
		}
	}
	
	/**
	 * <h1> check_Tile_Item </h1>
	 * 
	 * <ul> <li>After a Player has Moved if their space has an Item on it and it matches the players current card </br>
	 *      their score increases by one. Then The next card is drawn from their hand.
	 *      <li> If the player has an empty hand then it compares the start position (if it exists). If it matches
	 *      The players color then the set_Victory is called and the player wins. </ul>
	 * @param p : The Current Player Whose Turn it is
	 */
	public void check_Tile_Item(Player p) {
		Tile position = b.get_Tile(p.x_pos,p.y_pos);
		
		if(p.get_Item().equals("Goal!")) {
			if(p.get_Color().equals(position.get_Start_Space())) {
				sb.update_Item(p, "WIN!!!");
				set_Victory(p);
				game_Over = true;
				return;
			}
		}
		if(p.get_Item().equals(position.item)) {
			++p.score;
			sb.update_Item(p, p.get_Next_Item());
			sb.update_Score(p);
		}
	}
	
	/**
	 * <h1> play </h1>
	 * 
	 * <ul> Controls the phases of play and turn order. </br> </br>
	 * 
	 * 		<li> The infoBox is changed based on the phase to inform the next action needed to continue.
	 * 		<li> Shift Phase: A Player Inserts the Extra Tile into the board shifting the row/col 1 tile over.
	 * 		<li> Move Phase: The player may move to any new tile that is connected by a path. </br> </br>
	 * 
	 *      A While loop is used to keep the function asleep untill the appropriate listeners toggle the shifted and moved booleans.
	 *      Once true the play function continues to the next phase. When the players turn ends both are switched back to false. </br> </ul>
	 *      
	 *      The function then loops, incrementing the player_turn counter so a new player may take their turn.
	 */
	public void play() {
		int player_turn = 1;
		
		while(!game_Over) {
			Player current = get_Player(player_turn);
			infoBox.change_Header(current.get_Num());

			shift();
			infoBox.change_Message(1);
			while(!shifted) {
				try {
					Thread.sleep(30);
				} catch (InterruptedException e) { break; }
			}
			b.remove_Shift_Listeners();
			
			infoBox.change_Message(0);
			move(current.get_Num());
			while(!moved) {
				try {
					Thread.sleep(30);
				} catch (InterruptedException e) { break; }
			}
			b.remove_Move_Listener();
		    
			check_Tile_Item(current);
			
			moved = false;
			shifted = false;
			
			++player_turn;
			if(player_turn == number_Of_Players+1) { player_turn = 1; }
		}
	}
}
