package labyrinth;

/**
 * The {@code Main} Class Initializes all the sub components of the application and commands the Engine to begin the main gameplay loop. </br> </br> 
 * 
 *  Command Line Args must be in the form of: (Program Will Terminate if Conditions aren't Met)
 *  	<ul><ul> Number of Items </br> (Max 24)
 *               Player1 Name </br>
 *               Player2 Name . . . (Max 4 Players) </br> </ul></ul>
 * 		<ul>
 * 			<li> Creates Each Player Using the Inputed Names </br>
 * 			<li> Initializes the Starting State of the game through creating Engine, Board, and GUI Objects. </br>
 *          	<ul>  (See Constructors For Each on their effects).    
 *          	      {@link Engine} {@link Board} {@link Gui} </ul>
 *          <li> Moves the Created Players to Their respective Starting Tiles
 *          <li> Begins Executing the {@link Engine.play} Loop function </ul>
 *
 * @author Kyle Kolpack
 * 
 */

public class Main {
	
	private static Engine e;
	
	public static void main(String args[]) {
		
		if(args.length < 1) { System.out.println("Number of items must be included (2-24), No Names Found! (1-4 Players)\nClosing Application..."); return; }
		System.out.println("Number of Items: " + args[0] + "\nPlayer Names:");
		
		if(args.length == 1) { System.out.println("Must Include Player Names After Item Count."); return; }
		
		e = new Engine();
		
		for(int i = 1; i < args.length; ++i) {
			System.out.println("Added Player: " + args[i]);
			e.add_Player(args[i]);
		}
		
		e.set_Score_Limit(Integer.valueOf(args[0]));
		e.create_Board();
		e.create_GUI();
		
		switch(Engine.number_Of_Players) {
		case 4: e.move_Player(4, 6, 6);
		case 3: e.move_Player(3, 6, 0);
		case 2: e.move_Player(2, 0, 6);
		case 1: e.move_Player(1, 0, 0);
		}
		
		e.set_Hands();
		e.set_Cards();
		e.play();
		
	}

}
