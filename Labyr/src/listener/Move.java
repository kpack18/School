package listener;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import labyrinth.Engine;
import unit.Board;
import unit.Player;
import unit.Tile;
/**
 * <ul> The {@code Move} Class is Used to Move the Player from their current position to the Tile this listener is assigned to </br> </br>
 * 
 *      A Player is Moved via the following: </br> </br>
 *      
 *      <li> The Jbutton behind the tile is enabled and the border is highlighted
 *      <li> If the button is clicked the player is removed from the tile it's coordinates point to (retrieved via the board parameter).
 *      <li> The player is then added to this tile and it's coordinates are updated
 *      <li> The tile and board panel are repainted to reflect the change.
 *      <li> The Moved bool (within Engine) is turned to true so the moved phase ends and the Listener will be removed from every tile it was assigned to. </br>
 *      
 * @author Kyle Kolpack
 *
 */
public class Move implements ActionListener {
	
	private Player p;
	private Board b;
	private Tile t;
	
	public Move(Tile this_Tile, Player player, Board board) {
		p = player;
		b = board;
		
		t = this_Tile;
		t.get_Image().setEnabled(true);
		t.set_Highlight(true);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		Engine.moved = true;
		
		int x = p.get_X();
		int y = p.get_Y();
		
		b.get_Tile(x,y).remove_Player(p.get_Num());
		t.add_Player(p.get_Num());
		
		p.set_Coors(t.get_Y(), t.get_X());
		
		b.get_Board_Panel().validate();
		b.get_Board_Panel().repaint();
	}

}
