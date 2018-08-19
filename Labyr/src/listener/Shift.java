package listener;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;

import javax.swing.JPanel;

import graphic.ExtraTileBox;
import labyrinth.Engine;
import unit.Board;
import unit.Player;
import unit.Tile;

/**
 * 
 * <ul> The {@code Shift} Class Shifts the Selected Row or Column away from the listening tile. Only the outer edge of tiles
 *      are available during the shifting phase: </br> </br>
 * 
 * 		<ul> Through iterative copying each tile is shifted 1 place over and the Extra tile is placed in front
 *           of the row. Then the last tile in the Row replaces the extra tile. </ul>
 *      <ul> If a player chooses a tile on the left or right side of the board a row will be shifted. If on the bottom
 *           or the top a collumn will be shifted. </ul>
 *           
 *           <h2>Important Note* </h2> 
 *           	<ul> Although items remain tied to their tiles, players cannot be moved off the board. If they are, then they
 *           are placed onto the beginning of the row/col just shifted (The Extra Tile). </ul> </ul>
 *           
 *      <ul> To Shift a column tiles the attributes of each tile are iterativly copied to the new position of each:
 *         <ul> 1 gets copied to 2, 2 gets copied to 3... 6 gets copied to Extra Tile and Extra Tile gets copied to 0. </ul>
 *         
 *      After a Row/Col has been shifted the board is repainted and the shifted boolean in {@link Engine} is set to true 
 *      removing the listeners and signaling the end of the shift phase within the play method.
 *      
 * @author Kyle Kolpack
 *
 */
public class Shift implements ActionListener {
    
	private Tile t;
	private int x_coor;
	private int y_coor;
	
	private Board b;
	private ExtraTileBox extra_Tile_Box;
	
	private LinkedList<Player> player_List;
	
	public Shift(Board board, int x, int y, ExtraTileBox etb, LinkedList<Player> p) {
		x_coor = x;
		y_coor = y;
		b = board;
		extra_Tile_Box = etb;
		
		player_List = p;
		
		t = board.get_Tile(x_coor, y_coor);
		t.get_Image().setEnabled(true);
		t.set_Highlight(true);
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		
		Engine.shifted = true;
		
		JPanel board_Panel = b.get_Board_Panel();
		
		Tile extra = new Tile();
		
		if(y_coor == 0) {
			extra.copy_Tile(b.get_Tile(x_coor, 6));
			b.get_Tile(x_coor,6).copy_Tile(b.get_Tile(x_coor, 5));
			b.get_Tile(x_coor,5).copy_Tile(b.get_Tile(x_coor, 4));
			b.get_Tile(x_coor,4).copy_Tile(b.get_Tile(x_coor, 3));
			b.get_Tile(x_coor,3).copy_Tile(b.get_Tile(x_coor, 2));
			b.get_Tile(x_coor,2).copy_Tile(b.get_Tile(x_coor, 1));
			b.get_Tile(x_coor,1).copy_Tile(b.get_Tile(x_coor, 0));
			b.get_Tile(x_coor,0).copy_Tile(b.get_Extra_Tile());
			
			for(int i = 0; i < player_List.size(); ++i) {
				int player_X = player_List.get(i).x_pos;
				int player_Y = player_List.get(i).y_pos;
				int player_num = player_List.get(i).get_Num();
				if(player_Y == 6 && player_X == x_coor) {
					extra.remove_Player(player_num);
					b.get_Tile(x_coor, 0).add_Player(player_num);
					player_List.get(i).set_Coors(player_X,0);
				}
				else if(player_X == x_coor) {
					player_List.get(i).set_Coors(player_X, player_Y+1);
				}
			}
			
			extra_Tile_Box.set_Extra_Tile(extra);
			b.set_Extra_Tile(extra);
		}
		else if(y_coor == 6) {
			extra.copy_Tile(b.get_Tile(x_coor, 0));
			b.get_Tile(x_coor,0).copy_Tile(b.get_Tile(x_coor, 1));
			b.get_Tile(x_coor,1).copy_Tile(b.get_Tile(x_coor, 2));
			b.get_Tile(x_coor,2).copy_Tile(b.get_Tile(x_coor, 3));
			b.get_Tile(x_coor,3).copy_Tile(b.get_Tile(x_coor, 4));
			b.get_Tile(x_coor,4).copy_Tile(b.get_Tile(x_coor, 5));
			b.get_Tile(x_coor,5).copy_Tile(b.get_Tile(x_coor, 6));
			b.get_Tile(x_coor,6).copy_Tile(b.get_Extra_Tile());
			
			for(int i = 0; i < player_List.size(); ++i) {
				int player_X = player_List.get(i).x_pos;
				int player_Y = player_List.get(i).y_pos;
				int player_num = player_List.get(i).get_Num();
				if(player_Y == 0 && player_X == x_coor) {
					extra.remove_Player(player_num);
					b.get_Tile(x_coor, 6).add_Player(player_num);
					player_List.get(i).set_Coors(player_X,6);
				}
				else if(player_X == x_coor) {
					player_List.get(i).set_Coors(player_X, player_Y-1);
				}
			}
			
			extra_Tile_Box.set_Extra_Tile(extra);
			b.set_Extra_Tile(extra);
		}
		else if(x_coor == 0) {
			extra.copy_Tile(b.get_Tile(6, y_coor));
			b.get_Tile(6,y_coor).copy_Tile(b.get_Tile(5,y_coor));
			b.get_Tile(5,y_coor).copy_Tile(b.get_Tile(4,y_coor));
			b.get_Tile(4,y_coor).copy_Tile(b.get_Tile(3,y_coor));
			b.get_Tile(3,y_coor).copy_Tile(b.get_Tile(2,y_coor));
			b.get_Tile(2,y_coor).copy_Tile(b.get_Tile(1,y_coor));
			b.get_Tile(1,y_coor).copy_Tile(b.get_Tile(0,y_coor));
			b.get_Tile(0,y_coor).copy_Tile(b.get_Extra_Tile());
			
			for(int i = 0; i < player_List.size(); ++i) {
				int player_X = player_List.get(i).x_pos;
				int player_Y = player_List.get(i).y_pos;
				int player_num = player_List.get(i).get_Num();
				if(player_X == 6 && player_Y == y_coor) {
					extra.remove_Player(player_num);
					b.get_Tile(0, y_coor).add_Player(player_num);
					player_List.get(i).set_Coors(0,player_Y);
				}
				else if(player_Y == y_coor) {
					player_List.get(i).set_Coors(player_X+1, player_Y);
				}
			}
			
			extra_Tile_Box.set_Extra_Tile(extra);
			b.set_Extra_Tile(extra);
		}
		else {
			extra.copy_Tile(b.get_Tile(0, y_coor));
			b.get_Tile(0,y_coor).copy_Tile(b.get_Tile(1,y_coor));
			b.get_Tile(1,y_coor).copy_Tile(b.get_Tile(2,y_coor));
			b.get_Tile(2,y_coor).copy_Tile(b.get_Tile(3,y_coor));
			b.get_Tile(3,y_coor).copy_Tile(b.get_Tile(4,y_coor));
			b.get_Tile(4,y_coor).copy_Tile(b.get_Tile(5,y_coor));
			b.get_Tile(5,y_coor).copy_Tile(b.get_Tile(6,y_coor));
			b.get_Tile(6,y_coor).copy_Tile(b.get_Extra_Tile());
			
			for(int i = 0; i < player_List.size(); ++i) {
				int player_X = player_List.get(i).x_pos;
				int player_Y = player_List.get(i).y_pos;
				int player_num = player_List.get(i).get_Num();
				if(player_X == 0 && player_Y == y_coor) {
					extra.remove_Player(player_num);
					b.get_Tile(6, y_coor).add_Player(player_num);
					player_List.get(i).set_Coors(6,player_Y);
				}
				else if(player_Y == y_coor) {
					player_List.get(i).set_Coors(player_X-1, player_Y);
				}
			}
			
			extra_Tile_Box.set_Extra_Tile(extra);
			b.set_Extra_Tile(extra);
		}
		
		board_Panel.validate();
		board_Panel.repaint();
	}

}
