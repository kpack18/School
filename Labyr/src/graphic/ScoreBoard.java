package graphic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.util.ArrayList;
import java.util.LinkedList;

import javax.swing.JPanel;

import labyrinth.Engine;
import unit.Player;

/**
 * One of the Three Major Panels (Others include InfoBoard and Board Classes). The Bottom most panel houses the Information pertaining to each
 * Player defined by the input args. </br>
 * The JPanel is created and {@link Score} classes are created for each player in the player_List Iteratively. </br>
 * 
 * @author Kyle Kolpack
 *
 */
public class ScoreBoard {
	
	private JPanel score_Panel;
	
	private ArrayList<Score> score_List;
	
	public ScoreBoard(LinkedList<Player> player_List){
		
		score_Panel = new JPanel();
		score_Panel.setBackground(Color.LIGHT_GRAY);
		score_Panel.setPreferredSize(new Dimension(0,200));
		
		GridBagLayout g = new GridBagLayout();
		score_Panel.setLayout(g);
			
		score_List = new ArrayList<Score>();
		
		for(int i = 0; i < Engine.number_Of_Players; ++i) {
			Player current = player_List.get(i);
			Score p = new Score(i, current, current.get_Color(), current.get_Back_Color());
			score_Panel.add(p.get_Label(),p.get_Layout());
			score_List.add(p);
		}
		
	}
	
	public JPanel get_Score_Panel() {
		return score_Panel;
	}
	
	/**
	 * The {@code update_Item} function changes the Item Text within the given player's score panel </br> 
	 * ({@link Score} class) </br>
	 * 
	 * @param player Given Player to Update
	 * @param new_Item The New Item the Player must find.
	 */
	public void update_Item(Player player, String new_Item) {
		int player_num = player.get_Num();
		Score player_score = score_List.get(player_num-1);
		player_score.set_Item_Text(new_Item);
		
		score_Panel.repaint();
	}
	
	/**
	 * The {@code update_Score} function replaces the score label within the given player's score panel to whatever their current
	 * score variable is. </br>
	 * @param player The given player to update.
	 */
	public void update_Score(Player player) {
		int player_score = player.get_Score();
		Score score = score_List.get(player.get_Num()-1);
		score.set_Score_Text(player_score);
	}

}
