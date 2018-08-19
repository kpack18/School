package graphic;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JLabel;
import javax.swing.SwingConstants;

import labyrinth.Engine;
import unit.Player;
/**
 * JPanel Used to Show Information on the State of each player in the game including their Name, Score, Item, and Color. </br>
 * @author Kyle Kolpack
 *
 */
public class Score {
	
	private JLabel main_Label;

	private JLabel name_Text;
	private JLabel extra_Text;
	private JLabel item_Text;
	private JLabel score_Text;
	
	private GridBagLayout g;
	
	private GridBagConstraints main_Layout;
	private GridBagConstraints minor_Layout;
	
	private void set_MainLayout(int grid_X) {
		main_Layout = new GridBagConstraints();
		main_Layout.insets = new Insets(0,0,0,2);
		if(grid_X == 2) {
			main_Layout.ipady = 142;
			main_Layout.ipadx = 210;
			return;
		}
		main_Layout.gridwidth = 4;
		main_Layout.ipady = 142;
		main_Layout.ipadx = 200;
		
	}
	
	private void set_Layout(int grid_Y, int padX, int padY) {
		minor_Layout.gridy = grid_Y;
		minor_Layout.ipadx = padX;
		minor_Layout.ipady = padY;
	}
	
	public GridBagConstraints get_Layout() {
		return main_Layout;
	}
	
	public JLabel get_Label() {
		return main_Label;
	}
	/**
	 * Adds One Score Panel to the OverLay. (Iterates in the ScoreBoard Class for Every Player).
	 * @param grid_X The position on the JPanel (Player Number Essentially).
	 * @param player The Player this Score Will Represent
	 * @param textColor The Color of the Header representing the Player
	 * @param backColor The Color of the Background representing the Player
	 */
	public Score(int grid_X, Player player, Color textColor, Color backColor) {
		
		set_MainLayout(grid_X);
		
		g = new GridBagLayout();
		
		minor_Layout = new GridBagConstraints();
		minor_Layout.gridx = grid_X;
		
		main_Label = new JLabel();
        main_Label.setHorizontalAlignment(SwingConstants.CENTER);
        main_Label.setBackground(backColor);
        main_Label.setOpaque(true);
        main_Label.setLayout(g);  
		
	    
		 if(player != null) {
			name_Text = new JLabel(player.get_Name());
		    name_Text.setHorizontalAlignment(SwingConstants.CENTER);
		    name_Text.setVerticalAlignment(SwingConstants.TOP);
		    name_Text.setFont(new Font("Serif", Font.PLAIN, 24));
		    name_Text.setForeground(textColor);
		    set_Layout(0,0,0);
		    main_Label.add(name_Text,minor_Layout);
		    	
		    extra_Text = new JLabel("Card:");
		    extra_Text.setHorizontalAlignment(SwingConstants.CENTER);
		    extra_Text.setFont(new Font("Serif", Font.PLAIN, 12));
		    extra_Text.setForeground(textColor);
		    set_Layout(1,0,0);
		    main_Label.add(extra_Text,minor_Layout);
		    
		    item_Text = new JLabel("Item");
		    item_Text.setHorizontalAlignment(SwingConstants.CENTER);
		    item_Text.setFont(new Font("Serif", Font.PLAIN, 30));
		    set_Layout(2,0,10);
		    main_Label.add(item_Text,minor_Layout);
		    
		    
		    score_Text = new JLabel("Score 0/" + Integer.toString(Engine.score_Limit));
	    	score_Text.setVerticalAlignment(SwingConstants.BOTTOM);
	    	score_Text.setHorizontalAlignment(SwingConstants.RIGHT);
	    	score_Text.setFont(new Font("Serif", Font.PLAIN, 16));
	    	score_Text.setForeground(textColor);
	    	set_Layout(3,0,20);
	    	main_Label.add(score_Text,minor_Layout);
		    	
		 }
	}
	
	/**
	 * Update's the Item Text of their Score
	 * @param new_text The new Item to be Updated To.
	 */
	public void set_Item_Text(String new_text) {
		item_Text.setText(new_text);
		item_Text.repaint();
	}
	
	/**
	 * Used to Update the Score Text to the given integer
	 * @param player_score The Given Integer to Update To.
	 */
	public void set_Score_Text(int player_score) {
		score_Text.setText("Score: " + Integer.toString(player_score) + "/" + Engine.score_Limit);
		score_Text.repaint();
	}

}
