package graphic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;

import unit.Board;
import unit.Tile;

/**
 * One of the Three Major Panels (The other two being the ScoreBoard and Board Classes). Holds the Extra Tile and InfoBoxes used to relay game
 * Information to the player (Tall Left Panel). </br>
 * 							  </br>
 * Given a reference to the Board so the Extra Tile can be retrieved it creates The Extra Tile and Info Boxes and Set's the layout of the Panel.
 * 
 * @author Kyle Kolpack
 *
 */
public class InfoBoard {
 
	private JPanel main_Panel;
	
	
	private ExtraTileBox Extra_Tile_Box;
	private InfoBox Info_Box;
	
	public JPanel get_Panel() {
		return main_Panel;
	}
	
	public InfoBoard(Board b, Tile extra_Tile) {
		main_Panel = new JPanel();
		main_Panel.setBackground(Color.LIGHT_GRAY);
		main_Panel.setPreferredSize(new Dimension(200,0));
		
		GridBagLayout g = new GridBagLayout();
		GridBagConstraints layout = new GridBagConstraints();
		main_Panel.setLayout(g);
		
		layout.gridy = 0;
    	layout.gridx = 0;
    	layout.ipadx = 160;
    	layout.ipady = 310;
    	
		Extra_Tile_Box = new ExtraTileBox(b, extra_Tile);
		JLabel Extra_Tile_Label = Extra_Tile_Box.get_Main_Label();
    	main_Panel.add(Extra_Tile_Label,layout);
    	
    	layout.gridy = 1;
    	layout.ipadx = 200;
    	
    	Info_Box = new InfoBox();
    	JLabel Info_Label = Info_Box.get_Main_Label();
    	main_Panel.add(Info_Label,layout);
    	
	}
	
	public InfoBox get_InfoBox() {
		return Info_Box;
	}
	
	public ExtraTileBox get_ExtraTileBox() {
		return Extra_Tile_Box;
	}
}
