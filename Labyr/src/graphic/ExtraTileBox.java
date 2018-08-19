package graphic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.border.Border;

import unit.Board;
import unit.Tile;
/**
 * The {@code ExtraTileBox} Class Contains the JLabel for the Extra Tile that is not located within the 7x7 board. It is continuously rotated
 * into the board every turn when shifting the row's and columns through the {@link Shift} Listener. </br>
 * 																									 </br>
 * It also Contains two JButtons used to Rotate the both the Image of the Tile as well as the directional Booleans located within the Tile Object
 * itself. 
 * @author Kyle Kolpack
 *
 */
public class ExtraTileBox {
	
	private GridBagLayout g;
	private GridBagConstraints main_Layout;
	private GridBagConstraints button_Layout;
	
	private JLabel main_Label;
	
	private JLabel tile_Text;
	private JLabel extra_Tile_Label;
	private JLabel item_Label;
	
	private JLabel button_Label;
	private JButton rotate_Left;
	private JButton rotate_Right;
	
	private Board board;
	
	private String type; // Options: "Corner" "Straight" or "T"
	private int angle;
	
	public JLabel get_Main_Label() {
		return main_Label;
	}
	
	private void set_Button_Layout() {
		button_Layout.gridy = 2;
    	button_Layout.gridx = 0;
    	button_Layout.ipadx = 160;
    	button_Layout.ipady = 80;
	}
	/**
	 * Set's the Layout of the JPanel using a GridBagLayout. The JButtons for Rotating are stored in a secondary JPanel with a layout as well.
	 * @param b A reference to the Board Object
	 * @param extra_Tile A Reference to the Extra_Tile object within the board.
	 */
	public ExtraTileBox(Board b, Tile extra_Tile) {
		
		board = b;
		type = extra_Tile.get_Type();
		angle = extra_Tile.get_Angle();
		
		g = new GridBagLayout();
		main_Layout = new GridBagConstraints();
		
		main_Label = new JLabel();
		main_Label.setLayout(g);
		
		tile_Text = new JLabel("Tile:");
    	tile_Text.setFont(new Font("Serif", Font.PLAIN, 30));
    	main_Label.add(tile_Text,main_Layout);
    	
    	extra_Tile_Label = new JLabel();
    	ImageIcon image = new ImageIcon("images/ExtraTile/" + type + "_E_" + Integer.toString(angle) + ".png");
    	Border border = BorderFactory.createLineBorder(Color.DARK_GRAY, 5);
    	extra_Tile_Label.setIcon(image);
    	extra_Tile_Label.setBorder(border);
    	extra_Tile_Label.setPreferredSize(new Dimension(150,150));
    	main_Layout.gridy = 1;
    	main_Label.add(extra_Tile_Label,main_Layout);
    	
    	item_Label = new JLabel();
    	item_Label.setMaximumSize(new Dimension(150,150));
		item_Label.setHorizontalAlignment(SwingConstants.LEFT);
		item_Label.setFont(new Font("Arial", Font.BOLD, 30));
    	extra_Tile_Label.setLayout(new GridBagLayout());
    	extra_Tile_Label.add(item_Label);
    	
    	button_Label = new JLabel();
    	button_Label.setLayout(g);
    	
    	rotate_Left = new JButton("<");
    	main_Layout.gridy = 0;
    	rotate_Left.setPreferredSize(new Dimension(70,50));
    	rotate_Left.setFont(new Font("Serif", Font.PLAIN, 30));
    	rotate_Left.setFocusable(false);
    	rotate_Left.addActionListener(get_Listener(true));
    	button_Label.add(rotate_Left,main_Layout);
    	
    	rotate_Right = new JButton(">");
    	main_Layout.gridx = 1;
    	rotate_Right.setPreferredSize(new Dimension(70,50));
    	rotate_Right.setFont(new Font("Serif", Font.PLAIN, 30));
    	rotate_Right.setFocusable(false);
    	rotate_Right.addActionListener(get_Listener(false));
    	button_Label.add(rotate_Right,main_Layout);
    	
    	button_Layout = new GridBagConstraints();
    	set_Button_Layout();
    	button_Label.setPreferredSize(new Dimension(50,50));
    	main_Label.add(button_Label,button_Layout);
		
	}
	
	/**
	 * If the inputed boolean is true a Listener for rotating the ExtraTile Label Left is returned. Otherwise the Right one is returned. </br>
	 * 
	 * When the JButton is pressed it will rotate the Image within the Extra Tile's JLabel 90 degrees in the appropriate direction. It also
	 * updates the type and angle within the Tile Object itself so when it is shifted into the board, the orientation matches it's new rotated state.
	 * 													</br> </br>
	 * @param is_Left if true the Left rotation listener is returned. Otherwise the Right one is.
	 * @return The Correct Actionlistener for the Rotation Buttons
	 */
	public ActionListener get_Listener(boolean is_Left) {
		ActionListener listener_Left = new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				if(type == "Straight") {
                	switch(angle) {
                	case 0: angle = 90; break;
                	case 90: angle = 0; break;
                	}
				}
                else { 
                	angle = angle + 90;
                }
				if(angle == 360) { angle = 0; }
				ImageIcon new_Icon = new ImageIcon("images/ExtraTile/" + type + "_E_" + Integer.toString(angle) + ".png");
				board.get_Extra_Tile().rotate_Left();
				extra_Tile_Label.setIcon(new_Icon);
				extra_Tile_Label.repaint();
			}
			
		};
		
		ActionListener listener_Right = new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
                if(type == "Straight") {
                	switch(angle) {
                	case 0: angle = 90; break;
                	case 90: angle = 0; break;
                	}
				}
                else { 
                	angle = angle - 90;
                }
				if(angle == -90) { angle = 270; }
				ImageIcon new_Icon = new ImageIcon("images/ExtraTile/" + type + "_E_" + Integer.toString(angle) + ".png");
				board.get_Extra_Tile().rotate_Right();
				extra_Tile_Label.setIcon(new_Icon);
				extra_Tile_Label.repaint();
			}
			
		};
		
		if(is_Left) {
			return listener_Left;
		} else {
			return listener_Right;
		}
		
	}
	
	/**
	 * Used to Update the Image of the Extra Tile depending on it's type and Label. </br>
	 * The Image is retrieved using the form "type" + "_E_" + "Angle" + ".png". Thus the correct image can be gathered using only the type and angle variables. </br>
	 *																						</br>
	 *  Note* The Item Text in the new Tile is also transfered to the ExtraTile Representation.
	 * @param new_Tile Refence to the new Tile needed to be represented by the Extra Tile Box
	 */
	public void set_Extra_Tile(Tile new_Tile) {
		type = new_Tile.get_Type();
	    angle = new_Tile.get_Angle();
		extra_Tile_Label.setIcon(new ImageIcon("images/ExtraTile/" + type + "_E_" + Integer.toString(angle) + ".png"));
		item_Label.setText(new_Tile.item_Image.getText());
		item_Label.validate();
		item_Label.repaint();
		extra_Tile_Label.repaint();
	}

	
}
