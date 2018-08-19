package graphic;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.border.Border;
/**
 * The {@code InfoBox} Class Controls the JLabel that relay's information of the state of the game to the Players. Generally it is used
 * to show who's turn it is and which phase they are in.
 * 
 * @author Kyle Kolpack
 *
 */
public class InfoBox {
	
	private JLabel main_Label;
	
	private JLabel info_Label;
	private JLabel info_Text;
	private JLabel info_Detail_Text;
	
	private GridBagLayout g;
	private GridBagConstraints layout;
	
	public JLabel get_Main_Label() {
		return main_Label;
	}
	/**
	 * Just Initializes the layout of the JPanels
	 */
	public InfoBox() {
		
		g = new GridBagLayout();
		layout = new GridBagConstraints();
		main_Label = new JLabel();
		main_Label.setLayout(g);
		
		info_Label = new JLabel();
    	info_Label.setBackground(new Color(247,247,247));
    	Border border = BorderFactory.createLineBorder(Color.DARK_GRAY, 5);
    	info_Label.setBorder(border);
    	info_Label.setOpaque(true);
    	info_Label.setFocusable(false);
    	info_Label.setLayout(g);
    	
    	info_Text = new JLabel("Player 2:");
    	info_Text.setFont(new Font("Serif", Font.BOLD, 20));
    	
    	layout.insets = new Insets(0,0,0,90);
    	layout.gridx = 0;
    	layout.gridy = 0;
    	layout.ipadx = 0;
    	layout.ipady = 0; //180
    	info_Label.add(info_Text,layout);
    	
    	info_Detail_Text = new JLabel("<html>Shift Phase:<br/> Please Select A Col/Line to Shift</html>");
    	info_Detail_Text.setFont(new Font("Serif", Font.PLAIN, 18));
        
    	layout.insets = new Insets(5,20,110,0);
    	layout.gridx = 0;
    	layout.gridy = 1;
    	layout.ipadx = 100;
    	layout.ipady = 0;
    	info_Label.add(info_Detail_Text,layout);
    	
    	layout.gridy = 3;
    	layout.ipadx = 180;
    	layout.ipady = 220;
    	layout.insets = new Insets(75,0,0,0);
    	main_Label.add(info_Label,layout);
    	
    	//For Info Box: Surround the string with <html></html> and break the lines with <br/>.
    	//JLabel l = new JLabel("<html>Hello World!<br/>blahblahblah</html>", SwingConstants.CENTER);
	}
	
	/**
	 * Used to Change the Message from 3 Presets based on the inputed integer. The Three options for messages are used during:
	 * 										<ul> Message 0: Move Phase </br>
	 * 											 Message 1: Shift Phase </br>
	 * 											 Message 2: Victory Phase (End of Game) </ul>
	 * @param type integer that controls which message appears.
	 */
	public void change_Message(int type) {
		String message = "";
		switch(type) {
		case 0: message = "<html>Move Phase:<br/> Please Move to a new available tile</html>"; break;
		case 1: message = "<html>Shift Phase:<br/> Please Select A Col/Line to Shift</html>"; break;
		case 2: message = "<html>Congrats!<br/>You Have Won!</html>"; break;
		}
		info_Detail_Text.setText(message);
		info_Detail_Text.repaint();
	}
	
	/**
	 * Used to Change Which Player is represented in the Header of the InfoBox. It also changes the background of the box to their color.
	 * @param player_num The Given Player to Change To.
	 */
	public void change_Header(int player_num) {
		info_Text.setText("Player " + Integer.toString(player_num));
		switch(player_num) {
		case 1: info_Label.setBackground(Gui.RedBack); break;
		case 2: info_Label.setBackground(Gui.BlueBack); break;
		case 3: info_Label.setBackground(Gui.GreenBack); break;
		case 4: info_Label.setBackground(Gui.PurpleBack); break;
		}
		info_Text.repaint();
		info_Label.repaint();
	}
	
	
}
