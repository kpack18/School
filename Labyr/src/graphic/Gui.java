package graphic;

import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.JFrame;
import javax.swing.JPanel;

import unit.Board;
/**
 * The {@code Gui} Class Initializes the Main JPanel and JFrame that holds the entire Application. All Sub Components of the
 * Window are given via the Constructor and added to the JPanel. </br>
 * 
 * 			<ul> Components Needed: </br>
 * 				<ul> {@link ScoreBoard} </br>
 * 					 {@link InfoBoard} </br>
 * 					 {@link Board} </br> </ul> </ul>
 * 																 </br>
 * It Also holds references to the Main Color's used for the four players Score Panel's and Icon's
 * 
 * @author Kyle Kolpack
 */
public class Gui {
	
	public static final Color myRed = new Color(205,15,15);
	public static final Color myBlue = new Color(0,113,153);
	public static final Color myGreen = new Color(39,115,51);
	public static final Color myPurple = new Color(124,94,173);
	
	public static final Color RedBack = new Color(255,204,204);
	public static final Color BlueBack = new Color(204,242,255);
	public static final Color GreenBack = new Color(204,255,205);
	public static final Color PurpleBack = new Color(234,224,255);
	
	private JFrame frame;
	private JPanel info_Panel;
	private JPanel score_Panel;
	private JPanel board_Panel;
	
	public Gui(ScoreBoard scoreboard, InfoBoard infoboard, Board board) {
		frame = new JFrame("Labyrinth");
		frame.setSize(1000,1022); //900 x 900
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		info_Panel = infoboard.get_Panel();
		score_Panel = scoreboard.get_Score_Panel();
		board_Panel = board.get_Board_Panel();
		
		//Panel on portion of window: frame.add(panel,BorderLayout.SOUTH);
		frame.add(score_Panel,BorderLayout.SOUTH);
		frame.add(info_Panel,BorderLayout.WEST);
		frame.add(board_Panel,BorderLayout.CENTER);
		
		frame.setVisible(true);
		
	}

}
