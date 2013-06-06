import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;


public class IOClass extends JFrame {
	private static final long serialVersionUID = 1L;
	private JTextField input;
	private JTextArea output;
	public String log;
	private JScrollPane outPane;
	private JButton wykonaj;
	
	public IOClass()
	{
		JFrame main = new JFrame("DungeonQuest");
		main.setDefaultCloseOperation(EXIT_ON_CLOSE);
		
		input = new JTextField();
		wykonaj = new JButton("Wykonaj");
		wykonaj.setFocusable(true);
		wykonaj.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				synchronized(input)
				{
					input.notify();
				}
			}
		});
		input.setPreferredSize(new Dimension(4,20));
		output = new JTextArea(4,50);
		output.setEditable(false);
		output.setFocusable(false);
		output.setLineWrap(true);
		outPane = new JScrollPane(output);
		
		main.getContentPane().setLayout(new BorderLayout());
		main.getContentPane().add(outPane, BorderLayout.CENTER);
		main.getContentPane().add(input, BorderLayout.PAGE_END);
		main.getContentPane().add(wykonaj,BorderLayout.LINE_END);
		main.pack();
		main.setVisible(true);
	}
	
	public String getInput()
	{
		wykonaj.requestFocus();
		try
		{	
			synchronized(input)
			{
				input.wait();
			}
		}
		catch(Exception exc)
		{
			System.out.print(exc);
		}
		String s = input.getText();
		toOutput(s);
		log+=s;
		s.trim();
		input.setText("");
		return s;
	}
	
	public void toOutput(String s)
	{
		s+="\n";
		log+=s;
		output.append(s);
		output.setCaretPosition(output.getDocument().getLength());
	}
}
