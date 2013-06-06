import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


public class EdytorPojazdow extends JComponent {

	/**
	 Auto-generated:
	 */
	private static final long serialVersionUID = -5776284511011286188L;
	
	Pojazd ModelRef;
	JTextField kolor;
	JTextField wlasc;
	JTextField rok;
	String filename;
	public EdytorPojazdow()
	{}
	
	public EdytorPojazdow(Pojazd M, String f)
	{
		ModelRef=M;
		filename = f;
		JFrame frame = new JFrame("Edycja pojazdu");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		Container kontener = frame.getContentPane();
		GridLayout layout = new GridLayout(4, 2);
		kontener.setLayout(layout);
		
		JLabel rok_etykieta = new JLabel("Rok Produkcji");
		kontener.add(rok_etykieta);
		rok = new JTextField(Integer.toString(ModelRef.Rok_Produkcji), 40);
		kontener.add(rok);
		
		JLabel wlasc_etykieta = new JLabel("Wlasciciel");
		kontener.add(wlasc_etykieta);
		wlasc = new JTextField(ModelRef.Wlasciciel, 40);
		kontener.add(wlasc);
		
		JLabel kolor_etykieta = new JLabel("Kolor");
		kontener.add(kolor_etykieta);
		kolor = new JTextField(ModelRef.kolor, 40);
		kontener.add(kolor);
		
		JButton b = new JButton("Zapisz");
		b.addActionListener(
				new ActionListener() 
				{
					public void actionPerformed(ActionEvent evt)
					{
						ModelRef.set_k(kolor.getText());
						ModelRef.set_rok(Integer.parseInt(rok.getText()));
						ModelRef.set_W(wlasc.getText());
						ModelRef.Zapisz(filename);
					}
				});
		kontener.add(b);
		frame.pack();
		frame.setVisible(true);
	}

}
