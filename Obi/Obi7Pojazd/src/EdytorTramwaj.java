import javax.swing.*;

import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.*;


public class EdytorTramwaj extends EdytorPojazdow {

	/**
	 Auto-generated:
	 */
	private static final long serialVersionUID = -1277816025619604206L;
	
	JTextField Producent;
	JTextField Model;
	JTextField Rozstaw_szyn;
	Tramwaj ModelRef;
	
	
	public EdytorTramwaj(Tramwaj M, String f) 
	{	
		ModelRef=M;
		filename = f;
		
		JFrame frame = new JFrame("Edycja tramwaju");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		Container kontener = frame.getContentPane();
		GridLayout layout = new GridLayout(7, 2);
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
						ModelRef.set_P(Producent.getText());
						ModelRef.set_M(Model.getText());
						ModelRef.set_r(Integer.parseInt(Rozstaw_szyn.getText()));
						ModelRef.Zapisz(filename);
					}
				});
		
		JLabel rozstaw_etykieta = new JLabel("Rozstaw szyn");
		kontener.add(rozstaw_etykieta);
		Rozstaw_szyn = new JTextField(Integer.toString(ModelRef.rozstaw_szyn), 40);
		kontener.add(Rozstaw_szyn);
		
		JLabel pro_etykieta = new JLabel("Producent");
		kontener.add(pro_etykieta);
		Producent = new JTextField(ModelRef.Producent, 40);
		kontener.add(Producent);
		
		JLabel mo_etykieta = new JLabel("Model");
		kontener.add(mo_etykieta);
		Model = new JTextField(ModelRef.Model, 40);
		kontener.add(Model);

		kontener.add(b);
		frame.pack();
		frame.setVisible(true);
		
	}

}
