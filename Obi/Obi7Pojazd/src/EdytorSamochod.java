import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;


public class EdytorSamochod extends EdytorPojazdow {

	/**
	 Auto-generated:
	 */
	private static final long serialVersionUID = -5055969205152403059L;
	JTextField marka;
	JTextField przebieg;
	JTextField spalanie;
	Samochod ModelRef;
	
	public EdytorSamochod(Samochod M, String f) {
		
		ModelRef=M;
		filename = f;
		JFrame frame = new JFrame("Edycja samochodu");
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
						ModelRef.set_m(marka.getText());
						ModelRef.set_p(Integer.parseInt(przebieg.getText()));
						ModelRef.set_s(Integer.parseInt(spalanie.getText()));
						ModelRef.Zapisz(filename);
					}
					
				});
		
		JLabel sp_etykieta = new JLabel("Spalanie");
		kontener.add(sp_etykieta);
		rok = new JTextField(Integer.toString(ModelRef.spalanie), 40);
		kontener.add(rok);
		
		JLabel ma_etykieta = new JLabel("Marka");
		kontener.add(ma_etykieta);
		wlasc = new JTextField(ModelRef.marka, 40);
		kontener.add(wlasc);
		
		JLabel pr_etykieta = new JLabel("Przebieg");
		kontener.add(pr_etykieta);
		kolor = new JTextField(Integer.toString(ModelRef.przebieg), 40);
		kontener.add(kolor);
		
		kontener.add(b);
		frame.pack();
		frame.setVisible(true);
		
	}


}
