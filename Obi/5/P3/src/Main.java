//import java.io.*;

public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		//4+2*x:
		//Wyrazenie w = new Dodaj(new Stala(4), new Mnozenie(new Stala(2), new Zmienna(0)));
		/*Instrukcja Potega3 = new zlozenie
				(new podstawienie
						(1, new Stala(1)), 
				new While(
						new Zmienna(0), 
						new zlozenie
							(new podstawienie
									(1, new Mnozenie(new Zmienna(1), new Stala(3))), 
							new podstawienie
									(0, new Odejmowanie(new Zmienna(0), new Stala(1)))
							)
						)
				);*/
		Instrukcja Silnia = new zlozenie
		(new podstawienie
				(1, new Stala(1)), 
		new While(
				new Zmienna(0), 
				new zlozenie
					(new podstawienie
							(1, new Mnozenie(new Zmienna(1), new Zmienna(0))), 
					new podstawienie
							(0, new Odejmowanie(new Zmienna(0), new Stala(1)))
					)
				)
		);
		State v;
		v = new State();
		try
		{
			
			for(int i=0; i<6; i++)
			{
				v.set(0, i);
				//System.out.println(w.oblicz(v));
				//System.out.println(Potega3.wykonaj(v).get(1));
				System.out.println(Silnia.wykonaj(v).get(1));
			}
		}
		catch(Exception str)
		{
			System.out.print(str);
		}
	}

}
