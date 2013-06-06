
public class Stala extends Wyrazenie {
	int Wartosc;
	@Override
	public int oblicz(State V) throws Exception {
		return Wartosc;
	}
	public Stala(int w)
	{
		Wartosc=w;
	}

}
