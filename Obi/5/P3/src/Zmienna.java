
public class Zmienna extends Wyrazenie {

	int Indeks;
	@Override
	public int oblicz(State V) throws Exception {
		return V.get(Indeks);
	}
	public Zmienna(int I)
	{
		Indeks=I;
	}

}
