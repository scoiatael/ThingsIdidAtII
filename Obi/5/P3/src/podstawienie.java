
public class podstawienie extends Instrukcja {
	int zmienna;
	Wyrazenie Aexp;
	@Override
	public State wykonaj(State v) throws Exception {
		v.set(zmienna, Aexp.oblicz(v));
		return v;
	}
	public podstawienie(int z, Wyrazenie A)
	{
		zmienna=z;
		Aexp=A;
	}

}
