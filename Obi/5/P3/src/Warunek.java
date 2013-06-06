
public class Warunek extends Instrukcja {
	Wyrazenie Bexp;
	Instrukcja Stm;
	@Override
	public State wykonaj(State v) throws Exception {
		if(Bexp.oblicz(v)==0)
			return v;
		return Stm.wykonaj(v);
	}
	
	public Warunek(Wyrazenie b, Instrukcja s)
	{
		Bexp=b;
		Stm=s;
	}

}
