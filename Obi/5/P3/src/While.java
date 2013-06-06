
public class While extends Warunek {

	public While(Wyrazenie b, Instrukcja s) {
		super(b, s);
	}
	
	@Override
	public State wykonaj(State v) throws Exception {
		while(Bexp.oblicz(v)!=0)
			v= Stm.wykonaj(v);
		return v;
	}

}
