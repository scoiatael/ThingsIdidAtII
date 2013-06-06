
public class zlozenie extends Instrukcja {
	Instrukcja A;
	Instrukcja B;
	@Override
	public State wykonaj(State v) throws Exception {
			v = A.wykonaj(v);
			v= B.wykonaj(v);
			return v;
	}
	public zlozenie(Instrukcja a, Instrukcja b)
	{
		A=a;
		B=b;
	}

}
