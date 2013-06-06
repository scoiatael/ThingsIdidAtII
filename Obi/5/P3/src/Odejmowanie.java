
public class Odejmowanie extends Operator2ar {

	public Odejmowanie(Wyrazenie t1, Wyrazenie t2) {
		super(t1, t2);
	}

	@Override
	public int oblicz(State V) throws Exception {
		return T1.oblicz(V)-T2.oblicz(V);
	}

}
