
abstract class Operator2ar extends Wyrazenie {

	Wyrazenie T1;
	Wyrazenie T2;
	public abstract int oblicz(State V) throws Exception;
	public Operator2ar(Wyrazenie t1, Wyrazenie t2)
	{
		T2=t2;
		T1=t1;
	}
}
