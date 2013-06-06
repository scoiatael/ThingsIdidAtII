
public class Samochod extends Pojazd {

	/**
	 Auto-generated:
	 */
	private static final long serialVersionUID = -6818950032361721248L;

	String marka;
	int przebieg;
	int spalanie;
	
	public void set_m(String m)
	{
		marka=m;
	}
	
	public void set_p(int p)
	{
		przebieg=p;
	}
	
	public void set_s(int s)
	{
		spalanie=s;
	}
	public Samochod()
	{
		super();
	}
	
	public Samochod(int r, String W, String k, String m, int p, int s) {
		super(r, W, k);
		marka=m;
		przebieg=p;
		spalanie=s;
	}
	@Override
	public String toString()
	{
		return "Samochod:\n"+super.toString()+"\nMarki: "+marka+"\nO przebiegu: "+przebieg+"km\nSpala " + spalanie + "na 100km.";
	}

}
