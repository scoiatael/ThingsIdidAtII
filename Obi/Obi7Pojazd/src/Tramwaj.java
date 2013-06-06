
public class Tramwaj extends Pojazd {

	/**
	 Auto-generated:
	 */
	private static final long serialVersionUID = -669560790913617337L;

	String Producent;
	String Model;
	int rozstaw_szyn;
	public Tramwaj()
	{
		super();
	}
	public Tramwaj(int r, String W, String k, String p,String m,int ro) {
		super(r, W, k);
		Producent=p;
		Model=m;
		rozstaw_szyn=ro;
	}
	
	public void set_P(String P)
	{
		Producent=P;
	}
	
	public void set_M(String M)
	{
		Model=M;
	}
	
	public void set_r(int r)
	{
		rozstaw_szyn=r;
	}
	
	@Override
	public String toString()
	{
		return "Tramwaj:\n"+super.toString()+"\nProducent: "+Producent+"\nModel: "+Model+"\nRozstaw szyn: "+rozstaw_szyn;
	}

}
