import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;


public class Pojazd implements Serializable {

	/**
	 Auto-generated:
	 */
	private static final long serialVersionUID = -4441225940908834853L;

	int Rok_Produkcji;
	String Wlasciciel;
	String kolor;
	
	public Pojazd(int r, String W, String k)
	{
		Rok_Produkcji=r;
		Wlasciciel=W;
		kolor=k;
	}
	
	public Pojazd()
	{}
	
	public void set_rok(int r)
	{
		Rok_Produkcji=r;
	}
	public void set_W(String W)
	{
		Wlasciciel=W;
	}
	public void set_k(String k)
	{
		kolor=k;
	}
	
	@Override
	public String toString()
	{
		return new String("Pojazd koloru" + kolor + "\nWlasciciel: "+ Wlasciciel +"\nWyprodukowany w roku: "+Rok_Produkcji);
	}
	

	public void Zapisz(String filename)
	{
		try
		{
			FileOutputStream fos = new FileOutputStream(filename);
			ObjectOutputStream oos = new ObjectOutputStream(fos);
			oos.writeObject(this);
			oos.close();
		}
		catch(Exception exc)
		{
			exc.printStackTrace();
		}
	}
	
	public static Pojazd Wczytaj(String filename)
	{
		Pojazd temp;
		try
		{
			FileInputStream fos = new FileInputStream(filename);
			InputStream buffer = new BufferedInputStream( fos );
			ObjectInputStream oos = new ObjectInputStream(buffer);
			temp = (Pojazd) oos.readObject();
			oos.close();
			return temp;
		}
		catch(Exception exc)
		{
			exc.printStackTrace();
		}
		return null;
	}
}
