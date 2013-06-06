import java.io.File;


public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		switch(args[0])
		{
		case("Pojazd"):
		{
			Pojazd t = new Pojazd();
			args[1]+="."+t.getClass().getCanonicalName()+"ser";
			File f = new File(args[1]);
			if (f.exists())
				t = Pojazd.Wczytaj(args[1]);
			new EdytorPojazdow(t,args[1]);
			break;
		}
		case("Tramwaj"):
		{
			Tramwaj t  = new Tramwaj();
			args[1]+="."+t.getClass().getCanonicalName()+"ser";
			File f = new File(args[1]);
			if (f.exists())
				t = (Tramwaj) Pojazd.Wczytaj(args[1]);
			new EdytorTramwaj(t,args[1]);
			break;
		}
		case("Samochod"):
		{
			Samochod t  = new Samochod();
			args[1]+="."+t.getClass().getCanonicalName()+"ser";
			File f = new File(args[1]);
			if (f.exists())
				t = (Samochod) Pojazd.Wczytaj(args[1]);
			new EdytorSamochod(t,args[1]);
			break;
		}
		default:
		{
			System.out.println("Brak takiego typu");
		}
		}
		
	}

}
