
public class Main {

	public static void main(String[] args) {
		try 
		{
			Semafor czyKon = new Semafor();
			String[] wejscie = new String[1000];
			for(int i=0;i<1000;i++)
				wejscie[i]=new String(new inttoStr(i).toString()+"ta liczba");
			Bufor komunikacja = new Bufor(10);
			Thread[] Konsumpcja = new Thread[2];
			for(int i=0;i<2;i++)
			{
				Konsumpcja[i]= new Thread(new Konsument(komunikacja,i,czyKon));
				Konsumpcja[i].start();
			}
			Thread Produkcja = new Thread(new Producent(komunikacja,wejscie,1000,czyKon));
			Produkcja.start();
		} 
		catch (Exception e) 
		{
			System.out.println(e);
		}
		
	}

}
