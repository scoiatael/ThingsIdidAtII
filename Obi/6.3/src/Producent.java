public class Producent implements Runnable 
{
	Semafor czyKon;
	Bufor buf;
	String[] napisy;
	int size;
	public Producent(Bufor in, String[] wejscie, int rozmiar, Semafor arg)
	{
		buf=in;
		napisy=wejscie;
		size=rozmiar;
		czyKon=arg;
	}
	public void run()
	{
		czyKon.set(false);
		for(int i=size-1;i>=0;i--)
		{
			if(i%22==0)
			{
				if(!buf.full())
					synchronized(czyKon)
					{
						if(!buf.full())
						{
							while(!buf.full())
							{
								boolean x = buf.push(new String(napisy[i]+" jest podzielna przez 22"));
								if(x==false)
									System.out.println("bufor pelny");
							}
							czyKon.notifyAll();
						}
						else
						{
							try
							{
								czyKon.wait();
							}
							catch (Exception e)
							{
								System.out.println("Producent: "+e);
							}
						}
					}
			}
		}
		synchronized(czyKon)
		{
			czyKon.notifyAll();
		}
		System.out.println("Producent zakonczyl dzialanie");
		czyKon.set(true);
	}

}
