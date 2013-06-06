
public class Konsument implements Runnable 
{
	Semafor czyKon;
	StrList Lista;
	Bufor buf;
	int nr;
	public Konsument(Bufor in, int numer,Semafor arg)
	{
		buf=in;
		nr=numer;
		Lista = new StrList();
		czyKon=arg;
	}
	@Override
	public void run() 
	{
		while(!czyKon.get() || (!buf.empty()))
		{
			if(!buf.empty())
				synchronized(czyKon)
				{
					try 
					{		
						if(!buf.empty())
						{
							//while(!buf.empty())
							{
								Object s = buf.pop();
								Lista.insert(s.toString());
							}
							czyKon.notifyAll();
						}
						else
							if(!czyKon.get())
								czyKon.wait();
					
					} 
					catch (Exception e) {
						System.out.println("Thread nr"+Integer.toString(nr)+" "+e);
					}
				}
		}
		//System.out.println("Kons nr"+nr+" ok");
		System.out.println("Konsument nr"+Integer.toString(nr)+"\n"+Lista);
			
	}

}
