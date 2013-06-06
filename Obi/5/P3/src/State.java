
public class State {
	int[] Zmienne;
	boolean[] Initialized;
	int size;
	public int get (int indeks) throws Exception
	{
		if(indeks<0)
			throw new Exception("index below 0");
		if(indeks<size)
		{
			if(Initialized[indeks])
				return Zmienne[indeks];
			else 
				throw new Exception("variable not initialized");
		}
		else
			throw new Exception("index out of range");
	}
	public void set ( int indeks, int zmienna) throws Exception
	{
		if(indeks<0)
			throw new Exception("index below 0");
		if(size>indeks)
		{
			Zmienne[indeks]=zmienna;
			Initialized[indeks]=true;
		}
		else
		{
			int cur = size*2+1;
			if (indeks>cur)
			{
				cur=indeks;
			}
			int[] temp = new int [cur];
			boolean[] t2 = new boolean[cur];
			for(int i=0; i<size;i++)
			{
				temp[i]=Zmienne[i];
				t2[i]=Initialized[i];
			}
			Zmienne=temp;
			Initialized=t2;
			Zmienne[indeks]=zmienna;
			for(int i=size; i<cur;i++)
			{
				Initialized[i]=false;
			}
			Initialized[indeks]=true;
			size=cur;
		}
	}
	public State()
	{
		size=5;
		Zmienne = new int[size];
		Initialized = new boolean[size];
		for(int i=0;i<size;i++)
			Initialized[i]=false;
	}
}
