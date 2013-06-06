//Operuje na obiektach klasy Obiekt, po wywolaniu push trzeba sie upewniac ze dobrze uzywamy obiektu.
public class Bufor
{
	int size;
	int cur_size;
	int first;
	int last;
	Object elementy[];
	public Bufor(int s) throws Exception
	{
		size=s;
		if(s<0)
		{
			throw new Exception("Wrong size");
		}
		cur_size=0;
		first=0;
		last=-1;
		elementy = new Object[s];
	}
	
	public Object pop() throws Exception
	{
		synchronized(this)
		{
			if(cur_size<=0)
				throw new Exception("Empty buffer");
			cur_size--;
			int t=first;
			first++;
			first = first%size;
			return elementy[t];
		}
	}
	
	public boolean push(Object o)
	{
		synchronized(this)
		{
			if(cur_size>=size)
				return false;
			last++;
			last=last%size;
			elementy[last]=o;
			cur_size++;
			return true;
		}
	}
	
	public synchronized boolean empty()
	{
		return (cur_size==0);
	}
	
	public synchronized boolean full()
	{
		return (cur_size>=size);
	}
}
