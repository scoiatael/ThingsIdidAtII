using System;

class ListaLeniwa
{
	protected int[] Tab;
	protected int size;
	public ListaLeniwa()
	{
		size=0;
	}
	public int element(int i)
	{
		Random random = new Random();
		if(i+1<size)
			return Tab[i];
		int[] TabT = new int[i+1];
		for(int j=0; j<size;j++)
			TabT[j]=Tab[j];
		for(int j=size; j<i+1;j++)
			TabT[j]=random.Next();
		Tab=TabT;
		size=i+1;
		return Tab[i];
	}
}

class IntStream
{
	int cur;
	protected int max=200;
	public int next()
	{
		if (cur+1<max)
		{
			cur+=1;
			return cur;
		}
		return 0;
		//throw string("Blad: przekroczono limit int");
	}
	public bool eos()
	{
		return cur>=max;
	}
	public void reset()
	{
		cur=-1;
	}
	public IntStream()
	{
		cur=-1;
	}
}

class PrimeStream: IntStream
{
	bool[] Primes;
	int ind;
	public new int next()
	{	
		//Console.Write (max);
		//Console.Write (ind);
		ind++;
		for(;ind < max;ind++)
			if(Primes[ind])
				return ind;
		return 0;
	}
	public new bool eos()
	{
		return (ind>max);
	}
	public PrimeStream()
	{
		ind=1;
		Primes = new bool[base.max+1];
		Primes[0]=false;
		Primes[1]=false;
		for(int i=2;i<base.max;i++)
			Primes[i]=true;
		for(int i=2;i<base.max;i++)
			if(Primes[i])
			{
				for(int y=2*i;y<base.max;y+=i)
					Primes[y]=false;
			}
	}
	public new void reset()
	{
		ind=1;
	}
}
class Pierwsze: ListaLeniwa
{
	PrimeStream PierwszeL = new PrimeStream();
	public new int element(int i)
	{
		if(i+1<size)
			return Tab[i];
		int []TabT = new int[i+1];
		for(int j=0; j<size;j++)
			TabT[j]=Tab[j];
		for(int j=size; j<i+1;j++)
			TabT[j]=PierwszeL.next();
		Tab=TabT;
		size=i+1;
		return Tab[i];
	}
}


class Run
{
	public static int Main()
	{
		Pierwsze ListaPierw = new Pierwsze();
		Random r = new Random();
		for(int i=0; i<20; i++)
			Console.WriteLine(ListaPierw.element(r.Next(30)));
		return 0;
	}
}