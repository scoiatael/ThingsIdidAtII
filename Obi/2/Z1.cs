using System;

class IntStream
{
	protected int cur;
	protected int max=int.MaxValue/100;
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
	public new int next()
	{	
		//Console.Write (max);
		//Console.Write (ind);
		cur++;
		for(;cur < max;cur++)
			if(Primes[cur])
				return cur;
		return 0;
	}
	public new bool eos()
	{
		return (cur>max);
	}
	public PrimeStream()
	{
		cur=1;
		Primes = new bool[base.max];
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
		cur=1;
	}
}
class RandomStream: IntStream
{
	Random random = new Random();
	public new int next()
	{
		return random.Next(base.max);
	}
	public new bool eos()
	{
		return false;
	}
}

class RandomWordStream
{
	PrimeStream Pierw = new PrimeStream();
	RandomStream Los = new RandomStream();
	bool eos()
	{
		return Pierw.eos();
	}
	public string next()
	{
		if(!eos())
		{
			int length=Pierw.next();
			char[]Res=new char[length];
			//Console.Write(length);
			for(int i=0; i<length; i++)
			{
				//if(Los.eos())
				//	Los.reset();
				Res[i] = (char)(Los.next()%('z'-'a')+'a');
			}
			//Console.WriteLine(new string(Res));
			return new string(Res);
		}
		return null;
	}
}

class Run
{
	public static int Main()
	{
		RandomWordStream x = new RandomWordStream();
		for(int i=0; i<10; i++)
			Console.WriteLine(x.next());
		return 0;
	}
}