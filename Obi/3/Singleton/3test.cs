using System;

class App
{	
	public static void Main()
	{	
		for(int i=0; i<7; i++)
		{
			singleton a = singleton.Create();
			a.VAL=i;
		}
		for(int i=0; i<7; i++)
		{
			singleton a = singleton.Create();
			Console.WriteLine(a.VAL);
		}
	}	
}
