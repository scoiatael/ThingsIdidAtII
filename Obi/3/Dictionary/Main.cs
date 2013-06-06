using System;

	class MainClass
	{
		public static void Main (string[] args)
		{
			dict<int,string> Klasa = new dict<int, string>();
			for(int i=0; i<2; i++)
			{
				string t=Console.ReadLine();
				int t1=t[0]-'0';
				string t2= Console.ReadLine();
				Console.WriteLine(Klasa.dodaj(t1, t2));
				//Console.WriteLine(t1);
				//Console.WriteLine(t2);
			}
			for(int i=0;; i++)
			{
				string t=Console.ReadLine();
				int t1=t[0]-'0';
				//Console.WriteLine(t1);
				string x = Klasa.znajdz(t1);
				if(x!=null)
					Console.WriteLine(x);
				else
					Console.WriteLine ("not found");
				t=Console.ReadLine();
				t1=t[0]-'0';
				//Console.WriteLine(t1);
				Console.WriteLine(Klasa.usun(t1));
			}
		}
	}

