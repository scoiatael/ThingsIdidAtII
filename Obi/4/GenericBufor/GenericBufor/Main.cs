using System;

namespace GenericBufor
{
	class Bufor <T>
	{
		int first;
		int last;
		int size;
		T[] Table;
		int cur_size;
		public T Element
		{
			get 
			{
				if (cur_size==0)
					throw new System.Exception("Blad: pusty bufor");
				int temp=first;
				first++;
				if(first==size)
					first=0;
				cur_size--;
				return Table[temp]; 
			} 
		}
		private bool dodaj(T element)
		{
			if(cur_size>=size)
				return false;
			cur_size++;
			last++;
			if(last==size)
				last=0;
			Table[last]=element;
			return true;
		}
		public static bool operator +(Bufor<T> buf, T element)
		{
			return buf.dodaj(element);
		}
		public Bufor(int size)
		{
			if(size<=0)
				throw new Exception("Bledny rozmiar bufora");
			cur_size=0;
			Table=new T[size];
			first=0;
			last=-1;
			this.size=size;
		}
		public Bufor()
		{
			cur_size=0;
			Table = new T[1];
			first=0;
			size=1;
			last=-1;
		}
	}
	class MainClass
	{
		public static void Main (string[] args)
		{
			try
			{
				int size=Int32.Parse (Console.ReadLine ());
				Bufor<string> buf = new Bufor<string>(size);
				while(true)
				{
					string polecenie = Console.ReadLine ();
					if(polecenie=="dodaj")
					{
						string a = Console.ReadLine ();
						Console.WriteLine(buf+a);
					}
					else
						Console.WriteLine(buf.Element);
				}
			}
			catch(Exception exc)
			{
				Console.WriteLine(exc.ToString());
			}
		}
	}
}
