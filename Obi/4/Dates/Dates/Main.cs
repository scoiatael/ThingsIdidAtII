using System;
namespace Dates
{
	class Godziny
	{
		int h,min,sek;
		override public string ToString()
		{
			string a = String.Concat(h.ToString()," ", min.ToString ()," ",sek.ToString());
			return a;		}
		public int godzina
		{
			get {return h;}
		}
		public int minuta
		{
			get {return min;}
		}
		public int sekunda
		{
			get {return sek;}
		}
		public int Sekundy
		{
			get
			{
				int sek1=godzina;
				sek1*=60;
				sek1+=minuta;
				sek1*=60;
				sek1+=sekunda;
				return sek1;
			}
		}
		public Godziny(int h,int min,int sek)
		{
			this.min=min%60;
			this.h=h%24;
			this.sek=sek%60;
			if(this.min!=min || this.h!=h || this.sek!=sek)
				throw new Exception("Bledna godzina");
		}
		public Godziny()
		{
			this.h=0;
			this.min=0;
			this.sek=0;
		}
		public void dodaj(int sek)
		{
			this.sek+=sek%60;
			sek/=60;
			this.min+=sek%60;
			sek/=60;
			this.h+=sek%24;
			if (this.sek>=60)
			{
				this.min++;
				this.sek-=60;
			}
			if (this.min>=60)
			{
				this.h++;
				this.min-=60;
			}
			if (this.h>=24)
				this.h%=24;
		}
		public static int operator-(Godziny A,Godziny B)
		{
			int sek1=A.Sekundy,sek2=B.Sekundy;
			return sek1-sek2;
		}
	}
	class Data
	{
		int dzien,miesiac,rok;
		public int Dzien
		{
			get{return dzien;}
		}
		public int Miesiac
		{
			get {return miesiac;}
		}
		public int Rok
		{
			get {return rok;}
		}
		public Data(int dzien, int miesiac, int rok)
		{
			this.miesiac=miesiac%13;
			this.rok=rok;
			if(miesiac==2)
				dzien=dzien%(28+przestepny(rok)+1);
			else
				dzien=dzien%(30+dluzszy (miesiac)+1);
			this.dzien=dzien;
			if(this.dzien!=dzien || this.miesiac!=miesiac ||this.rok!=rok)
				throw new Exception("Bledna data");
		}
		public Data()
		{
			dzien=1;
			miesiac=1;
			rok=2000;
		}
		static int dluzszy(int miesiac)
		{
			if(miesiac<=6)
				return (miesiac)%2;
			return (miesiac+1)%2;
		}
		static int przestepny(int rok)
		{
			if((rok%4==0 && rok%100 != 0) || rok%400 == 0)
				return 1;
			return 0;
		}
		public int Dni
		{
			get
			{
				int aku=0;
				if(miesiac>1)
					aku+=31;
				if(miesiac>2)
					aku+=28+przestepny(rok);
				for(int i=3;i<miesiac;i++)
					aku+=30+dluzszy(i);
				aku+=dzien;
				return aku;
			}
		}
		public void dodaj(int dni)
		{
			while(dni>0)
			{
				if(miesiac!=2)
				{
					if(dni<30+dluzszy(miesiac)-dzien)
					{
						dzien+=dni;
						dni=0;
					}
					else
					{
						dni-= 30+dluzszy(miesiac)-dzien+1;
						miesiac++;
						dzien=1;
						if(miesiac==13)
						{
							rok++;
							miesiac=1;
						}
					}
				}
				else
				{
					if(dni<28+przestepny (rok)-dzien)
					{
						dzien+=dni;
						dni=0;
					}
					else
					{
						dni-=28+przestepny (rok)-dzien+1;
						miesiac++;
						dzien=1;
					}
				}
			}
		}
		override public string ToString()
		{
			string a = String.Concat(rok.ToString()," ", miesiac.ToString ()," ",dzien.ToString());
			return a;
		}
		public static int operator-(Data A, Data B)
		{
			int aku=0, ARok=A.rok, BRok=B.rok;
			if(BRok>ARok)
			{
				return (-1)*(B-A);
			}
			else
			{
				if(ARok==BRok&&B.miesiac>A.miesiac)
				{
					return (-1)*(B-A);
				}
				else
				{
					if(ARok==BRok&&A.miesiac==B.miesiac&&B.Dzien>A.Dzien)
					{
						return (-1)*(B-A);
					}
				}
			}
			while(ARok-1>BRok)
			{
				aku+=365+przestepny(ARok);
				ARok--;
			}
			aku+=(A.Dni-B.Dni);
			return aku;
		}
	}
	class MainClass
	{
		public static void Main (string[] args)
		{
			try
			{
				while(true)
				{
					Console.WriteLine ("Data -> modul dat, Godzina-> modul godzin");
					Console.WriteLine ("w osobnych liniach: 3 wartosci dla konstruktora klasy (dwukrotnie), potem wartosc do dodania");
					string polecenie = (Console.ReadLine());
					int r1 = Int32.Parse(Console.ReadLine());
					int m1 = Int32.Parse(Console.ReadLine());
					int d1 = Int32.Parse(Console.ReadLine());
					int r2 = Int32.Parse(Console.ReadLine());
					int m2 = Int32.Parse(Console.ReadLine());
					int d2 = Int32.Parse(Console.ReadLine());
					int dd = Int32.Parse (Console.ReadLine ());
					Console.WriteLine ("------------------");
					Console.WriteLine ("dane, wynik dodania, efekt, roznica");
					if(polecenie=="data")
					{
						Data Var1 = new Data(d1,m1,r1);
						Data Var2 = new Data(d2,m2,r2);
						Console.WriteLine (Var1);
						Console.WriteLine (Var2);
						Var2.dodaj (dd);
						Var1.dodaj(dd);
						
						Console.WriteLine (Var1);
						Console.WriteLine (Var2);
						int dni = Var1-Var2;
						Console.WriteLine(dni);
					}
					else
					{
						Godziny Var1 = new Godziny(r1,m1,d1);
						Godziny Var2 = new Godziny(r2,m2,d2);
						Console.WriteLine (Var1);
						Console.WriteLine (Var2);
						Var2.dodaj (dd);
						Var1.dodaj(dd);
						int diff = Var1-Var2;
						Console.WriteLine(diff);
						Console.WriteLine (Var1);
						Console.WriteLine (Var2);
					}
				}
			}
		catch (Exception ee)
		{
			Console.WriteLine (ee.ToString());
		}
		}
	}
}
