using System;

public class dict <K,V>
	{
		K[] klucze;
		int kl_size=0;
		V[] obiekty;
		
		public bool dodaj(K klucz, V obiekt)
		{
			for(int i=0; i<kl_size; i++)
				if(klucze[i].Equals(default(K)) || klucze[i].Equals(klucz))
				{
					klucze[i]=klucz;
					obiekty[i]=obiekt;
					return true;
				}
			int new_size=kl_size*2+1;
			
			K[] kluczeT = new K[new_size];
			for(int i=0; i<kl_size; i++)
				kluczeT[i]=klucze[i];
			
			V[] obiektyT = new V[new_size];
			for(int i=0; i<kl_size; i++)
				obiektyT[i]=obiekty[i];
			
			obiektyT[kl_size]=obiekt;
			kluczeT[kl_size]=klucz;
			
			kl_size=new_size;
			obiekty=obiektyT;
			klucze=kluczeT;
			return true;
			
		}
		
		public bool usun(K klucz)
		{
			for(int i=0; i<kl_size;i++)
				if(klucze[i].Equals(klucz))
				{
					klucze[i]=default(K);
					return true;
				}
			return false;
		}
		
		public V znajdz(K klucz)
		{
			for(int i=0; i<kl_size; i++)
				if(klucze[i].Equals(klucz))
					return obiekty[i];
			return default(V);
			//throw string(wyjatek);
		}
	}
