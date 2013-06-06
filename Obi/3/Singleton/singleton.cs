using System;


	public sealed class singleton
	{
		public int VAL;
		static int MAX=5;
		static singleton[] Tab = new singleton[MAX];
		static int cur=-1;
		static bool full=false;
		singleton ()
		{
			cur++;
		}
		static public singleton Create()
		{
			if (!full)
			{
				singleton nowy = new singleton();
				Tab[cur] = nowy;
				if (cur>=MAX-1)
					full=true;
				return nowy;
			}
			else
			{
				cur++;
				if(cur>=MAX)
					cur=0;
				return Tab[cur];
			}
				
		}
	}


