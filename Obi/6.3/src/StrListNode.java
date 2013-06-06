
public class StrListNode 
{
	String val;
	StrListNode Mniejszy;
	StrListNode Wiekszy;
	public StrListNode(String arg)
	{
		val=arg;
		Mniejszy=null;
		Wiekszy=null;
	}
	public void insert(String arg)
	{
		synchronized(this)
		{
			if(val.compareTo(arg)>=0)
			{
				if(Mniejszy==null)
					Mniejszy=new StrListNode(arg);
				else
					Mniejszy.insert(arg);
			}
			else
			{
				if(Wiekszy==null)
					Wiekszy=new StrListNode(arg);
				else
					Wiekszy.insert(arg);
			}
		}
	}
	@Override
	public String toString()
	{
		String efekt=val;
		if(Mniejszy!=null)
			efekt=Mniejszy+val;
		efekt=efekt+"\n";
		if(Wiekszy!=null)
			efekt=efekt+Wiekszy;
		return efekt;
	}
}
