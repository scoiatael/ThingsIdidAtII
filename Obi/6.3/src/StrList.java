
public class StrList 
{
	StrListNode poczatek;
	@Override
	public String toString()
	{
		if(poczatek!=null)
			return poczatek.toString();
		else
			return null;
	}
	public void insert (String S)
	{
		if(poczatek!=null)
			poczatek.insert(S);
		else
			poczatek=new StrListNode(S);
	}
	public StrList()
	{
		poczatek=null;
	}
}
