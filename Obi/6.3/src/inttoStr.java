
public class inttoStr 
{
	int num;
	public inttoStr(int x)
	{
		num=x;
	}
	@Override
	public String toString()
	{
		String efekt;
		if(num<10)
			efekt="00"+Integer.toString(num);
		else
		{
			if(num<100)
				efekt="0"+Integer.toString(num);
			else 
				efekt=Integer.toString(num)+"-";
		}
		return efekt;
	}
	
}
