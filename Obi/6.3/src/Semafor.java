
public class Semafor 
{
	boolean done;
	public synchronized boolean get()
	{
		return done;
	}
	public Semafor()
	{
		done=false;
	}
	public synchronized void set(boolean arg)
	{
		done=arg;
	}
}
