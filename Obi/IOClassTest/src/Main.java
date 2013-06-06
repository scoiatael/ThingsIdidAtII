
public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		IOClass test = new IOClass();
		for(int i=0; i<10; i++)
			test.toOutput("test" + Integer.toString(i));
		test.toOutput("next test");
		while(true)
			test.getInput();
	}

}
