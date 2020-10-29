

public class Bank {
	
	public static void main(String[] args){
		MicroAccount account= new MicroAccount(3);
		
		Client alice = new Client("Alice", account);
		Client bob = new Client("Bob", account);
		alice.start();
		bob.start();
		
	}

}
