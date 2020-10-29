//CPDS Exam January 2013

public class MicroAccount {
	private int i=0;
	private int M;
	
	public MicroAccount(int bound) {
		M=bound;
		
	}
	public synchronized void deposit()
			throws InterruptedException{
		while(i==M)wait();
		++i;
		System.out.println("deposit");
		notifyAll();
	}

	public synchronized void withdraw()
			throws InterruptedException{
		while(i==0)wait();
		--i;
		System.out.println("withdraw");
		notifyAll();
	}
}
	
