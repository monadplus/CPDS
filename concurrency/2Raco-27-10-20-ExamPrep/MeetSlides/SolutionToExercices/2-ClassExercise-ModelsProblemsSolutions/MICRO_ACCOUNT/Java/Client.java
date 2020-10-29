import java.util.Random;

/*
 * Created on 07/01/2013
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */

public class  Client extends Thread{
	MicroAccount account;
	String name;
	
	public Client(String name, MicroAccount account){
		this.name=name;
		this.account=account;
	}

	public void run(){	
		while(true){
			try{
				if(Math.random()<0.5) {
					account.deposit();
					System.out.println(name + " deposit one euro");
				}
				else {
					account.withdraw();
					System.out.println(name + " withdraw one euro");
				}
			Thread.sleep(3000);
			}catch (InterruptedException e){}
		}
	}
}
