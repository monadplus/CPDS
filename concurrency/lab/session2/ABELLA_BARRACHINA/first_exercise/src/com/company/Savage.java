package com.company;

public class Savage extends Thread {
    Pot pot;

    public Savage(Pot pot) {
        this.pot = pot;
    }

    @Override
    public void run() {
        while (true) {
            System.out.println(Thread.currentThread().getName() + " is hungry and would like to eat");
            try {
                Thread.sleep(200);
                pot.getserving();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
