package com.company;

public class Account {
    private int balance = 0;

    public synchronized void withdraw(int amount) throws InterruptedException {
        while (balance < amount) {
            System.out.println("Not enough balance! " + Thread.currentThread().getName() + " has to wait");
            wait();
        }
        this.balance -= amount;
        System.out.printf("%s withdrawn %d dollars from the account...\n"
                , Thread.currentThread().getName()
                , amount);
        print_account_balance();
    }

    public synchronized void deposit(int amount) {
        this.balance += amount;
        System.out.printf("%s deposited %d dollars to the account...\n"
                , Thread.currentThread().getName()
                , amount);
        notifyAll();
        print_account_balance();
    }

    public synchronized void print_account_balance() {
        System.out.printf("Total account balance = %d\n"
                , this.balance);
    }
}