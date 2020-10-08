package com.company;

import java.util.Random;

public class User extends Thread {
    Account account;
    Random rng = new Random();

    public User(String name, Account account) {
        this.setName(name);
        this.account = account;
    }

    public Action nextAction() throws InterruptedException {
        return (rng.nextBoolean()) ? Action.WITHDRAW : Action.DEPOSIT;
    }

    @Override
    public void run() {
        while (true) {
            try {
                Thread.sleep(200);
                int amount = rng.nextInt(10);
                switch (nextAction()) {
                    case WITHDRAW:
                        System.out.printf("%s is willing to withdraw %d dollars\n"
                                , Thread.currentThread().getName()
                                , amount);
                        account.withdraw(amount);
                        break;
                    case DEPOSIT:
                        account.deposit(amount);
                }
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
