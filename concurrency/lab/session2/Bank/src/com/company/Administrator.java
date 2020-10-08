package com.company;

/**
 * The Administrator only injects capital to the bank.
 */
public class Administrator extends User {
    public Administrator(Account account) {
        super("Administrator", account);
    }

    @Override
    public Action nextAction() throws InterruptedException {
        Thread.sleep(1000);
        return Action.DEPOSIT;
    }
}
