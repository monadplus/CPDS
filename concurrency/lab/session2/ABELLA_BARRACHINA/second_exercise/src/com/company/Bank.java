package com.company;

public class Bank {
    public static void main(String args[]) {
        Account account = new Account();
        User alice = new User("alice", account);
        User bob = new User("bob", account);
        Administrator admin = new Administrator(account);
        alice.start(); bob.start(); admin.start();
    }
}
