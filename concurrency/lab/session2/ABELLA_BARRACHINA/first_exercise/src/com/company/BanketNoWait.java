package com.company;

public class BanketNoWait {
    public static void getSavage(String name, Pot pot) {
        Thread r = new Savage(pot);
        r.setName(name);
        r.start();
    }
    public static void main(String args[]) {
        Pot pot = new Pot(5);
        getSavage("alice", pot);
        getSavage("bob", pot);
        getSavage("peter", pot);
        getSavage("xana", pot);
        getSavage("tom", pot);
        getSavage("jerry", pot);
        getSavage("kim", pot);
        getSavage("berta", pot);

        Thread c = new Cook(pot);
        c.setName("cook");
        c.start();
    }
}
