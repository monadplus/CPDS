package com.company;

public class BadPot {
    private int servings = 0;
    private int capacity;

    public BadPot(int capacity) {
        this.capacity = capacity;
    }

    public synchronized void getserving() throws InterruptedException {
        if (servings == 0) {
            System.out.println(Thread.currentThread().getName() + " has to wait");
            // wait();
            return;
        }
        -- servings;
        System.out.println(Thread.currentThread().getName() + " is served");
        // Free the cooker
        if (servings == 0) notifyAll();
        print_servings();
    }

    public synchronized void fillpot() throws InterruptedException {
        if (servings > 0) {
            System.out.println(Thread.currentThread().getName() + " has to wait");
            // wait();
            return;
        }
        servings = capacity;
        System.out.println(Thread.currentThread().getName() + " fills the pot");
        print_servings();
        notifyAll();
    }

    public synchronized void print_servings() {
        System.out.println("servings in the pot: " + servings);
    }
}