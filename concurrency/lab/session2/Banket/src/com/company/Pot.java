package com.company;

public class Pot {
    private int servings = 0;
    private int capacity;

    public Pot(int capacity) {
        this.capacity = capacity;
    }

    public synchronized void getserving() throws InterruptedException {
        while (servings == 0) {
            System.out.println(Thread.currentThread().getName() + " has to wait");
            wait();
        }
        -- servings;
        System.out.println(Thread.currentThread().getName() + " is served");
        // Free the cooker
        if (servings == 0) notifyAll();
        print_servings();
    }

    public synchronized void fillpot() throws InterruptedException {
        while (servings > 0) {
            System.out.println(Thread.currentThread().getName() + " has to wait");
            wait();
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