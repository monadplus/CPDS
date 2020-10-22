public class Neighbor extends Thread {
    private Flags flags;
    private Card card;

    public Neighbor(Flags flags, Card card) {
        this.flags = flags;
        this.card = card;
    }

    @Override
    public void run() {
        while (true) {
            try {
               String name = Thread.currentThread().getName();
               System.out.println("try again, my name is: " + name);
               // Not greedy
               // Thread.sleep((int)(200*Math.random()));
               flags.set_true(name);
               card.set(name);
               // Greedy
               Thread.sleep((int)(200*Math.random()));
               while (flags.query_flag(name) && card.sameCard(name)) {
                 System.out.println(name + " waiting...");
                 Thread.sleep(400);
               }
               System.out.println(name + " enter");
               Thread.sleep(400);
               System.out.println(name + " exits");
               Thread.sleep((int)(200*Math.random()));
               flags.set_false(name);
            } catch (InterruptedException e ) {};
        }
    }
}
