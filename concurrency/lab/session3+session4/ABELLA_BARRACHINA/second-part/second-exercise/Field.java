public class Field {
    public static void main(String[] args) {
        Flags flags = new Flags();
        Card card = new Card();

        Thread a = new Neighbor(flags, card);
        a.setName("alice");

        Thread b = new Neighbor(flags, card);
        b.setName("bob");

        a.start();b.start();
    }
}
