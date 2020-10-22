public class Card {

    // 0 = neighbor1
    // 1 = neighbor2
    private boolean card;

    public Card() {
        this.card = false;
    }

    // Return true if the card I set in the past is the current one.
    public synchronized boolean sameCard(String name) {
        if (name == "alice") {
            return this.card;
        } else {
            return !this.card;
        }
    }

    public synchronized void set(String name) {
        this.card = (name == "alice");
    }
}
