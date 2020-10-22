public class Flags {
    private boolean flag_alice;
    private boolean flag_bob;

    public Flags() {
        flag_alice = false;
        flag_bob = false;
    }

    // Returns the other neighbor current flag status
    public synchronized boolean query_flag(String s) {
        if (s.equals("bob")) return flag_alice;
        else return flag_bob;
    }

    private void set_flag(String s, boolean flag) {
        if (s.equals("alice")) { flag_alice = flag;}
        else { flag_bob = flag;}
    }

    public synchronized void set_true(String s) {
        set_flag(s, true);
    }

    public synchronized void set_false(String s) {
        set_flag(s, false);
    }
}
