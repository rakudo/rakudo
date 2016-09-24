public class Foo {
    public static String bar() { return "baz"; }
    public String quux() { return "womble"; }
    public static String trizzle(Object[] in) {
        String out = "";
        for(Object elem : in) {
            out += elem;
        }
        return out;
    }
    public static String suzzle(java.util.List in) {
        String out = "";
        for(Object elem : in) {
            out += elem;
        }
        return out;
    }
    public static String foozzle(java.util.Map in) {
        String out = "";
        Object[] keys = in.keySet().toArray();
        java.util.Arrays.sort(keys);
        for(Object key : keys) {
            out += key + " => " + in.get(key) + ", ";
        }
        return out;
    }
}
