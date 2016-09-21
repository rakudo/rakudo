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
}
