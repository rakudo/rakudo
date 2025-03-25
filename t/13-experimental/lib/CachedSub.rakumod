use experimental :cached;

package CachedSub {
    sub frobnicate($a) is cached is export { 2 * $a }
}

# vim: expandtab shiftwidth=4
