# XXX should really be my X::Base eventually
my package X {
    class Base {
        has $.backtrace;
        has $.message;

        method Str() {
        # Just a stub so far.
        $!message.Str // 'Something went wrong'
        }
    #    method ID() { ... }
    }
}
