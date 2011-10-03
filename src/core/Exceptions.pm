# XXX should really be my X::Base eventually
my package X {
    class Base is Exception {
        has $.message;

        multi method Str(Base:D:) {
            # Just a stub so far.
            $!message.Str // 'Something went wrong'
        }
        method ID() { ... }
    }
    role OS {
        has $.os-error;
    }

    role Comp {
        has $.filename;
        has $.line;
        has $.column;
    }

    role NYI {
        has $.feature;
    }

}
