# XXX should really be my X::Base eventually
my package X {
    class Base is Exception {
        has $.message;

        multi method Str(Base:D:) {
            $.message.Str // 'Something went wrong'
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

    class NYI is Base {
        method message() { "$.feature not yet implemented. Sorry. " }
        has $.feature;
    }

    class OutOfRange is Base {
        has $.what = 'Argument';
        has $.got = '<unknown>';
        has $.range = '<unknown>';
        method message() {
            "$.what out of range. Is: $.got, should be in $.range"
        }
    }
}
