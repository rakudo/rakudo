# XXX should really be my X::Base eventually
my class X::Base is Exception {
    has $.message;

    multi method Str(X::Base:D:) {
        $.message.Str // 'Something went wrong'
    }
    method ID() { ... }
}
my role X::OS {
    has $.os-error;
}

my role X::Comp {
    has $.filename;
    has $.line;
    has $.column;
}

my class X::NYI is X::Base {
    method message() { "$.feature not yet implemented. Sorry. " }
    has $.feature;
}

my class X::OutOfRange is X::Base {
    has $.what = 'Argument';
    has $.got = '<unknown>';
    has $.range = '<unknown>';
    method message() {
        "$.what out of range. Is: $.got, should be in $.range"
    }
}

my class X::Buf::AsStr {
    has $.method;
    method message() {
        "Cannot use a Buf as a string, but you called the $.method method on it";
    }
}
