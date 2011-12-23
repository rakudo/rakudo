# XXX should really be my X::Base eventually
my class X::Base is Exception {
    has $.message;

    multi method Str(X::Base:D:) {
        $.message.Str // 'Something went wrong'
    }
    method ID() { ... }
    multi method gist(X::Base:D:) {
        $.message ~ "\n" ~ $.backtrace;
    }
}
my role X::OS {
    has $.os-error;
}

my role X::Comp {
    has $.filename;
    has $.line;
    has $.column;
    multi method gist(::?CLASS:D:) {
        "===SORRY!===\n$.message\nat $.filename():$.line";
    }
}

my class X::NYI is X::Base {
    has $.feature;
    method message() { "$.feature not yet implemented. Sorry. " }
}

my class X::OutOfRange is X::Base {
    has $.what = 'Argument';
    has $.got = '<unknown>';
    has $.range = '<unknown>';
    method message() {
        "$.what out of range. Is: $.got, should be in $.range"
    }
}

my class X::Buf::AsStr is X::Base {
    has $.method;
    method message() {
        "Cannot use a Buf as a string, but you called the $.method method on it";
    }
}

my class X::Signature::Placeholder is X::Base does X::Comp {
    method message() {
        'Placeholder variable cannot override existing signature';
    }
}

my class X::Attribute::Undeclared is X::Base does X::Comp {
    has $.name;
    has $.package-type;
    has $.package-name;
    method message() {
        "Attribute $.name not declared in $.package-type $.package-name";
    }
}

my class X::Obsolete is X::Base does X::Comp {
    has $.old;
    has $.new;
    has $.when = 'in Perl 6';
    method message() { "Unsupported use of $.old; $.when please use $.new" }
}
