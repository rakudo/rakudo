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

my role X::Comp is X::Base {
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

my class X::Signature::Placeholder does X::Comp {
    method message() {
        'Placeholder variable cannot override existing signature';
    }
}

my class X::Placeholder::Block does X::Comp {
    has $.placeholder;
    method message() {
        "Placeholder variable $.placeholder may not be used here because the surrounding block takes no signature";
    }
}

my class X::Placeholder::Mainline does X::Comp {
    has $.placeholder;
    method message() {
        "Cannot use placeholder parameter $.placeholder in the mainline"
    }
}

my class X::Attribute::Undeclared does X::Comp {
    has $.name;
    has $.package-type;
    has $.package-name;
    method message() {
        "Attribute $.name not declared in $.package-type $.package-name";
    }
}

my class X::Redeclaration does X::Comp {
    has $.symbol;
    has $.postfix = '';
    has $.what    = 'symbol';
    method message() {
        "Redeclaration of $.what $.symbol$.postfix";
    }
}

my class X::Phaser::Once does X::Comp {
    has $.block;
    method message() { "Only one $.block block is allowed" }
}

my class X::Obsolete does X::Comp {
    has $.old;
    has $.new;
    has $.when = 'in Perl 6';
    method message() { "Unsupported use of $.old; $.when please use $.new" }
}

my class X::Parameter::Default does X::Comp {
    has $.how;
    method message() { "Cannot put default on $.how parameter" };
}

my class X::Parameter::Placeholder does X::Comp {
    has $.parameter;
    has $.right;
    method message() {
        "In signature parameter, placeholder variables like $.parameter are illegal\n"
        ~ "you probably meant a named parameter: '$.right'";
    }
}

my class X::Parameter::Twigil does X::Comp {
    has $.parameter;
    has $.twigil;
    method message() {
        "In signature parameter $.parameter, it is illegal to use the $.twigil twigil";
    }
}

my class X::Parameter::TypeConstraint does X::Comp {
    method message() {
        "A parameter may only have on prefix type constraint";
    }
}

my class X::Signature::NameClash does X::Comp {
    has $.name;
    method message() {
        "Name $.name used for more than one named parameter";
    }
}

my class X::Method::Private::Permission does X::Comp {
    has $.method;
    has $.source-package;
    has $.calling-package;
    method message() {
        "Cannot call private method '$.method' on package $.source-package because it does not trust $.calling-package";
    }
}

my class X::Method::Private::Qualified does X::Comp {
    has $.method;
    method message() {
        "Private method call to $.method must be fully qualified with the package containing the method";
    }
}
