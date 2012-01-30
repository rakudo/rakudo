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

my role X::Syntax does X::Comp { }
my role X::Pod                 { }

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

my class X::Phaser::Multiple does X::Comp {
    has $.block;
    method message() { "Only one $.block block is allowed" }
}

my class X::Obsolete does X::Comp {
    has $.old;
    has $.replacement; # can't call it $.new, collides with constructor
    has $.when = 'in Perl 6';
    method message() { "Unsupported use of $.old; $.when please use $.replacement" }
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

my class X::Parameter::MultipleTypeConstraints does X::Comp {
    method message() {
        "A parameter may only have on prefix type constraint";
    }
}

my class X::Parameter::WrongOrder does X::Comp {
    has $.misplaced;
    has $.after;
    method message() {
        "Cannot put $.misplaced parameter after $.after parameters";
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

my class X::Method::Private::Unqualified does X::Comp {
    has $.method;
    method message() {
        "Private method call to $.method must be fully qualified with the package containing the method";
    }
}

my class X::Bind::WrongLHS does X::Comp {
    method message() { 'Cannot use bind operator with this left-hand side' }
}
my class X::Bind::NativeType does X::Comp {
    method message() {
        'Cannot bind to a natively typed variable; use assignment instead'
    }
}

my class X::Syntax::Name::Null does X::Syntax {
    method message() { 'Name component my not be null'; }
}

my class X::Syntax::UnlessElse does X::Syntax {
    method message() { 'unless does not take "else", please rewrite using "if"' }
}

my class X::Syntax::P5 does X::Syntax {
    method message() { 'This appears to be Perl 5 code' }
}

my class X::Syntax::NegatedPair does X::Syntax {
    method message() { 'Argument not allowed on negated pair' }
}

my class X::Syntax::Variable::Numeric does X::Syntax {
    method message() { 'Cannot declare a numeric variable' }
}

my class X::Syntax::Variable::Match does X::Syntax {
    method message() { 'Cannot declare a match variable' }
}

my class X::Syntax::Variable::Twigil does X::Syntax {
    has $.twigil;
    has $.scope;
    method message() { "Cannot use $.twigil twigil on $.scope variable" }
}

my class X::Syntax::Variable::IndirectDeclaration does X::Syntax {
    method message() { 'Cannot declare a variable by indirect name (use a hash instead?)' }
}

my class X::Syntax::Augment::WithoutMonkeyTyping does X::Syntax {
    method message() { "augment not allowed without 'use MONKEY_TYPING'" };
}

my class X::Syntax::Augment::Role does X::Syntax {
    method message() { "Cannot augment a role, since roles are immutable" };
}

my class X::Syntax::Comment::Embedded does X::Syntax {
    method message() { "Opening bracket required for #` comment" }
}

my class X::Syntax::Pod::BeginWithoutIdentifier does X::Syntax does X::Pod {
    method message() {
        '=begin must be followed by an identifier; (did you mean "=begin pod"?)'
    }
}

my class X::Syntax::Pod::BeginWithoutEnd does X::Syntax does X::Pod {
    method message() { '=begin without matching =end' }
}

my class X::Syntax::Confused does X::Syntax {
    method message() { 'Confused' }
}

my class X::Syntax::SigilWithoutName does X::Syntax {
    method message() { 'Non-declarative sigil is missing its name' }
}
