my role X::Comp { ... }

my class Exception {
    has $!ex;

    method backtrace() { Backtrace.new(self) }


    multi method Str(Exception:D:) {
        self.?message.Str // 'Something went wrong'
    }
    multi method gist(Exception:D:) {
        my $str = try self.?message ~ "\n" ~ $.backtrace;
        $! ?? "Error while creating error string" 
           !! $str;
    }

    method throw() is hidden_from_backtrace {
        nqp::bindattr(self, Exception, '$!ex', pir::new('Exception'))
            unless pir::defined($!ex);
        pir::setattribute__vPsP($!ex, 'payload', nqp::p6decont(self));
        my $msg := self.?message;
        pir::setattribute__vPsP($!ex, 'message', nqp::unbox_s($msg.Str))
            if $msg.defined;
        pir::throw__0P($!ex)
    }
    method rethrow() is hidden_from_backtrace {
        pir::rethrow__0P($!ex)
    }
}

my class X::AdHoc is Exception {
    has $.payload;
    method message() { $.payload.Str     }
    method Numeric() { $.payload.Numeric }
}

sub EXCEPTION(|$) {
    my Mu $parrot_ex := nqp::shift(pir::perl6_current_args_rpa__P());
    my Mu $payload   := nqp::atkey($parrot_ex, 'payload');
    if nqp::p6bool(pir::type_check__IPP($payload, Exception)) {
        nqp::bindattr($payload, Exception, '$!ex', $parrot_ex);
        $payload;
    } else {
        my $ex := nqp::create(X::AdHoc);
        nqp::bindattr($ex, Exception, '$!ex', $parrot_ex);
        nqp::bindattr($ex, X::AdHoc, '$!payload', nqp::p6box_s(nqp::atkey($parrot_ex, 'message')));
        $ex;
    }
}


do {
    sub is_runtime($bt) {
        for $bt.keys {
            try {
                return True if nqp::iseq_s($bt[$_]<sub>, 'eval')
                    && nqp::iseq_s(
                            pir::join(';', $bt[$_]<sub>.get_namespace.get_name),
                            'nqp;HLL;Compiler'
                    );
                return False if nqp::iseq_s($bt[$_]<sub>, 'compile')
                    && nqp::iseq_s(
                            pir::join(';', $bt[$_]<sub>.get_namespace.get_name),
                            'nqp;HLL;Compiler'
                    );
            }
        }
        return False;
    }


    sub print_exception(|$) is hidden_from_backtrace {
        my Mu $ex := nqp::atpos(pir::perl6_current_args_rpa__P(), 0);
        try {
            my $e := EXCEPTION($ex);
            my Mu $err := pir::getstderr__P();

            if X::Comp.ACCEPTS($e) || is_runtime($ex.backtrace) {
                $err.print: $e.gist;
                $err.print: "\n";
            }
            else {
                $err.print: "===SORRY!===\n";
                $err.print: $ex;
                $err.print: "\n";
            }
        }
        if $! {
            pir::perl6_based_rethrow__vPP(nqp::getattr($!, Exception, '$!ex'), $ex);
        }
    }

    sub print_control(|$) is hidden_from_backtrace {
        my Mu $ex := nqp::atpos(pir::perl6_current_args_rpa__P(), 0);
        my $type = nqp::p6box_i(nqp::atkey($ex, 'type'));
        if ($type == nqp::p6box_i(pir::const::CONTROL_OK)) {
            my Mu $err := pir::getstderr__P();
            my $msg = nqp::p6box_s(nqp::atkey($ex, 'message'));
            $err.print: $msg ?? "$msg" !! "Warning";
            $err.print: Backtrace.new($ex.backtrace, 0).nice(:oneline);
            $err.print: "\n";
            my $resume := nqp::atkey($ex, 'resume');
            if ($resume) {
                $resume();
            }
        }
        if ($type == nqp::p6box_i(pir::const::CONTROL_RETURN)) {
            die("stray return control exception");
        }
        if ($type == nqp::p6box_i(pir::const::CONTROL_LOOP_LAST)) {
            die("last without loop construct");
        }
        if ($type == nqp::p6box_i(pir::const::CONTROL_LOOP_NEXT)) {
            die("next without loop construct");
        }
        if ($type == nqp::p6box_i(pir::const::CONTROL_LOOP_REDO)) {
            die("redo without loop construct");
        }
        if ($type == nqp::p6box_i(pir::const::CONTROL_CONTINUE)) {
            die("proceed without when clause");
        }
        if ($type == nqp::p6box_i(pir::const::CONTROL_BREAK)) {
            # XXX: should work like leave() ?
            die("succeed without when clause");
        }
        if ($type == nqp::p6box_i(pir::const::CONTROL_TAKE)) {
            die("stray take statement");
        }
    }
            
    my Mu $comp := pir::compreg__Ps('perl6');
    $comp.HOW.add_method($comp, 'handle-exception',
        method (|$) {
            my Mu $ex := nqp::atpos(pir::perl6_current_args_rpa__P(), 1);
            pir::perl6_invoke_catchhandler(&print_exception, $ex);
            pir::exit(1);
            0;
        }
    );
    $comp.HOW.add_method($comp, 'handle-control',
        method (|$) {
            my Mu $ex := nqp::atpos(pir::perl6_current_args_rpa__P(), 1);
            pir::perl6_invoke_catchhandler(&print_control, $ex);
            pir::rethrow__0P($ex);
        }
    );

}

my role X::OS {
    has $.os-error;
}

my role X::IO does X::OS { };

my class X::IO::Rename does X::IO is Exception {
    has $.from;
    has $.to;
    method message() {
        "Failed to rename '$.from' to '$.to': $.os-error"
    }
}

my class X::IO::Copy does X::IO is Exception {
    has $.from;
    has $.to;
    method message() {
        "Failed to copy '$.from' to '$.to': $.os-error"
    }
}

my class X::IO::Mkdir does X::IO is Exception {
    has $.path;
    has $.mode;
    method message() {
        "Failed to create directory '$.path' with mode '0o{$.mode.fmt("%03o")}': $.os-error"
    }
}

my class X::IO::Chdir does X::IO is Exception {
    has $.path;
    method message() {
        "Failed to change the working directory to '$.path': $.os-error"
    }
}

my class X::IO::Dir does X::IO is Exception {
    has $.path;
    method message() {
        "Failed to get the directory contents of '$.path': $.os-error"
    }
}

my class X::IO::Cwd does X::IO is Exception {
    method message() {
        "Failed to get the working directory: $.os-error"
    }
}

my class X::IO::Rmdir does X::IO is Exception {
    has $.path;
    method message() {
        "Failed to remove the directory '$.path': $.os-error"
    }
}

my class X::IO::Unlink does X::IO is Exception {
    has $.path;
    method message() {
        "Failed to remove the file '$.path': $.os-error"
    }
}

my class X::IO::Chmod does X::IO is Exception {
    has $.path;
    has $.mode;
    method message() {
        "Failed to set the mode of '$.path' to '0o{$.mode.fmt("%03o")}': $.os-error"
    }
}

my role X::Comp is Exception {
    has $.filename;
    has $.line;
    has $.column;
    multi method gist(::?CLASS:D:) {
        "===SORRY!===\n$.message\nat $.filename():$.line";
    }
}

my role X::Syntax does X::Comp { }
my role X::Pod                 { }

my class X::NYI is Exception {
    has $.feature;
    method message() { "$.feature not yet implemented. Sorry. " }
}
my class X::Comp::NYI is X::NYI does X::Comp { };

my class X::OutOfRange is Exception {
    has $.what = 'Argument';
    has $.got = '<unknown>';
    has $.range = '<unknown>';
    method message() {
        "$.what out of range. Is: $.got, should be in $.range.gist()"
    }
}

my class X::Buf::AsStr is Exception {
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

my class X::Undeclared does X::Comp {
    has $.what = 'Variable';
    has $.symbol;
    method message() {
        "$.what $.symbol is not declared";
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
my class X::Bind::ZenSlice is Exception {
    has Str $.what = 'array';

    method message() { "Cannot bind to a zen $.what slice." }
}

my class X::Value::Dynamic does X::Comp {
    has $.what;
    method message() { "$.what value must be known at compile time" }
}

my class X::Syntax::Name::Null does X::Syntax {
    method message() { 'Name component my not be null'; }
}

my class X::Syntax::UnlessElse does X::Syntax {
    method message() { '"unless" does not take "else", please rewrite using "if"' }
}

my class X::Syntax::Reserved does X::Syntax {
    has $.reserved;
    has $.instead = '';
    method message() { "The $.reserved is reserved$.instead" }
}

my class X::Syntax::P5 does X::Syntax {
    method message() { 'This appears to be Perl 5 code' }
}

my class X::Syntax::NegatedPair does X::Syntax {
    method message() { 'Argument not allowed on negated pair' }
}

my class X::Syntax::Variable::Numeric does X::Syntax {
    has $.what = 'variable';
    method message() { "Cannot declare a numeric $.what" }
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

my class X::Does::TypeObject is Exception {
    method message() { "Cannot use 'does' operator with a type object." }
}

my class X::Role::Initialization is Exception {
    method message() { 'Can only supply an initialization value for a role if it has a single public attribute' }
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

my class X::Syntax::Malformed does X::Syntax {
    has $.what;
    method message() { "Malformed $.what" }
}
my class X::Syntax::Missing does X::Syntax {
    has $.what;
    method message() { "Missing $.what" }
}

my class X::Syntax::SigilWithoutName does X::Syntax {
    method message() { 'Non-declarative sigil is missing its name' }
}

my class X::Syntax::Self::WithoutObject does X::Syntax {
    method message() { "'self' used where no object is available" }
}
my class X::Syntax::VirtualCall does X::Syntax {
    has $.call;
    method message() { "Virtual call $.call may not be used on partially constructed objects" }
}
my class X::Syntax::NoSelf does X::Syntax {
    has $.variable;
    method message() { "Variable $.variable used where no 'self' is available" }
}

my class X::Syntax::Number::RadixOutOfRange does X::Syntax {
    has $.radix;
    method message() { "Radix $.radix out of range (allowed: 2..36)" }
}

my class X::Syntax::Regex::Adverb does X::Syntax {
    has $.adverb;
    has $.construct;
    method message() { "Adverb $.adverb not allowed on $.construct" }
}

my class X::Syntax::Signature::InvocantMarker does X::Syntax {
    method message() {
        "Can only use : as invocant marker in a signature after the first parameter"
    }
}

my class X::Syntax::Extension::Category does X::Syntax {
    has $.category;
    method message() {
        "Cannot add tokens of category '$.category'";
    }
}

my class X::Attribute::Package does X::Comp {
    has $.package-type;
    method message() { "A $.package-type cannot have attributes" }
}
my class X::Attribute::NoPackage does X::Comp {
    method message() { "You cannot declare an attribute here; maybe you'd like a class or a role?" }
}
my class X::Declaration::Scope does X::Comp {
    has $.scope;
    has $.declaration;
    method message() { "Cannot use '$.scope' with $.declaration declaration" }
}

my class X::Declaration::Scope::Multi is X::Declaration::Scope {
    method message() {
        "Cannot use '$.scope' with individual multi candidates. Please declare an {$.scope}-scoped proto instead";
    }
}

my class X::Anon::Multi does X::Comp {
    has $.multiness;
    has $.routine-type = 'routine';
    method message() { "Cannot put $.multiness on anonymous $.routine-type" }
}
my class X::Anon::Augment does X::Comp {
    has $.package-type;
    method message() { "Cannot augment anonymous $.package-type" }
}
my class X::Augment::NoSuchType does X::Comp {
    has $.package-type;
    has $.package;
    method message() { "You tried to agument $.package-type $.package, but it does not exist" }
}

my class X::Routine::Unwrap is Exception {
    method message() { "Cannot unwrap routine: invalid wrap handle" }
}

my class X::Constructor::Positional is Exception {
    method message() { "Default constructor only takes named arguments" }
}

my class X::Hash::Store::OddNumber is Exception {
    method message() { "Odd number of elements found where hash expected" }
}

my class X::Package::Stubbed does X::Comp {
    has @.packages;
    # TODO: supress display of line number
    method message() {
        "The following packages were stubbed but not defined:\n    "
        ~ @.packages.join("\n    ");
    }
}

my class X::Phaser::PrePost is Exception {
    has $.phaser = 'PRE';
    has $.condition;
    method message {
        my $what = $.phaser eq 'PRE' ?? 'Precondition' !! 'Postcondition';
        $.condition.defined
            ?? "$what '$.condition.trim()' failed"
            !! "$what failed";
    }
}

my class X::Str::Numeric is Exception {
    has $.source;
    has $.pos;
    has $.reason;
    method source-indicator {
        constant marker = chr(0x23CF);
        join '', "in '",
                $.source.substr(0, $.pos),
                marker,
                $.source.substr($.pos),
                "' (indicated by ",
                marker,
                ")",
                ;
    }
    method message() {
        "Cannot convert string to number: $.reason $.source-indicator";
    }
}

my class X::Sequence::Deduction is Exception {
    method message() { 'Unable to deduce sequence' }
}

my class X::TypeCheck is Exception {
    has $.operation;
    has $.got;
    has $.exepcted;
    method message() {
        "Type check failed in $.operation; expected '{$.expected.^name}' but got '{$.got.^name}'";

    }
}

# vim: ft=perl6
