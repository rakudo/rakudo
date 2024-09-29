use Test;

plan 14;

# GH #2739
# Prior to the fix the original exception would be lost hidden under 'no handler found' error.
throws-like q<sub foo ( ::T $val ) { my T $a is default($val); }; foo(42)>,
        X::Parameter::Default::TypeCheck,
        "exception isn't lost",
        # Default type should be Any but as an implementation artefact of the old compiler frontend
        # at compile time it's Mu. The RakuAST frontend fares better. For now don't be picky.
        message => rx:s/Default value \'\(\w+\)\' will never bind to a variable of type T/;

# https://github.com/Raku/old-issue-tracker/issues/5728
throws-like q[multi sub f(int $foo is rw) { }; f(42)],
        X::Comp,
        'calling multi sub that expects a rw native argument with a literal is caught at compile time';

throws-like q[multi sub f(Int $foo is rw) { }; f(42)],
        X::Comp,
        'calling multi sub that expects a rw non-native argument with a literal is caught at compile time';

throws-like q:to/CODE/,
                my \parent := Metamodel::ClassHOW.new_type(:name('Parent'));
                my \child := Metamodel::ClassHOW.new_type(:name('Child'));
                child.^add_parent(parent);
                child.^compose;
                CODE
        X::Inheritance::NotComposed,
        'inheriting from uncomposed parent';

throws-like q[role R {}; my $foo = 42 but R(1)],
        X::Role::Initialization,
        'passing an initializer when role cannot accept it';

# Rebinding exceptions.  These tests are good candidates to move to Roast once there's enough agreement
# that the behavior tested below should be specified.  See rakudo#4536 for related discussion
subtest "Sigiled variables can be rebound", {
    plan 3;
    my $scalar := 'old scalar';
    $scalar    := 'new';
    is $scalar, 'new', '$-sigiled variable can be rebound';

    my @positional := ['old'];
    @positional    := ['new'];
    is @positional, ['new'], '@-sigiled variable can be rebound';

    my %associative := {:old};
    %associative    := {:new};
    is %associative, {:new}, '%-sigiled variable can be rebound';
}

subtest "Scalars in signatures be rebound if they are 'copy' or 'rw'", {
    plan 2;
    is-deeply do {
        sub f($bound-scalar-copy is copy) { $bound-scalar-copy := 'new' }
        f 'old-value' }, 'new',
        "Scalars in function signatures can be rebound when they are 'is copy'";
    is-deeply do {
        sub f($bound-scalar-rw is rw) { $bound-scalar-rw := 'new' }
        f my $ = 'old-value' }, 'new',
        "Scalars in function signatures can be rebound when they are 'is rw' (and get a writable container)";
}

subtest "Scalars in signatures that aren't 'copy' or 'rw' cannot be rebound", {
    plan 4;
    throws-like {
        ‘sub f($bound-scalar-in-sig) { $bound-scalar-in-sig := 'new' }’.EVAL },
        X::Bind::Rebind, message => /'$bound-scalar-in-sig'/ & /'signature'/,
        "Scalars in function signatures cannot be rebound";
    throws-like {
        'sub f(Mu $bound-scalar-with-type) { $bound-scalar-with-type := 0 }'.EVAL },
        X::Bind::Rebind, message => /'$bound-scalar-with-type'/ & /'signature'/,
        "Scalars in function signatures cannot be rebound even if they have a type constraint";
    throws-like {
        ‘my ($bound-scalar,) := ('original',); $bound-scalar := 'new'’.EVAL },
        X::Bind::Rebind, message => /'$bound-scalar'/ & /'signature'/,
        "Scalars in bound signatures cannot be rebound";
    throws-like {
        ‘my ($a, $b, Int $bound-scalar) := (0, 0, 0); $bound-scalar := 42’.EVAL },
        X::Bind::Rebind, message => /'$bound-scalar'/ & /'signature'/,
        "Scalars in more complex bound signatures cannot be rebound";
}

subtest "Positional and Associative variables in signatures can be rebound if they are 'copy'", {
    plan 4;
    is-deeply do {
        sub f(@positional-copy is copy) { @positional-copy := ['new'] }
        f ['old-value'] }, ['new'],
        "Positional variables in function signatures can be rebound when they are 'is copy'";
    is-deeply do {
        my ($a, @positional-copy is copy) := (0, ['old']);
        @positional-copy := ['new'] }, ['new'],
        "Positional variables in bound signatures can be rebound when they are 'is copy'";
    is-deeply do {
        sub f(%associative-copy is copy) { %associative-copy := {:new} }
        f {:old-value}  }, %(:new),
        "Associative variables in function signatures can be rebound when they are 'is copy'";
    is-deeply do {
        my ($a, %associative-copy is copy) := (0, {:old});
        %associative-copy := {:new} }, %(:new),
        "Associative variables bound in signatures can be rebound when they are 'is copy'";
}

subtest "Positional and Associative variables in signatures that aren't 'copy' cannot be rebound", {
    plan 4;
    throws-like {
        ‘sub f(@bound-positional-in-sig) { @bound-positional-in-sig := ['new'] }’.EVAL },
        X::Bind::Rebind, message => /'@bound-positional-in-sig'/ & /'signature'/,
        "Positional variables in function signatures cannot be rebound";
    throws-like {
        ‘my ($a, @bound-positional) := (0, []); @bound-positional := ['new']’.EVAL },
        X::Bind::Rebind, message => /'@bound-positional'/ & /'signature'/,
        "Positional variables in bound signatures cannot be rebound";
    throws-like {
        'sub f(%bound-associative-in-sig) { %bound-associative-in-sig := {:new} }'.EVAL },
        X::Bind::Rebind, message => /'%bound-associative-in-sig'/ & /'signature'/,
        "Associative variables in function signatures cannot be rebound";
    throws-like {
        ‘my ($a, %bound-associative) := (0, []); %bound-associative := {:new}’.EVAL },
        X::Bind::Rebind, message => /'%bound-associative'/ & /'signature'/,
        "Associative variables in bound signatures cannot be rebound";
}

subtest 'Sigilless "variables" can never be rebound', {
    plan 4;
    throws-like {
        ‘my \sigilless = 'old'; sigilless := 'new'’.EVAL },
        X::Bind::Rebind, message => /'sigilless'/ & /'signature'/.none,
        "Sigilless scalar terms cannot be rebound";
    throws-like {
        ‘my \sigilless-array = ['old']; sigilless-array := ['new']’.EVAL },
        X::Bind::Rebind, message => /'sigilless-array'/ & /'signature'/.none,
        "Sigilless positional terms cannot be rebound";
    throws-like {
        ‘my \sigilless-hash = {:old}; sigilless-hash := {:new}’.EVAL },
        X::Bind::Rebind, message => /'sigilless-hash'/ & /'signature'/.none,
        "Sigilless associative terms cannot be rebound";
    throws-like {
        ‘constant con = 'old'; con := 'new'’.EVAL },
        X::Bind::Rebind, message => /'con'/ & /'signature'/.none,
        "Constants cannot be rebound";
}

subtest "Code items can never be rebound", {
    plan 2;
    throws-like {
        'sub f() { }; &f := &say;'.EVAL },
        X::Bind::Rebind, message => /'&f'/ & /'signature'/.none,
        "A sub cannot be rebound";
    throws-like {
        'my regex reg-exp { old }; &reg-exp := /new/;'.EVAL },
        X::Bind::Rebind, message => /'&reg-exp'/ & /'signature'/.none,
        "A regex cannot be rebound";
}

subtest "Terms can never be rebound", {
    plan 4;
    throws-like {
        'my class C { has $.old }; C := class { has $.new }'.EVAL },
        X::Bind::Rebind, message => /'C'/ & /'signature'/.none,
        "A class cannot be rebound";
    throws-like {
        'my role R { has $.old }; R := role { has $.new }'.EVAL },
        X::Bind::Rebind, message => /'R'/ & /'signature'/.none,
        "A role cannot be rebound";
    throws-like {
        'my grammar G {  }; G := grammar { }'.EVAL },
        X::Bind::Rebind, message => /'G'/ & /'signature'/.none,
        "A grammar cannot be rebound";
    throws-like {
        'int := str;'.EVAL },
        X::Bind::Rebind, message => /'int'/ & /'signature'/.none,
        "Native types cannot be rebound";
}

subtest "Items that were never bound don't throw *re*binding errors", {
    plan 8;
    given (try { ‘my int $var := 'new'’.EVAL }) {
        cmp-ok $!, &[!~~], X::Bind::Rebind,          ‘Binding to a native type doesn't throw X::Bind::Rebind’;
        throws-like {$!.throw}, X::Bind::NativeType, 'Binding to a native type throws X::Bind::NativeType' }
    given (try { ‘'literal string' := 'new'’.EVAL}) {
        cmp-ok $!, &[!~~], X::Bind::Rebind,          ‘Binding to a literal doesn't throw X::Bind::Rebind’;
        throws-like {$!.throw}, X::Bind,             'Binding to a literal throws X::Bind' }
    given (try { ‘sub f {}; f() := 'new'’.EVAL }) {
        cmp-ok $!, &[!~~], X::Bind::Rebind,          ‘Binding to a function call LHS doesn't throw X::Bind::Rebind’;
        throws-like {$!.throw}, X::Bind,             'Binding to a function call LHS throws X::Bind' }
    given (try { ‘::OUTER := 'new'’.EVAL }) {
        cmp-ok $!, &[!~~], X::Bind::Rebind,          ‘Binding to a pseudo-package LHS doesn't throw X::Bind::Rebind’;
        throws-like {$!.throw}, X::Bind,             'Binding to a pseudo-package LHS throws X::Bind' }
}


done-testing;

# vim: expandtab shiftwidth=4
