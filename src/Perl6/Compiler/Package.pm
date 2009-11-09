class Perl6::Compiler::Package;

# The block associated with this package.
has $!block;

# This is the name of the HOW this package is based on.
has $!how;

# The name of the package.
has $!name; 

# The scope of the package.
has $!scope;

# Table of methods we're adding (name => PAST::Block).
has $!methods;

# Table of attributes meta-data hashes. Maps name to hash.
has $!attributes;

# List of traits.
has $!traits;

# Accessor for block.
method block($block?) {
    if $block { $!block := $block }
    $!block
}

# Accessor for how.
method how($how?) {
    if $how { $!how := $how }
    $!how
}

# Accessor for name.
method name($name?) {
    if $name { $!name := $name }
    $!name
}

# Accessor for scope.
method scope($scope?) {
    if $scope { $!scope := $scope }
    $!scope
}

# Accessor for methods hash.
method methods() {
    unless $!methods { $!methods := Q:PIR { %r = new ['Hash'] } }
    $!methods
}

# Accessor for attributes hash.
method attributes() {
    unless $!attributes { $!attributes := Q:PIR { %r = new ['Hash'] } }
    $!attributes
}

# Accessor for traits list.
method traits() {
    unless $!traits { $!traits := PAST::Node.new() }
    $!traits
}

# This method drives the code generation and fixes up the block.
# XXX Need to support lexical and anonymous.
method finish($block) {
    my $decl := PAST::Stmts.new();

    # Create or look up meta-class.
    my $how := $!how;
    my @how := Perl6::Grammar::parse_name(~$how);
    my $metaclass := PAST::Var.new( :name(@how.pop), :namespace(@how), :scope('package') );
    my $meta_reg := PAST::Var.new( :name('meta'), :scope('register') );
    my $name := $!name ?? ~$!name !! '';
    if $!scope ne 'augment' {
        $decl.push(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name('meta'), :scope('register'), :isdecl(1) ),
            PAST::Op.new(
                :pasttype('callmethod'),
                :name('new'),
                $metaclass,
                $name
            )
        ));
    }
    else {
        # Augment, so look up existing metaclass and grab it's HOW.
        unless $name { pir::die('Can not augment an anonymous package') }
        my @name := Perl6::Grammar::parse_name($name);
        $decl.push(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name('meta'), :scope('register'), :isdecl(1) ),
            PAST::Op.new(
                :pasttype('callmethod'),
                :name('HOW'),
                PAST::Var.new( :name(@name.pop), :namespace(@name), :scope('package') )
            )
        ));
    }

    # Methods.
    my %methods := $!methods;
    for %methods {
        $decl.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_method'),
            $metaclass, $meta_reg, ~$_, %methods{~$_}<code_ref>
        ));
    }

    # Attributes.
    my %attrs := $!attributes;
    for %attrs {
        $decl.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_attribute'),
            $metaclass, $meta_reg, ~$_
        ));
    }

    # Traits.
    if $!traits {
        for @($!traits) {
            $_.unshift($meta_reg);
            $decl.push($_);
        }
    }

    # Finally, compose call, and we're done with the decls.
    $decl.push(PAST::Op.new( :pasttype('callmethod'), :name('compose'), $metaclass, $meta_reg ));

    # Check scope and put decls in the right place.
    if $!scope eq 'our' || $!scope eq 'augment' {
        $block.loadinit().push($decl);
        $block.blocktype('immediate');
        $block.namespace(Perl6::Grammar::parse_name($name));
    }
    else {
        pir::die("Can't handle scope declarator " ~ $!scope ~ " on packages yet");
    }

    return $block;
}
