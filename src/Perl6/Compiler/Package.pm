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

# Table of methods we're adding to the meta model
has $!meta_methods;

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
    unless $!methods { $!methods := Q:PIR { %r = root_new ['parrot';'Hash'] } }
    $!methods
}

# Accessor for meta_methods hash.
method meta_methods() {
    unless $!meta_methods { $!meta_methods := Q:PIR { %r = root_new ['parrot';'Hash'] } }
    $!meta_methods
}

# Accessor for attributes hash.
method attributes() {
    unless $!attributes { $!attributes := Q:PIR { %r = root_new ['parrot';'Hash'] } }
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
    my $obj_reg := PAST::Var.new( :name('obj'), :scope('register') );
    my $meta_reg := PAST::Var.new( :name('meta'), :scope('register') );
    my $name := $!name ?? ~$!name !! '';
    if $!scope ne 'augment' {
        $decl.push(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name('obj'), :scope('register'), :isdecl(1) ),
            PAST::Op.new(
                :pasttype('callmethod'),
                :name('new'),
                $metaclass,
                $name
            )
        ));
    }
    else {
        # Augment, so look up existing class.
        unless $name { pir::die('Can not augment an anonymous package') }
        my @name := Perl6::Grammar::parse_name($name);
        $decl.push(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name('obj'), :scope('register'), :isdecl(1) ),
            PAST::Var.new( :name(@name.pop), :namespace(@name), :scope('package') )
        ));
    }
    $decl.push(PAST::Op.new(
        :pasttype('bind'),
        PAST::Var.new( :name('meta'), :scope('register'), :isdecl(1) ),
        PAST::Op.new( :pasttype('callmethod'), :name('HOW'), $obj_reg )
    ));

    # Meta Methods.
    my %meta_methods := $!meta_methods;
    for %meta_methods {
        $decl.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_meta_method'),
            $meta_reg, $obj_reg, ~$_, %meta_methods{~$_}<code_ref>
        ));
    }

    # Methods.
    my %methods := $!methods;
    for %methods {
        $decl.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_method'),
            $meta_reg, $obj_reg, ~$_, %methods{~$_}<code_ref>
        ));
    }

    # Attributes.
    my %attrs := $!attributes;
    for %attrs {
        my $attr := PAST::Op.new(
            :pasttype('callmethod'),
            :name('new'),
            PAST::Var.new( :name('Attribute'), :namespace(''), :scope('package') ),
            PAST::Val.new( :value(~$_),                  :named('name') ),
            PAST::Val.new( :value(%attrs{$_}<accessor>), :named('has_accessor') ),
            PAST::Val.new( :value(%attrs{$_}<rw>),       :named('rw') )
        );
        if %attrs{$_}<build> {
            %attrs{$_}<build>.named('build');
            $attr.push(%attrs{$_}<build>);
        }
        $decl.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_attribute'),
            $meta_reg, $obj_reg, $attr
        ));
    }

    # Traits.
    if $!traits {
        for @($!traits) {
            $_.unshift($obj_reg);
            $decl.push($_);
        }
    }

    # Finally, compose call, and we're done with the decls.
    $decl.push(PAST::Op.new( :pasttype('callmethod'), :name('compose'), $meta_reg, $obj_reg ));

    # Check scope and put decls in the right place.
    if $!scope eq 'our' || $!scope eq 'augment' {
        my $init_decl_name := $block.unique('!class_init_');
        my @ns := Perl6::Grammar::parse_name($name);
        $block.push(PAST::Block.new( :name($init_decl_name), :blocktype('declaration'), $decl ));
        my @PACKAGE := Q:PIR { %r = get_hll_global ['Perl6'; 'Actions'], '@PACKAGE' };
        @PACKAGE[0].block.loadinit().push(PAST::Op.new(
            :pasttype('call'),
            PAST::Var.new( :name($init_decl_name), :namespace(@ns), :scope('package') )
        ));
        $block.blocktype('immediate');
        $block.namespace(@ns);
    }
    else {
        pir::die("Can't handle scope declarator " ~ $!scope ~ " on packages yet");
    }

    return $block;
}
