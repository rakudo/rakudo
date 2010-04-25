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

# List attributes meta-data hashes. Should be Attribute class instances one day.
has $!attributes;

# List of traits.
has $!traits;

# List of colonpair adverbs.
has $!name_adverbs;

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

# Accessor for attributes list.
method attributes() {
    unless $!attributes { $!attributes := PAST::Node.new() }
    $!attributes
}

# Checks if there is already an attribute with the given name.
method has_attribute($name) {
    if $!attributes {
        for @($!attributes) {
            if $_<name> eq $name {
                return 1;
            }
        }
    }
    0
}

# Accessor for traits list.
method traits() {
    unless $!traits { $!traits := PAST::Node.new() }
    $!traits
}

# Accessor for traits list.
method name_adverbs() {
    unless $!name_adverbs { $!name_adverbs := PAST::Node.new() }
    $!name_adverbs
}

# This method drives the code generation and fixes up the block.
method finish($block) {
    my $decl := PAST::Stmts.new();

    # Emit code to install the current scope as $*SCOPE.
    $decl.push(PAST::Op.new( :pasttype('bind'),
        PAST::Var.new( :scope('lexical'), :name('$*SCOPE'), :isdecl(1) ),
        ~$!scope || 'our'
    ));

    # Create or look up meta-class.
    my $how := $!how;
    my @how := Perl6::Grammar::parse_name(~$how);
    my $metaclass := PAST::Var.new( :name(@how.pop), :namespace(@how), :scope('package') );
    my $obj_reg := PAST::Var.new( :name('obj'), :scope('register') );
    my $meta_reg := PAST::Var.new( :name('meta'), :scope('register') );
    my $name := $!name ?? ~$!name !! '';
    if $!scope ne 'augment' {
        my $new_call :=  PAST::Op.new(
            :pasttype('callmethod'), :name('new'),
            $metaclass, $name
        );
        for @(self.name_adverbs) {
            my $param := $_[2];
            $param.named(~$_[1].value());
            $new_call.push($param);
        }
        $decl.push(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name('obj'), :scope('register'), :isdecl(1) ),
            $new_call
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
    my $attr_list := self.attributes();
    for @($attr_list) {
        my $attr := PAST::Op.new(
            :pasttype('callmethod'),
            :name('new'),
            PAST::Var.new( :name('Attribute'),   :namespace(''), :scope('package') ),
            PAST::Val.new( :value($_<name>),     :named('name') ),
            PAST::Val.new( :value($_<accessor>), :named('has_accessor') ),
            PAST::Val.new( :value($_<rw>),       :named('rw') )
        );
        if $_<build> {
            $_<build>.named('build');
            $attr.push($_<build>);
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
    if $!scope eq 'anon' || $!name eq '' {
        $block.blocktype('immediate');
        $block.push($decl);
    }
    elsif $!scope eq 'our' || $!scope eq 'augment' {
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
    elsif $!scope eq 'my' {
        # Install a binding of the declaration to a name in the lexpad.
        @Perl6::Actions::BLOCK[0][0].push(PAST::Var.new(
            :name($!name), :isdecl(1),  :viviself($decl), :scope('lexical')
        ));
        @Perl6::Actions::BLOCK[0].symbol($!name, :scope('lexical'), :does_abstraction(1));
    }
    else {
        pir::die("Scope declarator " ~ $!scope ~ " is not supported on packages");
    }

    return $block;
}
