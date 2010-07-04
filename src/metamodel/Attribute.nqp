class Attribute is Any;

has $!name;
has $!type;
has $!build;
has $!has_accessor;
has $!rw;
has $!handles;
has $!trait_applier;

method new(:$name, :$type, :$build, :$has_accessor, :$rw, :$handles) {
    my $attr := pir::new__PS('Attribute');
    pir::setattribute__vPSP($attr, '$!name', $name);
    pir::setattribute__vPSP($attr, '$!type', $type);
    pir::setattribute__vPSP($attr, '$!build', $build);
    pir::setattribute__vPSP($attr, '$!has_accessor', $has_accessor);
    pir::setattribute__vPSP($attr, '$!rw', $rw);
    pir::setattribute__vPSP($attr, '$!handles', $handles);
    $attr
}

method name() {
    $!name
}

method type() {
    $!type
}

method build() {
    $!build || Mu
}

method has_accessor() {
    $!has_accessor
}

method rw() {
    $!rw ?? Bool::True !! Bool::False
}

method handles() {
    $!handles
}

method readonly() {
    !$!rw
}

method apply_traits($metaclass, $container) {
    if $!trait_applier {
        my $decl := AttributeDeclarand.new(
            container => $container,
            how => $metaclass,
            name => $!name
        );
        $!trait_applier($decl);
    }
}

method compose($package) {
    my $name := $!name;
    
    # Generate an accessor, if we need one.
    if $!has_accessor {
        # Accessor helper subs.
        sub accessor_helper_ro($self) {
            pir::new__PsP('Perl6Scalar', pir::getattribute__PPS($self, $name))
        }
        sub accessor_helper_rw($self) {
            pir::getattribute__PPS($self, $name)
        }

        # XXX check there isn't already one...

        # Decide whether we want an rw one or not.
        my $is_rw := pir::defined($!rw) ?? $!rw !!
                     pir::can__ips($package, 'rw') ?? $package.rw !! 0;
        my $meth := pir::find_lex__ps($is_rw ?? 'accessor_helper_rw' !! 'accessor_helper_ro');
        $meth := pir::clone($meth);

        # introspection looks at the actual sub name, so set it
        # to the value the user expects
        # set $P0, $S0  is parrot's clunky PIR API for setting the sub name.
        my $meth_name := pir::substr__SSi($name, 2);
        pir::set__vps($meth, $meth_name);
        $package.add_method($package, $meth_name, $meth);
    }

    # If we've a handles, pass it along to the handles setup helper.
    unless pir::isa__ips($!handles, 'Undef') {
        Rakudo::Guts.add_handles_method($package, $!name, $!handles);
    }
}

# vim: ft=perl6
