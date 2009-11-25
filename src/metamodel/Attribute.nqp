class Attribute;

has $!name;
has $!type;
has $!build;
has $!has_accessor;
has $!rw;

method new(:$name, :$type, :$build, :$has_accessor, :$rw, :$handles) {
    my $attr := pir::new__PS('Attribute');
    pir::setattribute__vPSP($attr, '$!name', $name);
    pir::setattribute__vPSP($attr, '$!type', $type);
    pir::setattribute__vPSP($attr, '$!build', $build);
    pir::setattribute__vPSP($attr, '$!has_accessor', $has_accessor);
    pir::setattribute__vPSP($attr, '$!rw', $rw);
    pir::setattribute__vPSP($attr, '$!handles', $handles);
}

method name() {
    $!name
}

method type() {
    $!type
}

method build() {
    $!build;
}

method has_accessor() {
    $!has-accessor
}

method rw() {
    $!rw
}

method handles() {
    $!handles
}

method readonly() {
    !$!rw
}

method compose($package) {
    my $name := $!name;
    
    # Generate an accessor, if we need one.
    if $!accessor {
        # Accessor helper subs.
        sub accessor_helper_ro($self) {
            pir::new__PSP('ObjectRef', pir::getattribute__PPS($self, $name))
        }
        sub accessor_helper_rw($self) {
            pir::getattribute__PPS($self, $name)    
        }

        # XXX check there isn't already one...
        my $meth := $!rw ?? accessor_helper_rw !! accessor_helper_ro;
        my $meth_name := pir::substr__SSI($name, 2);
        $package.add_method($package, $meth_name, pir::clone__PP($meth));
    }

    # XXX Handles...
}

# vim: ft=perl6
