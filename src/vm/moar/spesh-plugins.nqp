# Private method resolution can be specialized based on invocant type. This is
# used for speeding up resolution of private method calls in roles; those in
# classes can be resolved by static optimization.
nqp::speshreg('perl6', 'privmeth', -> $obj, str $name {
    nqp::speshguardtype($obj, $obj.WHAT);
    $obj.HOW.find_private_method($obj, $name)
});

# A resolution like `self.Foo::bar()` can have the resolution specialized. We
# fall back to the dispatch:<::> if there is an exception that'd need to be
# thrown.
nqp::speshreg('perl6', 'qualmeth', -> $obj, str $name, $type {
    nqp::speshguardtype($obj, $obj.WHAT);
    if nqp::istype($obj, $type) {
        # Resolve to the correct qualified method.
        nqp::speshguardtype($type, $type.WHAT);
        $obj.HOW.find_method_qualified($obj, $type, $name)
    }
    else {
        # We'll throw an exception; return a thunk that will delegate to the
        # slow path implementation to do the throwing.
        -> $inv, *@pos, *%named {
            $inv.'dispatch:<::>'($name, $type, |@pos, |%named)
        }
    }
});
