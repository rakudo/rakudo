# Private method resolution can be specialized based on invocant type. This is
# used for speeding up resolution of private method calls in roles; those in
# classes can be resolved by static optimization.
nqp::speshreg('perl6', 'privmeth', -> $obj, str $name {
    nqp::speshguardtype($obj, $obj.WHAT);
    $obj.HOW.find_private_method($obj, $name)
});
