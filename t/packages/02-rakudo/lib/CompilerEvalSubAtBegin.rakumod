use nqp;
# Pins the compiler linking an EVAL's code objects to their coderefs on its
# own: a Sub compiled at BEGIN time through the compiler object, the way a
# module embedding another language compiles the Raku code it hosts, must
# survive its outer module's precomp. The outer context marks the compile
# as an EVAL, which nests it in the unit being compiled.
our &foo;
our &bar;
BEGIN {
    my $ctx := nqp::getattr(CORE::, PseudoStash, '$!ctx');
    my $compiler := nqp::getcomp('Raku');

    my $compiled := $compiler.compile('sub () { "from-precomped-compiler-eval" }', :outer_ctx($ctx));
    nqp::forceouterctx(nqp::getattr($compiled, ForeignCode, '$!do'), $ctx);
    &foo = $compiled();

    # A caller keeping the compilation unit takes the mainline itself.
    my $unit := $compiler.compile('sub () { "from-precomped-compiler-unit" }', :outer_ctx($ctx), :compunit_ok(1));
    my $mainline := $compiler.backend.compunit_mainline($unit);
    nqp::forceouterctx(nqp::getattr($mainline, ForeignCode, '$!do'), $ctx);
    &bar = $mainline();
}
