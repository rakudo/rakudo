my $ops := nqp::getcomp('QAST').operations;

sub register_op_desugar($op, $desugar) is export {
    nqp::getcomp('QAST').operations.add_op($op, sub ($comp, $node, :$want, :$cps) {
        $comp.as_js($desugar($node), :$want, :$cps);
    });
}

# Stub
register_op_desugar('', -> $qast {
    QAST::Op.new(:op('null'));
});

register_op_desugar('p6sink', -> $qast {
    my $name := $qast.unique('sink');
    QAST::Stmts.new(
        QAST::Op.new(:op<bind>,
            QAST::Var.new(:$name, :scope<local>, :decl<var>),
            $qast[0],
        ),
        QAST::Op.new(:op<if>,
            QAST::Op.new(:op<if>,
                QAST::Op.new(:op<isconcrete>,
                    QAST::Var.new(:$name, :scope<local>),
                ),
                QAST::Op.new(:op<can>,
                    QAST::Var.new(:$name, :scope<local>),
                    QAST::SVal.new(:value('sink')),
                )
            ),
            QAST::Op.new(:op<callmethod>, :name<sink>,
                QAST::Var.new(:$name, :scope<local>),
            ),
        ),
    );
});

# Stub
register_op_desugar('p6invokeflat', -> $qast {
    $qast[1].flat(1);
    QAST::Op.new( :op('call'), $qast[0], $qast[1]);
    QAST::Op.new(:op('null'));
});

# Signature binding related bits.

$ops.add_simple_op('p6setbinder', $ops.VOID, [$ops.OBJ], :side_effects, sub ($binder) {"nqp.p6binder = $binder"});
$ops.add_op('p6bindsig', :!inlinable, sub ($comp, $node, :$want, :$cps) {
    my $ops := nqp::getcomp('QAST').operations;
    my $tmp := $*BLOCK.add_tmp;
    $ops.new_chunk($ops.VOID, "", [
        "$tmp = nqp.p6binder.bind_sig($*CTX, null, nqp.p6binder, nqp.op.savecapture(Array.prototype.slice.call(arguments)));\n",
        "if ($tmp !== nqp.Null) return $tmp;\n"
    ]);
});

$ops.add_simple_op('p6configposbindfailover', $ops.VOID, [$ops.OBJ, $ops.OBJ], sub ($pos, $pos_bind_failover) {
    "nqp.p6binder.set_pos_bind_failover($*CTX, null, nqp.p6binder, $pos, $pos_bind_failover)"
}, :side_effects);

$ops.add_simple_op('p6setautothreader', $ops.VOID, [$ops.OBJ], sub ($autothreader) {
    "nqp.p6binder.set_autothreader($*CTX, null, nqp.p6binder, $autothreader)"
}, :side_effects);

$ops.add_simple_op('p6setitertype', $ops.VOID, [$ops.OBJ], sub ($iterable) {
    "nqp.p6binder.set_iterable($*CTX, null, nqp.p6binder, $iterable)"
}, :side_effects);

$ops.add_simple_op('p6trialbind', $ops.OBJ, [$ops.OBJ, $ops.OBJ, $ops.OBJ], :!inlinable, sub ($sig, $args, $sig_flags) {
        "nqp.p6binder.trial_bind($*CTX, null, nqp.p6binder, $sig, $args, $sig_flags)"
});

$ops.add_simple_op('p6isbindable', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :!inlinable, sub ($sig, $cap) {
    "nqp.p6binder.is_bindable($*CTX, null, nqp.p6binder, $sig, $cap)"
});

$ops.add_simple_op('p6bindcaptosig', $ops.OBJ, [$ops.OBJ, $ops.OBJ], sub ($sig, $cap) {
    "nqp.p6binder.bind_cap_to_sig($*CTX, null, nqp.p6binder, $sig, $cap)"
}, :side_effects);

$ops.add_op('p6bindattrinvres', $ops.bindattr($ops.OBJ, :inverted_result));

$ops.add_simple_op('p6invokeunder', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :side_effects, sub ($fake, $code) {
    "$code.\$\$call($*CTX, null)"
});

$ops.add_simple_op('p6settypes', $ops.OBJ, [$ops.OBJ], :side_effects);
$ops.add_simple_op('p6init', $ops.OBJ, [], :side_effects, -> {"nqp.extraRuntime('perl6', {$ops.quote_string($*PERL6_RUNTIME)})"});
$ops.add_simple_op('p6bool', $ops.OBJ, [$ops.BOOL], :side_effects);

$ops.add_simple_op('p6box_s', $ops.OBJ, [$ops.STR], :side_effects);
$ops.add_simple_op('p6box_i', $ops.OBJ, [$ops.INT], :side_effects);
$ops.add_simple_op('p6box_n', $ops.OBJ, [$ops.NUM], :side_effects);

$ops.add_simple_op('p6typecheckrv', $ops.OBJ, [$ops.OBJ, $ops.OBJ, $ops.OBJ]);

$ops.add_simple_op('p6decontrv', $ops.OBJ, [$ops.OBJ, $ops.OBJ]);

$ops.add_simple_op('p6definite', $ops.OBJ, [$ops.OBJ], :decont(0));

$ops.add_simple_op('p6captureouters2', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :ctx);
$ops.add_simple_op('p6capturelex', $ops.OBJ, [$ops.OBJ], :side_effects, sub ($codeObj) {
    # Use $*BLOCK.ctx instead of $*CTX so it doesn't get overwriten by exception handling
    "nqp.op.p6capturelex({$*BLOCK.ctx}, $codeObj)"
});

$ops.add_simple_op('p6capturelexwhere', $ops.OBJ, [$ops.OBJ], :side_effects, sub ($codeObj) {
    # Use $*BLOCK.ctx instead of $*CTX so it doesn't get overwriten by exception handling
    "nqp.op.p6capturelexwhere({$*BLOCK.ctx}, $codeObj)"
});

$ops.add_simple_op('p6bindassert', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :ctx, :side_effects);
$ops.add_simple_op('p6store', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :ctx, :side_effects);

$ops.add_simple_op('p6var', $ops.OBJ, [$ops.OBJ], :side_effects); # TODO not really :side_effects just needs marking as returning a fresh value

$ops.add_simple_op('p6recont_ro', $ops.OBJ, [$ops.OBJ], :side_effects);

$ops.add_simple_op('p6argvmarray', $ops.OBJ, [], :side_effects, sub () {
    "nqp.op.p6argvmarray($*CTX, Array.prototype.slice.call(arguments))"
});

$ops.add_simple_op('p6stateinit', $ops.INT, [], sub () { $*BLOCK.first_time_marker ~ 'Init' });

$ops.add_simple_op('p6scalarfromdesc', $ops.OBJ, [$ops.OBJ], :side_effects); # TODO not really :side_effects just needs marking as returning a fresh value

$ops.add_op('p6return', :!inlinable, sub ($comp, $node, :$want, :$cps) {
    my $ops := nqp::getcomp('QAST').operations;
    unless $*RETURN_FROM_HANDLER {
        $*RETURN_FROM_HANDLER := $*BLOCK.add_tmp;
    }
    my $value := $comp.as_js($node[0], :$want, :$cps);
    $ops.new_chunk($ops.VOID, "", [
        $value,
        "$*RETURN_FROM_HANDLER = {$value.expr};\n"
    ]);
});

$ops.add_simple_op('p6decodelocaltime', $ops.OBJ, [$ops.INT], :side_effects); # TODO not really :side_effects just needs marking as returning a fresh value

$ops.add_simple_op('p6finddispatcher', $ops.OBJ, [$ops.STR], :side_effects, sub ($usage) {
    "nqp.op.p6finddispatcher({$*BLOCK.ctx}, $usage)"
});

# HACK before we implement PRE fully
$ops.add_simple_op('p6inpre', $ops.INT, [],  sub () {"(0)"});

$ops.add_simple_op('p6argsfordispatcher', $ops.OBJ, [$ops.OBJ], :side_effects, sub ($dispatcher) {
    "nqp.op.p6argsfordispatcher({$*BLOCK.ctx}, $dispatcher)"
});
