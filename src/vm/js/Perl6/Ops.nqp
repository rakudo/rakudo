my $ops := nqp::getcomp('QAST').operations;

sub register_op_desugar($op, $desugar, :$compiler = 'Raku') is export {
    nqp::getcomp('QAST').operations.add_op(:hll($compiler), $op, sub ($comp, $node, :$want) {
        $comp.as_js($desugar($node), :$want);
    });
}

# Stub
register_op_desugar('', -> $qast {
    QAST::Op.new(:op('null'));
});

$ops.add_simple_op('p6sink', $ops.VOID, [$ops.OBJ], :ctx, :side_effects, :await);

$ops.add_simple_op('p6reprname', $ops.OBJ, [$ops.OBJ], :decont(0));

# Stub
register_op_desugar('p6invokeflat', -> $qast {
    $qast[1].flat(1);
    QAST::Op.new( :op('call'), $qast[0], $qast[1]);
});

# TODO only override for js
# Override defor to call defined method.
register_op_desugar('defor', -> $op {
    if +$op.list != 2 {
        nqp::die("Operation 'defor' needs 2 operands");
    }
    my $tmp := $op.unique('defined');
    QAST::Stmts.new(
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
            $op[0]
        ),
        QAST::Op.new(
            :op('if'),
            QAST::Op.new(
                :op('callmethod'), :name('defined'),
                QAST::Var.new( :name($tmp), :scope('local') )
            ),
            QAST::Var.new( :name($tmp), :scope('local') ),
            $op[1]
        ));
});

$ops.add_hll_unbox('Raku', $ops.INT, 'getInt');
$ops.add_hll_unbox('Raku', $ops.NUM, 'getNum');
$ops.add_hll_unbox('Raku', $ops.STR, 'getStr');

$ops.add_hll_unbox('Raku', $ops.INT64, 'getInt64');
$ops.add_hll_unbox('Raku', $ops.UINT64, 'getUint64');

# Signature binding related bits.

$ops.add_simple_op('p6setbinder', $ops.VOID, [$ops.OBJ], :side_effects, sub ($binder) {"nqp.p6binder = $binder"});
$ops.add_op('p6bindsig', :!inlinable, sub ($comp, $node, :$want) {
    my $ops := nqp::getcomp('QAST').operations;
    my $tmp := $*BLOCK.add_tmp;
    $ops.new_chunk($ops.VOID, "", [
        "$tmp = /*await*/ nqp.p6binder.p6\$bind_sig($*CTX, null, nqp.p6binder, nqp.op.savecapture(Array.prototype.slice.call(arguments)));\n",
        "if ($tmp !== nqp.Null) return $tmp;\n"
    ]);
});

$ops.add_simple_op('p6configposbindfailover', $ops.VOID, [$ops.OBJ, $ops.OBJ], sub ($pos, $pos_bind_failover) {
    "/*await*/ nqp.p6binder.p6\$set_pos_bind_failover($*CTX, null, nqp.p6binder, $pos, $pos_bind_failover)"
}, :side_effects);

$ops.add_simple_op('p6setautothreader', $ops.VOID, [$ops.OBJ], sub ($autothreader) {
    "/*await*/ nqp.p6binder.p6\$set_autothreader($*CTX, null, nqp.p6binder, $autothreader)"
}, :side_effects);

$ops.add_simple_op('p6trialbind', $ops.OBJ, [$ops.OBJ, $ops.OBJ, $ops.OBJ], :!inlinable, sub ($sig, $args, $sig_flags) {
        "/*await*/ nqp.p6binder.p6\$trial_bind($*CTX, null, nqp.p6binder, $sig, $args, $sig_flags)"
});

$ops.add_simple_op('p6isbindable', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :!inlinable, sub ($sig, $cap) {
    "nqp.retval(HLL, /*await*/ nqp.p6binder.p6\$is_bindable($*CTX, null, nqp.p6binder, $sig, $cap))"
});

$ops.add_simple_op('p6bindcaptosig', $ops.OBJ, [$ops.OBJ, $ops.OBJ], sub ($sig, $cap) {
    "/*await*/ nqp.p6binder.p6\$bind_cap_to_sig($*CTX, null, nqp.p6binder, $sig, $cap)"
}, :side_effects);

$ops.add_op('p6bindattrinvres', $ops.bindattr($ops.OBJ, :inverted_result));

$ops.add_simple_op('p6invokeunder', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :side_effects, :ctx, :takes_hll, :await);

$ops.add_simple_op('p6settypes', $ops.OBJ, [$ops.OBJ], :side_effects);
$ops.add_simple_op('p6init', $ops.OBJ, [], :side_effects, -> {"nqp.extraRuntime('Raku', {$ops.quote_string($*PERL6_RUNTIME)})"});
$ops.add_simple_op('p6bool', $ops.OBJ, [$ops.BOOL], :side_effects);

$ops.add_simple_op('p6typecheckrv', $ops.OBJ, [$ops.OBJ, $ops.OBJ, $ops.OBJ], :ctx, :await);

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

$ops.add_simple_op('p6bindassert', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :ctx, :side_effects, :await);
$ops.add_simple_op('p6store', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :ctx, :side_effects, :await);

$ops.add_simple_op('p6argvmarray', $ops.OBJ, [], :side_effects, sub () {
    "/*await*/ nqp.op.p6argvmarray($*CTX, Array.prototype.slice.call(arguments))"
});

$ops.add_simple_op('p6stateinit', $ops.INT, [], sub () {
    # XXX - this semantics seems suspect but rakudo seems to rely on this
    my $block := $*BLOCK;
    while $block.qast.blocktype eq 'immediate' {
        $block := $block.outer;
    }
    $block.first_time_marker ~ 'Init';
});

$ops.add_op('p6return', :!inlinable, sub ($comp, $node, :$want) {
    my $ops := nqp::getcomp('QAST').operations;
    unless $*RETURN_FROM_HANDLER {
        $*RETURN_FROM_HANDLER := $*BLOCK.add_tmp;
    }
    my $value := $comp.as_js($node[0], :$want);
    $ops.new_chunk($ops.VOID, "", [
        $value,
        "$*RETURN_FROM_HANDLER = {$value.expr};\n"
    ]);
});

$ops.add_op('p6decontrv', :!inlinable, sub ($comp, $node, :$want) {
    my $is_rw;
    if nqp::istype($node[0], QAST::WVal) {
        $is_rw := nqp::istrue($node[0].value.rw);
    }
    else {
        nqp::die('p6decontrv expects a QAST::WVal as its first child');
    }
    if $is_rw {
        $comp.as_js($node[1], :$want)
    }
    else {
        $comp.as_js(QAST::Op.new( :op('p6decontrv_internal'), $node[1]), :$want);
    }
});


$ops.add_simple_op('p6finddispatcher', $ops.OBJ, [$ops.STR], :side_effects, sub ($usage) {
    "/*await*/ nqp.op.p6finddispatcher({$*BLOCK.ctx}, $usage)"
});


$ops.add_simple_op('p6argsfordispatcher', $ops.OBJ, [$ops.OBJ], :side_effects, sub ($dispatcher) {
    "nqp.op.p6argsfordispatcher({$*BLOCK.ctx}, $dispatcher)"
});

$ops.add_simple_op('p6setfirstflag', $ops.OBJ, [$ops.OBJ], :side_effects);
$ops.add_simple_op('p6takefirstflag', $ops.INT, [], :ctx, :side_effects);

$ops.add_simple_op('p6setpre', $ops.OBJ,  [], :ctx, :side_effects);
$ops.add_simple_op('p6clearpre', $ops.OBJ, [], :ctx, :side_effects);
$ops.add_simple_op('p6inpre', $ops.INT, [], :ctx, :side_effects);

$ops.add_simple_op('p6getouterctx', $ops.OBJ, [$ops.OBJ], :decont(0), :!inlinable);

$ops.add_simple_op('p6staticouter', $ops.OBJ, [$ops.OBJ], :ctx, :side_effects);

$ops.add_simple_op('p6fakerun', $ops.OBJ, [$ops.OBJ], :side_effects, :takes_hll, :await);

# vim: expandtab sw=4
