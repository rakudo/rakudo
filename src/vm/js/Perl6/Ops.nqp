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

# Signature binding related bits.

$ops.add_simple_op('p6setbinder', $ops.VOID, [$ops.OBJ], :sideffects, sub ($binder) {"nqp.p6binder = $binder"});
$ops.add_op('p6bindsig', :!inlinable, sub ($comp, $node, :$want, :$cps) {
    my $ops := nqp::getcomp('QAST').operations;
    my $tmp := $*BLOCK.add_tmp;
    $ops.new_chunk($ops.VOID, "", [
        "$tmp = nqp.p6binder.bind_sig($*CTX, null, nqp.p6binder, nqp.op.savecapture(Array.prototype.slice.call(arguments)));\n",
        "if ($tmp !== null) return $tmp;\n"
    ]);
});

$ops.add_simple_op('p6isbindable', $ops.INT, [$ops.OBJ, $ops.OBJ], :!inlinable, sub ($sig, $cap) {
    "nqp.p6binder.is_bindable($*CTX, null, nqp.p6binder, $sig, $cap)"
});

$ops.add_op('p6bindattrinvres', $ops.bindattr($ops.OBJ, :inverted_result));

$ops.add_simple_op('p6invokeunder', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :sideffects, sub ($fake, $code) {
    "$code.\$\$call($*CTX, null)"
});

$ops.add_simple_op('p6settypes', $ops.OBJ, [$ops.OBJ], :sideffects);
$ops.add_simple_op('p6init', $ops.OBJ, [], :sideffects, -> {'require(process.env.RAKUDOJS_RUNTIME)'});
$ops.add_simple_op('p6bool', $ops.OBJ, [$ops.BOOL], :sideffects);

$ops.add_simple_op('p6box_s', $ops.OBJ, [$ops.STR]);
$ops.add_simple_op('p6box_i', $ops.OBJ, [$ops.INT]);
$ops.add_simple_op('p6box_n', $ops.OBJ, [$ops.NUM]);

$ops.add_simple_op('p6typecheckrv', $ops.OBJ, [$ops.OBJ, $ops.OBJ, $ops.OBJ]);

$ops.add_simple_op('p6decontrv', $ops.OBJ, [$ops.OBJ, $ops.OBJ]);

$ops.add_simple_op('p6definite', $ops.OBJ, [$ops.OBJ], :decont(0));

$ops.add_simple_op('p6captureouters2', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :ctx);
$ops.add_simple_op('p6captureouters', $ops.OBJ, [$ops.OBJ], :ctx);
$ops.add_simple_op('p6capturelex', $ops.OBJ, [$ops.OBJ], :ctx);

$ops.add_simple_op('p6bindassert', $ops.OBJ, [$ops.OBJ, $ops.OBJ], :ctx);

$ops.add_simple_op('p6var', $ops.OBJ, [$ops.OBJ], :sideffects); # TODO not really just needs marking as returning a fresh value
