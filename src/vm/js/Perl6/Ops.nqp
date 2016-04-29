my $ops := nqp::getcomp('QAST').operations;

sub register_op_desugar($op, $desugar) is export {
    nqp::getcomp('QAST').operations.add_op($op, sub ($comp, $node, :$want, :$cps) {
        $comp.as_js($desugar($op), :$want, :$cps);
    });
}

# Stub
register_op_desugar('p6setbinder', -> $qast {
    QAST::Op.new(:op('null'));
});

register_op_desugar('p6init', -> $qast {
    QAST::Op.new(:op('null'));
});

$ops.add_simple_op('p6bindattrinvres', $ops.OBJ, [$ops.OBJ, $ops.OBJ, $ops.STR, $ops.OBJ], :sideffects,
    sub ($obj, $type, $attr, $value) {
        # TODO take second argument into account
        "($obj[$attr] = $value, $obj)";
    }
);
