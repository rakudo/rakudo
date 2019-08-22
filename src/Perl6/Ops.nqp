
# Registers ops so they're availabe to both NQP for Metamodel and Perl6
sub _register_op_with_nqp($name, $desugar) {
    register_op_desugar($name, $desugar, :compiler<nqp>);
    register_op_desugar($name, $desugar, :compiler<perl6>);
}

# Find the nearest caller from different package with CORE as its outer and returns its context
_register_op_with_nqp('p6clientctx', -> $qast {
    my $ctx := QAST::Node.unique('ctx');
    my $pkg := QAST::Node.unique('pkg');
    QAST::Stmts.new(
        QAST::Op.new(
            :op<bind>,
            QAST::Var.new( :name($ctx), :scope<local>, :decl<var> ),
            QAST::Op.new( :op<ctx> )
        ),
        QAST::Op.new(
            :op<bind>,
            QAST::Var.new( :name($pkg), :scope<local>, :decl<var> ),
            QAST::Op.new( :op<getlex>, QAST::SVal.new( :value<$?PACKAGE> ) )
        ),
        # Find first frame where CORE-SETTING-REV is visible. This is our first candidate for client context.
        QAST::Op.new(
            :op<while>,
            QAST::Op.new(
                :op<if>,
                QAST::Op.new(
                    :op<isnull>,
                    QAST::Var.new( :name($ctx), :scope<local> )
                ),
                QAST::IVal.new( :value(0) ),
                QAST::Op.new(
                    :op<isnull>,
                    QAST::Op.new(
                        :op<getlexrel>,
                        QAST::Var.new( :name($ctx), :scope<local> ),
                        QAST::SVal.new( :value<CORE-SETTING-REV> )
                    )
                )
            ),
            QAST::Op.new(
                :op<bind>,
                QAST::Var.new( :name($ctx), :scope<local> ),
                QAST::Op.new(
                    :op<ctxcallerskipthunks>,
                    QAST::Var.new( :name($ctx), :scope<local> ),
                )
            )
        ),
        # Second, find a caller with different package
        QAST::Op.new(
            :op<while>,
            QAST::Op.new(
                :op<if>,
                QAST::Op.new(
                    :op<isnull>,
                    QAST::Var.new( :name($ctx), :scope<local> )
                ),
                QAST::IVal.new( :value(0) ),
                QAST::Op.new(
                    :op<eqaddr>,
                    QAST::Op.new(
                        :op<getlexrel>,
                        QAST::Var.new( :name($ctx), :scope<local> ),
                        QAST::SVal.new( :value<$?PACKAGE> )
                    ),
                    QAST::Var.new( :name($pkg), :scope<local> )
                )
            ),
            # repeat_until body
            QAST::Op.new(
                :op<bind>,
                QAST::Var.new( :name($ctx), :scope<local> ),
                QAST::Op.new( :op<ctxcallerskipthunks>, QAST::Var.new( :name($ctx), :scope<local> ) )
            )
        ),
        QAST::Var.new( :name($ctx), :scope<local> ),
    )
});
# Finds client's CORE ctx. Note that it's not setting but the CORE itself.
_register_op_with_nqp('p6clientcorectx', -> $qast {
    my $ctx := QAST::Node.unique('ctx');
    QAST::Stmts.new(
        QAST::Op.new(
            :op<bind>,
            QAST::Var.new( :name($ctx), :scope<local>, :decl<var> ),
            QAST::Op.new( :op<p6clientctx> )
        ),
        # Return VMNull unless $ctx is not null
        QAST::Op.new(
            :op<unless>,
            QAST::Op.new(
                :op<isnull>,
                QAST::Var.new( :name($ctx), :scope<local> )
            ),
            QAST::Stmts.new(
                QAST::Op.new(
                    :op<until>,
                    QAST::Op.new(
                        :op<unless>,
                        QAST::Op.new(
                            :op<isnull>,
                            QAST::Var.new( :name($ctx), :scope<local> ),
                        ),
                        QAST::Op.new(
                            :op<existskey>,
                            QAST::Op.new(
                                :op<ctxlexpad>,
                                QAST::Var.new( :name($ctx), :scope<local> )
                            ),
                            QAST::SVal.new( :value<CORE-SETTING-REV> )
                        )
                    ),
                    QAST::Op.new(
                        :op<bind>,
                        QAST::Var.new( :name($ctx), :scope<local> ),
                        QAST::Op.new( :op<ctxouterskipthunks>, QAST::Var.new( :name($ctx), :scope<local> ) )
                    )
                ),
                QAST::Var.new( :name($ctx), :scope<local> ),
            ),
            QAST::Op.new( :op<null> )
        )
    )
});
# Similar to p6clientcorectx but returns language revision letter
_register_op_with_nqp('p6clientcorerev', -> $qast {
    my $ctx := QAST::Node.unique('ctx');
    QAST::Stmts.new(
        QAST::Op.new(
            :op<bind>,
            QAST::Var.new(
                :name($ctx), :scope<local>, :decl<var>
            ),
            QAST::Op.new( :op<p6clientcorectx> )
        ),
        QAST::Op.new(
            :op<unless>,
            QAST::Op.new(
                :op<isnull>,
                QAST::Var.new( :name($ctx), :scope<local> )
            ),
            QAST::Op.new(
                :op<atkey>,
                QAST::Op.new(
                    :op<ctxlexpad>,
                    QAST::Var.new( :name($ctx), :scope<local> )
                ),
                QAST::SVal.new( :value<CORE-SETTING-REV> ),
          ),
          QAST::Op.new( :op<null> )
        )
    )
});
# Simialr to p6clientcorectx but returns `6.<rev>` version string
_register_op_with_nqp('p6clientcorever', -> $qast {
    my $ctx := QAST::Node.unique('ctx');
    QAST::Stmts.new(
        QAST::Op.new(
            :op<bind>,
            QAST::Var.new(
                :name($ctx), :scope<local>, :decl<var>
            ),
            QAST::Op.new( :op<p6clientcorectx> )
        ),
        QAST::Op.new(
            :op<unless>,
            QAST::Op.new(
                :op<isnull>,
                QAST::Var.new( :name($ctx), :scope<local> )
            ),
            QAST::Op.new(
                :op<concat>,
                QAST::SVal.new( :value<6.> ),
                QAST::Op.new(
                    :op<atkey>,
                    QAST::Op.new(
                        :op<ctxlexpad>,
                        QAST::Var.new( :name($ctx), :scope<local> )
                    ),
                    QAST::SVal.new( :value<CORE-SETTING-REV> ),
                )
            ),
            QAST::Op.new( :op<null> )
        )
    )
});
