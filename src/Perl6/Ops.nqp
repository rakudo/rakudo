
# Registers ops so they're availabe to both NQP for Metamodel and Raku
sub _register_op_with_nqp($name, $desugar) {
    register_op_desugar($name, $desugar, :compiler<nqp>);
    register_op_desugar($name, $desugar, :compiler<Raku>);
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

_register_op_with_nqp( 'p6getlexclient', -> $qast {
        my $ctx := QAST::Node.unique('$ctx');
        my $PseudoStash := QAST::Node.unique('$PseudoStash');
        my $Map := QAST::Node.unique('$Map');
        my $stash := QAST::Node.unique('$stash');
        my $setting-only := QAST::Node.unique('$setting-only');
        my $setting-only-var := QAST::Var.new( :name($setting-only), :scope<local> );
        my $setting-only-named := QAST::Var.new( :name($setting-only), :scope<local> );
        $setting-only-named.named('setting-only');
        QAST::Stmts.new(
            QAST::Op.new(
                :op<bind>,
                QAST::Var.new( :name($setting-only), :scope<local>, :decl<var> ),
                (nqp::atpos($qast, 1) || QAST::IVal.new( :value(0) ))
            ),
            QAST::Op.new(
                :op<if>,
                QAST::Op.new(
                    :op<isconcrete>,
                    QAST::VarWithFallback.new(
                        :name<$*OPTIMIZER-SYMBOLS>,
                        :fallback(
                            QAST::WVal.new( :value(Mu) )
                        ),
                        :scope<contextual>
                    )
                ),
                QAST::Op.new(
                    :op<if>,
                    $setting-only-var,
                    QAST::Op.new(
                        :op<callmethod>,
                        QAST::Var.new( :name<$*OPTIMIZER-SYMBOLS>, :scope<contextual> ),
                        QAST::SVal.new( :value<find_in_setting> ),
                        $qast[0]
                    ),
                    QAST::Op.new(
                        :op<callmethod>,
                        QAST::Var.new( :name<$*OPTIMIZER-SYMBOLS>, :scope<contextual> ),
                        QAST::SVal.new( :value<find_symbol> ),
                        QAST::Op.new(
                            :op<split>,
                            QAST::SVal.new( :value<::> ),
                            $qast[0]
                        )
                    ),
                ),
                QAST::Op.new(
                    :op<if>,
                    QAST::Op.new(
                        :op<isconcrete>,
                        QAST::VarWithFallback.new(
                            :name<$*W>,
                            :fallback(
                                QAST::Op.new( :op<null> )
                                # QAST::WVal.new( :value(NQPMu) )
                            ),
                            :scope<contextual>
                        )
                    ),
                    QAST::Op.new(
                        :op<callmethod>,
                        QAST::Var.new( :name<$*W>, :scope<contextual> ),
                        QAST::SVal.new( :value<find_symbol> ),
                        QAST::Op.new(
                            :op<split>,
                            QAST::SVal.new( :value<::> ),
                            $qast[0]
                        ),
                        $setting-only-named
                    ),
                    QAST::Stmts.new(
                        QAST::Op.new(
                            :op<bind>,
                            QAST::Var.new( :name($ctx), :scope<local>, :decl<var> ),
                            QAST::Op.new( :op<p6clientctx> ),
                        ),
                        QAST::Op.new(
                            :op<bind>,
                            QAST::Var.new( :name($PseudoStash), :scope<local>, :decl<var> ),
                            QAST::Op.new(
                                :op<getlexrel>,
                                QAST::Var.new( :name($ctx), :scope<local> ),
                                QAST::SVal.new( :value<PseudoStash> )
                            ),
                        ),
                        QAST::Op.new(
                            :op<bind>,
                            QAST::Var.new( :name($Map), :scope<local>, :decl<var> ),
                            QAST::Op.new(
                                :op<getlexrel>,
                                QAST::Var.new( :name($ctx), :scope<local> ),
                                QAST::SVal.new( :value<Map> )
                            ),
                        ),
                        QAST::Op.new(
                            :op<bind>,
                            QAST::Var.new( :name($stash), :scope<local>, :decl<var> ),
                            QAST::Op.new(
                                :op<create>,
                                QAST::Var.new( :name($PseudoStash), :scope<local> ),
                            )
                        ),
                        QAST::Op.new(
                            :op<bindattr>,
                            QAST::Var.new( :name($stash), :scope<local> ),
                            QAST::Var.new( :name($Map), :scope<local> ),
                            QAST::SVal.new( :value<$!storage> ),
                            QAST::Op.new(
                                :op<ctxlexpad>,
                                QAST::Var.new( :name($ctx), :scope<local> ),
                            ),
                        ),
                        QAST::Op.new(
                            :op<bindattr>,
                            QAST::Var.new( :name($stash), :scope<local> ),
                            QAST::Var.new( :name($PseudoStash), :scope<local> ),
                            QAST::SVal.new( :value<$!ctx> ),
                            QAST::Var.new( :name($ctx), :scope<local> ),
                        ),
                        QAST::Op.new(
                            :op<bindattr_i>,
                            QAST::Var.new( :name($stash), :scope<local> ),
                            QAST::Var.new( :name($PseudoStash), :scope<local> ),
                            QAST::SVal.new( :value<$!mode> ),
                            QAST::IVal.new( :value(1) ) # PseudoStash::STATIC_CHAIN constant value
                        ),
                        QAST::Op.new(
                            :op<if>,
                            $setting-only-var,
                            QAST::Op.new(
                                :op<bind>,
                                QAST::Var.new( :name($stash), :scope<local> ),
                                QAST::Op.new(
                                    :op<who>,
                                    QAST::Op.new(
                                        :op<callmethod>,
                                        QAST::Var.new( :name($stash), :scope<local> ),
                                        QAST::SVal.new( :value<AT-KEY> ),
                                        QAST::Op.new(
                                            :op<if>,        # If we can't 'see' '!UNIT_MARKER' then client is in CORE and SETTING cannot be used
                                            QAST::Op.new(
                                                :op<callmethod>,
                                                QAST::Var.new( :name($stash), :scope<local> ),
                                                QAST::SVal.new( :value<EXISTS-KEY> ),
                                                QAST::SVal.new( :value<!UNIT_MARKER> )
                                            ),
                                            QAST::SVal.new( :value<SETTING> ),
                                            QAST::SVal.new( :value<CORE> )
                                        )
                                    )
                                )
                            )
                        ),
                        QAST::Op.new(
                            :op<call>,
                            QAST::Op.new(
                                :op<getlexrel>,
                                QAST::Var.new( :name($ctx), :scope<local> ),
                                QAST::SVal.new( :value<&INDIRECT_NAME_LOOKUP> )
                            ),
                            QAST::Var.new( :name($stash), :scope<local> ),
                            $qast[0]
                        )
                    ),
                ),
            )
        )
    }
);

# vim: expandtab sw=4
