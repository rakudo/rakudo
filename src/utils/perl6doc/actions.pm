# Copyright (C) 2008, The Perl Foundation.
# $Id$

class Perl6doc::Grammar::Actions;

method TOP($/) {
    my $stmts := PAST::Stmts.new();
    if $<pod_comment> {
        for $<pod_comment> {
            $stmts.push(
                PAST::Op.new(
                    $( $_ ),
                    :pirop('say')
                )
            );
        }
    }
    else {
        $stmts.push(
            PAST::Op.new(
                PAST::Val.new( :value('No POD found.') ),
                :pirop('say')
            )
        );
    }
    make PAST::Block.new(
        $stmts,
        :blocktype('declaration')
    );
}


method pod_comment($/) {
    my $past := PAST::Val.new(
        :value( ~$<block> ~ "\n" ),
        :node( $/ )
    );

    make $past;
}


# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
