# Copyright (C) 2007-2009, The Perl Foundation.

=head1 NAME

signature.pir - a plug-in to the PAST::Compiler for signatures

=head1 DESCRIPTION

This adds another multi-variant so when we see a Perl6::Compiler::Signature
in the PAST tree, we know what to do with it. This prevents us from having
to make sure we emit code to build the signature.

=cut

.include "interpinfo.pasm"
.namespace [ 'PAST';'Compiler' ]
.sub 'as_post' :method :multi(_, ['Perl6';'Compiler';'Signature'])
    .param pmc node
    .param pmc options         :slurpy :named
    node = node.'ast'()
    .tailcall self.'as_post'(node, options :flat :named)
.end
