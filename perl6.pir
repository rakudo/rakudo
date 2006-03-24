=head1 TITLE

perl6.pir - A Perl 6 parser (someday a Perl 6 compiler?)

=head2 Description

This is the base file for the Perl 6 parser; eventually
it will likely become the base file for the Perl 6 compiler.

This file simply includes the parsing and grammar rules
from the Perl6/ directory, provides a ":load" routine
to make sure the relevant PGE libraries are loaded,
and then registers a compiler under the name "Perl6".

Just to make sure we aren't fooling anyone -- at the
moment the "Perl6" compiler here only parses Perl 6 code.
But that should change soon.

=cut

.include "lib/parse.pir"
.include "lib/grammar.pir"

.namespace [ "Perl6" ]

=item C<onload()>

Loads the PGE libraries needed for running the parser,
and registers the "parse" subroutine as the "Perl6"
compiler.

=cut

.sub "__onload" :load
    load_bytecode "PGE.pbc"
    load_bytecode "PGE/Text.pir"

    $P0 = compreg "PGE::P6Rule"
    $P1 = $P0("^<Perl6::Grammar::program>")
    compreg "Perl6", $P1
.end


