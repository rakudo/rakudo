# This is a proof-of-concept renderer of Safe RakuDoc, based on a given
# RakuAST tree.
#
# Most common way is to use the --rakudoc=SafeRakuDoc commandline option:
#
# $ RAKUDO_RAKUAST=1 raku --rakudoc=SafeRakuDoc filename.raku
#
# Alternately, one can call this as a class method with a given AST on the
# RakuDoc::To::SafeRakuDoc class:
#
# use RakuDoc::To::SafeRakuDoc;
# say RakuDoc::To::SafeRakuDoc.render($ast)
#
# Note that a RakuAST of a source / documentation file can easily be
# obtained as follows:
#
# my $ast = $filename.IO.slurp.AST;

use v6.e.PREVIEW;

unit class RakuDoc::To::SafeRakuDoc;

method render(@ast) { @ast.map(*.DEPARSE).join.chomp }

# vim: expandtab shiftwidth=4
