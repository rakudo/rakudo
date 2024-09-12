# This is a proof-of-concept renderer of safe RakuDoc, based on a given
# RakuAST tree.
#
# Most common way is to use the --rakudoc commandline option:
#
# $ RAKUDO_RAKUAST=1 raku --rakudoc filename.raku
#
# Alternately, one can call this as a class method with a given AST on the
# RakuDoc::To::SafeRakuDoc class:
#
# use RakuDoc::To::RakuDoc;
# say RakuDoc::To::RakuDoc.render($ast)
#
# Note that a RakuAST of a source / documentation file can easily be
# obtained as follows:
#
# my $ast = $filename.IO.slurp.AST;

use v6.e.PREVIEW;

unit class RakuDoc::To::RakuDoc;

method render(@ast) { @ast.map(*.DEPARSE).join.chomp }

# vim: expandtab shiftwidth=4
