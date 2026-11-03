use v6;
use nqp;
use Test;

plan 9;

# A finished statement drops its captures once its node exists, except when
# the compile targets the parse stage, whose output is the match tree.

my $source = q:to/SRC/;
FOO: for 1 { last FOO }
my $y = 1;
{ say 42; my $z = 3 }
SRC

sub statements-of(Mu $match) {
    my $list := nqp::atkey($match.hash, 'statementlist');
    nqp::atkey($list.hash, 'statement')
}

for <parse Parse> -> $target {
    my $tree := nqp::getcomp('Raku').compile($source, :$target, :compunit_ok(1));
    my $statements := statements-of($tree);
    is nqp::elems($statements), 3,
      "target $target keeps every statement of the statement list";
    ok nqp::existskey(nqp::atpos($statements, 0).hash, 'label'),
      "target $target keeps the label capture of a labelled statement";
    ok nqp::existskey(nqp::atpos($statements, 1).hash, 'EXPR'),
      "target $target keeps the EXPR capture of an expression statement";
}

my $frontend = nqp::gethllsym('Raku', 'COMPILER-FRONTEND');
if $frontend eq 'rakuast' {
    # The top level statement list restores its braid before its action
    # runs, so the hook only sees the statement list of the nested block.
    BEGIN $?LANG.refine_slang('MAIN', role {}, role {
        method statementlist(Mu $/) {
            my $statements := nqp::atkey($/.hash, 'statement');
            if nqp::elems($statements) {
                my $first := nqp::atpos($statements, 0);
                nqp::bindcurhllsym('DROP-STATEMENT-CAPTURES-SEEN', nqp::list(
                  nqp::elems($first.hash), $first.Str, $first.ast.^name));
            }
            nextsame
        }
    });
    { say 42; my $z = 3 }

    my $seen := nqp::getcurhllsym('DROP-STATEMENT-CAPTURES-SEEN');
    is nqp::atpos($seen, 0), 0,
      'a finished statement holds no captures once its node exists';
    is nqp::atpos($seen, 1), 'say 42',
      'a finished statement still stringifies from its source range';
    is nqp::atpos($seen, 2), 'RakuAST::Statement::Expression',
      'a finished statement still carries its node';
}
else {
    skip 'statement captures are only dropped by the RakuAST frontend', 3;
}

# vim: expandtab shiftwidth=4
