use Test;

# A "will" trait (my $x will begin { }) parses to a RakuAST::Trait::Will,
# whose attributes are .phase and .block. Both .DEPARSE and .raku used to
# reference non-existent .type / .expr and threw; check they round-trip.

plan 4;

my $ast := Q/my $x will begin { 42 }/.AST;

my $deparsed := $ast.DEPARSE;
like $deparsed, /'will begin'/,
  'a will trait deparses with its phase';

is $deparsed.AST.DEPARSE, $deparsed,
  'the deparsed source reparses to the same deparse';

my $raku;
lives-ok { $raku := $ast.raku },
  'a will trait produces .raku without referencing missing attributes';

like $raku, / 'RakuAST::Trait::Will.new' \s* '(' \s* 'phase' /,
  'the .raku reconstruction names the trait with its phase';
