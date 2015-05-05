use v6;
use Test;

plan 14;

for (

  ( '/foo/bar' =>
    [ 'file#/foo/bar' ] ),
  ( 'file#/foo/bar' =>
    [ 'file#/foo/bar' ] ),
  ( 'inst#/installed' =>
    [ 'inst#/installed' ] ),
  ( 'CompUnitRepo::Local::Installation#/installed' =>
    [ 'CompUnitRepo::Local::Installation#/installed' ] ),
  ( 'inst#name<work>#/installed' =>
    [ 'inst#name<work>#/installed' ] ),
  ( 'inst#name[work]#/installed' =>
    [ 'inst#name<work>#/installed' ] ),
  ( 'inst#name{work}#/installed' =>
    [ 'inst#name<work>#/installed' ] ),
  ( "/foo/bar  ,  /foo/baz" =>
    [ 'file#/foo/bar', 'file#/foo/baz' ] ),
  ( "inst#/installed, /also" =>
    [ 'inst#/installed', 'inst#/also' ] ),
  ( "/foo/bar , inst#/installed" =>
    [ 'file#/foo/bar', 'inst#/installed' ] )

) -> $to-check { parse_ok( $to-check ) };

dies_ok { CompUnitRepo.parse-spec('CompUnitRepo::GitHub#masak/html-template') },
  "must have module loaded";

# need EVAL to create and check class at runtime
EVAL '
class CompUnitRepo::GitHub {
    method short-id { "gith" };
};
for (
  ( "CompUnitRepo::GitHub#masak/html-template" =>
    [ "CompUnitRepo::GitHub#masak/html-template" ] ),
  ( "gith#masak/html-template" =>
    [ "gith#masak/html-template" ] ),
) -> $to-check { parse_ok( $to-check ) };
';

is_deeply @*INC, PARSE-INCLUDE-SPECS(CREATE-INCLUDE-SPECS(@*INC)),
  'can we reproduce an @*INC setting';

#========================================================
sub parse_ok ($to-check) {
    my $checking := $to-check.key;
    my $answers  := $to-check.value;

    my $result = PARSE-INCLUDE-SPECS($checking);
    is_deeply $result, $answers, "'$checking' returned the right thing";
}
