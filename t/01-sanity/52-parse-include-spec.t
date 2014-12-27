use v6;
use Test;

plan 13;

for (

  ( '/foo/bar' =>
    [ $( CompUnitRepo::Local::File, '/foo/bar', ().hash ) ] ),
  ( 'file:/foo/bar' =>
    [ $( CompUnitRepo::Local::File, '/foo/bar', ().hash ) ] ),
  ( 'inst:/installed' =>
    [ $( CompUnitRepo::Local::Installation, '/installed', ().hash ) ] ),
  ( 'CompUnitRepo::Local::Installation:/installed' =>
    [ $( CompUnitRepo::Local::Installation, '/installed', ().hash ) ] ),
  ( 'inst:name<work>:/installed' =>
    [ $( CompUnitRepo::Local::Installation, '/installed', (:name<work>).hash)]),
  ( 'inst:name[work]:/installed' =>
    [ $( CompUnitRepo::Local::Installation, '/installed', (:name<work>).hash)]),
  ( 'inst:name{work}:/installed' =>
    [ $( CompUnitRepo::Local::Installation, '/installed', (:name<work>).hash)]),
  ( '/foo/bar  ,  /foo/baz' =>
    [ $( CompUnitRepo::Local::File, '/foo/bar', ().hash ),
      $( CompUnitRepo::Local::File, '/foo/baz', ().hash ) ] ),
  ( 'inst:/installed, /also' =>
    [ $( CompUnitRepo::Local::Installation, '/installed', ().hash ),
      $( CompUnitRepo::Local::Installation, '/also', ().hash ) ] ),

) -> $to-check { parse_ok( $to-check ) };

#?rakudo todo 'RT #122137'
ok 1, '#122137';
#parse_ok( 
#  ( '/foo/bar , inst:/installed' =>
#    [ $( CompUnitRepo::Local::File, '/foo/bar', ().hash ),
#      $( CompUnitRepo::Local::Installation, '/installed', ().hash ) ] )
#);

dies_ok { CompUnitRepo.parse-spec('CompUnitRepo::GitHub:masak/html-template') },
  "must have module loaded";

# need EVAL to create and check class at runtime
EVAL '
class CompUnitRepo::GitHub { method short-id { "gith" } };
for (
  ( "CompUnitRepo::GitHub:masak/html-template" =>
    [ $( CompUnitRepo::GitHub, "masak/html-template", ().hash ) ] ),
  ( "gith:masak/html-template" =>
    [ $( CompUnitRepo::GitHub, "masak/html-template", ().hash ) ] ),
) -> $to-check { parse_ok( $to-check ) };
';

#========================================================
sub parse_ok ($to-check) {
    my $checking := $to-check.key;
    my $answers  := $to-check.value;

    my $result = PARSE-INCLUDE-SPEC($checking);
    is_deeply $result, $answers, "'$checking' returned the right thing";
}
