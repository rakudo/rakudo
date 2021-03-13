#!/usr/bin/env raku
use v6.d;
unit sub MAIN(:$foo);

my @*ARGS         = '--help';
my $*PROGRAM-NAME = 'usage-tester';

sub generate-test-usage($sig) {
    use MONKEY-SEE-NO-EVAL;
    my &*EXIT = {;}
    my $result;
    my $*OUT = class { method print(*@args) { $result ~= @args.join; }
                       method flush {} }
    with try { EVAL('sub MAIN(\qq[$sig]) {}; &MAIN ')} { RUN-MAIN($_, Nil);
                                                         ($sig, $result) }
    else { Empty }
}

my @tests =
     ('$pos, #= help1', '$opt-pos?, #= help2', ':$named, #= help3',
         ':$req-named!, #= help4', '‘literal’ #= help5', 'Int #= help6'
     ).combinations
      .map(|*.permutations.map: "\n" ~*.join("\n").indent(24)  ~"\n{' ' x 22}")
      .skip(1)
      .map(&generate-test-usage)
      .race
      .map( -> ($sig, $usage) {
          my $sig-no-comments = $sig.lines.map(*.split(‘#’)[0].trim).join(" ").trim.chop(1);
          q:to/§test/
          is-run(
              q:to/§script/,
                  my @*ARGS         = '--help';
                  my $*PROGRAM-NAME = 'usage-tester';
                  my &main = sub MAIN(\qq[{$sig}]) {};
                  RUN-MAIN(&main, Nil);
                  §script
              :out(q:to/§usage-msg/),
                  \qq[{$usage.indent(8).chomp.trim-leading}]
                  §usage-msg
              'unchanged usage for MAIN(\qq[{$sig-no-comments}])'
          );
          §test
});

say qq:to/§all-tests/;
    use lib <t/packages/>;
    use Test;
    use Test::Helpers;
    plan {+@tests};

    {@tests.join("\n")}
    §all-tests
