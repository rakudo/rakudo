class CompUnit::DependencySpecification {
    has str $.short-name is required;
    has int $.source-line-number = 0;
    has str $.from = 'Perl6';
    has $.version-matcher = True;
    has $.auth-matcher = True;
    has $.api-matcher = True;

    method Str(CompUnit::DependencySpecification:D:) {
        join '', $.short-name,
          ($.version-matcher//True) ~~ Bool ?? '' !! ":ver<$.version-matcher>",
          ($.auth-matcher   //True) ~~ Bool ?? '' !! ":auth<$.auth-matcher>",
          ($.api-matcher    //True) ~~ Bool ?? '' !! ":api<$.api-matcher>";
    }

    my grammar DepSpec {
        regex TOP { ^^ <name> [':' <key> <value>]* $$ }

        regex name  { <-restricted +name-sep>+ }
        token key   { <-restricted>+ }
        token value { '<' ~ '>'  [<( [[ <!before \>|\\> . ]+]* % ['\\' . ] )>] }

        token restricted { [':' | '<' | '>' | '(' | ')'] }
        token name-sep   { < :: > }
    }

    my class DepSpec::Actions {
        method TOP($/) { make %('name'=> $/<name>.made, %($/<key> Z=> $/<value>>>.ast)) if $/ }

        method name($/)  { make $/.Str }
        method key($/)   { my $str = make $/.Str; ($str eq 'ver') ?? 'version' !! $str }
        method value($/) { make $/.Str }
    }

    method from-string(CompUnit::DependencySpecification:U: Str $spec --> CompUnit::DependencySpecification) {
        my %spec-parts = DepSpec.parse($spec, :actions(DepSpec::Actions.new)).ast;
        self.new(short-name => %spec-parts<name>, auth-matcher => %spec-parts<auth>, version-matcher => %spec-parts<version>, api-matcher => %spec-parts<api>);
    }
}

# vim: ft=perl6 expandtab sw=4
