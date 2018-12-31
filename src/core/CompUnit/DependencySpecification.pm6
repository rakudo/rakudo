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
}

# vim: ft=perl6 expandtab sw=4
