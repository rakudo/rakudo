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

    # Provide an human readable *unique* serialization string that does *not*
    # contain the line number in the source where it is invoked.
    method serialize-id() {
        my str $str = self.^name
          ~ '.new(:short-name<'
          ~ $.short-name
          ~ '> :from<'
          ~ $.from
          ~ '>';
        $str = $str ~ ' :!version-matcher' unless $.version-matcher;
        $str = $str ~ ' :!auth-matcher'    unless $.auth-matcher;
        $str = $str ~ ' :!api-matcher'     unless $.api-matcher;

        $str ~ ')'
    }
}

# vim: ft=perl6 expandtab sw=4
