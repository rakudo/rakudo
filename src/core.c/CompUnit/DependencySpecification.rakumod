class CompUnit::DependencySpecification {
    has str $.short-name  is built(:bind) is required;  # must be native
    has str $.from        is built(:bind) = 'Perl6';    # must be native
    has str $.stringified;  # needs to be public for the JVM

    # The matcher methods will return True if implicitly or explicitly
    # initialized with an undefined value.  In the case of version and
    # API, a Version object for the given defined value, will be returned
    # if it was not already specified as a Version object.  This does
    # *not* cause an issue with stringification, as the stringification
    # of a Version object returns its original string.
    has $.auth-matcher    is built(:bind);  # either True or smartmatchee
    has $.version-matcher is built(:bind);  # either True or Version
    has $.api-matcher     is built(:bind);  # either True or Version

    # Sadly this is needed because we have a spectest that explicitly
    # passes True for unspecified matchers.  Hopefully this can go when
    # we accept that an undefined value indicates no interest in matching.
    method TWEAK() {
        $!version-matcher := Any if nqp::eqaddr($!version-matcher,True);
        $!auth-matcher    := Any if nqp::eqaddr($!auth-matcher,True);
        $!api-matcher     := Any if nqp::eqaddr($!api-matcher,True);
    }

    method auth-matcher() {
        $!auth-matcher // True
    }
    method version-matcher() {
        nqp::defined($!version-matcher)
          ?? nqp::istype($!version-matcher,Version)
            ?? $!version-matcher
            !! ($!version-matcher := Version.new($!version-matcher))
          !! True
    }
    method api-matcher() {
        nqp::defined($!api-matcher)
          ?? nqp::istype($!api-matcher,Version)
            ?? $!api-matcher
            !! ($!api-matcher := Version.new($!api-matcher))
          !! True
    }

    method !stringify() {
        my $parts := nqp::list_s($!short-name);
        nqp::push_s($parts,":from<$!from>")
          if $!from ne 'Raku' && $!from ne 'Perl6';
        nqp::push_s($parts,":ver<$!version-matcher>")
          if nqp::defined($!version-matcher);
        nqp::push_s($parts,":auth<$!auth-matcher>")
          if nqp::defined($!auth-matcher);
        nqp::push_s($parts,":api<$!api-matcher>")
          if nqp::defined($!api-matcher);
        $!stringified = nqp::join('',$parts)
    }

    # Oddly enough, the exact format of this method can make / break
    # dependency testing.  This should probably depend on the .WHICH.
    method Str(CompUnit::DependencySpecification:D: --> Str:D) {
        $!stringified ?? $!stringified !! self!stringify
    }

    multi method raku(CompUnit::DependencySpecification:D: --> Str:D) {
        my $parts := nqp::list_s(
          "CompUnit::DependencySpecification.new(:short-name<$!short-name>"
        );
        nqp::push_s($parts,",:from<$!from>")
          if $!from ne 'Raku' && $!from ne 'Perl6';
        nqp::push_s($parts,",:version-matcher<$!version-matcher>")
          if nqp::defined($!version-matcher);
        nqp::push_s($parts,",:auth-matcher<$!auth-matcher>")
          if nqp::defined($!auth-matcher);
        nqp::push_s($parts,",:api-matcher<$!api-matcher>")
          if nqp::defined($!api-matcher);
        nqp::push_s($parts,')');
        nqp::join('',$parts)
    }

    multi method WHICH(CompUnit::DependencySpecification:D --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat(
            nqp::concat(self.^name, '|'),
            self.Str
          ),
          ValueObjAt
        )
    }
}

# vim: expandtab shiftwidth=4
