class CompUnit::DependencySpecification {
    has str $.short-name  is built(:bind) is required;  # must be native
    has str $.from        is built(:bind) = 'Perl6';    # must be native
    has $.version-matcher is built(:bind) = True;
    has $.auth-matcher    is built(:bind) = True;
    has $.api-matcher     is built(:bind) = True;

    # Oddly enough, the exact format of this method can make / break
    # dependency testing.  This should probably depend on the .WHICH.
    method Str(CompUnit::DependencySpecification:D: --> Str:D) {
        my $parts := nqp::list_s($!short-name);
        nqp::push_s($parts,":from<$!from>")
          if $!from ne 'Perl6';
        nqp::push_s($parts,":ver<$!version-matcher>")
          unless nqp::eqaddr($!version-matcher,True);
        nqp::push_s($parts,":auth<$!auth-matcher>")
          unless nqp::eqaddr($!auth-matcher,True);
        nqp::push_s($parts,":api<$!api-matcher>")
          unless nqp::eqaddr($!api-matcher,True);
        nqp::join('',$parts)
    }

    multi method raku(CompUnit::DependencySpecification:D: --> Str:D) {
        my $parts := nqp::list_s(
          "CompUnit::DependencySpecification.new(:short-name<$!short-name>"
        );
        nqp::push_s($parts,",:from<$!from>")
          if $!from ne 'Perl6';
        nqp::push_s($parts,",:version-matcher<$!version-matcher>")
          unless nqp::eqaddr($!version-matcher,True);
        nqp::push_s($parts,",:auth-matcher<$!auth-matcher>")
          unless nqp::eqaddr($!auth-matcher,True);
        nqp::push_s($parts,",:api-matcher<$!api-matcher>")
          unless nqp::eqaddr($!api-matcher,True);
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
