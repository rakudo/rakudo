class CompUnit {
    has Str:D   $.from       is built(:bind) = 'Perl6';
    has Str:D   $.short-name is built(:bind) is required;
    has Version $.version    is built(:bind);
    has Str     $.auth       is built(:bind);

    # The CompUnit::Repository that loaded this CompUnit.
    has CompUnit::Repository:D $.repo is built(:bind) is required;

    # That repository's identifier for the compilation unit.
    # This is not globally unique.
    has Str:D $.repo-id is built(:bind) is required;

    # The low-level handle.
    has CompUnit::Handle $.handle is built(:bind);

    # Whether the module was loaded from a precompilation or not.
    has Bool:D $.precompiled is built(:bind) = False;

    # The distribution that this compilation unit was installed as part of
    # (if known).
    has Distribution $.distribution is built(:bind);
    has ValueObjAt $!WHICH;

    multi method WHICH(CompUnit:D: --> ValueObjAt:D) {
        nqp::isconcrete($!WHICH) ?? $!WHICH !! self!WHICH
    }
    method !WHICH() {
        my $parts :=
          nqp::list_s($!from,$!short-name,$!repo-id,$!precompiled.Str);
        nqp::push_s($parts,$!version.Str)      if $!version;
        nqp::push_s($parts,$!auth)             if $!auth;
        nqp::push_s($parts,$!distribution
          ?? CompUnit::Repository::Distribution.new(
               $!distribution,
               :repo($!repo-id)
             ).Str
          !! $!repo-id
        );

        $!WHICH := nqp::box_s(
          nqp::concat(
            nqp::concat(self.^name, '|'),
            nqp::sha1(nqp::join("\0",$parts))
          ),
          ValueObjAt
        )
    }

    multi method Str(CompUnit:D: --> Str:D)  { $!short-name }
    multi method gist(CompUnit:D: --> Str:D) { self.short-name }

    method unit() {
        $.handle.unit
    }
}

# vim: expandtab shiftwidth=4
