class CompUnit {
    has Str     $.from;
    has Str     $.short-name;
    has Version $.version;
    has Str     $.auth;
    has Str     $!WHICH;

    # The CompUnit::Repository that loaded this CompUnit.
    has CompUnit::Repository $.repo is required;

    # That repository's identifier for the compilation unit. This is not globally unique.
    has Str:D $.repo-id is required;

    # The low-level handle.
    has CompUnit::Handle $.handle is required;

    # Whether the module was loaded from a precompilation or not.
    has Bool $.precompiled = False;

    # The distribution that this compilation unit was installed as part of
    # (if known).
    has Distribution $.distribution;

    my $default-from = 'Perl6';

    method new(CompUnit:U:
      Str                  :$short-name is copy,
      Version              :$version,
      Str                  :$auth,
      Str                  :$from = $default-from,
      CompUnit::Handle     :$handle = CompUnit::Handle,
      CompUnit::Repository :$repo,
      Str                  :$repo-id,
      Bool                 :$precompiled = False,
      Distribution         :$distribution,
    ) {
        self.bless(
          :$short-name,
          :$version,
          :$auth,
          :$from,
          :$handle,
          :$repo,
          :$repo-id,
          :$precompiled,
          :$distribution,
        );
    }

    multi method WHICH(CompUnit:D:) { $!WHICH //= self.^name }
    multi method Str(CompUnit:D: --> Str:D)  { $!short-name }
    multi method gist(CompUnit:D: --> Str:D) { self.short-name }

    method unit() {
        $.handle.unit
    }
}

# vim: ft=perl6 expandtab sw=4
