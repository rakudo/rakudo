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

    my Lock $global = Lock.new;
    my $default-from = 'Perl6';
    my %instances;

    method new(CompUnit:U:
      Str                  :$short-name is copy,
      Version              :$version,
      Str                  :$auth,
      Str                  :$from = $default-from,
      CompUnit::Handle     :$handle = CompUnit::Handle,
      CompUnit::Repository :$repo,
      Str                  :$repo-id,
    ) {
        $global.protect( { %instances{$short-name} //= self.bless(
          :$short-name,
          :$version,
          :$auth,
          :$from,
          :$handle,
          :$repo,
          :$repo-id,
        ) } );
    }

    multi method WHICH(CompUnit:D:) { $!WHICH //= self.^name }
    multi method Str(CompUnit:D: --> Str)  { $!short-name }
    multi method gist(CompUnit:D: --> Str) { self.short-name }

    method unit() {
        $.handle.unit
    }
}

# vim: ft=perl6 expandtab sw=4
