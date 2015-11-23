class CompUnit {
    has Str  $.from;
    has Str  $.name;
    has Str  $!WHICH;

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
      :$name is copy,
      :$from = $default-from,
      :$handle = CompUnit::Handle,
      :$repo,
      :$repo-id,
    ) {
        $global.protect( { %instances{$name} //= self.bless(
          :$name,
          :$from,
          :$handle,
          :$repo,
          :$repo-id,
        ) } );
    }

    multi method WHICH(CompUnit:D:) { $!WHICH //= self.^name }
    multi method Str(CompUnit:D: --> Str)  { $!name }
    multi method gist(CompUnit:D: --> Str) { self.name }

    method unit() {
        $.handle.unit
    }
}

# vim: ft=perl6 expandtab sw=4
