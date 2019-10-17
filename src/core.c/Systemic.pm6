role Systemic {
    has Str $.name;
    has Str $.auth;
    has Version $.version;
    has Blob $.signature;
    has Str $.desc;

    submethod BUILD(
      :$!name    = "unknown",
      :$!auth    = "unknown",
      :$!version = Version.new(["unknown"], "unknown")
      --> Nil
    ) { }
    multi method gist(Systemic:D:) {
        $.name ~ (" ($!version)" if $.version.gist ne "vunknown")
    }
    method Str  { $.name }
}

# vim: ft=perl6 expandtab sw=4
