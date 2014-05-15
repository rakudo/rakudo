role Application {
    has Str $.name;
    has Str $.auth;
    has Version $.version;

    submethod BUILD (
      :$!name    = "unknown",
      :$!auth    = "unknown",
      :$!version = Version.new("unknown"),
    ) {}
    method gist { $!name ~ (" ($!version)" if $!version ne "vunknown") }
    method Str  { $!name }
}
