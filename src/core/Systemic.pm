role Systemic {
    has Str $.name;
    has Str $.auth;
    has Version $.version;
    has Blob $.signature;
    has Str $.desc;

    submethod BUILD (
      :$!name    = "unknown",
      :$!auth    = "unknown",
      :$!version = Version.new("unknown"),
    ) {}
    method gist { $!name ~ (" ($!version)" if $!version ne "vunknown") }
    method Str  { $!name }
}

# Since we cannot see attributes of roles done by other roles, the below is
# a copy of the above (minus .desc), instead of doing the Universal role in
# Systemic (as is the spec).  Once we can see attributes of roles of roles
# inside a class, we can fix this abomination.
role Universal {
    has Str $.name;
    has Str $.auth;
    has Version $.version;
    has Blob $.signature;

    submethod BUILD (
      :$!name    = "unknown",
      :$!auth    = "unknown",
      :$!version = Version.new("unknown"),
    ) {}
    method gist { $!name ~ (" ($!version)" if $!version ne "vunknown") }
    method Str  { $!name }
}
