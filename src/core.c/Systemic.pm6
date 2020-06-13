role Systemic {
    has Str $.name is built(:bind) = 'unknown';
    has Str $.auth is built(:bind) = 'unknown';
    has Version $.version is built(:bind);
    has Blob $.signature;
    has Str $.desc;

    multi method gist(Systemic:D: --> Str:D) {
        $!version
          ?? "$.name ($.version)"
          !! $.name
    }
    method Str(--> Str:D) { $.name }
}

# vim: expandtab shiftwidth=4
