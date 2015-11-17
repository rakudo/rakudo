class Distribution {
    has $.name;
    has $.auth;
    has $.author;
    has $.authority;
    has $.api;
    has $.ver;
    has $.version;
    has $.description;
    has @.depends;
    has %.provides;
    has %.files;
    has $.source-url;
    method auth { $!auth // $!author // $!authority }
    method ver  { $!ver // $!version }
    method hash {
        {
            :$!name,
            :$.auth,
            :$.ver,
            :$!description,
            :@!depends,
            :%!provides,
            :%!files,
            :$!source-url,
        }
    }
    method Str() {
        return "{$.name}:ver<{$.ver  // ''}>:auth<{$.auth // ''}>:api<{$.api // ''}>";
    }
    method id() {
        return nqp::sha1(self.Str);
    }
}

# during CURLI migration period
class CompUnitRepo::Distribution is Distribution {
    method Hash { self.hash }
}

# vim: ft=perl6 expandtab sw=4
