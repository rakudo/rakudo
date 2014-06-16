class Distribution {
    has $.id is rw;
    has $.name;
    has $.auth;
    has $.author;
    has $.authority;
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
            :$!id,
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
}

# during panda migration period
class CompUnitRepo::Distribution is Distribution {
    method Hash { self.hash }
}
