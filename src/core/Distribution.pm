class Distribution {
    has $.id is rw;
    has $.name;
    has $.auth;
    has $.ver;
    has $.description;
    has @.depends;
    has %.provides;
    has %.files;
    has $.source-url;

    submethod BUILD(
        :$!id,
        :authority(:author(:$!auth)) = '',
        :$!name                      = '',
        :version(:$!ver)             = Version.new('0'),
        :$!description,
        :@!depends,
        :%!provides,
        :%!files,
        :$!source-url,
    ) {
        unless nqp::istype($!ver, Version) {
            $!ver = !$!ver || $!ver eq '*'
                  ?? Version.new('0')
                  !! Version.new(~$!ver)
        }
    }
}

# XXX Needed for Inline::Perl5:
#   https://github.com/niner/Inline-Perl5/blob/master/lib/Inline/Perl5.pm6#L22
multi sub postcircumfix:<{ }> (Distribution:D \d, "files" ) {
    d.files
}
