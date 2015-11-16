class CompUnit {
    has Lock $!lock;
    has Str  $.from;
    has Str  $.name;
    has Str  $.path;
    has Str  $!WHICH;
    has Bool $.is-loaded;

    # The CompUnit::Repository that loaded this CompUnit.
    has CompUnit::Repository $.repo is required;
    # That repository's identifier for the compilation unit. This is not globally unique.
    # has Str:D $.repo-id is required;

    # The low-level handle.
    has CompUnit::Handle $.handle; # is required;

    my Lock $global = Lock.new;
    my $default-from = 'Perl6';
    my %instances;

    method new(CompUnit:U:
      $path,
      :$name is copy,
      :$from = $default-from,
      :$handle = CompUnit::Handle,
      :$repo,
    ) {

        # set name if not already given
        if !$name {
            my IO::Spec $SPEC := $*SPEC;
            $name = $SPEC.basename($path);
        }

        $global.protect( { %instances{$path} //= self.bless(
          :$path,
          :lock(Lock.new),
          :$name,
          :$from,
          :$handle,
          :$repo,
          :!is-loaded,
        ) } );
    }

    multi method WHICH(CompUnit:D:) { $!WHICH //= "{self.^name}|$!path.abspath()" }
    multi method Str(CompUnit:D: --> Str)  { $!path.abspath }
    multi method gist(CompUnit:D: --> Str) { "{self.name}:{$!path.abspath}" }

    method load(CompUnit:D:) {
        $global.protect( {
            my int $DEBUG = $*RAKUDO_MODULE_DEBUG;
            RAKUDO_MODULE_DEBUG("going to load $!name") if $DEBUG;

            # If we didn't already do so, load the module and capture
            # its mainline. Otherwise, we already loaded it so go on
            # with what we already have.
            unless $!is-loaded {
                # Read source file.
                RAKUDO_MODULE_DEBUG("loading ", ~$!path) if $DEBUG;

                $!handle := CompUnit::Loader.load-source-file(~$!path);
                RAKUDO_MODULE_DEBUG("done: ", $!path) if $DEBUG;

                $!is-loaded = True;
            }
    } ) }

    method unit() {
        $.handle.unit
    }
}

# vim: ft=perl6 expandtab sw=4
