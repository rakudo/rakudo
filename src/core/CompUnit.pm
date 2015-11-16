class CompUnit {
    has Lock $!lock;
    has Str  $.from;
    has Str  $.name;
    has Str  $.extension;
    has Str  $.path;
    has Str  $.precomp-path;
    has Str  $!WHICH;
    has Bool $.has-source;
    has Bool $.has-precomp;
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
      :$extension is copy,
      :$from = $default-from,
      :$has-source is copy,
      :$has-precomp is copy,
      :$handle = CompUnit::Handle,
      :$repo,
    ) {

        # set name / extension if not already given
        if !$name or !$extension.defined {
            my IO::Spec $SPEC := $*SPEC;
            $name      ||= $SPEC.basename($path);
            $extension ||= $SPEC.extension($name);
        }

        # sanity test
        my $VM = $*VM;
        my $precomp-path = $path ~ '.' ~ $VM.precomp-ext;  # XXX temporary
        $precomp-path = 
          $VM.precomp-dir ~ '/' ~ $name ~ '.' ~ $VM.precomp-ext
          unless $precomp-path.IO.f;
        $has-source  //= ?$path.IO.f;
        $has-precomp //= ?$precomp-path.IO.f;
        return Nil unless $has-source or $has-precomp;

        $global.protect( { %instances{$path} //= self.bless(
          :$path,
          :lock(Lock.new),
          :$name,
          :$extension,
          :$precomp-path,
          :$from,
          :$has-source,
          :$has-precomp,
          :$handle,
          :$repo,
          :!is-loaded,
        ) } );
    }

    multi method WHICH(CompUnit:D:) { $!WHICH //= "{self.^name}|$!path.abspath()" }
    multi method Str(CompUnit:D: --> Str)  { $!path.abspath }
    multi method gist(CompUnit:D: --> Str) { "{self.name}:{$!path.abspath}" }

    method key(CompUnit:D: --> Str) {
        $!has-precomp ?? $*VM.precomp-ext !! $!extension;
    }

    method load(CompUnit:D:) {
        $global.protect( {
            my int $DEBUG = $*RAKUDO_MODULE_DEBUG;
            RAKUDO_MODULE_DEBUG("going to load $!name") if $DEBUG;

            # If we didn't already do so, load the module and capture
            # its mainline. Otherwise, we already loaded it so go on
            # with what we already have.
            unless $!is-loaded {
                my $trace            = { module => $!name, filename => ~$!path };
                my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);

                # Read source file.
                RAKUDO_MODULE_DEBUG("loading ", ~$!path) if $DEBUG;

                $!handle := CompUnit::Loader.load-source-file(~$!path);
                RAKUDO_MODULE_DEBUG("done: ", $!path) if $DEBUG;

                nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                $!is-loaded = True;

                CATCH {
                    default {
                        nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                        .throw;
                    }
                }
            }
    } ) }

    method unit() {
        $.handle.unit
    }
}

# TEMPORARY ACCESS TO COMPUNIT INTERNALS UNTIL WE CAN LOAD DIRECTLY
multi sub postcircumfix:<{ }> (CompUnit:D \c, "provides" ) {
    my % = (
      c.name => {
        pm => {
          file => c.path
        },
        c.key => {
          file => c.has-precomp ?? c.precomp-path !! c.path
        }
      }
    );
}
multi sub postcircumfix:<{ }> (CompUnit:D \c, "key" ) {
    c.key;
}
multi sub postcircumfix:<{ }> (CompUnit:D \c, "ver" ) {
    Version.new('0');
}

# vim: ft=perl6 expandtab sw=4
