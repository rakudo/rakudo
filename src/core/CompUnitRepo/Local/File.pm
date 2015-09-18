class CompUnitRepo::Local::File does CompUnitRepo::Locally {

    my %extensions =
      Perl6 => <pm6 pm>,
      Perl5 => <pm5 pm>,
      NQP   => <nqp>,
      JVM   => ();

    # global cache of files seen
    my %seen;

    method files($file, :$name, :$auth, :$ver) {
        my $base := $file.IO;
        $base.f
         ?? { files => { $file => $base.path }, ver => Version.new('0') }
         !! ();
    }

    method candidates(
      $name,
      :$from = 'Perl6',
      :$file,
      :$auth,           # not used here (yet)
      :$ver,            # not used here (yet)
      ) {

        # sorry, cannot handle this one
        return () unless %extensions.EXISTS-KEY($from);

        state Str $precomp-ext = $*VM.precomp-ext;  # should be $?VM probably
        my $dir-sep           := $*SPEC.dir-sep;

        # We have a $file when we hit: require "PATH" or use/require Foo:file<PATH>;
        if $file {
            my $has_precomp = $file.ends-with($precomp-ext);
            my $path = $file.IO.is-absolute
                    ?? $file
                    !! $!IO.abspath ~ $dir-sep ~ $file;

            return %seen{$path} = CompUnit.new(
              $path, :$name, :extension(''), :has-source(!$has_precomp), :$has_precomp
            ) if IO::Path.new-from-absolute-path($path).f;
        }
        # deduce path to compilation unit from package name
        else {
            my $base := $!IO.abspath ~ $dir-sep ~ $name.subst(:g, "::", $dir-sep) ~ '.';
            if %seen{$base} -> $found {
                return $found;
            }

            # have extensions to check
            if %extensions{$from} -> @extensions {
                for @extensions -> $extension {
                    my $path = $base ~ $extension;
                    return %seen{$base} = CompUnit.new(
                      $path, :$name, :$extension, :has-source
                    ) if IO::Path.new-from-absolute-path($path).f;
                    return %seen{$base} = CompUnit.new(
                      $path, :$name, :$extension, :!has-source, :has-precomp
                    ) if IO::Path.new-from-absolute-path($path ~ '.' ~ $precomp-ext).f;
                }
            }

            # no extensions to check, just check compiled version
            elsif $base ~ $precomp-ext -> $path {
                return %seen{$base} = CompUnit.new(
                  $path, :$name, :extension(''), :!has-source, :has-precomp
                ) if IO::Path.new-from-absolute-path($path).f;
            }
        }

        # alas
        ();
    }

    method short-id() { 'file' }
}
