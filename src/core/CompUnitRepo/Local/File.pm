class CompUnitRepo::Local::File does CompUnitRepo::Locally {

    my %extensions =
      Perl6 => <pm6 pm>,
      Perl5 => <pm5 pm>,
      NQP   => <nqp>,
      JVM   => ();

    # global cache of files seen
    my %seen;

    method install($source, $from?) { ... }
    method files($file, :$name, :$auth, :$ver) {
        my $base := $file.IO;
        $base.f ?? Distribution.new( :files($file => $base.path), :ver(Version.new('0')) )
                !! ();
    }

    method candidates(
      $name,
      :$from = 'Perl6',
      :$file,           # not used here (yet)
      :$auth,           # not used here (yet)
      :$ver,            # not used here (yet)
      ) {

        # sorry, cannot handle this one
        return () unless %extensions.EXISTS-KEY($from);

        my $dir-sep := $*SPEC.dir-sep;
        my $base := $!IO.abspath ~ $dir-sep ~ $name.subst(:g, "::", $dir-sep) ~ '.';
        if %seen{$base} -> $found {
            return $found;
        }

        state Str $precomp-ext = $*VM.precomp-ext;  # should be $?VM probably

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

        # alas
        ();
    }

    method short-id() { 'file' }
}
