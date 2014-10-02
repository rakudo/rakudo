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
        $base.f
         ?? { files => { $file => $base.path }, ver => Version.new('0') }
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
        return () unless %extensions{$from}:exists;

        my $slash := $*SPEC.rootdir;
        my $base := $!path ~ $slash ~ $name.subst(:g, "::", $slash) ~ '.';
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
                ) if $path.IO.f;
                return %seen{$base} = CompUnit.new(
                  $path, :$name, :$extension, :!has-source, :has-precomp
                ) if ($path ~ '.' ~ $precomp-ext).IO.f;
            }
        }

        # no extensions to check, just check compiled version
        elsif $base ~ $precomp-ext -> $path {
            return %seen{$base} = CompUnit.new(
              $path, :$name, :extension(''), :!has-source, :has-precomp
            ) if $path.IO.f;
        }

        # alas
        ();
    }

    method short-id() { 'file' }
}
