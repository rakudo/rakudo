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
        return () unless %extensions.exists_key($from);

        my $base := $!IO.abspath ~ "/" ~ $name.subst(:g, "::", "/") ~ '.';
        if %seen{$base} -> $found {
            return $found;
        }

        state Str $precomp-ext = $*VM.precomp-ext;  # should be $?VM probably

        # have extensions to check
        if %extensions{$from} -> @extensions {
            for @extensions -> $extension {
                my $abspath = $base ~ $extension;
                return %seen{$base} = CompUnit.new(
                  $abspath, :$name, :$extension, :has-source
                ) if FILETEST-E($abspath) && FILETEST-F($abspath);

                $abspath = $abspath ~ '.' ~ $precomp-ext;
                return %seen{$base} = CompUnit.new(
                  $abspath, :$name, :$extension, :!has-source, :has-precomp
                ) if FILETEST-E($abspath) && FILETEST-F($abspath);
            }
        }

        # no extensions to check, just check compiled version
        elsif $base ~ $precomp-ext -> $abspath {
            return %seen{$base} = CompUnit.new(
              $abspath, :$name, :extension(''), :!has-source, :has-precomp
            ) if FILETEST-E($abspath) && FILETEST-F($abspath);
        }

        # alas
        ();
    }

    method short-id() { 'file' }
}
