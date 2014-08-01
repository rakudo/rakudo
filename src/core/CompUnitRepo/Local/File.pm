class CompUnitRepo::Local::File does CompUnitRepo::Locally {

    my Str $precomp-ext     := $*VM.precomp-ext;
    my Int $precomp-ext-dot := $precomp-ext.chars + 1;
    my %extensions =
      Perl6 => <pm6 pm>,
      Perl5 => <pm5 pm>,
      NQP   => <nqp>,
      JVM   => ();
    my Str $slash := IO::Spec.rootdir;

    # global cache of files seen
    my %seen;

    method install($source, $from?) { ... }
    method files($file, :$name, :$auth, :$ver) { ... }

    method candidates(
      $name,
      :$from = 'Perl6',
      :$file,           # not used here (yet)
      :$auth,           # not used here (yet)
      :$ver,            # not used here (yet)
      ) {

        # sorry, cannot handle this one
        return () unless %extensions{$from}:exists;

        my $base := $!path ~ $slash ~ $name.subst(:g, "::", $slash) ~ '.';
        if %seen{$base} -> $found {
            return $found;
        }

        # have extensions to check
        if %extensions{$from} -> @extensions {
            for @extensions -> $extension {
                my $path = $base ~ $extension;
                return %seen{$base} = CompUnit.new($path,:$name,:$extension)
                  if $path.IO.f;
            }
        }

        # no extensions to check, just check compiled version
        elsif $base ~ $precomp-ext -> $path {
            return %seen{$base} = CompUnit.new($path, :$name)
              if $path.IO.f;
        }

        # alas
        ();
    }

    method short-id() { 'file' }
}
