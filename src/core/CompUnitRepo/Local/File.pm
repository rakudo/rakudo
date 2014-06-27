class CompUnitRepo::Local::File does CompUnitRepo::Locally {

    method install($source, $from?) { ... }
    method files($file, :$name, :$auth, :$ver) { ... }

    method candidates($name, :$from = 'perl6', :$file, :$auth, :$ver) {
        my @extensions = $from eq 'perl6'
          ?? ($*VM.precomp-ext,'pm6','pm')
          !! $from eq 'perl5'
            ?? ($*VM.precomp-ext,'pm5','pm')
            !! $from eq 'nqp'
               ?? ($*VM.precomp-ext,'nqp')
               !! die "Don't know how to handle :from<$from>";
        my $anyextensions = any(@extensions);
        my @candidates;
        my %seen;

        FILES:
        for $!path.contents -> $file {
            if $file.basename -> $base {
                my $root;
                my $ext;
                for $base.rindex(".") -> $i {
                    next FILES unless $i.defined; # could not find extension

                    $root = $base.substr(0,$i);
                    next FILES unless $root ~~ $name;         # not right name

                    $ext = $base.substr($i + 1);
                    next FILES unless $ext ~~ $anyextensions; # not right ext
                }

                if %seen{$root} -> $seenroot {                # seen name before
                    $seenroot{$ext} := \(~$file, :name($root) );
                }

                else {                                        # first time
                    %seen{$root}{$ext} := \(~$file, :name($root) );
                    @candidates.push: %seen{$root};
                }
            }
        }

        CANDIDATE:
        for @candidates <-> $can {
            for @extensions -> $extension {
                if $can{$extension} -> $sig { # use this extension
                    $can = CompUnit.new(|$sig, :$extension);
                    next CANDIDATE;
                }
            }
        }

        @candidates;
    }

    method short-id() { 'file' }
}
