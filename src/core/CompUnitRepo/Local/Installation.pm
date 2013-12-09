class CompUnitRepo::Distribution {
    has $.id;
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
    method Hash {
        {
            :$!id,
            :$!name,
            :auth( $!auth // $!author // $!authority ),
            :ver( $!ver // $!version ),
            :$!description,
            :@!depends,
            :%!provides,
            :%!files,
            :$!source-url,
        }
    }
}

class CompUnitRepo::Local::Installation {
    has %!dists;
    has $!cver = nqp::gethllsym('perl6', '$COMPILER_CONFIG')<version>;
    has $.path;
    method new(*@locations) {
        self.bless(:@locations)
    }

    method BUILD(:@locations) {
        for @locations -> $path {
            $!path = $path.path unless $!path;
            %!dists{$path} = "$path/MANIFEST".IO.e ?? from-json( slurp "$path/MANIFEST" ) !! {};
            %!dists{$path}<file-count> //= 0;
            %!dists{$path}<dist-count> //= 0;
            %!dists{$path}<dists>      //= [ ];
        }
        self
    }

    method Str { $!path.Str }

    method writeable-path {
        %!dists.keys.first( *.IO.w )
    }

    my $unix_wrapper = '#!/usr/bin/env #perl#
sub MAIN(:$name, :$auth, :$ver, *@pos, *%named) {
    my @binaries = CompUnitRepo.files(\'bin/#name#\', :$name, :$auth, :$ver);
    unless +@binaries {
        @binaries = CompUnitRepo.files(\'bin/#name#\');
        if +@binaries {
            note q:to/SORRY/;
                ===SORRY!===
                No candidate found for \'#name#\' that match your criteria.
                Did you perhaps mean one of these?
                SORRY
            my %caps = :name([\'Distribution\', 12]), :auth([\'Author(ity)\', 11]), :ver([\'Version\', 7]);
            for @binaries -> $dist {
                for %caps.kv -> $caption, @opts is rw {
                    @opts[1] = max @opts[1], ($dist{$caption} // \'\').Str.chars
                }
            }
            note \'  \' ~ %caps.values.map({ sprintf(\'%-*s\', .[1], .[0]) }).join(\' | \');
            for @binaries -> $dist {
                note \'  \' ~ %caps.kv.map( -> $k, $v { sprintf(\'%-*s\', $v.[1], $dist{$k}) } ).join(\' | \')
            }
        }
        else {
            note "===SORRY!===\nNo candidate found for \'#name#\'.\n";
        }
        exit 1;
    }

    my $options = join(\' \', %named.map({\'--\' ~ .key ~ \'=\' ~ .value}), @pos);
    exit shell("$*EXECUTABLE_NAME {@binaries[0]<files><bin/#name#>} $options").exit
}';

    method install(:$dist!, *@files) {
        my $path     = self.writeable-path or die "No writeable path found";
        my $repo     = %!dists{$path};
        my $file-id := $repo<file-count>;
        my $d        = CompUnitRepo::Distribution.new( |$dist.metainfo, :id($repo<dist-count>++) );
        
        # Build patterns to choose what goes into "provides" section.
        my $ext = regex { [pm|pm6|pir|pbc] };
        my @provides;
        for %($d.provides).kv -> $k, $v is copy {
            $v.=subst(/\.<$ext>$/, '.');
            @provides.push: regex { $v <ext=.$ext> { make $k } }
        }
        
        # Initialize "provides" section.
        for %($d.provides).kv -> $k, $v is rw {
            $v = {};
        }
        
        # Walk the to be installed files, decide whether we put them into
        # "provides" or just "files".
        for @files -> $file is copy {
            $file.=Str;
            if [||] @provides>>.ACCEPTS($file) -> $/ {
                $d.provides{ $/.ast }{ $<ext> } = {
                    :file($file-id),
                    :time(try $file.IO.modified),
                    :$!cver
                }
            }
            else {
                if $file ~~ /^bin<[\\\/]>/ {
                    mkdir "$path/bin" unless "$path/bin".IO.d;
                    my $basename = $file.IO.path.basename;
                    for '', < -p -j > -> $ext {
                        "$path/bin/$basename$ext".IO.spurt:
                            $unix_wrapper.subst('#name#', $basename, :g).subst('#perl#', "perl6$ext");
                        "$path/bin/$basename$ext".IO.chmod(0o755) unless $*OS eq 'MSWin32';
                    }
                }
                $d.files{$file} = $file-id
            }
            copy($file, $path ~ '/' ~ $file-id);
            $file-id++;
        }
        
        $repo<dists>.push: $d.Hash;
        
        # XXX Create path if needed.
        "$path/MANIFEST".IO.spurt: to-json( $repo )
    }

    method files($file, :$name, :$auth, :$ver) {
        my @candi;
        for %!dists.kv -> $path, $repo {
            for @($repo<dists>) -> $dist {
                my $dver = $dist<ver>
                        ?? $dist<ver> ~~ Version
                            ?? $dist<ver>
                            !! Version.new( $dist<ver> )
                        !! Version.new('0');
                
                if (!$name || $dist<name> ~~ $name)
                && (!$auth || $dist<auth> ~~ $auth)
                && (!$ver  || $dver ~~ $ver)
                && $dist<files>{$file} {
                    my $candi   = %$dist;
                    $candi<ver> = $dver;
                    $candi<files>{$file} = $path ~ '/' ~ $candi<files>{$file}
                        unless $candi<files>{$file} ~~ /^$path/;
                    @candi.push: $candi;
                }
            }
        }
        @candi
    }

    method candidates($name, :$file, :$auth, :$ver) {
        my @candi;
        for %!dists.kv -> $path, $repo {
            for @($repo<dists>) -> $dist {
                my $dver = $dist<ver>
                        ?? $dist<ver> ~~ Version
                            ?? $dist<ver>
                            !! Version.new( ~$dist<ver> )
                        !! Version.new('0');
                
                if (!$auth || $dist<auth> ~~ $auth)
                && (!$ver  || $dver ~~ $ver)
                && $dist<provides>{$name} {
                    my $candi   = %$dist;
                    $candi<ver> = $dver;
                    for $candi<provides>.kv -> $ln, $files {
                        for $files.kv -> $type, $file {
                            $candi<provides>{$ln}{$type}<file> = $path ~ '/' ~ $file<file>
                                unless $candi<provides>{$ln}{$type}<file> ~~ /^$path/
                        }
                    }
                    @candi.push: $candi;
                }
            }
        }
        @candi
    }
}
