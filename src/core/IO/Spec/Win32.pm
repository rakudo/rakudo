my class IO::Spec::Win32 is IO::Spec::Unix {

    # Some regexes we use for path splitting
    my $slash       = regex {  <[\/ \\]> }
    my $notslash    = regex { <-[\/ \\]> }
    my $driveletter = regex { <[A..Z a..z]> ':' }
    my $UNCpath     = regex { [<$slash> ** 2] <$notslash>+  <$slash>  [<$notslash>+ | $] }
    my $volume_rx   = regex { <$driveletter> | <$UNCpath> }

    method canonpath ($patharg, :$parent) {
        my $path = $patharg.Str;
        $path eq '' ?? '' !! self!canon-cat($path, :$parent);
    }

    method catdir(*@dirs) {
        return "" unless @dirs;
        return self!canon-cat( "\\", @dirs ) if @dirs[0] eq "";
        self!canon-cat(|@dirs);
    }

    method dir-sep        { '\\' }
    method splitdir($dir) { $dir.split($slash)  }
    method catfile(|c)    { self.catdir(|c)     }
    method devnull        { 'nul'               }
    method rootdir        { '\\'                }

    method basename(\path) {
        my str $str = nqp::unbox_s(path);
        my int $indexf = nqp::rindex($str,'/');
        my int $indexb = nqp::rindex($str,'\\');
        nqp::p6bool($indexf == -1 && $indexb == -1)
          ?? path
          !! $indexf > $indexb
             ?? substr(path,nqp::box_i($indexf + 1,Int) )
             !! substr(path,nqp::box_i($indexb + 1,Int) );
    }

    method tmpdir {
        my $ENV := %*ENV;
        my $io;
        first( {
            if .defined {
                $io = .IO;
                $io.d && $io.r && $io.w && $io.x;
            }
        },
          $ENV<TMPDIR>,
          $ENV<TEMP>,
          $ENV<TMP>,
          'SYS:/temp',
          'C:\system\temp',
          'C:/temp',
          '/tmp',
          '/',
        ) ?? $io !! IO::Path.new(".");
    }

    method path {
       (".",
         split(';', %*ENV<PATH> // %*ENV<Path> // '').map( {
           .subst(:global, q/"/, '') } ).grep: *.chars );
   }

    method is-absolute ($path) {
        so $path ~~ /^ [ <$driveletter> <$slash> | <$slash> | <$UNCpath> ]/
    }

    method split (Str() $path is copy) {
        $path ~~ s[ <$slash>+ $] = ''                       #=
            unless $path ~~ /^ <$driveletter>? <$slash>+ $/;

        $path ~~
            m/^ ( <$volume_rx> ? )
            ( [ .* <$slash> ]? )
            (.*)
             /;
        my ($volume, $dirname, $basename) = (~$0, ~$1, ~$2);
        $dirname ~~ s/ <?after .> <$slash>+ $//;


        if all($dirname, $basename) eq '' && $volume ne '' {
            $dirname = $volume ~~ /^<$driveletter>/
                     ?? '.' !! '\\';
        }
        $basename = '\\' if $dirname eq any('/', '\\') && $basename eq '';
        $dirname  = '.'  if $dirname eq ''             && $basename ne '';

        # temporary, for the transition period
        (:$volume, :$dirname, :$basename, :directory($dirname));
#        (:$volume, :$dirname, :$basename);
    }

    method join ($volume, $dirname is copy, $file is copy) {
        $dirname = '' if $dirname eq '.' && $file.chars;
        if $dirname.match( /^<$slash>$/ ) && $file.match( /^<$slash>$/ ) {
            $file    = '';
            $dirname = '' if $volume.chars > 2; #i.e. UNC path
        }
        self.catpath($volume, $dirname, $file);
    }

    method splitpath(Str() $path, :$nofile = False) {

        if $nofile {
            $path ~~ /^ (<$volume_rx>?) (.*) /;
            (~$0, ~$1, '');
        }
        else {
            $path ~~
                m/^ ( <$volume_rx> ? )
                ( [ .* <$slash> [ '.' ** 1..2 $]? ]? )
                (.*)
                 /;
            (~$0, ~$1, ~$2);
        }
    }

    method catpath($volume is copy, $dirname, $file) {

        # Make sure the glue separator is present
        # unless it's a relative path like A:foo.txt
        if $volume.chars and $dirname.chars
           and $volume !~~ /^<$driveletter>/
           and $volume !~~ /<$slash> $/
           and $dirname !~~ /^ <$slash>/
            { $volume ~= '\\' }
        if $file.chars and $dirname.chars
           and $dirname !~~ /<$slash> $/
            { $volume ~ $dirname ~ '\\' ~ $file; }
        else     { $volume ~ $dirname     ~    $file; }
    }

    method rel2abs ($path is copy, $base? is copy, :$omit-volume) {

        my $is_abs = ($path ~~ /^ [<$driveletter> <$slash> | <$UNCpath>]/ && 2)
                  || ($path ~~ /^ <$slash> / && 1)
                  || 0;

        # Check for volume (should probably document the '2' thing...)
        return self.canonpath( $path ) if $is_abs == 2 || ($is_abs == 1 && $omit-volume);

        if $is_abs {
            # It's missing a volume, add one
            my $vol;
            $vol = self.splitpath($base)[0] if $base.defined;
            $vol ||= self.splitpath($*CWD)[0];
            return self.canonpath( $vol ~ $path );
        }

        if not defined $base {
        # TODO: implement _getdcwd call ( Windows maintains separate CWD for each volume )
        # See: http://msdn.microsoft.com/en-us/library/1e5zwe0c%28v=vs.80%29.aspx
            #$base = Cwd::getdcwd( (self.splitpath: $path)[0] ) if defined &Cwd::getdcwd ;
            #$base //= $*CWD ;
            $base = $*CWD;
        }
        elsif ( !self.is-absolute( $base ) ) {
            $base = self.rel2abs( $base );
        }
        else {
            $base = self.canonpath( $base );
        }

        my ($path_directories, $path_file) = self.splitpath( $path )[1..2] ;

        my ($base_volume, $base_directories) = self.splitpath( $base, :nofile ) ;

        $path = self.catpath(
                    $base_volume,
                    self.catdir( $base_directories, $path_directories ),
                    $path_file
                    ) ;

        return self.canonpath( $path ) ;
    }


    method !canon-cat ( $first, *@rest, :$parent --> Str) {

        $first ~~ /^ ([   <$driveletter> <$slash>?
                        | <$UNCpath>
                        | [<$slash> ** 2] <$notslash>+
                        | <$slash> ]?)
                       (.*)
                   /;
        my Str ($volume, $path) = ~$0, ~$1;

        $volume.=subst(:g, '/', '\\');
        if $volume ~~ /^<$driveletter>/ {
            $volume.=uc;
        }
        elsif $volume.chars && $volume !~~ / '\\' $/ {
            $volume ~= '\\';
        }

        $path = join "\\", $path, @rest.flat;
        $path ~~ s:g/ <$slash>+ /\\/;                              # /xx\\yy   --> \xx\yy
        $path ~~ s:g/[ ^ | '\\']   '.'  '\\.'*  [ '\\' | $ ]/\\/;  # xx/././yy --> xx/yy
        if $parent {
            while $path ~~ s:g { [^ | <?after '\\'>] <!before '..\\'> <-[\\]>+ '\\..' ['\\' | $ ] } = '' { };
        }
        $path ~~ s/^ '\\'+ //;        # \xx --> xx  NOTE: this is *not* root
        $path ~~ s/ '\\'+ $//;        # xx\ --> xx
        if $volume ~~ / '\\' $ / {    # <vol>\.. --> <vol>\
            $path ~~ s/ ^  '..'  '\\..'*  [ '\\' | $ ] //;
        }

        if $path eq '' {        # \\HOST\SHARE\ --> \\HOST\SHARE
            $volume ~~ s/<?after '\\\\' .*> '\\' $ //;
            $volume || '.';
        }
        else {
            $volume ~ $path;
        }
    }
}

# vim: ft=perl6 expandtab sw=4
