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

    # NOTE: IO::Path.resolve assumes dir sep is 1 char
    method dir-sep {  ｢\｣  }
    method devnull { 'nul' }
    method rootdir {  ｢\｣  }
    method splitdir(Cool:D $path) {
        nqp::p6bindattrinvres(
          nqp::create(List), List, '$!reified',
          nqp::split('/', nqp::join('/', nqp::split(｢\｣, $path.Str))))
        || ('',)
    }

    method basename(str \path) {
        my int $indexf = nqp::rindex(path,'/');
        my int $indexb = nqp::rindex(path,'\\');
        nqp::iseq_i($indexf,-1) && nqp::iseq_i($indexb,-1)
          ?? path
          !! nqp::substr(path,($indexf > $indexb ?? $indexf !! $indexb) + 1)
    }

    method tmpdir {
        my $ENV := %*ENV;
        for $ENV<TMPDIR>, $ENV<TEMP>, $ENV<TMP>,
          'SYS:/temp', 'C:\system\temp', 'C:/temp', '/tmp', '/' {
            if .defined {
                my $io := IO::Path.new($_);
                return $io if $io.d && $io.rwx
            }
        }

        IO::Path.new(".")
    }

    method path {
        my $parts := nqp::split(";",%*ENV<PATH> // %*ENV<Path> // '');
        nqp::push((my $buffer := nqp::create(IterationBuffer)),".");

        nqp::while(
          nqp::elems($parts),
          # unsure why old code removed all `"`, but keeping code same
          # https://colabti.org/irclogger/irclogger_log/perl6-dev?date=2017-05-15#l240
          nqp::if(
            ($_ := nqp::join('',nqp::split('"',nqp::shift($parts)))),
            nqp::push($buffer,$_)
          )
        );
        $buffer.Seq
    }

    method is-absolute ( Str() $path) {
        nqp::hllbool(
          nqp::iseq_i(($_ := nqp::ord($path)), 92) # /^ ｢\｣ /
          || nqp::iseq_i($_, 47)                   # /^ ｢/｣ /
          || (nqp::eqat($path, ':', 1) # /^ <[A..Z a..z]> ':' [ ｢\｣ | ｢/｣ ] /
              && ( (nqp::isge_i($_, 65) && nqp::isle_i($_, 90)) # drive letter
                || (nqp::isge_i($_, 97) && nqp::isle_i($_, 122)))
              && ( nqp::iseq_i(($_ := nqp::ordat($path, 2)), 92) # slash
                || nqp::iseq_i($_, 47))))
    }

    method split(IO::Spec::Win32: Cool:D $path is copy) {
        $path ~~ s[ <$slash>+ $] = ''                       #=
            unless $path ~~ /^ <$driveletter>? <$slash>+ $/;

        $path ~~
            m/^ ( <$volume_rx> ? )
            ( [ .* <$slash> ]? )
            (.*)
             /;

        my str $volume   = $0.Str;
        my str $dirname  = $1.Str;
        my str $basename = $2.Str;

        nqp::stmts(
          nqp::while( # s/ <?after .> <$slash>+ $//
            nqp::isgt_i(($_ := nqp::sub_i(nqp::chars($dirname), 1)), 0)
            && (nqp::eqat($dirname, ｢\｣, $_) || nqp::eqat($dirname, '/', $_)),
            $dirname = nqp::substr($dirname, 0, $_)),
          nqp::if(
            $volume && nqp::isfalse($dirname) && nqp::isfalse($basename),
            nqp::if(
              nqp::eqat($volume, ':', 1) # /^ <[A..Z a..z]> ':'/
                && ( (nqp::isge_i(($_ := nqp::ord($volume)), 65) # drive letter
                    && nqp::isle_i($_, 90))
                  || (nqp::isge_i($_, 97) && nqp::isle_i($_, 122))),
              ($dirname = '.'),
              ($dirname = ｢\｣))),
          nqp::if(
            (nqp::iseq_s($dirname, ｢\｣) || nqp::iseq_s($dirname, '/'))
            && nqp::isfalse($basename),
            $basename = ｢\｣),
          nqp::if(
            $basename && nqp::isfalse($dirname),
            $dirname = '.'));

        IO::Path::Parts.new($volume, $dirname, $basename)
    }

    method join (Str \vol, Str $dir is copy, Str $file is copy) {
        nqp::stmts(
          nqp::if(
            $file && nqp::iseq_s($dir, '.'),
            ($dir = ''),
            nqp::if(
                 (nqp::iseq_s($dir,  ｢\｣) || nqp::iseq_s($dir,  ｢/｣))
              && (nqp::iseq_s($file, ｢\｣) || nqp::iseq_s($file, ｢/｣)),
              nqp::stmts(
                ($file = ''),
                nqp::if(
                  nqp::isgt_i(nqp::chars(vol), 2), # i.e. UNC path
                  $dir = '')))),
          self.catpath: vol, $dir, $file)
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

    method catpath(Str $vol is copy, Str \dir, Str \file) {
        nqp::stmts(
          nqp::if(       # Make sure the glue separator is present
            $vol && dir  # unless it's a relative path like A:foo.txt
            && nqp::isfalse(
              nqp::iseq_i(nqp::ord($vol, 1), 58) # /^ <[A..Z a..z]> ':'/
                && (  (nqp::isge_i(nqp::ord($vol), 65) # 'A'
                    && nqp::isle_i(nqp::ord($vol), 90)) # 'Z'
                  ||  (nqp::isge_i(nqp::ord($vol), 97)  # 'a'
                    && nqp::isle_i(nqp::ord($vol), 122)))) # 'z'
            && nqp::isfalse( # /<[/\\]> $/
                nqp::iseq_i(92, nqp::ord( # '\'
                  $vol, nqp::sub_i(nqp::chars($vol), 1)))
                || nqp::iseq_i(47, nqp::ord( # '/'
                  $vol, nqp::sub_i(nqp::chars($vol), 1))))
            && nqp::isfalse( # /^ /<[/\\]>/
                nqp::iseq_i(92, nqp::ord(dir)) # '\'
                || nqp::iseq_i(47, nqp::ord(dir))), # '/'
            $vol = nqp::concat($vol, ｢\｣)),
            nqp::if(
              dir && file
              && nqp::isfalse( # /<[/\\]> $/
                  nqp::iseq_i(92, nqp::ord( # '\'
                    dir, nqp::sub_i(nqp::chars(dir), 1)))
                  || nqp::iseq_i(47, nqp::ord( # '/'
                    dir, nqp::sub_i(nqp::chars(dir), 1)))),
              nqp::concat($vol, nqp::concat(dir, nqp::concat(｢\｣, file))),
              nqp::concat($vol, nqp::concat(dir,                  file))))
    }

    method rel2abs (Str() $path is copy, $base? is copy, :$omit-volume) {
        nqp::if(
          (nqp::eqat($path, ':', 1) # /^ <[A..Z a..z]> ':' [ ｢\｣ | ｢/｣ ] /
              && ( (nqp::isge_i(($_ := nqp::ord($path)), 65) # drive letter
                  && nqp::isle_i($_, 90))
                || (nqp::isge_i($_, 97) && nqp::isle_i($_, 122)))
              && ( nqp::iseq_i(($_ := nqp::ordat($path, 2)), 92) # slash
                || nqp::iseq_i($_, 47)))
          || 0, #($path ~~ /^ <$UNCpath>/),
          self.canonpath($path),
          nqp::if(
            nqp::iseq_i(($_ := nqp::ord($path)), 92) # /^ ｢\｣ /
            || nqp::iseq_i($_, 47),                  # /^ ｢/｣ /
            nqp::if(
              $omit-volume,
              self.canonpath($path),
              nqp::stmts(
                (my $vol),
                nqp::if(
                  nqp::defined($base),
                  ($vol := self.splitpath($base).AT-POS(0))),
                nqp::unless(
                  $vol,
                  ($vol := self.splitpath($*CWD)[0])),
                self.canonpath($vol ~ $path))),
            nqp::stmts(
              nqp::unless(
                nqp::defined($base),
                ($base = $*CWD),
                nqp::unless(
                  self.is-absolute($base),
                  ($base = self.rel2abs: $base),
                  ($base = self.canonpath: $base))),
              (my ($path_directories, $path_file)
                = self.splitpath($path)[1, 2]),
              (my ($base_volume, $base_directories)
                = self.splitpath($base, :nofile)),
              self.canonpath(
                self.catpath(
                  $base_volume,
                  self.catdir($base_directories, $path_directories),
                  $path_file)))))
    }


    method !canon-cat ( $first, *@rest, :$parent --> Str:D) {
        $first ~~ /^ ([   <$driveletter> <$slash>?
                        | <$UNCpath>
                        | [<$slash> ** 2] <$notslash>+
                        | <$slash> ]?)
                       (.*)
                   /;
        my str $volume = ~$0;
        my str $path   = ~$1;
        my int $temp;


        $volume = nqp::join(｢\｣, nqp::split('/', $volume));
        $temp   = nqp::ord($volume);

        nqp::if(
          nqp::eqat($volume, ':', 1) # this chunk == ~~ /^<[A..Z a..z]>':'/
            && ( (nqp::isge_i($temp, 65) && nqp::isle_i($temp, 90))
              || (nqp::isge_i($temp, 97) && nqp::isle_i($temp, 122))),
          ($volume = nqp::uc($volume)),
          nqp::if(
            ($temp = nqp::chars($volume))
              && nqp::isfalse(nqp::eqat($volume, ｢\｣, nqp::sub_i($temp, 1))),
            ($volume = nqp::concat($volume, ｢\｣))));

        $path = join ｢\｣, $path, @rest.flat;

        # /xx\\\yy\/zz  --> \xx\yy\zz
        $path = nqp::join(｢\｣, nqp::split('/', $path));
        nqp::while(
          nqp::isne_i(-1, $temp = nqp::index($path, ｢\\｣)),
          ($path = nqp::replace($path, $temp, 2, ｢\｣)));

        # xx/././yy --> xx/yy
        $path ~~ s:g/[ ^ | ｢\｣]   '.'  ｢\.｣*  [ ｢\｣ | $ ]/\\/;

        nqp::if($parent,
          nqp::while(
            ($path ~~ s:g {
                [^ | <?after ｢\｣>] <!before ｢..\｣> <-[\\]>+ ｢\..｣ [ ｢\｣ | $ ]
              } = ''),
            nqp::null));

        nqp::while( # \xx --> xx  NOTE: this is *not* root
          nqp::iseq_i(0, nqp::index($path, ｢\｣)),
          ($path = nqp::substr($path, 1)));

        nqp::while( # xx\ --> xx
          nqp::eqat($path, ｢\｣, ($temp = nqp::sub_i(nqp::chars($path), 1))),
          ($path = nqp::substr($path, 0, $temp)));

        nqp::if( # <vol>\.. --> <vol>\
          nqp::eqat($volume, ｢\｣, nqp::sub_i(nqp::chars($volume), 1)),
          $path ~~ s/ ^  '..'  ｢\..｣*  [ ｢\｣ | $ ] //);

        nqp::if(
          $path,
          nqp::concat($volume, $path),
          nqp::stmts( # \\HOST\SHARE\ --> \\HOST\SHARE
            nqp::iseq_i(0, nqp::index($volume, ｢\\｣))
                && nqp::iseq_i(nqp::rindex($volume, ｢\｣),
                  ($temp = nqp::sub_i(nqp::chars($volume), 1)))
                && ($volume = nqp::substr($volume, 0, $temp)),
            $volume || '.'))
    }
}

# vim: expandtab shiftwidth=4
