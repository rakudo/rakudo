my class IO::Spec::Unix is IO::Spec {

    method canonpath( $patharg, :$parent --> Str:D) {
        nqp::if(
          (my str $path = $patharg.Str),
          nqp::stmts(
            nqp::while(                # // -> /
              nqp::isne_i(nqp::index($path,'//'),-1),
              $path = nqp::join('/',nqp::split('//',$path))
            ),
            nqp::while(                # /./ -> /
              nqp::isne_i(nqp::index($path,'/./'),-1),
              $path = nqp::join('/',nqp::split('/./',$path))
            ),
            nqp::if(                   # /. $ -> /
              nqp::eqat($path,'/.',nqp::sub_i(nqp::chars($path),2)),
              $path = nqp::substr($path,0,nqp::sub_i(nqp::chars($path),1))
            ),
            nqp::if(                   # ^ ./ ->
              nqp::eqat($path,'./',0) && nqp::isgt_i(nqp::chars($path),2),
              $path = nqp::substr($path,2)
            ),
            nqp::if(
              $parent,
              nqp::stmts(
                nqp::while(          # ^ /.. -> /
                  ($path ~~ s:g {  [^ | <?after '/'>] <!before '../'> <-[/]>+ '/..' ['/' | $ ] } = ''),
                  nqp::null
                ),
                nqp::unless(
                  $path,
                  $path = '.'
                )
              )
            ),
            nqp::if(                       # ^ /
              nqp::eqat($path,'/',0),
              nqp::stmts(
                nqp::while(                # ^ /../ -> /
                  nqp::eqat($path,'/../',0),
                  $path = nqp::substr($path,3)
                ),
                nqp::if(                   # ^ /.. $ -> /
                  nqp::iseq_s($path,'/..'),
                  $path = '/'
                )
              )
            ),
            nqp::if(                       # .+/ -> .+
              nqp::isgt_i(nqp::chars($path),1)
                && nqp::eqat($path,'/',nqp::sub_i(nqp::chars($path),1)),
              nqp::substr($path,0,nqp::sub_i(nqp::chars($path),1)),
              $path
            )
          ),
          ''
        )
    }

    method dir-sep  {  '/' }
    method curdir   {  '.' }
    method updir    { '..' }
    method curupdir { none('.','..') }
    method rootdir  { '/' }
    method devnull  { '/dev/null' }

    method basename(\path) {
        my str $str = nqp::unbox_s(path);
        my int $index = nqp::rindex($str,'/');
        nqp::p6bool($index == -1)
          ?? path
          !! substr(path,nqp::box_i($index + 1,Int) );
    }

    method extension(\path) {
        my str $str = nqp::unbox_s(path);
        my int $index = nqp::rindex($str,'.');
        nqp::p6bool($index == -1)
          ?? ''
          !! substr(path,nqp::box_i($index + 1,Int) );
    }

    method tmpdir {
        my $io;
        first( {
            if .defined {
                $io = .IO;
                $io.d && $io.r && $io.w && $io.x;
            }
          },
          %*ENV<TMPDIR>,
          '/tmp',
        ) ?? $io !! IO::Path.new(".");
    }

    method is-absolute( Str() \path ) {
        nqp::p6bool(nqp::eqat(path, '/', 0));
    }

    method path {
        if %*ENV<PATH> -> $PATH {
            $PATH.split( ':' ).map: { $_ || '.' };
        }
        else {
            ();
        }
    }

    method splitpath( $path, :$nofile = False ) {
        if $nofile {
            ( '', $path, '' );
        }
        else {
            $path ~~ m/^ ( [ .* \/ [ '.'**1..2 $ ]? ]? ) (<-[\/]>*) /;
            ( '', ~$0, ~$1 );
        }
    }

    multi method split(IO::Spec::Unix: Cool:D $path) {
        my str $p = $path.Str;
        my int $chars = nqp::chars($p);

        nqp::while(
            nqp::if(
                ($chars = nqp::sub_i(nqp::chars($p), 1)),
                nqp::eqat($p, '/', $chars),
            ),
            $p = nqp::substr($p, 0, $chars),
        );

        my str $dirname;
        my str $basename;
        my int $slash-at = nqp::rindex($p, '/');
        nqp::if(
            $slash-at,
            nqp::if(
                nqp::iseq_i($slash-at, -1),
                nqp::stmts(
                    ($dirname = ''),
                    $basename = $p,
                ),
                nqp::stmts(
                    ($dirname = nqp::substr($p, 0, $slash-at)),
                    $basename = nqp::substr($p, nqp::add_i($slash-at, 1)),
                ),
            ),
            nqp::stmts(
                ($dirname = '/'),
                $basename = nqp::substr($p, 1),
            ),
        );

        nqp::while(
            nqp::if(
                ($chars = nqp::sub_i(nqp::chars($dirname), 1)),
                nqp::eqat($dirname, '/', $chars),
            ),
            $dirname = nqp::substr($dirname, 0, $chars),
        );

        nqp::if(
            $basename,
            nqp::unless($dirname, $dirname = '.'),
            nqp::if(
                nqp::iseq_s($dirname, '/'),
                $basename = '/',
            ),
        );

        # shell dirname '' produces '.', but we don't because it's probably user error

        # temporary, for the transition period
        (:volume(''), :$dirname, :$basename, :directory($dirname));
#        (:volume(''), :$dirname, :$basename);
    }

    method join ($, \dir, \file) {
        self.catpath(
            '',
            nqp::if(
                nqp::unless(
                    nqp::if( nqp::iseq_s(dir, '/'), nqp::iseq_s(file, '/'), ),
                    nqp::if( nqp::iseq_s(dir, '.'), file ),
                ),
                '',
                dir,
            ),
            file,
        );
    }

    method catpath( $, \dirname, \file ) {
        nqp::if(
            nqp::if(
                nqp::isne_s(dirname, ''),
                nqp::if(
                    nqp::isne_s(file, ''),
                    nqp::if(
                        nqp::isfalse(nqp::eqat(
                            dirname, '/', nqp::sub_i(nqp::chars(dirname), 1)
                        )),
                        nqp::isfalse(nqp::eqat(file, '/', 0)),
                    ),
                ),
            ),
            nqp::concat(dirname, nqp::concat('/', file)),
            nqp::concat(dirname, file),
        )
    }

    method catdir (*@parts) {
        self.canonpath: nqp::concat(
            @parts.join('/'),
            nqp::if(@parts, '/', ''),
        )
    }
    method splitdir( $path ) { $path.split( '/' )  }
    method catfile( |c )     { self.catdir(|c) }

    method abs2rel( $path is copy, $base is copy = $*CWD ) {
        if self.is-absolute($path) || self.is-absolute($base) {
            $path = self.rel2abs( $path );
            $base = self.rel2abs( $base );
        }
        else {
            # save a couple of cwd()s if both paths are relative
            $path = self.catdir( self.rootdir, $path );
            $base = self.catdir( self.rootdir, $base );
        }

        my ($path_volume, $path_directories) = self.splitpath( $path, :nofile );
        my ($base_volume, $base_directories) = self.splitpath( $base, :nofile );

        # Can't relativize across volumes
        return $path unless $path_volume eq $base_volume;

        # For UNC paths, the user might give a volume like //foo/bar that
        # strictly speaking has no directory portion.  Treat it as if it
        # had the root directory for that volume.
        if !$base_directories && self.is-absolute( $base ) {
            $base_directories = self.rootdir;
        }

        # Now, remove all leading components that are the same
        my @pathchunks = self.splitdir( $path_directories );
        my @basechunks = self.splitdir( $base_directories );

        if $base_directories eq self.rootdir {
            @pathchunks.shift;
            return self.canonpath( self.catpath('', self.catdir( @pathchunks ), '') );
        }

        while @pathchunks && @basechunks && @pathchunks[0] eq @basechunks[0] {
            @pathchunks.shift;
            @basechunks.shift;
        }
        return self.curdir unless @pathchunks || @basechunks;

        # $base now contains the directories the resulting relative path
        # must ascend out of before it can descend to $path_directory.
        my $result_dirs = self.catdir( self.updir() xx @basechunks.elems, @pathchunks );
        return self.canonpath( self.catpath('', $result_dirs, '') );
    }

    method rel2abs(Str() \path, $base? is copy) {
        nqp::if(
          nqp::eqat(path, '/', 0),
          self.canonpath(path),
          self.catdir(
            self.canonpath(
                nqp::if(
                    $base.defined,
                    nqp::if(
                        nqp::eqat(($base = $base.Str), '/', 0),
                        $base,
                        nqp::if(
                            nqp::iseq_s($base, (my $cwd = $*CWD.Str)),
                            $base, self.rel2abs($base, $cwd),
                        ),
                    ),
                    $*CWD.Str,
                ),
            ),
            path,
          ),
        )
    }
}

# vim: ft=perl6 expandtab sw=4
