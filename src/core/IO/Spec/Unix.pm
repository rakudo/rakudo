my class IO::Spec::Unix is IO::Spec {

    method canonpath( $patharg, :$parent --> Str) {
        my $path = $patharg.Str;
        return '' if $path eq '';

        $path ~~ s:g { '//' '/'* }         = '/';     # xx////xx  -> xx/xx
        $path ~~ s:g { '/.'+ ['/' | $] }   = '/';     # xx/././xx -> xx/xx
        $path ~~ s { ^ './' <!before $> }  = '';      # ./xx      -> xx
        if $parent {
            Nil while $path ~~ s:g {  [^ | <?after '/'>] <!before '../'> <-[/]>+ '/..' ['/' | $ ] } = '';
            $path = '.' if $path eq '';
        }
        $path ~~ s { ^ '/..'+ ['/' | $] }  = '/';     # /../..(/xx) -> /(xx)
        unless $path eq "/" {
            $path ~~ s { '/' $ }       = '';      # xx/       -> xx    :)
        }
        $path
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

    method is-absolute( $file ) {
        substr( $file, 0, 1 ) eq '/';
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

    multi method split(IO::Spec::Unix: Cool:D $path is copy ) {
        $path  ~~ s/<?after .> '/'+ $ //;

        $path  ~~ m/^ ( [ .* \/ ]? ) (<-[\/]>*) /;
        my ($dirname, $basename) = ~$0, ~$1;

        $dirname ~~ s/<?after .> '/'+ $ //; #/

        if $basename eq '' {
            $basename = '/'  if $dirname eq '/';
        }
        else {
            $dirname = '.'  if $dirname eq '';
        }
        # shell dirname '' produces '.', but we don't because it's probably user error

        # temporary, for the transition period
        (:volume(''), :$dirname, :$basename, :directory($dirname));
#        (:volume(''), :$dirname, :$basename);
    }


    method join ($, $dirname, $file) {
        self.catpath(
          '',
          ($dirname eq '/' && $file eq '/' or $dirname eq '.' && $file.chars)
            ?? '' !! $dirname,
          $file,
        );
    }

    method catpath( $, $dirname, $file ) {
        $dirname ne ''
          && $file ne ''
          && substr($dirname, *-1 ) ne '/'
          && substr($file, 0, 1 )   ne '/'
          ?? $dirname ~ '/' ~ $file
          !! $dirname ~ $file
    }

    method catdir( *@parts ) { self.canonpath( (flat @parts, '').join('/') ) }
    method splitdir( $path ) { $path.split( '/' )  }
    method catfile( |c )     { self.catdir(|c) }

    method abs2rel( $path is copy, $base is copy = Str ) {
        $base = $*CWD unless $base.defined && $base.chars;

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
        if !$base_directories.chars && self.is-absolute( $base ) {
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

    method rel2abs( $path, $base? is copy) {
        return self.canonpath($path) if self.is-absolute($path);

        my $cwd := $*CWD;
        if !self.is-absolute( $base //= $cwd ) {
            $base = self.rel2abs( $base, $cwd ) unless $base eq $cwd;
        }
        self.catdir( self.canonpath($base), $path );
    }
}

# vim: ft=perl6 expandtab sw=4
