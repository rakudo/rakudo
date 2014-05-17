my class IO::Spec { ... }

my class IO::Spec::Unix {

    method canonpath( $path is copy, :$parent --> Str) {
        return '' if $path eq '';

        $path ~~ s:g { '//' '/'* }         = '/';     # xx////xx  -> xx/xx  
        $path ~~ s:g { '/.'+ ['/' | $] }   = '/';     # xx/././xx -> xx/xx  
        $path ~~ s { ^ './' <!before $> }  = '';      # ./xx      -> xx
        if $parent {
            while $path ~~ s:g {  [^ | <?after '/'>] <!before '../'> <-[/]>+ '/..' ['/' | $ ] } = '' { };
            $path = '.' if $path eq '';
        }
        $path ~~ s { ^ '/..'+ ['/' | $] }  = '/';     # /../..(/xx) -> /(xx)
        unless $path eq "/" {
            $path ~~ s { '/' $ }       = '';      # xx/       -> xx    :)
        }
        $path
    }

    method curdir {  '.' }
    method updir  { '..' }
    method rootdir { '/' }
    method devnull { '/dev/null' }

    method tmpdir {
        self.canonpath: first( { .defined && .IO.d && .IO.w },
                %*ENV<TMPDIR>,
                '/tmp') 
            || self.curdir;
    }

    method no-parent-or-current-test { none('.', '..')  }

    method is-absolute( $file ) {
        so $file.match(/^\//)
    }

    method path {
        return () unless %*ENV{'PATH'};
        my @path = %*ENV{'PATH'}.split( ':' );
        for @path {
            $_ = '.' if $_ eq ''
        }
        return @path
    }

    method splitpath( $path, :$nofile = False ) {
        my ( $directory, $file ) = ( '', '' );

        if $nofile {
            $directory = $path;
        }
        else {
            $path      ~~ m/^ ( [ .* \/ [ '.'**1..2 $ ]? ]? ) (<-[\/]>*) /; 
            $directory = ~$0;
            $file      = ~$1;
        }

        return ( '', $directory, $file );
    }

    method split (Cool:D $path is copy ) {
        $path  ~~ s/<?after .> '/'+ $ //;

        $path  ~~ m/^ ( [ .* \/ ]? ) (<-[\/]>*) /;
        my ($directory, $basename) = ~$0, ~$1;

        $directory ~~ s/<?after .> '/'+ $ //; #/

        $basename = '/'   if $directory eq '/' && $basename eq '';
        $directory = '.'  if $directory eq ''  && $basename ne '';
        # shell dirname '' produces '.', but we don't because it's probably user error

        return (:volume(''), :$directory, :$basename );
    }


    method join ($volume, $directory is copy, $file) {
        $directory = '' if all($directory, $file) eq '/'
                or $directory eq '.' && $file.chars;
        self.catpath($volume, $directory, $file);
    }

    method catpath( $volume, $directory is copy, $file ) {
        if $directory               ne ''
        && $file                    ne ''
        && $directory.substr( *-1 ) ne '/'
        && $file.substr( 0, 1 )     ne '/' {
            $directory ~= "/$file"
        }
        else {
            $directory ~= $file
        }

        return $directory
    }

    method catdir( *@parts ) { self.canonpath( (@parts, '').join('/') ) }
    method splitdir( $path ) { $path.split( /\// )  }
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

    method rel2abs( $path, $base is copy = $*CWD) {
        return self.canonpath($path) if self.is-absolute($path);
        if !self.is-absolute( $base ) {
            $base = self.rel2abs( $base, $*CWD ) unless $base eq $*CWD;
        }
        self.catdir( self.canonpath($base), $path );
    }
}

# vim: ft=perl6 expandtab sw=4
