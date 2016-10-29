my role IO::Spec::Any {

    method canonpath { ... }

    method dir-sep  { ... }
    method curdir   { '.'  }
    method updir    { '..' }
    method curupdir { none('.','..') }
    method rootdir  { ... }
    method devnull  { ... }

    method basename { ... }

    method extension(\path) {
        my str $str = nqp::unbox_s(path);
        my int $index = nqp::rindex($str,'.');
        nqp::p6bool($index == -1)
          ?? ''
          !! substr(path,nqp::box_i($index + 1,Int) );
    }

    method is-absolute { ... }
    method path { ... }
    method splitpath { ... }
    method splitdir { ... }
    method split { ... }
    method join { ... }
    method catpath { ... }
    method catfile { ... }

    method abs2rel($path is copy, $base is copy = Str ) {
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

    method rel2abs { ... }
}


