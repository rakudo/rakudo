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

    method dir-sep  {  '/' } # NOTE: IO::Path.resolve assumes dir sep is 1 char
    method curdir   {  '.' }
    method updir    { '..' }
    method rootdir  { '/' }
    method devnull  { '/dev/null' }

    my $curupdir := -> str $dir {
        nqp::hllbool(nqp::isne_s($dir,'.') && nqp::isne_s($dir,'..'))
    }
    method curupdir { $curupdir }

    method basename(str \path) {
        my int $index = nqp::rindex(path,'/');
        nqp::iseq_i($index,-1)
          ?? path
          !! nqp::substr(path,$index + 1)
    }

    method extension(str \path) {
        my int $index = nqp::rindex(path,'.');
        nqp::iseq_i($index,-1)
          ?? ''
          !! nqp::substr(path,$index + 1)
    }

    method tmpdir {
        for %*ENV<TMPDIR>, '/tmp' {
            if .defined {
                my $io := IO::Path.new($_);
                return $io if $io.d && $io.rwx
            }
        }

        IO::Path.new(".")
    }

    method is-absolute( Str() \path ) {
        nqp::hllbool(nqp::iseq_i(nqp::ord(path), 47)) # '/'
    }

    method path {
        my $parts  := nqp::split(':',%*ENV<PATH>);
        my $buffer := nqp::create(IterationBuffer);
        nqp::while(
          nqp::elems($parts),
          nqp::push($buffer,nqp::shift($parts) || ".")
        );
        $buffer.Seq
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

    method split(IO::Spec::Unix: Cool:D $path) {
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
       IO::Path::Parts.new('', $dirname, $basename)
    }

    method join ($, \dir, \file) {
        nqp::if(
             (nqp::iseq_s(dir, '/') && nqp::iseq_s(file, '/'))
          || (nqp::iseq_s(dir, '.') && file),
          file,
          nqp::concat(dir,
            nqp::if(
              dir && file
                && nqp::isfalse(
                    nqp::eqat(dir, '/', nqp::sub_i(nqp::chars(dir), 1)))
                && nqp::isne_i(nqp::ord(file), 47), # '/'
              nqp::concat('/', file),
              file)))
    }

    method catpath( $, \dirname, \file ) {
        nqp::concat(dirname,
          nqp::if(
            dirname && file
              && nqp::isfalse(
                  nqp::eqat(dirname, '/',
                    nqp::sub_i(nqp::chars(dirname), 1)))
              && nqp::isne_i(nqp::ord(file), 47), # '/'
            nqp::concat('/', file),
            file))
    }

    method catdir (*@parts) {
        self.canonpath: nqp::concat(
            @parts.join('/'),
            nqp::if(@parts, '/', ''),
        )
    }
    method splitdir(Cool:D $path) {
        nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', nqp::split('/', $path.Str))
        || ('',)
    }
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
        self.canonpath:
          nqp::if(
            nqp::iseq_i(nqp::ord(path), 47), # .starts-with: '/'
            path,
            nqp::concat(
              nqp::if(
                nqp::defined($base),
                nqp::if(
                  nqp::iseq_i(nqp::ord(($base = $base.Str)), 47), # /^ '/'/
                  $base,
                  nqp::if(
                    nqp::iseq_s($base, (my $cwd := $*CWD.Str)),
                    $base, self.rel2abs($base, $cwd))),
                $*CWD.Str),
                nqp::concat('/', path)))
    }
}

# vim: expandtab shiftwidth=4
