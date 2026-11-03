my class CompUnit::PrecompilationUnit::File does CompUnit::PrecompilationUnit {
    has CompUnit::PrecompilationId:D $.id   is built(:bind) is required;
    has IO::Path                     $.path is built(:bind);
    has Str $!checksum        is built;
    has Str $!source-checksum is built;
    has CompUnit::PrecompilationDependency @!dependencies is built(:bind);
    has $!bytecode            is built(:bind);
    has $!store               is built(:bind);

    has Bool $!initialized;
    has IO::Handle $!handle;
    has $!update-lock;

    submethod TWEAK(--> Nil) {
        if $!bytecode {
            $!checksum = nqp::sha1($!bytecode.decode('iso-8859-1'));
            $!initialized := True;
        }
        else {
            $!initialized := False;
        }
        $!update-lock := Lock.new;
    }

    method modified(--> Instant:D) {
        $!path.modified
    }

    method !read-dependencies(--> Nil) {
        $!initialized || $!update-lock.protect: {
            unless $!initialized {  # another thread beat us
                $!handle := $!path.open(:r) unless $!handle;

                $!checksum        = $!handle.get;
                $!source-checksum = $!handle.get;
                my $dependency   := $!handle.get;
                my $dependencies := nqp::create(IterationBuffer);
                # last entry is either an empty line, or has padding to make
                # the file start at a multiple-of-8 byte.
                # Anything that is not padding is necessarily longer than that.
                while $dependency.chars > 8 {
                    nqp::push(
                      $dependencies,
                      CompUnit::PrecompilationDependency::File.deserialize($dependency)
                    );
                    $dependency := $!handle.get;
                }
                nqp::bindattr(@!dependencies,List,'$!reified',$dependencies);
                $!initialized := True;
            }
        }
    }

    method dependencies(--> Array[CompUnit::PrecompilationDependency]) {
        self!read-dependencies;
        @!dependencies
    }

    method bytecode(--> Buf:D) {
        $!update-lock.protect: {
            unless $!bytecode {
                self!read-dependencies;
                $!bytecode := $!handle.slurp(:bin,:close)
            }
            $!bytecode
        }
    }

    method bytecode-handle(--> IO::Handle:D) {
        self!read-dependencies;
        $!handle
    }

    method source-checksum() is rw {
        self!read-dependencies;
        $!source-checksum
    }

    method checksum() is rw {
        self!read-dependencies;
        $!checksum
    }

    method Str(--> Str:D) {
        self.path.Str
    }

    method close(--> Nil) {
        $!update-lock.protect: {
            $!handle.close if $!handle;
            $!handle      := IO::Handle;
            $!initialized := False;
        }
    }

    method save-to(IO::Path $precomp-file) {
        my $handle := $precomp-file.open(:w);
        $handle.print($!checksum ~ "\n");
        $handle.print($!source-checksum ~ "\n");
        $handle.print($_.serialize ~ "\n") for @!dependencies;
        my $pos_mod_8 = ($handle.tell + 1) % 8;
        if $pos_mod_8 {
            $handle.print("_" x (8 - $pos_mod_8));
        }
        $handle.print("\n");
        $handle.write($!bytecode);
        $handle.close;
        $!path := $precomp-file;
    }

    method is-up-to-date(
      CompUnit::PrecompilationDependency:D $dependency,
      Bool :$check-source
    --> Bool:D) {
        my $result := self.CompUnit::PrecompilationUnit::is-up-to-date($dependency, :$check-source);
        $!store.remove-from-cache($.id) unless $result;
        $result
    }
}

# vim: expandtab shiftwidth=4
