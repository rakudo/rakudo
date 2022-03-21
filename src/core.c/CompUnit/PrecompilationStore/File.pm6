class CompUnit::PrecompilationStore::File
  does CompUnit::PrecompilationStore
{
    has IO::Path:D $.prefix is built(:bind) is required;

    has IO::Handle $!lock;
#?if moar
    has atomicint $!lock-count;
#?endif
#?if !moar
    has int $!lock-count;
#?endif
    has $!loaded;
    has $!dir-cache;
    has $!compiler-cache;
    has Lock $!update-lock;

    submethod TWEAK(--> Nil) {
        $!update-lock := Lock.new;
        $!loaded         := nqp::hash;
        $!dir-cache      := nqp::hash;
        $!compiler-cache := nqp::hash;
    }

    method new-unit(|c) {
        CompUnit::PrecompilationUnit::File.new(|c, :store(self))
    }

    method !dir(
      CompUnit::PrecompilationId:D $compiler-id,
      CompUnit::PrecompilationId:D $precomp-id
    ) {
        $!update-lock.protect: {
            my str $compiler = $compiler-id.Str;
            my str $precomp  = $precomp-id.Str;
            nqp::ifnull(
              nqp::atkey($!dir-cache,nqp::concat($compiler,$precomp)),
              nqp::bindkey($!dir-cache,nqp::concat($compiler,$precomp),
                nqp::ifnull(
                  nqp::atkey($!compiler-cache,$compiler),
                  nqp::bindkey($!compiler-cache,$compiler,
                    self.prefix.add($compiler)
                  )
                ).add(nqp::substr($precomp,0,2))
              )
            )
        }
    }

    method path(
      CompUnit::PrecompilationId:D $compiler-id,
      CompUnit::PrecompilationId:D $precomp-id,
      Str:D :$extension = ''
    ) {
        self!dir($compiler-id, $precomp-id).add($precomp-id ~ $extension)
    }

    method !lock($path --> Nil) {
        $!update-lock.lock;
        $!lock := "$path.lock".IO.open(:create, :rw)
          unless $!lock;
#?if moar
        $!lock-count⚛++;
        $!lock.lock if ⚛$!lock-count == 1;
#?endif
#?if !moar
        $!lock.lock if $!lock-count++ == 0;
#?endif
    }

    method unlock() {
        LEAVE $!update-lock.unlock;
#?if moar
        die "unlock when we're not locked!" if ⚛$!lock-count == 0;

        $!lock-count⚛-- if ⚛$!lock-count > 0;
        if $!lock && ⚛$!lock-count == 0 {
#?endif
#?if !moar
        die "unlock when we're not locked!" if $!lock-count == 0;

        $!lock-count-- if $!lock-count > 0;
        if $!lock && $!lock-count == 0 {
#?endif
            $!lock.unlock;
            $!lock.close;
            $!lock := IO::Handle;
        }
        True
    }

    method load-unit(
      CompUnit::PrecompilationId:D $compiler-id,
      CompUnit::PrecompilationId:D $precomp-id
    ) {
        $!update-lock.protect: {
            my str $key = $precomp-id.Str;
            nqp::ifnull(
              nqp::atkey($!loaded,$key),
              do {
                  my $path := self.path($compiler-id, $precomp-id);
                  $path.s
                    ?? nqp::bindkey($!loaded,$key,
                         CompUnit::PrecompilationUnit::File.new(
                           :id($precomp-id), :$path, :store(self)))
                    !! Nil
              }
            )
        }
    }

    method load-repo-id(
      CompUnit::PrecompilationId:D $compiler-id,
      CompUnit::PrecompilationId:D $precomp-id
    ) {
        my $path := self.path($compiler-id, $precomp-id, :extension<.repo-id>);
        $path.s
          ?? $path.slurp
          !! Nil
    }

    method remove-from-cache(CompUnit::PrecompilationId:D $precomp-id) {
        $!update-lock.protect: {
            nqp::deletekey($!loaded,$precomp-id.Str);
        }
    }

    method destination(
      CompUnit::PrecompilationId:D $compiler-id,
      CompUnit::PrecompilationId:D $precomp-id,
      Str:D :$extension = ''
    --> IO::Path:D) {

        # have a writable prefix, assume it's a directory
        if $!prefix.w {
            self!lock(self!file($compiler-id, $precomp-id));
            self!file($compiler-id, $precomp-id, :$extension);
        }

        # directory creation successful and writeable
        elsif $!prefix.mkdir && $!prefix.w {

            # make sure we have a tag in it
            $!prefix.child('CACHEDIR.TAG').spurt:
'Signature: 8a477f597d28d172789f06886806bc55
# This file is a cache directory tag created by Rakudo.
# For information about cache directory tags, see:
# http://www.brynosaurus.com/cachedir';

            # call ourselves again, now that we haz a cache directory
            self.destination($compiler-id, $precomp-id, :$extension)
        }

        # huh?
        else {
            Nil
        }
    }

    method !file(
      CompUnit::PrecompilationId:D $compiler-id,
      CompUnit::PrecompilationId:D $precomp-id,
      Str:D :$extension = ''
    --> IO::Path:D) {
        my $compiler-dir := self.prefix.add($compiler-id);
        $compiler-dir.mkdir unless $compiler-dir.e;

        my $dest := self!dir($compiler-id, $precomp-id);
        $dest.mkdir unless $dest.e;

        $dest.add($precomp-id ~ $extension)
    }

    # File renaming can easily race and fail on Windows. There's no great solution,
    # so instead just try 10 times catching a failure (and returning out of the
    # loop and sub if it succeeds).
    my sub try-rename-n-times(&rename-block, $n is copy --> Bool:D) {
        while $n-- {
            &rename-block();
            CATCH {
                when X::IO::Rename {
                    sleep 0.1;
                    next;
                }
            }
            return True;
        }
        return False;
    }

    method store-file(
      CompUnit::PrecompilationId:D $compiler-id,
      CompUnit::PrecompilationId:D $precomp-id,
      IO::Path:D $path,
      Str:D :$extension = ''
    ) {
        my &rename-block = { $path.rename(self!file($compiler-id, $precomp-id, :$extension)); };
        if Rakudo::Internals.IS-WIN {
            # If the rename attempts don't succeed, we'll end up
            # trying again one more time but not catching any failures.
            return if try-rename-n-times(&rename-block, 10);
        }
        &rename-block();
    }

    method store-unit(
      CompUnit::PrecompilationId:D $compiler-id,
      CompUnit::PrecompilationId:D $precomp-id,
      CompUnit::PrecompilationUnit:D $unit
    ) {
        my $extension := self!tmp-extension;
        my $precomp-file := self!file($compiler-id, $precomp-id, :$extension);
        $unit.save-to($precomp-file);
        my &rename-block = {
            $precomp-file.rename(self!file($compiler-id, $precomp-id));
            self.remove-from-cache($precomp-id);
        };
        if Rakudo::Internals.IS-WIN {
            # If the rename attempts don't succeed, we'll end up
            # trying again one more time but not catching any failures.
            return if try-rename-n-times(&rename-block, 10);
        }
        &rename-block();
    }

    method store-repo-id(
      CompUnit::PrecompilationId:D $compiler-id,
      CompUnit::PrecompilationId:D $precomp-id,
      Str:D :$repo-id!
    ) {
        my $extension := ".repo-id" ~ self!tmp-extension;
        my $repo-id-file := self!file($compiler-id, $precomp-id, :$extension);
        $repo-id-file.spurt($repo-id);
        my &rename-block = { $repo-id-file.rename(self!file($compiler-id, $precomp-id, :extension<.repo-id>)); };
        if Rakudo::Internals.IS-WIN {
            # If the rename attempts don't succeed, we'll end up
            # trying again one more time but not catching any failures.
            return if try-rename-n-times(&rename-block, 10);
        }
        &rename-block();
    }

    method delete(
      CompUnit::PrecompilationId:D $compiler-id,
      CompUnit::PrecompilationId:D $precomp-id,
      Str:D :$extension = ''
    ) {
        self.path($compiler-id, $precomp-id, :$extension).unlink;
    }

    method delete-by-compiler(CompUnit::PrecompilationId:D $compiler-id) {
         my $compiler-dir := self.prefix.add($compiler-id);
         for $compiler-dir.dir -> $subdir {
             .unlink for $subdir.dir;
             $subdir.rmdir;
         }
         $compiler-dir.rmdir;
    }

    method !tmp-extension(--> Str:D) {
        '.' ~ (^2**128).pick.base(36) ~ '.tmp'
    }
}

# vim: expandtab shiftwidth=4
