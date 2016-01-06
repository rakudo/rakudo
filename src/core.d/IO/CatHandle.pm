class IO::CatHandle is IO::Handle {
    has IO::CatPath $.catpath;
    has int $!doing;
    has int $!bytes;
    has %!named;
    has $!handle;
    
    method BUILD($!catpath) { $!doing = -1; $!bytes = 0; self }
    multi method new(IO::CatHandle: +@todo) {
        nqp::create(self).BUILD(IO::CatPath.new(@todo))
    }
    multi method new(IO::CatHandle: IO::CatPath:D $catpath) {
        nqp::create(self).BUILD($catpath)
    }

    method !close() {
        if $!handle {
            $!bytes += $!handle.tell;
            my $result = $!handle.close;
            $!handle = Nil;
            $result
        }
        else {
            Nil
        }
    }

    method !next-handle() {
        self!close;
        if ++$!doing < $!catpath.todo {
            my $todo = $!catpath.todo[$!doing];
            $!handle = nqp::istype($todo,IO::Handle)
              ?? $todo
              !! nqp::istype($todo,Callable)
                ?? $todo()
                !! open($todo, |%!named)
        }
        else {
            Nil
        }
    }

    method !not-here($name) {
        fail "Method $name not supported on {self.^name}"
    }

    method open(IO::CatHandle:D: *%named) {
        fail if $!handle;
        fail unless $!catpath.todo;
        %!named = %named;
        self!next-handle;
        self
    }

    method close(IO::CatHandle:D:) {
        self!close;
    }

    method eof(IO::CatHandle:D: --> Bool) {
        !$!handle && $!doing >= 0
    }
    method tell(IO::CatHandle:D: --> Int) {
        $!handle ?? $!bytes + $!handle.tell !! Nil
    }
    method opened(IO::CatHandle:D: --> Bool) {
        ?$!handle
    }

    method get(IO::CatHandle:D:) {
        $!handle.get // do {
            while self!next-handle {
                .return with $!handle.get;
            }
            Nil
        }
    }

    method getc(IO::CatHandle:D:) {
        $!handle.getc // do {
            while self!next-handle {
                .return with $!handle.getc;
            }
            Nil
        }
    }

    method read(IO::CatHandle:D: Int(Cool:D) $bytes --> Buf) {
        if $!handle {
            my $toread;
            my buf8 $buf = $!handle.read($bytes);
            while ($toread = $bytes - $buf.elems) {
                last unless self!next-handle;
                $buf = $buf ~ $!handle.read($toread);
            }
            $buf
        }
        else {
            buf8.new
        }
    }

    method readchars(IO::CatHandle:D: Int(Cool:D) $chars = 65536 --> Str) {
        if $!handle {
            my $toread;
            my str $str = $!handle.readchars($chars);
            while ($toread = $chars - $str.chars) {
                last unless self!next-handle;
                $str = $str ~ $!handle.readchars($toread);
            }
            $str
        }
        else {
            ''
        }
    }

    multi method slurp-rest(IO::CatHandle:D: :$bin! --> Buf) {
        if $!handle {
            my $toread;
            my buf8 $buf = $!handle.slurp-rest(:bin);
            $buf = $buf ~ $!handle.slurp-rest(:bin) while self!next-handle;
            $buf
        }
        else {
            buf8.new
        }
    }
    multi method slurp-rest(IO::CatHandle:D: :$enc --> Str) {
        if $!handle {
            my @read;
            repeat while self!next-handle {
                @read.push($!handle.slurp-rest(:$enc));
            }
            @read.join
        }
        else {
            ''
        }
    }

    multi method Str(IO::CatHandle:D:) {
        $!catpath.todo>>.Str.join(" ")
    }
    multi method gist(IO::CatHandle:D:) {
        $!handle
          ?? "{self.Str}(opened, at octet {self.tell})"
          !! "{self.Str}(closed)"
    }
    multi method perl(IO::CatHandle:D:) {
        self.^name ~ ".new({$!catpath.todo.perl})"
    }

    # IO::Handle methods we don't know how to support yet
    method e(IO::CatHandle:D:) { self!not-here("e") }
    method d(IO::CatHandle:D:) { self!not-here("d") }
    method f(IO::CatHandle:D:) { self!not-here("f") }
    method s(IO::CatHandle:D:) { self!not-here("s") }
    method t(IO::CatHandle:D:) { self!not-here("t") }
    method l(IO::CatHandle:D:) { self!not-here("l") }
    method r(IO::CatHandle:D:) { self!not-here("r") }
    method w(IO::CatHandle:D:) { self!not-here("w") }
    method x(IO::CatHandle:D:) { self!not-here("x") }
    method modified(IO::CatHandle:D:) { self!not-here("modified") }
    method accessed(IO::CatHandle:D:) { self!not-here("accessed") }
    method changed(IO::CatHandle:D:)  { self!not-here("changed") }

    method encoding(IO::CatHandle:D: |)    { self!not-here("encoding") }
    method path(IO::CatHandle:D:)          { self!not-here("path") }
    method write(IO::CatHandle:D: |)       { self!not-here("write") }
    method print-nl(IO::CatHandle:D:)      { self!not-here("print-nl") }
    method chmod(IO::CatHandle:D: |)       { self!not-here("chmod") }
    method IO(IO::CatHandle:D: |)          { self!not-here("IO") }
    method flush(IO::Handle:D:)            { self!not-here("flush") }
    method seek(IO::CatHandle:D: |)        { self!not-here("seek") }
    multi method print(IO::CatHandle:D: |) { self!not-here("print") }
    multi method say(IO::CatHandle:D: |)   { self!not-here("say") }
#?if moar
    method watch(IO::CatHandle:D:)         { self!not-here("watch") }
#?endif
}

# vim: ft=perl6 expandtab sw=4
