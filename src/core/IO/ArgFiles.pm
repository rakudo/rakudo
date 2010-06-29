class IO::ArgFiles is IO {
    has $!filenames;
    has $!filename;
    has $!current_file;
    has $!ins;

    method new($filenames) {
        if $filenames.elems {
            self.bless(*, :filenames($filenames));
        } else {
            push $filenames, "-";
            self.bless(*, :filenames($filenames));
        }
    }

    method eof() {
        $.next_file;
        $!current_file.eof && $!filenames.elems == 0;
    }

    method getc() {
        ...
    }

    method get() {
        $.next_file;
        $!ins++;
        $!current_file.get;
    }

    method lines() {
        gather while !$.eof {
            my $line = $.get;
            take $line if defined $line;
        }
    }

    method filename() {
        $!filename;
    }

    method ins() {
        $!ins;
    }

    method next_file() {
        if (!defined $!current_file) || ($!current_file.eof) {
            $!current_file.close if $!current_file && $!filename ne '-';
            fail if $!filenames.elems == 0;
            $!filename = $!filenames.shift;
            $!current_file = $!filename eq '-' ?? $*IN !! open($!filename);
        }
    }
}

sub ARGFILES_CREATE() {
    IO::ArgFiles.new(@*ARGS);
}

# vim: ft=perl6
