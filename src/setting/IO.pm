class IO is also {

    multi method close() is export {
        try {
            ?$!PIO.close()
        }
        $! ?? fail($!) !! Bool::True
    }

    multi method eof() is export {
        return ?$!PIO.eof();
    }

    multi method lines() is export {
        my @result = ();
        while !$.eof {
            push @result, $!PIO.readline().chomp()
        }
        return @result;
    }

    multi method print(*@items) is export {
        try {
            for @items -> $item {
                $!PIO.print($item);
            }
        }
        return $! ?? fail($!) !! Bool::True;
    }

    multi method printf($format, *@args) {
        return self.print(sprintf($format, |@args));
    }

    multi method say(*@items) is export {
        @items.push("\n");
        return self.print(|@items);
    }

    multi method slurp() is export {
        return $!PIO.readall();
    }

}

multi sub lines(IO $filehandle,
                :$bin = False,
                :$enc = 'Unicode',
                :$nl = "\n",
                :$chomp = True) {

    fail 'Binary mode not supported yet'    if $bin;
    fail 'Encodings not supported yet'      if $enc ne 'Unicode';
    fail 'Fancy newlines not supported yet' if $nl ne "\n";
    fail 'Lack of chomp not supported yet'  if !$chomp;

    return $filehandle.lines();
}

# vim: ft=perl6
