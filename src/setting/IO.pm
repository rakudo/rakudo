class IO is also {

    multi method close() is export {
        try {
            ?$!PIO.close()
        }
        $! ?? fail($!) !! Bool::True
    }

    multi method print(*@items) is export {
        try {
            $!PIO.print($_) for @items;
        }
        $! ?? fail($!) !! Bool::True
    }

    multi method say(*@items) is export {
        my $print_res = self.print(|@items);
        if $print_res {
            try {
                $!PIO.print("\n");
            }
            return $! ?? fail($!) !! Bool::True
        }
        else {
            return $print_res;
        }
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
