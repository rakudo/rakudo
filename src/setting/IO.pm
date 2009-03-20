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

# vim: ft=perl6
