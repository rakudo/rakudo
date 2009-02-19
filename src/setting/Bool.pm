class Bool is also {

=begin item ACCEPTS

=end item
    method ACCEPTS($topic) {
        return self;
    }

=begin item perl

=end item
    method perl() {
        return self ?? 'Bool::True' !! 'Bool::False';
    }

=begin item pick

Returns True or False

=end item
    method pick() {
        return rand < 0.5 ?? Bool::True !! Bool::False;
    }

}

# vim: ft=perl6
