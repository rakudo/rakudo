role Buf[::T = Int] does Stringy {
    has T @.contents;

    multi method new(@contents) {
        self.bless(*, :contents(@contents.list));
    }

    multi method decode($encoding = 'UTF-8') {
        my @contents = @.contents;
        my $str = ~Q:PIR {
            $P0 = find_lex '@contents'

            .local pmc bb
            .local string s
            bb = new ['ByteBuffer']
            .local pmc it
            .local int i
            it = iter $P0
            i = 0
          loop:
            unless it goto done
            $P1 = shift it
            $I1 = $P1
            bb[i] = $I1
            inc i
            goto loop
          done:
            s = bb.'get_string_as'(utf8:unicode:"")
            %r = box s
        };
        return $str;
    }

    multi method elems() {
        @.contents.elems;
    }
}

our multi sub infix:<eqv>(Buf $a, Buf $b) {
    return $a.contents ~~ $b.contents;
}
