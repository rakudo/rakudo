role Buf[::T = Int] does Stringy {
    has @.contents;

    multi method new(@contents) {
        self.bless(*, :contents(@contents.list));
    }

    multi method new(*@contents) {
        self.bless(*, :contents(@contents.list));
    }

    multi method decode($encoding = 'UTF-8') {
        my @contents = @.contents;
        my $str = ~Q:PIR {
            $P0 = find_lex '@contents'

            .local pmc iterator
            iterator = iter $P0
            .local pmc sb
            sb = new 'StringBuilder'
            sb = unicode:""
            loop:
              unless iterator goto done
              $P1 = shift iterator
              $I1 = $P1
              $S1 = chr $I1
              sb .= $S1
              goto loop
            done:
              %r = sb
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
