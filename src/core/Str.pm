augment class Str does Stringy {
    multi method Bool { ?(pir::istrue__IP(self)); }

    method Str() { self }

    # CHEAT: this implementation is a bit of a cheat,
    # but works fine for now.
    multi method Int { (+self).Int; }
    multi method Num { (+self).Num; }

    method d() {
        self.e ?? ?pir::stat__ISI(self, 2) !! Bool;
    }

    method f() {
        self.e ?? !pir::stat__ISI(self, 2) !! Bool;
    }

    method s() {
        self.e ?? pir::stat__ISI(self, 1) !! Any;
    }

    # XXX: We have no $?ENC or $?NF compile-time constants yet.
    multi method encode($encoding = 'UTF-8', $nf = '') {
        my @bytes = Q:PIR {
            .local int byte
            .local pmc bytebuffer, it, result
            $P0 = find_lex 'self'
            $S0 = $P0
            bytebuffer = new ['ByteBuffer']
            bytebuffer = $S0

            result = new ['Parcel']
            it = iter bytebuffer
          bytes_loop:
            unless it goto done
            byte = shift it
            push result, byte
            goto bytes_loop
          done:
            %r = result
        };
        return Buf.new(@bytes);
    }
}
