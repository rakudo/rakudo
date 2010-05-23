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
            .local int bin_coding, i, max, byte
            .local string bin_string
            .local pmc it, result
            $P0 = find_lex 'self'
            $S0 = $P0
            bin_coding = find_encoding 'fixed_8'
            bin_string = trans_encoding $S0, bin_coding
            result = new ['Parcel']
            i = 0
            max = length bin_string
          bytes_loop:
            if i >= max goto bytes_done
            byte = ord bin_string, i
            push result, byte
            inc i
            goto bytes_loop
          bytes_done:
            %r = result
        };
        return Buf.new(@bytes);
    }
}
