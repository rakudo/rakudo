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

    our sub str2num-int($src) {
        Q:PIR {
            .local pmc src
            .local string src_s
            src = find_lex '$src'
            src_s = src
            .local int pos, eos
            .local num result
            pos = 0
            eos = length src_s
            result = 0
          str_loop:
            unless pos < eos goto str_done
            .local string char
            char = substr src_s, pos, 1
            if char == '_' goto str_next
            .local int digitval
            digitval = index "0123456789", char
            if digitval < 0 goto err_base
            if digitval >= 10 goto err_base
            result *= 10
            result += digitval
          str_next:
            inc pos
            goto str_loop
          err_base:
        src.'panic'('Invalid radix conversion of "', char, '"')
          str_done:
            %r = box result
        };
    }

    our sub str2num-base($src) {
        Q:PIR {
            .local pmc src
            .local string src_s
            src = find_lex '$src'
            src_s = src
            .local int pos, eos
            .local num result
            pos = 0
            eos = length src_s
            result = 1
          str_loop:
            unless pos < eos goto str_done
            .local string char
            char = substr src_s, pos, 1
            if char == '_' goto str_next
            result *= 10
          str_next:
            inc pos
            goto str_loop
          err_base:
        src.'panic'('Invalid radix conversion of "', char, '"')
          str_done:
            %r = box result
        };
    }

    our sub str2num-rat($negate, $int-part, $frac-part is copy) is export {
        $frac-part.=subst(/(\d)0+$/, { ~$_[0] });
        my $result = upgrade_to_num_if_needed(str2num-int($int-part))
                     + upgrade_to_num_if_needed(str2num-int($frac-part))
                       / upgrade_to_num_if_needed(str2num-base($frac-part));
        $result = -$result if $negate;
        $result;
    }

    our sub str2num-num($negate, $int-part, $frac-part, $exp-part-negate, $exp-part) is export {
        my $exp = str2num-int($exp-part);
        $exp = -$exp if $exp-part-negate;
        my $result = (str2num-int($int-part) + str2num-int($frac-part) / str2num-base($frac-part))
                     * 10 ** $exp;
        $result = -$result if $negate;
        $result;
    }
}
