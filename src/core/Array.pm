augment class Array {
    our Bool multi method exists(*@indices) {
        if !@indices.elems || (any(@indices) < 0 || any(@indices) > self.end) {
            return False;
        }
        [?&] map { self[$^a] !~~ Proxy }, @indices;
    }

    method at_pos($pos) {
        Q:PIR {
            .local pmc self, items
            .local int pos
            self = find_lex 'self'
            $P0  = find_lex '$pos'
            pos  = $P0
            $I0  = pos + 1
            items = self.'!fill'($I0)
            %r   = items[pos]
            unless null %r goto done
            %r = new ['Proxy']
            setattribute %r, '$!base', items
            $P0 = box pos
            setattribute %r, '$!key', $P0
            $P0 = get_hll_global ['Bool'], 'True'
            setprop %r, 'scalar', $P0
            setprop %r, 'rw', $P0
          done:
        }
    }

    multi method perl() {
        '[' ~ self.map({ $^a.perl }).join(', ') ~ ']';
    }

    our multi method splice($offset is copy = 0, $size? is copy, *@values) is export {
        $offset += self.elems if ($offset < 0);
        $size //= self.elems - $offset;
        $size = self.elems + $size - $offset if ($size < 0);
        self!splice(@values.Seq.eager, $offset, $size);
    }

    # This should probably handle lazy arrays too.
    multi method delete(@array is rw: *@indices) {
        self!fill;
        my @result;
        for @indices -> $index {
            my $i = $index >= 0
                        ?? $index
                        !! +@array + $index;
            @result.push(@array[$i]);
            undefine @array[$i];

            # next seems unnecessary but handles an obscure
            # edge case
            if $i == (@array - 1) {
                @array.pop;
            }
        }
        @array.pop while ?@array && !defined @array[@array.elems - 1];
        return @result;
    }
}

proto sub pop(@array) { @array.pop; }
proto sub shift(@array) { @array.shift; }
proto sub unshift(@array, *@values) { @array.unshift(@values); }
