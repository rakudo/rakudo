augment class Array {
    method perl() {
        # XXX: $_.perl and .perl don't work, but this does...
        '[' ~ self.map({ $^a.perl }).join(', ') ~ ']';
    }

    our Bool multi method exists(Int *@indices) {
        if !@indices.elems || (any(@indices) < 0 || any(@indices) > self.end) {
            return False;
        }
        [?&] map { self.values[$^a] !~~ Proxy }, @indices;
    }

    our multi method postcircumfix:<[ ]> (Int $i) {
        if $i < 0 { die "Cannot use negative index on arrays" }
        #XXX: .exists calls postcircumfix<[ ]>, so can't perl6ify this for now...
        return Q:PIR{
            .local pmc self, i, values
            self = find_lex 'self'
            i = find_lex '$i'
            $I0 = i
            inc $I0
            values = self.'!fill'($I0)
            %r = values[i]
            unless null %r goto have_elem
            %r = new ['Proxy']
            setattribute %r, '$!base', values
            setattribute %r, '$!key', i
          have_elem:
            $P0 = get_hll_global ['Bool'], 'True'
            setprop %r, 'rw', $P0
        }
    }

    our multi method postcircumfix:<[ ]>(Block $b) {
        return self.[$b.(self.elems)]
    }

    our multi method postcircumfix:<[ ]>(Whatever) {
        return self.values
    }

    our method push(*@values) is export {
        self!fill;
        pir::splice__0PPii( @!items, [@values].iterator.eager,
                            pir::elements(@!items), 0);
        self;
    }

    our method unshift(*@values) is export {
        # Reify self, then splice @values at the beginning.
        # We really should make this lazy, however.
        pir::splice__0PPii( self!fill, [@values].iterator.eager, 0, 0);
        self;
    }

    our multi method pop() {
        self!fill ?? pir::pop(@!items)
                  !! fail('Undefined value popped from empty array');
    }

    our multi method shift() {
        self!fill(1) ?? pir::shift(@!items)
                     !! fail('Undefined value shifted from empty array');
    }

    our multi method splice($offset is copy = 0, $size? is copy, *@values) is export {
        self!fill;
        $offset += self.elems if ($offset < 0);
        $size //= self.elems - $offset;
        $size = self.elems + $size - $offset if ($size < 0);
        my @ret = self[$offset..^($offset+$size)];
        pir::splice__0PPii(@!items, [@values].iterator.eager,
                           $offset, $size);
        @ret;
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
