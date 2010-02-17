augment class Array {
   method perl() {
        # XXX: $_.perl and .perl don't work, but this does...
        '[' ~ self.map({ $^a.perl }).join(', ') ~ ']';
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

    our multi method splice($offset = 0, $size? is copy, *@values) is export {
        self!fill;
        $size //= self.elems - $offset;
        my @ret = self[$offset..^($offset+$size)];
        pir::splice__0PPii(@!items, [@values].iterator.eager,
                           $offset, $size);
        @ret;
    }
}

our proto sub pop(@array) { @array.pop; }
our proto sub shift(@array) { @array.shift; }
