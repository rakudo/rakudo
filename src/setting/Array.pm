# "is export" on Array does not work (it's Perl6Array internally)

class Array is also {
    multi method delete(@array is rw: *@indices) {
        my @result;
        for @indices -> $index {
            my $i = $index >= 0
                        ?? $index
                        !! * + $index;
            @result.push(do { my $workaround = @array[$i] });
            undefine @array[$i];
            if $index == (@array - 1) | -1 {
                @array.pop;
            }
        }
        @array.pop while @array && !defined @array[* - 1];
        return @result;
    }

    multi method splice(@array is rw: $offset is copy = 0, $size? is copy, *@values) {
        my @spliced;
        my @deleted;

        $offset += @array.elems if $offset < 0;
        $offset = @array.elems min floor($offset);
        $size = floor( $size // (@array - $offset) );
        $size += @array.end if $size < 0;

        @spliced.push(@array.shift) while (--$offset >= 0 && @array);
        @deleted.push(@array.shift) while (--$size >= 0 && @array);
        @spliced.push(@values) if @values;
        @spliced.push(@array) if @array;

        @array = @spliced;
        return @deleted;
    }
}

multi delete(@array is rw, *@indices) {
    @array.delete(|@indices);
}

multi splice(@array is rw, $offset?, $size?, *@values) {
    @array.splice($offset,$size,@values);
}

# vim: ft=perl6
