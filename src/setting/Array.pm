class Array is also {
    multi method splice(@array is rw: Int $offset = 0, Int $size = @array.elems - $offset, *@values) is export {
        my @spliced;
        my @deleted;

        my $off = ($offset > @array.end) ?? @array.end !! $offset;
        my $len = $size;
        @spliced.push(@array.shift) while ($off-- > 0 && @array);
        @deleted.push(@array.shift) while ($len-- > 0 && @array);
        @spliced.push(@values) if @values;
        @spliced.push(@array) if @array;

        @array = @spliced;
        return @deleted;
    }
}
