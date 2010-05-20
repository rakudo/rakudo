class Cursor is Regex::Cursor {
    method !process_pastnode_results_for_interpolation( $node, $subtype ) {
        my @result;
        my $iterable = $node ~~ Iterable ?? $node !! [ $node ];
        @result = $iterable.map: {
            my $item = $_;
            if $subtype eq 'interp_regex' && $item !~~ Regex {
                .subst(rx/\//, '\\/', :g);
                $item = "rx/ $item /";
                $item = eval $item;
            }
            $item;
        };
        return @result;
    }
}
