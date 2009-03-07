class Any is also {
    our List multi method grep($values: Code $test) {
	gather {
	    take $_ if $test($_) for $values.list;
	}
    }
}

our List multi grep(Code $test, *@values) {
    @values.grep($test)
}
