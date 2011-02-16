use v6;
# Dummy implementation of Bag.
# Not really done, or functional; just a place to start.
class Bag does Associative {
	has %!storage of Int;
	sub bag(*@elems) {
		my %keys of Int;
		for @elems -> $key{
			%keys{$key}++; # Autovivification ftw.
		}
		Bag.new(:storage(%keys))
	}
	multi method pick($i = Whatever) {
		%!storage.kv.map: {$^a xx $^b;}.pick($i);
	}
	multi sub postcircumfix:<{ }>(Bag $bag: *@indices) {
		$bag!storage.{@indices};
	}
}

