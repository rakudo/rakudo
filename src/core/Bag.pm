use v6;
# Dummy implementation of Bag.
# Not really done, or functional; just a place to start.
class Bag does Associative is Cool {
	has %!storage of Int handles <elems keys values kv>
        method !weight() {
               %!storage.kv.map: {$^k xx $^v}
        }
	sub bag(*@elems) {
		my %keys of Int;
		for @elems -> $key{
			%keys{$key}++; # Autovivification ftw.
		}
		Bag.new(:storage(%keys))
	}
	multi method pick($i = *) {
		@!weight.pick($i);
	}
        multi method roll($i = *) {
                @!weight.roll($i);
        }
	multi sub postcircumfix:<{ }>(Bag $bag: *@indices, Bool :$exists = 0) {
		$bag!storage.{@indices} :$exists;
	}
        multi method Hash() {
                %!storage
        }
}

