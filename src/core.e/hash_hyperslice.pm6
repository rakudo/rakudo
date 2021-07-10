# assoc postcircumfix with HyperWhatever goes here for 6.e

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$kv!, *%other ) is raw {
    gather for SELF.kv -> $key, $value {
        $value ~~ Associative
        ?? $value.kv.map(&?BLOCK)
        !! take slip $key, $value;
    }
}

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$k!, *%other ) is raw {
    gather for SELF.kv -> $key, $value {
        $value ~~ Associative
        ?? $value.kv.map(&?BLOCK)
        !! take $key;
    }
}

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$v!, *%other ) is raw {
    gather for SELF.kv -> $key, $value {
        $value ~~ Associative
        ?? $value.kv.map(&?BLOCK)
        !! take $value;
    }
}

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$tree!, *%other ) is raw {
    gather for SELF.kv -> $key, $value {
        $value ~~ Associative
        ?? (take slip $key, $value; $value.kv.map(&?BLOCK))
        !! take slip $key, $value;
    }
}

# This requires v6.e .
multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$semi!, *%other ) is raw {
    multi sub recurse(%hash) {
        for %hash.kv -> $key, $value {
            recurse $key, $value;
        }
    }
    multi sub recurse($key is copy, Associative $value) {
        for $value.kv -> $child-key, $child-value {
            recurse slip($key, $child-key), $child-value
        }
    }
    
    multi sub recurse($key, $value) {
        take $key, $value
    }

    gather recurse(SELF);
}

# vim: expandtab shiftwidth=4
