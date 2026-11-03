# assoc postcircumfix with HyperWhatever goes here for 6.e

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, *%other ) is raw {
    sub recurse(\k, \v) {
        if v ~~ Associative {
            for v.kv -> \k, \v { recurse k, v }
        } else {
            take slip v
        }
    }

    gather for SELF.kv -> \k, \v {
        recurse k, v
    }
}

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$v!, *%other ) is raw {
    sub recurse(\k, \v) {
        if v ~~ Associative {
            for v.kv -> \k, \v { recurse k, v }
        } else {
            take slip v
        }
    }

    gather for SELF.kv -> \k, \v {
        recurse k, v
    }
}

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$kv!, *%other ) is raw {
    sub recurse(\k, \v) {
        if v ~~ Associative {
            for v.kv -> \k, \v { recurse k, v }
        } else {
            take slip k, v
        }
    }

    gather for SELF.kv -> \k, \v {
        recurse k, v
    }
}

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$k!, *%other ) is raw {
    sub recurse(\k, \v) {
        if v ~~ Associative {
            for v.kv -> \k, \v { recurse k, v }
        } else {
            take slip k
        }
    }

    gather for SELF.kv -> \k, \v {
        recurse k, v
    }
}

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$tree!, *%other ) is raw {
    sub recurse(\k, \v) {
        if v ~~ Associative {
            take slip k, v;
            for v.kv -> \k, \v { recurse k, v }
        } else {
            take slip k, v
        }
    }

    gather for SELF.kv -> \k, \v {
        recurse k, v
    }
}

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$deepk!, *%other ) is raw {
    sub recurse(\v, @keys){
        if v ~~ Associative {
            for v.kv -> \k, \v {
                recurse v, [@keys.Slip, slip k]
            }
        }else{
            take @keys
        }
    }

    gather for SELF.kv -> \k, \v {
        if v ~~ Associative {
            recurse(v, [k])
        } else {
            take [k]
        }
    };
}

multi sub postcircumfix:<{ }>( \SELF, HyperWhatever, :$deepkv!, *%other ) is raw {
    sub recurse(\v, @keys){
        if v ~~ Associative {
            for v.kv -> \k, \v {
                recurse v, [@keys.Slip, slip k]
            }
        }else{
            take slip(@keys, v)
        }
    }

    gather for SELF.kv -> \k, \v {
        if v ~~ Associative {
            recurse(v, [k])
        } else {
            take slip([k], v)
        }
    };
}

# vim: expandtab shiftwidth=4
