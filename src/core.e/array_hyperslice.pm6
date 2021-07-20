# Positinal postcircumfix with HyperWhatever goes here for 6.e

#| decent into LoL and return a shallow key per leaf
multi sub postcircumfix:<[ ]>( \SELF, HyperWhatever:D, :$k! ) is raw {
    sub recurse(\k, \v) {
        if v ~~ Positional {
            for v.kv -> \k, \v { recurse k, v }
        } else {
            take slip k
        }
    }

    gather for SELF.kv -> \k, \v {
        recurse k, v
    }
}

#| decent into LoL and return a shallow key and its sub tree or leaf
multi sub postcircumfix:<[ ]>( \SELF, HyperWhatever:D, :$tree! ) is raw {
    sub recurse(\k, \v){
        if v ~~ Positional {
            for v.kv -> \k, \v {
                take slip k, v;
                recurse k, v
            }
        }
    }

    gather recurse 0, SELF
}

#| decent into LoL and return a shallow key per leaf and the leaf
multi sub postcircumfix:<[ ]>( \SELF, HyperWhatever:D, :$kv! ) is raw {
    sub recurse(\k, \v) {
        if v ~~ Positional {
            for v.kv -> \k, \v { recurse k, v }
        } else {
            take slip k, v
        }
    }

    gather for SELF.kv -> \k, \v {
        recurse k, v
    }
}

#| decent into LoL and return a deep key per leaf
multi sub postcircumfix:<[ ]>( \SELF, HyperWhatever:D, :$deepk! ) is raw {
    sub recurse(\v, @keys){
        if v ~~ Positional {
            for v.kv -> \k, \v {
                recurse v, [@keys.Slip, slip k]
            }
        } else {
            take @keys
        }
    }

    gather recurse(SELF, [])
}

#| decent into LoL and return a deep key per leaf and the leaf
multi sub postcircumfix:<[ ]>( \SELF, HyperWhatever:D, :$deepkv! ) is raw {
    sub recurse(\v, @keys){
        if v ~~ Positional {
            for v.kv -> \k, \v {
                recurse v, [@keys.Slip, slip k]
            }
        }else{
            take slip(@keys, v)
        }
    }

    gather recurse(SELF, [])
}

# vim: expandtab shiftwidth=4
