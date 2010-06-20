class Rakudo::Guts {
    sub add_handles_method_helper(Mu $metaclass, $attr, $meth_name, $meth_rename = $meth_name) {
        $metaclass.add_method($metaclass, $meth_name, (method (|$c) {
            pir::getattribute__PPS(self, $attr)."$meth_rename"(|$c); 
        }).clone()); 
    }

    method add_handles_method(Mu $metaclass, $attr_name, $expr) {
        given $expr {
            when Str { add_handles_method_helper($metaclass, $attr_name, $expr); }
            when Parcel { 
                for $expr.list -> $x {
                    self.add_handles_method($metaclass, $attr_name, $x);
                }
            }
            when Pair { 
                add_handles_method_helper($metaclass, $attr_name, $expr.key, $expr.value);
            }
            default {
                # None of the cases we can generate methods for matched, so
                # instead we install it as a fallback.
                $metaclass.add_handles_fallback($metaclass, $attr_name, $expr);
            }
        }
    }
}
