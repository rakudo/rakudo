our sub add_handles_method_helper($metaclass, $attr, $meth-name, $meth-rename = $meth-name) {
    $metaclass.add_method($metaclass, $meth-name, (method (|$c) {
        pir::getattribute__PPS(self, $attr)."$meth-rename"(|$c); 
    }).clone() ); 
}

our sub add_handles_method($metaclass, $attr_name, $expr) {
    for ($expr) -> $x {
        given $x {
           when Str { add_handles_method_helper($metaclass, $attr_name, $x); }
           when Parcel { 
               for $x.list -> $x { add_handles_method_helper($metaclass, $attr_name, $x); }
           }
           when Pair { 
               add_handles_method_helper($metaclass, $attr_name, $x.key, $x.value);
           }
           default { die sprintf("add_handles_method can't handle %s in list", $x.WHAT); }
        }
    }
}
