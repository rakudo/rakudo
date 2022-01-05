my class Array::Element {
    method access(\SELF, \pos, %adverbs, $adverb, $value) {
        my $lookup := Rakudo::Internals.ADVERBS_AND_NAMED_TO_DISPATCH_INDEX(
          %adverbs, $adverb, $value
        );
        nqp::if(
          nqp::istype($lookup,X::Adverb),
          nqp::stmts(
            ($lookup.what   = "element access"),
            ($lookup.source = try { SELF.VAR.name } // SELF.^name),
            Failure.new($lookup)
          ),
          Rakudo::Internals.ACCESS-ELEMENT-DISPATCH-CLASS(
            $lookup
          ).element(SELF,pos)
        )
    }
    method access-any(\SELF, \pos, %adverbs, $adverb, $value) {
        my $lookup := Rakudo::Internals.ADVERBS_AND_NAMED_TO_DISPATCH_INDEX(
          %adverbs, $adverb, $value
        );
        nqp::if(
          nqp::istype($lookup,X::Adverb),
          nqp::stmts(
            ($lookup.what   = "element access"),
            ($lookup.source = try { SELF.VAR.name } // SELF.^name),
            Failure.new($lookup)
          ),
          Rakudo::Internals.ACCESS-ELEMENT-ANY-DISPATCH-CLASS(
            $lookup
          ).element(SELF,pos)
        )
    }
}

# Classes that take an Int position
my class Array::Element::Access::none {
    method element(\SELF,\pos) { SELF.AT-POS(pos) }
}
my class Array::Element::Access::kv {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? (pos,SELF.AT-POS(pos)) !! ()
    }
}
my class Array::Element::Access::not-kv {
    method element(\SELF,\pos) { (pos,SELF.AT-POS(pos)) }
}
my class Array::Element::Access::p {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? Pair.new(pos,SELF.AT-POS(pos)) !! ()
    }
}
my class Array::Element::Access::not-p {
    method element(\SELF,\pos) { Pair.new(pos,SELF.AT-POS(pos)) }
}
my class Array::Element::Access::k {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? pos !! ()
    }
}
my class Array::Element::Access::not-k {
    method element(\SELF,\pos) { pos }
}
my class Array::Element::Access::v {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? nqp::decont(SELF.AT-POS(pos)) !! ()
    }
}
my class Array::Element::Access::exists {
    method element(\SELF,\pos) { SELF.EXISTS-POS(pos) }
}
my class Array::Element::Access::exists-kv {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? (pos,True) !! ()
    }
}
my class Array::Element::Access::exists-not-kv {
    method element(\SELF,\pos) { (pos,SELF.EXISTS-POS(pos)) }
}
my class Array::Element::Access::exists-p {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? Pair.new(pos,True) !! ()
    }
}
my class Array::Element::Access::exists-not-p {
    method element(\SELF,\pos) { Pair.new(pos,SELF.EXISTS-POS(pos)) }
}
my class Array::Element::Access::exists-delete {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            True
        }
        else {
            False
        }
    }
}
my class Array::Element::Access::exists-delete-kv {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            (pos,True)
        }
        else {
            ()
        }
    }
}
my class Array::Element::Access::exists-delete-not-kv {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            (pos,True)
        }
        else {
            (pos,False)
        }
    }
}
my class Array::Element::Access::exists-delete-p {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            Pair.new(pos,True)
        }
        else {
            ()
        }
    }
}
my class Array::Element::Access::exists-delete-not-p {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            Pair.new(pos,True)
        }
        else {
            Pair.new(pos,False)
        }
    }
}
my class Array::Element::Access::not-exists {
    method element(\SELF,\pos) { !SELF.EXISTS-POS(pos) }
}
my class Array::Element::Access::not-exists-kv {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? (pos,False) !! ()
    }
}
my class Array::Element::Access::not-exists-not-kv {
    method element(\SELF,\pos) { (pos,!SELF.EXISTS-POS(pos)) }
}
my class Array::Element::Access::not-exists-p {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? Pair.new(pos,False) !! ()
    }
}
my class Array::Element::Access::not-exists-not-p {
    method element(\SELF,\pos) { Pair.new(pos,!SELF.EXISTS-POS(pos)) }
}
my class Array::Element::Access::not-exists-delete {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            False
        }
        else {
            True
        }
    }
}
my class Array::Element::Access::not-exists-delete-kv {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            (pos,False)
        }
        else {
            ()
        }
    }
}
my class Array::Element::Access::not-exists-delete-not-kv {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            (pos,False)
        }
        else {
            (pos,True)
        }
    }
}
my class Array::Element::Access::not-exists-delete-p {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            Pair.new(pos,False)
        }
        else {
            ()
        }
    }
}
my class Array::Element::Access::not-exists-delete-not-p {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            Pair.new(pos,False)
        }
        else {
            Pair.new(pos,True)
        }
    }
}
my class Array::Element::Access::delete {
    method element(\SELF,\pos) { SELF.DELETE-POS(pos) }
}
my class Array::Element::Access::delete-kv {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? (pos,SELF.DELETE-POS(pos)) !! ()
    }
}
my class Array::Element::Access::delete-not-kv {
    method element(\SELF,\pos) { (pos,SELF.DELETE-POS(pos)) }
}
my class Array::Element::Access::delete-p {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? Pair.new(pos,SELF.DELETE-POS(pos)) !! ()
    }
}
my class Array::Element::Access::delete-not-p {
    method element(\SELF,\pos) { Pair.new(pos,SELF.DELETE-POS(pos)) }
}
my class Array::Element::Access::delete-k {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos) {
            SELF.DELETE-POS(pos);
            pos
        }
        else {
            ()
        }
    }
}
my class Array::Element::Access::delete-not-k {
    method element(\SELF,\pos) {
        SELF.DELETE-POS(pos) if SELF.EXISTS-POS(pos);
        pos
    }
}
my class Array::Element::Access::delete-v {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? SELF.DELETE-POS(pos) !! ()
    }
}

# Classes that take an Any position
my class Array::Element::Access::none-any {
    method element(\SELF,\pos) { SELF.AT-POS(pos.Int) }
}
my class Array::Element::Access::kv-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? (pos,SELF.AT-POS(pos.Int)) !! ()
    }
}
my class Array::Element::Access::not-kv-any {
    method element(\SELF,\pos) { (pos,SELF.AT-POS(pos.Int)) }
}
my class Array::Element::Access::p-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos,SELF.AT-POS(pos.Int)) !! ()
    }
}
my class Array::Element::Access::not-p-any {
    method element(\SELF,\pos) { Pair.new(pos,SELF.AT-POS(pos.Int)) }
}
my class Array::Element::Access::k-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? pos !! ()
    }
}
my class Array::Element::Access::not-k-any {
    method element(\SELF,\pos) { pos }
}
my class Array::Element::Access::v-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? nqp::decont(SELF.AT-POS(pos.Int)) !! ()
    }
}
my class Array::Element::Access::exists-any {
    method element(\SELF,\pos) { SELF.EXISTS-POS(pos.Int) }
}
my class Array::Element::Access::exists-kv-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? (pos,True) !! ()
    }
}
my class Array::Element::Access::exists-not-kv-any {
    method element(\SELF,\pos) { (pos,SELF.EXISTS-POS(pos.Int)) }
}
my class Array::Element::Access::exists-p-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos,True) !! ()
    }
}
my class Array::Element::Access::exists-not-p-any {
    method element(\SELF,\pos) { Pair.new(pos,SELF.EXISTS-POS(pos.Int)) }
}
my class Array::Element::Access::exists-delete-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            True
        }
        else {
            False
        }
    }
}
my class Array::Element::Access::exists-delete-kv-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            (pos,True)
        }
        else {
            ()
        }
    }
}
my class Array::Element::Access::exists-delete-not-kv-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            (pos,True)
        }
        else {
            (pos,False)
        }
    }
}
my class Array::Element::Access::exists-delete-p-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            Pair.new(pos,True)
        }
        else {
            ()
        }
    }
}
my class Array::Element::Access::exists-delete-not-p-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            Pair.new(pos,True)
        }
        else {
            Pair.new(pos,False)
        }
    }
}
my class Array::Element::Access::not-exists-any {
    method element(\SELF,\pos) { !SELF.EXISTS-POS(pos.Int) }
}
my class Array::Element::Access::not-exists-kv-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? (pos,False) !! ()
    }
}
my class Array::Element::Access::not-exists-not-kv-any {
    method element(\SELF,\pos) { (pos,!SELF.EXISTS-POS(pos.Int)) }
}
my class Array::Element::Access::not-exists-p-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos,False) !! ()
    }
}
my class Array::Element::Access::not-exists-not-p-any {
    method element(\SELF,\pos) { Pair.new(pos,!SELF.EXISTS-POS(pos.Int)) }
}
my class Array::Element::Access::not-exists-delete-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            False
        }
        else {
            True
        }
    }
}
my class Array::Element::Access::not-exists-delete-kv-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            (pos,False)
        }
        else {
            ()
        }
    }
}
my class Array::Element::Access::not-exists-delete-not-kv-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            (pos,False)
        }
        else {
            (pos,True)
        }
    }
}
my class Array::Element::Access::not-exists-delete-p-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            Pair.new(pos,False)
        }
        else {
            ()
        }
    }
}
my class Array::Element::Access::not-exists-delete-not-p-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            Pair.new(pos,False)
        }
        else {
            Pair.new(pos,True)
        }
    }
}
my class Array::Element::Access::delete-any {
    method element(\SELF,\pos) { SELF.DELETE-POS(pos.Int) }
}
my class Array::Element::Access::delete-kv-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? (pos,SELF.DELETE-POS(pos.Int)) !! ()
    }
}
my class Array::Element::Access::delete-not-kv-any {
    method element(\SELF,\pos) { (pos,SELF.DELETE-POS(pos.Int)) }
}
my class Array::Element::Access::delete-p-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos,SELF.DELETE-POS(pos.Int)) !! ()
    }
}
my class Array::Element::Access::delete-not-p-any {
    method element(\SELF,\pos) { Pair.new(pos,SELF.DELETE-POS(pos.Int)) }
}
my class Array::Element::Access::delete-k-any {
    method element(\SELF,\pos) {
        if SELF.EXISTS-POS(pos.Int) {
            SELF.DELETE-POS(pos.Int);
            pos
        }
        else {
            ()
        }
    }
}
my class Array::Element::Access::delete-not-k-any {
    method element(\SELF,\pos) {
        SELF.DELETE-POS(pos.Int) if SELF.EXISTS-POS(pos.Int);
        pos
    }
}
my class Array::Element::Access::delete-v-any {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? SELF.DELETE-POS(pos.Int) !! ()
    }
}

# vim: expandtab shiftwidth=4
