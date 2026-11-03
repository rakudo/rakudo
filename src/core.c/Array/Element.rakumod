my class Array::Element is implementation-detail {
    method access(\SELF, \pos, %adverbs, $adverb, $value) {
        my $lookup := Rakudo::Internals.ADVERBS_AND_NAMED_TO_DISPATCH_INDEX(
          %adverbs, $adverb, $value
        );
        nqp::if(
          nqp::istype($lookup,X::Adverb),
          nqp::stmts(
            ($lookup.what   = "element access"),
            ($lookup.source = try { SELF.VAR.name } // SELF.^name),
            $lookup.Failure
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
            $lookup.Failure
          ),
          Rakudo::Internals.ACCESS-ELEMENT-ANY-DISPATCH-CLASS(
            $lookup
          ).element(SELF,pos)
        )
    }
}

# Classes that take an Int position
my class Array::Element::Access::none is implementation-detail {
    method element(\SELF,\pos) { SELF.AT-POS(pos) }
}
my class Array::Element::Access::kv is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? (pos,SELF.AT-POS(pos)) !! ()
    }
}
my class Array::Element::Access::not-kv is implementation-detail {
    method element(\SELF,\pos) { (pos,SELF.AT-POS(pos)) }
}
my class Array::Element::Access::p is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? Pair.new(pos,SELF.AT-POS(pos)) !! ()
    }
}
my class Array::Element::Access::not-p is implementation-detail {
    method element(\SELF,\pos) { Pair.new(pos,SELF.AT-POS(pos)) }
}
my class Array::Element::Access::k is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? pos !! ()
    }
}
my class Array::Element::Access::not-k is implementation-detail {
    method element(\SELF,\pos) { pos }
}
my class Array::Element::Access::v is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? nqp::decont(SELF.AT-POS(pos)) !! ()
    }
}
my class Array::Element::Access::exists is implementation-detail {
    method element(\SELF,\pos) { SELF.EXISTS-POS(pos) }
}
my class Array::Element::Access::exists-kv is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? (pos,True) !! ()
    }
}
my class Array::Element::Access::exists-not-kv is implementation-detail {
    method element(\SELF,\pos) { (pos,SELF.EXISTS-POS(pos)) }
}
my class Array::Element::Access::exists-p is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? Pair.new(pos,True) !! ()
    }
}
my class Array::Element::Access::exists-not-p is implementation-detail {
    method element(\SELF,\pos) { Pair.new(pos,SELF.EXISTS-POS(pos)) }
}
my class Array::Element::Access::exists-delete is implementation-detail {
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
my class Array::Element::Access::exists-delete-kv is implementation-detail {
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
my class Array::Element::Access::exists-delete-not-kv is implementation-detail {
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
my class Array::Element::Access::exists-delete-p is implementation-detail {
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
my class Array::Element::Access::exists-delete-not-p is implementation-detail {
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
my class Array::Element::Access::not-exists is implementation-detail {
    method element(\SELF,\pos) { !SELF.EXISTS-POS(pos) }
}
my class Array::Element::Access::not-exists-kv is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? (pos,False) !! ()
    }
}
my class Array::Element::Access::not-exists-not-kv is implementation-detail {
    method element(\SELF,\pos) { (pos,!SELF.EXISTS-POS(pos)) }
}
my class Array::Element::Access::not-exists-p is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? Pair.new(pos,False) !! ()
    }
}
my class Array::Element::Access::not-exists-not-p is implementation-detail {
    method element(\SELF,\pos) { Pair.new(pos,!SELF.EXISTS-POS(pos)) }
}
my class Array::Element::Access::not-exists-delete is implementation-detail {
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
my class Array::Element::Access::not-exists-delete-kv is implementation-detail {
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
my class Array::Element::Access::not-exists-delete-not-kv is implementation-detail {
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
my class Array::Element::Access::not-exists-delete-p is implementation-detail {
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
my class Array::Element::Access::not-exists-delete-not-p is implementation-detail {
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
my class Array::Element::Access::delete is implementation-detail {
    method element(\SELF,\pos) { SELF.DELETE-POS(pos) }
}
my class Array::Element::Access::delete-kv is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? (pos,SELF.DELETE-POS(pos)) !! ()
    }
}
my class Array::Element::Access::delete-not-kv is implementation-detail {
    method element(\SELF,\pos) { (pos,SELF.DELETE-POS(pos)) }
}
my class Array::Element::Access::delete-p is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? Pair.new(pos,SELF.DELETE-POS(pos)) !! ()
    }
}
my class Array::Element::Access::delete-not-p is implementation-detail {
    method element(\SELF,\pos) { Pair.new(pos,SELF.DELETE-POS(pos)) }
}
my class Array::Element::Access::delete-k is implementation-detail {
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
my class Array::Element::Access::delete-not-k is implementation-detail {
    method element(\SELF,\pos) {
        SELF.DELETE-POS(pos) if SELF.EXISTS-POS(pos);
        pos
    }
}
my class Array::Element::Access::delete-v is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos) ?? SELF.DELETE-POS(pos) !! ()
    }
}

# Classes that take an Any position
my class Array::Element::Access::none-any is implementation-detail {
    method element(\SELF,\pos) { SELF.AT-POS(pos.Int) }
}
my class Array::Element::Access::kv-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? (pos,SELF.AT-POS(pos.Int)) !! ()
    }
}
my class Array::Element::Access::not-kv-any is implementation-detail {
    method element(\SELF,\pos) { (pos,SELF.AT-POS(pos.Int)) }
}
my class Array::Element::Access::p-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos,SELF.AT-POS(pos.Int)) !! ()
    }
}
my class Array::Element::Access::not-p-any is implementation-detail {
    method element(\SELF,\pos) { Pair.new(pos,SELF.AT-POS(pos.Int)) }
}
my class Array::Element::Access::k-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? pos !! ()
    }
}
my class Array::Element::Access::not-k-any is implementation-detail {
    method element(\SELF,\pos) { pos }
}
my class Array::Element::Access::v-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? nqp::decont(SELF.AT-POS(pos.Int)) !! ()
    }
}
my class Array::Element::Access::exists-any is implementation-detail {
    method element(\SELF,\pos) { SELF.EXISTS-POS(pos.Int) }
}
my class Array::Element::Access::exists-kv-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? (pos,True) !! ()
    }
}
my class Array::Element::Access::exists-not-kv-any is implementation-detail {
    method element(\SELF,\pos) { (pos,SELF.EXISTS-POS(pos.Int)) }
}
my class Array::Element::Access::exists-p-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos,True) !! ()
    }
}
my class Array::Element::Access::exists-not-p-any is implementation-detail {
    method element(\SELF,\pos) { Pair.new(pos,SELF.EXISTS-POS(pos.Int)) }
}
my class Array::Element::Access::exists-delete-any is implementation-detail {
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
my class Array::Element::Access::exists-delete-kv-any is implementation-detail {
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
my class Array::Element::Access::exists-delete-not-kv-any is implementation-detail {
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
my class Array::Element::Access::exists-delete-p-any is implementation-detail {
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
my class Array::Element::Access::exists-delete-not-p-any is implementation-detail {
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
my class Array::Element::Access::not-exists-any is implementation-detail {
    method element(\SELF,\pos) { !SELF.EXISTS-POS(pos.Int) }
}
my class Array::Element::Access::not-exists-kv-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? (pos,False) !! ()
    }
}
my class Array::Element::Access::not-exists-not-kv-any is implementation-detail {
    method element(\SELF,\pos) { (pos,!SELF.EXISTS-POS(pos.Int)) }
}
my class Array::Element::Access::not-exists-p-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos,False) !! ()
    }
}
my class Array::Element::Access::not-exists-not-p-any is implementation-detail {
    method element(\SELF,\pos) { Pair.new(pos,!SELF.EXISTS-POS(pos.Int)) }
}
my class Array::Element::Access::not-exists-delete-any is implementation-detail {
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
my class Array::Element::Access::not-exists-delete-kv-any is implementation-detail {
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
my class Array::Element::Access::not-exists-delete-not-kv-any is implementation-detail {
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
my class Array::Element::Access::not-exists-delete-p-any is implementation-detail {
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
my class Array::Element::Access::not-exists-delete-not-p-any is implementation-detail {
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
my class Array::Element::Access::delete-any is implementation-detail {
    method element(\SELF,\pos) { SELF.DELETE-POS(pos.Int) }
}
my class Array::Element::Access::delete-kv-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? (pos,SELF.DELETE-POS(pos.Int)) !! ()
    }
}
my class Array::Element::Access::delete-not-kv-any is implementation-detail {
    method element(\SELF,\pos) { (pos,SELF.DELETE-POS(pos.Int)) }
}
my class Array::Element::Access::delete-p-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos,SELF.DELETE-POS(pos.Int)) !! ()
    }
}
my class Array::Element::Access::delete-not-p-any is implementation-detail {
    method element(\SELF,\pos) { Pair.new(pos,SELF.DELETE-POS(pos.Int)) }
}
my class Array::Element::Access::delete-k-any is implementation-detail {
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
my class Array::Element::Access::delete-not-k-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.DELETE-POS(pos.Int) if SELF.EXISTS-POS(pos.Int);
        pos
    }
}
my class Array::Element::Access::delete-v-any is implementation-detail {
    method element(\SELF,\pos) {
        SELF.EXISTS-POS(pos.Int) ?? SELF.DELETE-POS(pos.Int) !! ()
    }
}

# vim: expandtab shiftwidth=4
