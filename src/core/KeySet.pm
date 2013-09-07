my class KeySet does Setty {

    submethod BUILD (:%elems)  {
        nqp::bindattr(self, KeySet, '%!elems', %elems);
    }

    method Set { Set.new(self.keys) }
    method KeySet { self }
}
