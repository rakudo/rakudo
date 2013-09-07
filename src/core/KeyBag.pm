my class KeyBag does Baggy {

    submethod BUILD (:%elems)  {
        nqp::bindattr(self, KeyBag, '%!elems', %elems);
    }

    method Bag { Bag.new-fp(nqp::getattr(self, KeyBag, '%!elems').values) }
    method KeyBag { self }
}
