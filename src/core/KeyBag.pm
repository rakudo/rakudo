my class KeyBag does Baggy {

    method Bag { Bag.new-fp(nqp::getattr(self, KeyBag, '%!elems').values) }
    method KeyBag { self }
}
