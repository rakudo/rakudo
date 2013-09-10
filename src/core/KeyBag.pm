my class KeyBag does Baggy {

    method Bag (:$view) {
        if $view {
            my $bag := nqp::create(Bag);
            $bag.BUILD( :elems(nqp::getattr(self, KeyBag, '%!elems')) );
            $bag;
        }
        else {
            Bag.new-fp(nqp::getattr(self, KeyBag, '%!elems').values);
        }
    }           
    method KeyBag (:$clone) { $clone ?? KeyBag.new-fp(self.pairs) !! self }
}
