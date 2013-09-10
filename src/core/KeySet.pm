my class KeySet does Setty {

    method Set (:$view) {
        if $view {
            my $set := nqp::create(Set);
            $set.BUILD( :elems(nqp::getattr(self, KeySet, '%!elems')) );
            $set;
        }
        else {
            Set.new(self.keys);
        }
    }

    method KeySet { KeySet.new(self.keys) }
}
