augment class Iterable {
    multi method Seq() {
        self.iterator.Seq;
    }
}
