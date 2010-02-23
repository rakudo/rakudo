augment class Iterable {
    multi method Int() {
        $.elems.Int;
    }

    multi method Num() {
        $.elems;
    }

    multi method Seq() {
        $.iterator.Seq;
    }

    multi method Str() {
        pir::join(' ', $.iterator.eager);
    }

    multi method eager() {
        $.iterator.eager;
    }

    multi method elems() {
        $.iterator.elems;
    }
}
