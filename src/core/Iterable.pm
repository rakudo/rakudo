augment class Iterable {

    multi method Int() { $.elems.Int }

    multi method Num() { $.elems.Num }

    multi method Str() { $.list.Str }

    multi method elems() { $.list.elems }

    multi method list() { $.iterator.list }

}
