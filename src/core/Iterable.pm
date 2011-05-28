augment class Iterable {
    # N.B.: methods defined in src/builtins/Iterable.pir
    #     .item

    multi method Numeric() { $.elems.Int }

    multi method Int() { $.elems.Int }

    multi method Num() { $.elems.Num }

    multi method Str() { $.list.Str }

    multi method elems() { $.list.elems }

    multi method fmt($format = '%s', $separator = ' ') { $.list.fmt($format, $separator); }

    multi method list() { $.iterator.list }

}
