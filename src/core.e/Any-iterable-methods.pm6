augment class Any {
    proto method snip(|) {*}
    multi method snip(Any:D: \condition) {
        Seq.new: Rakudo::Iterator.Snip(condition, self.iterator)
    }
    multi method snip(Any:D: @conditions) {
        Seq.new: Rakudo::Iterator.Snip(@conditions, self.iterator)
    }
    multi method snip(Any:D: *@conditions) {
        Seq.new: Rakudo::Iterator.Snip(@conditions, self.iterator)
    }

    multi method skip(Iterable:D $skips) {
        Seq.new: Rakudo::Iterator.Skipper: self.iterator, $skips.iterator
    }
    multi method skip(*@skips) {
        self.skip(@skips)
    }

    method snitch() { note self; self }
}

proto sub rotor(|) {*}
multi sub rotor(Int:D $batch, \thing, *%_) {
    thing.rotor($batch, |%_)
}
# We have to emulate :(*@ [@list, \tail]) because the grammar wont cut it.
multi sub rotor(**@cycle-and-thing, *%_) {
    @cycle-and-thing.tail.rotor(@cycle-and-thing.head(*-1), |%_)
}

proto sub snip($, |) {*}
multi sub snip(\condition,  +values) { values.snip(condition)  }
multi sub snip(@conditions, +values) { values.snip(@conditions) }

# vim: expandtab shiftwidth=4
