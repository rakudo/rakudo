augment class Any {
    proto method span(|) {*}
    multi method span(Any:D: &condition) {
        Seq.new: Rakudo::Iterator.Span(&condition, self.iterator)
    }
    multi method span(Any:D: @conditions) {
        Seq.new: Rakudo::Iterator.Span(@conditions, self.iterator)
    }
    multi method span(Any:D: *@conditions) {
        Seq.new: Rakudo::Iterator.Span(@conditions, self.iterator)
    }
}

proto sub rotor(|) {*}
multi sub rotor(Int:D $batch, \thing, *%_) {
    thing.rotor($batch, |%_)
}
# We have to emulate :(*@ [@list, \tail]) because the grammar wont cut it.
multi sub rotor(**@cycle-and-thing, *%_) {
    @cycle-and-thing.tail.rotor(@cycle-and-thing.head(*-1), |%_)
}

proto sub span($, |) {*}
multi sub span(&condition,  +values) { values.span(&condition)  }
multi sub span(@conditions, +values) { values.span(@conditions) }

# vim: expandtab shiftwidth=4
