proto sub rotor(|) {*}
multi sub rotor(Int:D $batch, \thing, *%_) {
    thing.rotor($batch, |%_)
}
# We have to emulate :(*@ [@list, \tail]) because the grammar wont cut it.
multi sub rotor(**@cycle-and-thing, *%_) {
    @cycle-and-thing.tail.rotor(@cycle-and-thing.head(*-1), |%_)
}

multi sub hyper(Iterable:D \thing, *%_) {
    thing.hyper(|%_)
}

multi sub race(Iterable:D \thing, *%_) {
    thing.race(|%_)
}
