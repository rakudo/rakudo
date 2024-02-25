# This file contains additions to the CORE:: namespace for language level 6.e.
# This could be either as additional multi sub candidates, or new subs / terms
# altogether.

# introducing rotor-like capabilities to comb
multi sub comb(Pair:D $rotor, Cool:D $input, *%_) { $input.comb($rotor, |%_) }

# introducing nano as an alternetive to "time"
sub term:<nano>() { nqp::time }

# allow next/last to produce a value
multi sub next(\x --> Nil) { THROW(nqp::const::CONTROL_NEXT, x) }
multi sub last(\x --> Nil) { THROW(nqp::const::CONTROL_LAST, x) }

# introducing //foo as syntax for foo.defined
proto sub prefix:<//>($) is pure {*}
multi sub prefix:<//>(\a) { a.defined }

# Can be REMOVED **AFTER** the Raku grammar has become the default grammar
BEGIN &prefix:<//>.set_op_props;

# introducing rotor as a sub
proto sub rotor(|) {*}
multi sub rotor(Int:D $batch, \thing, *%_) {
    thing.rotor($batch, |%_)
}
# We have to emulate :(*@ [@list, \tail]) because the grammar wont cut it.
multi sub rotor(**@cycle-and-thing, *%_) {
    @cycle-and-thing.tail.rotor(@cycle-and-thing.head(*-1), |%_)
}

# introducing snip as a sub
proto sub snip($, |) {*}
multi sub snip(\condition,  +values) { values.snip(condition)  }
multi sub snip(@conditions, +values) { values.snip(@conditions) }

# introducing snitch as a sub
proto sub snitch($, |) {*}
multi sub snitch(Seq:D \SNITCHEE) is raw { note SNITCHEE.cache; SNITCHEE }
multi sub snitch(      \SNITCHEE) is raw { note SNITCHEE;       SNITCHEE }

multi sub snitch(&snitcher, Seq:D \SNITCHEE) is raw {
    snitcher SNITCHEE.cache;
    SNITCHEE
}
multi sub snitch(&snitcher, \SNITCHEE) is raw {
    snitcher SNITCHEE;
    SNITCHEE
}

# Introducing trans as a sub for use with feed operatos. Making this a multi is
# useless, as this candidate would always match
sub trans(**@a, *%_) {
    @a.tail.elems > 1
        ?? @a.tail.map(*.trans(|@a.head(*-1).list, |%_))
        !! @a.tail.trans(|@a.head(*-1).list, |%_)
}

# vim: expandtab shiftwidth=4
