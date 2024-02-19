# This file contains additions to the CORE:: namespace for language level 6.e.
# This could be either as additional multi sub candidates, or new subs / terms
# altogether.

# Deprecate multi-path operations and introduce single path candidate
proto sub chmod($, |) {*}
multi sub chmod($mode, $path) { $path.IO.chmod($mode) }
multi sub chmod($mode, *@filenames) {
    DEPRECATED('@paths.grep(*.IO.chmod)', :what("multi-path operation"));
    my @ok;
    for @filenames -> $file { @ok.push($file) if $file.IO.chmod($mode) }
    @ok;
}

# Deprecate multi-path operations and introduce single path candidate
proto sub chown($, |) {*}
multi sub chown($path, :$uid, :$gid) { $path.IO.chown(:$uid, :$gid) }
multi sub chown(*@filenames, :$uid, :$gid) {
    DEPRECATED('@paths.grep(*.IO.chown)', :what("multi-path operation"));
    @filenames.grep: *.IO.chown(:$uid, :$gid)
}

# Deprecate multi-path operations and introduce single path candidate
proto sub unlink(|) {*}
multi sub unlink() { "unlink()".no-zero-arg }
multi sub unlink($path) { $path.IO.unlink }
multi sub unlink(*@filenames) {
    DEPRECATED('@paths.grep(*.IO.unlink)', :what("multi-path operation"));
    my @ok;
    for @filenames -> $file { @ok.push($file) if $file.IO.unlink }
    @ok;
}

# Deprecate multi-path operations and introduce single path candidate
proto sub rmdir(|) {*}
multi sub rmdir() { "rmdir()".no-zero-arg }
multi sub rmdir($path) { $path.IO.rmdir }
multi sub rmdir(*@filenames) {
    DEPRECATED('@paths.grep(*.IO.rmdir)', :what("multi-path operation"));
    my @ok;
    for @filenames -> $file { @ok.push($file) if $file.IO.rmdir }
    @ok;
}

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

# vim: expandtab shiftwidth=4
