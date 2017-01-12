# This class contains generally usable methods creating Iterators.
# There are two reasons for having this in a separate class:
#
# 1. Nice to have a separate file for similar stuff.  Rakudo::Internals
#    has become a hodgepodge of stuff of late.
# 2. Improve readability/searchability of code using these iterators, as
#    many already have a long name, and having them prefixed with the more
#    general Rakudo::Internals in the code, as opposed for the definite
#    Rakudo::Iterator, feels better.

class Rakudo::Iterator {

#-------------------------------------------------------------------------------
# Roles that are used by iterators in the rest of the core settings, in
# alphabetical order for easier perusal.

    # Generic role for iterating over a Blob / Buf.  You need to
    # supply at least a .pull-one.  Takes a Blob / Buf as the only
    # parameter to .new.
    our role Blobby does Iterator {
        has $!blob;
        has Int $!i;   # sadly, this can not be a native int yet  :-(

        method SET-SELF(\blob) {
            nqp::if(
              nqp::isgt_i(nqp::elems(blob),0),
              nqp::stmts(               # something to iterator over
                ($!blob := blob),
                ($!i     = -1),
                self
              ),
              Rakudo::Iterator.Empty    # nothing to iterate
            )
        }
        method new(\blob) { nqp::create(self).SET-SELF(blob) }

        # We can provide a generic push-all to the iterator as the
        # result of a push-all is always immutable, so we can use
        # the atpos_i here in both cases.
        method push-all($target --> IterationEnd) {
            nqp::stmts(
              (my $blob := $!blob),     # attribute access is slower
              (my int $elems = nqp::elems($blob)),
              (my int $i = $!i),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                $target.push(nqp::atpos_i($blob,$i))
              )
            )
        }
        method count-only() { nqp::p6box_i(nqp::elems($!blob)) }
        method sink-all(--> IterationEnd) { $!i = nqp::elems($!blob) }
    }

    # Generic role for iterating over a Map / Hash.  You must
    # at least provide your own .pull-one.  Takes a Map / Hash
    # as the only parameter to .new.
    our role Mappy does Iterator {
        has $!storage;
        has $!iter;

        method SET-SELF(\hash) {
            $!storage := nqp::getattr(hash,Map,'$!storage');
            nqp::if(
              ($!storage.DEFINITE && nqp::elems($!storage)),
              nqp::stmts(              # we have something to iterate over
                ($!iter := nqp::iterator($!storage)),
                self
              ),
              Rakudo::Iterator.Empty   # nothing to iterate
            )
        }
        method new(\hash) { nqp::create(self).SET-SELF(hash) }
        method count-only() { nqp::p6box_i(nqp::elems($!storage)) }
        method bool-only(--> True) { }
        method sink-all(--> IterationEnd) { $!iter := nqp::null }
    }

#-------------------------------------------------------------------------------
# Methods that generate an Iterator

    # Returns a sentinel Iterator object that will never generate any value.
    # Does not take a parameter.
    method Empty() {
        BEGIN class :: does Iterator {
            method new() { nqp::create(self) }
            method pull-one(--> IterationEnd)  { }
            method push-all($ --> IterationEnd) { }
            method sink-all(--> IterationEnd)  { }
            method count-only(--> 0) { }
            method bool-only(--> False) { }
        }.new
    }

    # An often occurring use of the Mappy role to generate all of the
    # values of a Map / Hash.  Takes a Map / Hash as the only parameter.
    method Mappy-values(\map) {
        class :: does Mappy {
            method pull-one() is raw {
                nqp::if(
                  $!iter,
                  nqp::iterval(nqp::shift($!iter)),
                  IterationEnd
                )
            }
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(nqp::iterval(nqp::shift($!iter)))
                )
            }
        }.new(map)
    }
}

# vim: ft=perl6 expandtab sw=4
