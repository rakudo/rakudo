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

    # Generic role for iterating over a >1 dimensional shaped list
    # for its lowest branches.  The default .new method takes a List
    # to iterate over.  A consuming class needs to provide a .process
    # method, which will be called with each iteration with the
    # $!indices attribute set to the coordinates of the branch being
    # iterated for this time (with the highest element index set to 0).
    # Consuming class can optionally provide a .done method that will
    # be called just before the iterator returns IterationEnd.
    our role ShapeBranch does Iterator {
        has $!dims;
        has $!indices;
        has Mu $!list;
        has int $!maxdim;
        has int $!maxind;
        has int $!level;

        # Every time process() gets called, the following attributes are set:
        # $!indices  a list_i with current position, with the highest elem 0
        # $!level    level at which exhaustion happened
        # $!dims     a list_i with dimensions
        # $!maxdim   maximum element number in $!dims
        # $!maxind   maximum element number in lowest level list
        method process { ... }           # consumer needs to supply a .process
        method done(--> Nil) { }         # by default no action at end

        method dims() {                  # HLL version of $!dims
            nqp::stmts(
              (my $result := nqp::setelems(nqp::list,nqp::elems($!dims))),
              (my int $i = -1),
              nqp::while(                # convert list_i to list
                nqp::isle_i(($i = nqp::add_i($i,1)),$!maxdim),
                nqp::bindpos($result,$i,nqp::atpos_i($!dims,$i))
              ),
              $result
            )
        }

        method SET-SELF(Mu \list) {
            nqp::stmts(
              nqp::if(
                nqp::istype(list,List),
                nqp::stmts(                                 # List like
                  ($!list := nqp::getattr(list,List,'$!reified')),
                  (my $shape := list.shape),
                  (my int $dims = $shape.elems),     # reifies
                  ($!dims := nqp::setelems(nqp::list_i,$dims)),
                  (my int $i = -1),
                  nqp::while(
                    nqp::islt_i(($i = nqp::add_i($i,1)),$dims),
                    nqp::bindpos_i($!dims,$i,
                      nqp::atpos(nqp::getattr($shape,List,'$!reified'),$i))
                  )
                ),
                ($dims = nqp::elems($!dims := nqp::dimensions($!list := list)))
              ),
              ($!indices := nqp::setelems(nqp::list_i,$dims)),
              ($!maxdim = nqp::sub_i($dims,1)),
              ($!maxind = nqp::sub_i(nqp::atpos_i($!dims,$!maxdim),1)),
              self
            )
        }
        method new(Mu \list) { nqp::create(self).SET-SELF(list) }

        method pull-one() is raw {
            nqp::if(
              nqp::isge_i($!level,0),
              nqp::stmts(                      # still iterating
                (my $result := self.process),  # do the processing
                (my int $level = $!maxdim),
                nqp::until(                    # update indices
                  nqp::islt_i(                 # exhausted ??
                    ($level = nqp::sub_i($level,1)),0) # next level
                    || nqp::stmts(
                    nqp::bindpos_i($!indices,nqp::add_i($level,1),0),  # reset
                    nqp::islt_i(
                      nqp::bindpos_i($!indices,$level, # increment this level
                        nqp::add_i(nqp::atpos_i($!indices,$level),1)),
                      nqp::atpos_i($!dims,$level)      # out of range?
                    ),
                  ),
                  nqp::null
                ),
                ($!level = $level),            # set level for next call
                $result                        # what we found
              ),
              nqp::stmts(
                nqp::if(
                  nqp::iseq_i($!level,-1),
                  nqp::stmts(                  # first time telling we're done
                    self.done,                 # notify we're done
                    ($!level = -2)             # do this only once
                  )
                ),
                IterationEnd                   # done iterating
              )
            )
        }
    }

    # Generic role for iterating over a >1 dimensional shaped list
    # for its values (leaves).  The default .new method takes a List
    # to iterate over.  A consuming class needs to provide a .result
    # method, which will be called with each iteration with the
    # $!indices attribute set to the coordinates of the element being
    # iterated for this time.  In some cases, the iterator is iterated
    # over for the side-effects in .result only.  Which is why this
    # role supplies an optimized .sink-all.
    our role ShapeLeaf does Iterator {
        has $!dims;
        has $!indices;
        has Mu $!list;
        has int $!maxdim;
        has int $!max;

        # Every time .result gets called, the following attributes are set:
        # $!indices  a list_i with current coordinate
        # $!dims     a list_i with dimensions
        # $!maxdim   maximum element number in $!dims
        method result { ... }            # consumer needs to supply a .result

        method indices() {               # HLL version of $!indices
            nqp::stmts(
              (my $result := nqp::setelems(nqp::list,nqp::elems($!indices))),
              (my int $i = -1),
              nqp::while(                # convert list_i to list
                nqp::isle_i(($i = nqp::add_i($i,1)),$!maxdim),
                nqp::bindpos($result,$i,nqp::atpos_i($!indices,$i))
              ),
              $result
            )
        }
        method SET-SELF(Mu \list) {
            nqp::stmts(
              nqp::if(
                nqp::istype(list,List),
                nqp::stmts(                                 # List like
                  ($!list := nqp::getattr(list,List,'$!reified')),
                  (my $shape := list.shape),
                  (my int $dims = $shape.elems),            # reifies
                  ($!dims := nqp::setelems(nqp::list_i,$dims)),
                  (my int $i = -1),
                  nqp::while(
                    nqp::islt_i(($i = nqp::add_i($i,1)),$dims),
                    nqp::bindpos_i($!dims,$i,
                      nqp::atpos(nqp::getattr($shape,List,'$!reified'),$i))
                  )
                ),
                ($dims = nqp::elems($!dims := nqp::dimensions($!list := list)))
              ),
              ($!indices := nqp::setelems(nqp::list_i,$dims)),
              ($!maxdim = nqp::sub_i($dims,1)),
              ($!max    = nqp::atpos_i($!dims,$!maxdim)),
              self
            )
        }
        method new(Mu \list) { nqp::create(self).SET-SELF(list) }

        method pull-one() is raw {
            nqp::if(
              $!indices,
              nqp::stmts(                                 # still iterating
                (my $result := self.result),              # process
                nqp::if(
                  nqp::islt_i(
                    (my int $i =
                      nqp::add_i(nqp::atpos_i($!indices,$!maxdim),1)),
                    $!max
                  ),
                  nqp::bindpos_i($!indices,$!maxdim,$i),  # ready for next
                  nqp::stmts(                             # done for now
                    (my int $level = $!maxdim),
                    nqp::until(                           # update indices
                      nqp::islt_i(                        # exhausted ??
                        ($level = nqp::sub_i($level,1)),0)
                        || nqp::stmts(
                        nqp::bindpos_i($!indices,nqp::add_i($level,1),0),
                        nqp::islt_i(
                          nqp::bindpos_i($!indices,$level,
                            nqp::add_i(nqp::atpos_i($!indices,$level),1)),
                          nqp::atpos_i($!dims,$level)
                        ),
                      ),
                      nqp::null
                    ),
                    nqp::if(
                      nqp::islt_i($level,0),
                      $!indices := nqp::null              # done next time
                    )
                  )
                ),
                $result                                   # what we found
              ),
              IterationEnd                                # done now
            )
        }

        method push-all($target --> IterationEnd) {
            nqp::while(
              $!indices,
              nqp::stmts(                                   # still iterating
                (my int $i = nqp::atpos_i($!indices,$!maxdim)),
                nqp::while(
                  nqp::isle_i(($i = nqp::add_i($i,1)),$!max),
                  nqp::stmts(
                    $target.push(self.result),              # process
                    nqp::bindpos_i($!indices,$!maxdim,$i),  # ready for next
                  )
                ),
                (my int $level = $!maxdim),                 # done for now
                nqp::until(                                 # update indices
                  nqp::islt_i(                              # exhausted ??
                    ($level = nqp::sub_i($level,1)),0)
                    || nqp::stmts(
                    nqp::bindpos_i($!indices,nqp::add_i($level,1),0),
                    nqp::islt_i(
                      nqp::bindpos_i($!indices,$level,
                        nqp::add_i(nqp::atpos_i($!indices,$level),1)),
                      nqp::atpos_i($!dims,$level)
                    ),
                  ),
                  nqp::null
                ),
                nqp::if(
                  nqp::islt_i($level,0),
                  $!indices := nqp::null                    # done
                )
              )
            )
        }

        method sink-all(--> IterationEnd) {
            nqp::while(
              $!indices,
              nqp::stmts(                                   # still iterating
                (my int $i = nqp::atpos_i($!indices,$!maxdim)),
                nqp::while(
                  nqp::isle_i(($i = nqp::add_i($i,1)),$!max),
                  nqp::stmts(
                    self.result,                            # process
                    nqp::bindpos_i($!indices,$!maxdim,$i),  # ready for next
                  )
                ),
                (my int $level = $!maxdim),                 # done for now
                nqp::until(                                 # update indices
                  nqp::islt_i(                              # exhausted ??
                    ($level = nqp::sub_i($level,1)),0)
                    || nqp::stmts(
                    nqp::bindpos_i($!indices,nqp::add_i($level,1),0),
                    nqp::islt_i(
                      nqp::bindpos_i($!indices,$level,
                        nqp::add_i(nqp::atpos_i($!indices,$level),1)),
                      nqp::atpos_i($!dims,$level)
                    ),
                  ),
                  nqp::null
                ),
                nqp::if(
                  nqp::islt_i($level,0),
                  $!indices := nqp::null                    # done
                )
              )
            )
        }
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

    # Returns an iterator from a given iterator where the occurrence of
    # a Whatever value indicates that last value seen from the source
    # iterator should be repeated indefinitely until either another
    # non-Whatever value is seen from the source iterator, or the source
    # iterator is exhausted.
    method Whatever(\source) {
        class :: does Iterator {
            has $!source;
            has $!last;
            has int $!whatever;
            method new(\source) {
                nqp::p6bindattrinvres(nqp::create(self),self,'$!source',source)
            }
            method pull-one() is raw {
                nqp::if(
                  $!whatever,
                  nqp::if(                          # we're repeating
                    nqp::iseq_i($!whatever,2),      # source exhausted, repeat
                    $!last,
                    nqp::if(
                      nqp::eqaddr(
                        (my $value := $!source.pull-one),
                        IterationEnd
                      ),
                      nqp::stmts(                   # exhausted now, repeat
                        ($!whatever = 2),
                        $!last
                      ),
                      nqp::if(
                        nqp::istype($value,Whatever),
                        $!last,                     # another Whatever, repeat
                        nqp::stmts(                 # something else, no repeat
                          ($!whatever = 0),
                          ($!last := $value)
                        )
                      )
                    )
                  ),
                  nqp::if(                          # not repeating
                    nqp::eqaddr(
                      ($value := $!source.pull-one),
                      IterationEnd
                    ),
                    IterationEnd,                   # exhausted, stop
                    nqp::if(
                      nqp::istype($value,Whatever), # start repeating
                      nqp::stmts(
                        ($!whatever = 1),
                        $!last
                      ),
                      ($!last := $value)            # keep value for repeat
                    )
                  )
                )
            }
        }.new(source)
    }
}

# vim: ft=perl6 expandtab sw=4
