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
    my $empty := nqp::list;   # an empty list for nqp::splice

#-------------------------------------------------------------------------------
# Roles that are used by iterators in the rest of the core settings, in
# alphabetical order for easier perusal.

    # Generic role for iterating over a Blob / Buf.  You need to
    # supply at least a .pull-one.  Takes a Blob / Buf as the only
    # parameter to .new.
    our role Blobby does PredictiveIterator {
        has $!blob;
        has Int $!i;   # sadly, this can not be a native int yet  :-(

        method !SET-SELF(\blob) {
            nqp::stmts(               # something to iterator over
              ($!blob := blob),
              ($!i     = -1),
              self
            )
        }
        method new(\blob) {
            nqp::if(
              nqp::isgt_i(nqp::elems(blob),0),
              nqp::create(self)!SET-SELF(blob),
              Rakudo::Iterator.Empty    # nothing to iterate
            )
        }

        # We can provide a generic push-all to the iterator as the
        # result of a push-all is always immutable, so we can use
        # the atpos_i here in both cases.
        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              (my $blob := $!blob),     # attribute access is slower
              (my int $elems = nqp::elems($blob)),
              (my int $i = $!i),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                target.push(nqp::atpos_i($blob,$i))
              ),
              ($!i = $i)
            )
        }
        method count-only(--> Int:D) {
            nqp::p6box_i(
              nqp::elems($!blob)
                - $!i
                - nqp::islt_i($!i,nqp::elems($!blob))
            )
        }
        method sink-all(--> IterationEnd) { $!i = nqp::elems($!blob) }
    }

    # Generic role for iterating over a Map / Hash.  You must
    # at least provide your own .pull-one.  Takes a Map / Hash
    # as the only parameter to .new.
    our role Mappy does Iterator {
        has $!hash;
        has $!iter;

        method !SET-SELF(\hash) {
            nqp::if(
              ($!hash := nqp::if(
                nqp::istype(hash,Rakudo::Internals::IterationSet),
                hash,
                nqp::getattr(hash,Map,'$!storage')
              )) && ($!iter := nqp::iterator($!hash)),
              self,
              Rakudo::Iterator.Empty   # nothing to iterate
            )
        }
        method new(\hash) { nqp::create(self)!SET-SELF(hash) }
        method skip-one() { nqp::if($!iter,nqp::stmts(nqp::shift($!iter),1)) }
        method sink-all(--> IterationEnd) { $!iter := nqp::null }
    }

    # Generic role for iterating over a Map / Hash that has pairs
    # for values providing the "real" key and value.  A default
    # .pull-one and .push-all is provided.  Takes a Map / Hash as
    # the only parameter to .new.
    our role Mappy-kv-from-pairs does Iterator {
        has $!hash;
        has $!iter;
        has $!on;

        method !SET-SELF(\hash) {
            nqp::if(
              ($!hash := nqp::if(
                nqp::istype(hash,Rakudo::Internals::IterationSet),
                hash,
                nqp::getattr(hash,Map,'$!storage')
              )) && ($!iter := nqp::iterator($!hash)),
              self,
              Rakudo::Iterator.Empty   # nothing to iterate
            )
        }
        method new(\hash) { nqp::create(self)!SET-SELF(hash) }

        method pull-one() is raw {
            nqp::if(
              $!on,
              nqp::stmts(
                ($!on = 0),
                nqp::getattr(nqp::iterval($!iter),Pair,'$!value')
              ),
              nqp::if(
                $!iter,
                nqp::stmts(
                  ($!on = 1),
                  nqp::getattr(nqp::iterval(nqp::shift($!iter)),Pair,'$!key')
                ),
                IterationEnd
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!iter,
              nqp::stmts(  # doesn't sink
                (my $pair := nqp::decont(nqp::iterval(nqp::shift($!iter)))),
                target.push(nqp::getattr($pair,Pair,'$!key')),
                target.push(nqp::getattr($pair,Pair,'$!value'))
              )
            )
        }
        method skip-one() {               # must define our own skip-one
            nqp::if(
              $!on,
              nqp::not_i($!on = 0),
              nqp::if(
                $!iter,
                nqp::stmts(
                  nqp::shift($!iter),
                  ($!on = 1)
                )
              )
            )
        }
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
              (my $buffer :=
                nqp::setelems(nqp::create(IterationBuffer),nqp::elems($!dims))),
              (my int $i = -1),
              nqp::while(                # convert list_i to list
                nqp::isle_i(($i = nqp::add_i($i,1)),$!maxdim),
                nqp::bindpos($buffer,$i,nqp::atpos_i($!dims,$i))
              ),
              $buffer.List
            )
        }

        method !SET-SELF(Mu \list) {
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
        method new(Mu \list) { nqp::create(self)!SET-SELF(list) }

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
        method !SET-SELF(Mu \list) {
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
        method new(Mu \list) { nqp::create(self)!SET-SELF(list) }

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

        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!indices,
              nqp::stmts(                                   # still iterating
                (my int $i = nqp::atpos_i($!indices,$!maxdim)),
                nqp::while(
                  nqp::isle_i(($i = nqp::add_i($i,1)),$!max),
                  nqp::stmts(
                    target.push(self.result),              # process
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
# Methods that generate an Iterator (in alphabetical order)

    # Create iterator that produces all values *except* the last N values
    # of a given iterator.  Returns an empty iterator if the given iterator
    # produced fewer than N values.
    my class AllButLastNValues does Iterator {
        has $!iterator;
        has $!buffered;
        has int $!size;
        has int $!index;

        method !SET-SELF(\iterator, int $size) {
            nqp::stmts(
              (my int $i = -1),
              (my \buffered := nqp::setelems(nqp::list,$size)),
              nqp::while(                      # fill buffer to produce from
                nqp::islt_i(($i = nqp::add_i($i,1)),$size)
                  && nqp::not_i(nqp::eqaddr(
                       (my \pulled := iterator.pull-one),
                       IterationEnd
                     )),
                nqp::bindpos(buffered,$i,pulled)
              ),
              nqp::if(
                nqp::islt_i($i,$size),
                Rakudo::Iterator.Empty,        # didn't produce enough
                nqp::stmts(                    # we're in business
                  ($!iterator := iterator),
                  ($!buffered := buffered),
                  ($!size = $size),
                  self
                )
              )
            )
        }
        method new(\iterator,\n) {
            nqp::if(
              nqp::isle_i(n,0),
              iterator,                        # we wants it all
              nqp::create(self)!SET-SELF(iterator,n)
            )
        }
        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr((my \pulled := $!iterator.pull-one),IterationEnd),
              pulled,                          # we're done
              nqp::stmts(                      # produce/update buffer
                (my \value := nqp::atpos($!buffered,$!index)),
                nqp::bindpos($!buffered,$!index,pulled),
                ($!index = nqp::mod_i(nqp::add_i($!index,1),$!size)),
                value
              )
            )
        }
    }
    method AllButLastNValues(\iterator, \n) {
        AllButLastNValues.new(iterator,n)
    }

    # Return an iterator that will generate a pair with the value as the
    # key and as value the key of the given iterator, basically the
    # .antipairs functionality on 1 dimensional lists.
    my class AntiPair does Iterator {
        has Mu $!iter;
        has int $!key;

        method !SET-SELF(\iter) { $!iter := iter; $!key = -1; self }
        method new(\iter) { nqp::create(self)!SET-SELF(iter) }

        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr((my \pulled := $!iter.pull-one),IterationEnd),
              IterationEnd,
              Pair.new(pulled,+($!key = nqp::add_i($!key,1)))
            )
        }
        method push-all(\target --> IterationEnd) {
            my int $key = -1;
            nqp::until(
              nqp::eqaddr((my \pulled := $!iter.pull-one),IterationEnd),
              target.push(Pair.new(pulled,+($key = nqp::add_i($key,1))))
            )
        }
        method is-lazy() { $!iter.is-lazy }
    }
    method AntiPair(\iterator) { AntiPair.new(iterator) }

    # Return an iterator that takes an Associative and an Iterable that
    # generates keys, to call the AT-KEY method on the Associative.
    my class AssociativeIterableKeys does Iterator {
        has $!associative;
        has $!iterator;
        method !SET-SELF($!associative,$!iterator) { self }
        method new(\asso,\iter) { nqp::create(self)!SET-SELF(asso,iter) }

        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr((my \key := $!iterator.pull-one),IterationEnd),
              IterationEnd,
              $!associative.AT-KEY(key)
            )
        }
        method push-all(\target --> IterationEnd) {
            my \iterator    := $!iterator;
            my \associative := $!associative;
            nqp::until(
              nqp::eqaddr((my \key := iterator.pull-one),IterationEnd),
              target.push(associative.AT-KEY(key))
            )
        }
    }
    method AssociativeIterableKeys(\asso, \iterable) {
        AssociativeIterableKeys.new(asso,iterable.iterator)
    }

    # Return an iterator that batches the given source iterator in
    # batches of the given size.  The third parameter indicates whether
    # a partial batch should be returned when the source iterator has
    # exhausted.  The returned iterator is as lazy as the source iterator.
    my class Batch does Iterator {
        has $!iterator;
        has int $!size;
        has int $!complete;
        has int $!is-exhausted;
        method !SET-SELF(\iterator,\size,\partial) {
            nqp::stmts(
              ($!iterator := iterator),
              nqp::if(
                nqp::istype(size,Whatever),
                ($!size = -1),        # set to never stop and ok partial
                nqp::if(
                  size < 1,
                  X::OutOfRange.new(
                    what    => "Batching sublist length is",
                    got     => size,
                    range   => "1..^Inf",
                  ).throw,
                  nqp::if(
                    (nqp::istype(size,Int)
                      && nqp::isbig_I(nqp::decont(size)))
                      || size == Inf,
                    ($!size = -1),    # set to never stop and ok partial
                    nqp::stmts(
                      ($!size     = size),
                      ($!complete = !partial),
                    )
                  )
                )
              ),
              self
            )
        }
        method new(\it,\si,\pa) { nqp::create(self)!SET-SELF(it,si,pa) }
        method pull-one() is raw {
          nqp::if(
            $!is-exhausted,
            IterationEnd,
            nqp::stmts(
              (my \reified := nqp::create(IterationBuffer)),
              nqp::until(
                nqp::iseq_i(nqp::elems(reified),$!size)
                  || nqp::eqaddr(
                       (my \pulled := $!iterator.pull-one),
                       IterationEnd
                     ),
                nqp::push(reified,pulled)
              ),
              nqp::if(
                nqp::eqaddr(pulled,IterationEnd)
                  && ($!is-exhausted = 1) # set the flag
                  && ($!complete || nqp::not_i(nqp::elems(reified))),
                IterationEnd,
                reified.List
              )
            )
          )
        }
        method is-lazy() { $!iterator.is-lazy }
    }
    method Batch(\iterator,\size,\partial) { Batch.new(iterator,size,partial) }

    # Return an iterator for a given Callable.  The Callable is supposed
    # to return a value for the iterator, or IterationEnd to indicate the
    # data from the Callable is exhausted.  No checks for Slips are done,
    # so they will be passed on as is.  Also optionally takes a flag to
    # mark the iterator as lazy or not: default is False (not lazy)
    proto method Callable(|) {*}
    my class CallableEager does Iterator {
        has &!callable;
        method new(&callable) {
            nqp::p6bindattrinvres(
              nqp::create(self),self,'&!callable',&callable)
        }
        method pull-one() is raw { &!callable() }
    }
    multi method Callable(&callable) { CallableEager.new(&callable) }

    my class CallableLazy does Iterator {
        has &!callable;
        method new(&callable) {
            nqp::p6bindattrinvres(
              nqp::create(self),self,'&!callable',&callable)
        }
        method pull-one() is raw { &!callable() }
        method is-lazy(--> True) { }
    }
    multi method Callable(&callable, Bool() $lazy) {
        $lazy
          ?? CallableLazy.new(&callable)
          !! CallableEager.new(&callable)
    }

    # Return an iterator for the "thunk xx 42" functionality.
    my class Callable-xx-Times does Iterator {
        has @!slipped;
        has $!code;
        has $!times;
        method !SET-SELF(\code,\times) {
            nqp::stmts(
              ($!code := code),
              ($!times = times),
              self
            )
        }
        method new(\code,\times) {
            nqp::if(
              times > 0,
              nqp::create(self)!SET-SELF(code,times),
              Rakudo::Iterator.Empty
            )
        }
        method pull-one() {
            nqp::if(
              @!slipped,
              @!slipped.shift,
              nqp::if(
                $!times > 0,
                nqp::stmts(
                  --$!times,             # consumed a value
                  nqp::if(
                    nqp::istype((my \pulled := $!code()),Slip),
                    nqp::if(
                      (@!slipped = pulled),
                      @!slipped.shift,
                      IterationEnd
                    ),
                    nqp::if(
                      nqp::istype(pulled,Seq),
                      pulled.cache,
                      pulled
                    )
                  )
                ),
                IterationEnd
              )
            )
        }
    }
    method Callable-xx-Times(&code, Int:D \times) {
        Callable-xx-Times.new(&code,times)
    }

    # Return an iterator for the "thunk xx *" functionality.
    my class Callable-xx-Whatever does Iterator {
        has @!slipped;
        has $!code;
        method new(\code) {
            nqp::p6bindattrinvres(nqp::create(self),self,'$!code',code)
        }
        method pull-one() {
            nqp::if(
              @!slipped,
              @!slipped.shift,
              nqp::if(
                nqp::istype((my \pulled := $!code()),Slip),
                nqp::if(
                  (@!slipped = pulled),
                  @!slipped.shift,
                  IterationEnd
                ),
                nqp::if(
                  nqp::istype(pulled,Seq),
                  pulled.cache,
                  pulled
                )
              )
            )
        }
        method is-lazy(--> True) { }
    }
    method Callable-xx-Whatever(&code) { Callable-xx-Whatever.new(&code) }

    # Returns an iterator for a one character range of values.  Takes the
    # first, last character, and whether the first / last should be
    # excluded.
    my class CharFromTo does PredictiveIterator {
        has int $!i;
        has int $!n;

        method !SET-SELF(\min,\max,\excludes-min,\excludes-max) {
            $!i = nqp::ord(nqp::unbox_s(min))
                - (excludes-min ?? 0 !! 1);
            $!n = nqp::ord(nqp::unbox_s(max))
                - (excludes-max ?? 1 !! 0);
            self
        }
        method new(\min,\max,\excludes-min,\excludes-max) {
            nqp::create(self)!SET-SELF(
               min,max,excludes-min,excludes-max)
        }
        method pull-one() {
            ( $!i = $!i + 1 ) <= $!n
              ?? nqp::chr($!i)
              !! IterationEnd
        }
        method skip-one() { ( $!i = $!i + 1 ) <= $!n }
        method push-all(\target --> IterationEnd) {
            my int $i = $!i;
            my int $n = $!n;
            target.push(nqp::chr($i)) while ($i = $i + 1) <= $n;
            $!i = $i;
        }
        method count-only(--> Int:D) { nqp::p6box_i($!n - $!i) }
        method sink-all(--> IterationEnd) { $!i = $!n }
    }
    method CharFromTo(\min,\max,\excludes-min,\excludes-max) {
        CharFromTo.new(min,max,excludes-min,excludes-max)
    }

    # Return an iterator for a range of 0..^N with a number of elements.
    # The third parameter indicates whether an IterationBuffer should be
    # returned (1) for each combinatin, or a fully reified List (0).
    # Has a highly optimized count-only, for those cases when one is only
    # interested in the number of combinations, rather than the actual
    # combinations.  The workhorse of combinations().
    my class Combinations does PredictiveIterator {
        has int $!pulled-count = 0;
        has int $!n;
        has int $!k;
        has int $!b;
        has Mu $!stack;
        has Mu $!combination;
        method !SET-SELF(\n,\k,\b) {
            nqp::stmts(
              ($!n = n),
              ($!k = k),
              ($!b = b),
              ($!stack := nqp::list_i(0)),
              ($!combination := nqp::create(IterationBuffer)),
              self
          )
        }
        method new(\n,\k,\b) { nqp::create(self)!SET-SELF(n,k,b) }

        method pull-one() {
            nqp::stmts(
              (my int $n = $!n),          # lexicals faster
              (my int $k = $!k),
              (my int $running = 1),
              nqp::while(
                ($running && (my int $elems = nqp::elems($!stack))),
                nqp::stmts(
                  (my int $index = nqp::sub_i($elems,1)),
                  (my int $value = nqp::pop_i($!stack)),
                  nqp::while(
                    (nqp::islt_i($value,$n)
                      && nqp::islt_i($index,$k)),
                    nqp::stmts(
                      nqp::bindpos($!combination,
                        $index,nqp::clone($value)),
                      ($index = nqp::add_i($index,1)),
                      ($value = nqp::add_i($value,1)),
                      nqp::push_i($!stack,$value)
                    )
                  ),
                  ($running = nqp::isne_i($index,$k)),
                )
              ),
              nqp::if(
                nqp::iseq_i($index,$k),
                nqp::stmts(
                  ($!pulled-count = nqp::add_i($!pulled-count,1)),
                  nqp::if(
                    $!b,
                    nqp::clone($!combination),
                    nqp::clone($!combination).List
                  )
                ),
                IterationEnd
              )
            )
        }

        method count-only(--> Int:D) {
            (([*] ($!n ... 0) Z/ 1 .. min($!n - $!k, $!k)).Int)
            - $!pulled-count
        }
    }
    method Combinations($n, $k, int $b) {
        nqp::if(
          $n > 0 && nqp::isbig_I(nqp::decont($n)),    # must be HLL comparison
          X::OutOfRange.new(
            :what("First parameter"),
            :got($n),
            :range("-Inf^..{$?BITS == 32 ?? 2**28-1 !! 2**31-1}")
          ).throw,
          nqp::if(
            # k = 0 → can pick just 1 combination (empty list); return ((),)
            $k == 0,                                  # Must be HLL comparison
            Rakudo::Iterator.OneValue(
              nqp::create(nqp::if($b,IterationBuffer,List))
            ),
            nqp::if(
              # n < 1 → we have an empty list to pick from
              # n < k → not enough items to pick combination of k items
              $n < 1 || $n < $k || $k < 0,            # must be HLL comparisons
              Rakudo::Iterator.Empty,                 # nothing to return
              Combinations.new($n,$k,$b)
            )
          )
        )
    }

    # Return an iterator that will cross the given iterables (with &[,])
    # Basically the functionality of @a X @b
    my class CrossIterables does Iterator {
        has $!iterators;  # iterator per iterable, if any
        has $!reifieds;   # cached values (either complete, or so far)
        has $!indices;    # indices of virtual matrix of crossed values
        has $!next;       # IterationBuffer with next values to return
        has int $!lazy;   # whether the outer iterator is lazy
        has int $!top;    # index of top reified/iterator

        method !SET-SELF(\iterables) {
            nqp::stmts(
              (my $iterables := nqp::getattr(iterables,List,'$!reified')),
              (my int $elems  = nqp::elems($iterables)),
              ($!iterators   := nqp::setelems(nqp::list,$elems)),
              ($!reifieds    := nqp::setelems(nqp::list,$elems)),
              ($!next :=
                nqp::setelems(nqp::create(IterationBuffer),$elems)),

              # loop over all iterables
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),

                # set up initial value of index $i with...
                nqp::bindpos($!next,$i,nqp::if(
                  nqp::iscont(my $elem := nqp::atpos($iterables,$i))
                    || nqp::not_i(nqp::istype($elem,Iterable)),

                  # single value same as reified list of 1
                  nqp::bindpos(
                    nqp::bindpos($!reifieds,$i,nqp::list),
                    0,
                    $elem
                  ),

                  # something more elaborate
                  nqp::if(
                    nqp::istype($elem,List)
                      && nqp::not_i(nqp::isconcrete(
                           nqp::getattr($elem,List,'$!todo')
                    )),

                    # it's a List, may have a reified we can use directly
                    nqp::if(
                      nqp::isconcrete(
                        $elem := nqp::getattr($elem,List,'$!reified')
                      ) && nqp::isgt_i(nqp::elems($elem),0),

                      # use the available reified directly
                      nqp::stmts(
                        nqp::bindpos($!reifieds,$i,$elem),
                        nqp::atpos($elem,0)
                      ),

                      # cross with an empty list is always an empty list
                      return Rakudo::Iterator.Empty
                    ),

                    # need to set up an iterator
                    nqp::stmts(
                      nqp::if($elem.is-lazy,($!lazy = 1)),
                      nqp::if(
                        nqp::eqaddr(
                          (my $pulled :=
                            ($elem := $elem.iterator).pull-one),
                          IterationEnd
                        ),

                        # cross with an empty list is an empty list
                        (return Rakudo::Iterator.Empty),

                        # set up the iterator stuff
                        nqp::stmts(
                          nqp::bindpos($!iterators,$i,$elem),
                          nqp::bindpos($!reifieds,$i,nqp::list($pulled)),
                          $pulled
                        )
                      )
                    )
                  )
                ))
              ),

              # indices start with 0 xx $elems
              ($!indices := nqp::setelems(nqp::list_i,$elems)),
              ($!top = nqp::sub_i($elems,1)),
              self
            )
        }
        method new(\iterables) { nqp::create(self)!SET-SELF(iterables) }
        method pull-one() {
            nqp::if(
              nqp::eqaddr($!next,IterationEnd),
              IterationEnd,
              nqp::stmts(

                # set up result of this pull
                (my $result := nqp::clone($!next).List),

                # start working on next result
                nqp::unless(
                  nqp::isnull(nqp::atpos($!iterators,$!top)),

                  # top level is still iterator, fetch
                  nqp::if(
                    nqp::eqaddr(
                      (my $pulled :=
                        nqp::atpos($!iterators,$!top).pull-one),
                      IterationEnd
                    ),
                    # iterator no more
                    nqp::bindpos($!iterators,$!top,nqp::null),

                    # push value, let normal reifier handler handle
                    nqp::push(
                      nqp::atpos($!reifieds,$!top),
                      $pulled
                    )
                  )
                ),

                # no iterator, must use reified list
                nqp::if(
                  nqp::islt_i(
                    (my int $index =
                      nqp::add_i(nqp::atpos_i($!indices,$!top),1)),
                    nqp::elems(nqp::atpos($!reifieds,$!top))
                  ),

                  # within range, update next result and index
                  nqp::bindpos($!next,$!top,
                    nqp::atpos(
                      nqp::atpos($!reifieds,$!top),
                      nqp::bindpos_i($!indices,$!top,$index)
                    )
                  ),

                  # need to update lower levels
                  nqp::stmts(

                    # update topmost value (go back to first)
                    nqp::bindpos($!next,$!top,
                      nqp::atpos(
                        nqp::atpos($!reifieds,$!top),
                        nqp::bindpos_i($!indices,$!top,0)
                      )
                    ),

                    # until we're at the bottom
                    (my int $level = $!top),
                    nqp::while(
                      nqp::isge_i(($level = nqp::sub_i($level,1)),0),
                      nqp::if(
                        nqp::isnull(nqp::atpos($!iterators,$level)),

                        # can use reified list at this level
                        nqp::if(
                          nqp::islt_i(
                            ($index = nqp::add_i(
                              nqp::atpos_i($!indices,$level),1)),
                            nqp::elems(nqp::atpos($!reifieds,$level))
                          ),

                          # within range, update next result and index
                          nqp::stmts(
                            nqp::bindpos($!next,$level,
                              nqp::atpos(
                                nqp::atpos($!reifieds,$level),
                                nqp::bindpos_i($!indices,$level,$index)
                              )
                            ),
                            ($level = -1)  # done searching
                          ),

                          # reset this level
                          nqp::bindpos($!next,$level,
                            nqp::atpos(
                              nqp::atpos($!reifieds,$level),
                              nqp::bindpos_i($!indices,$level,0)
                            )
                          )
                        ),

                        # still an iterator at this level
                        nqp::if(
                          nqp::eqaddr(
                            ($pulled :=
                              nqp::atpos($!iterators,$level).pull-one),
                            IterationEnd
                          ),

                          # exhausted iterator, reset to reified
                          nqp::stmts(
                            nqp::bindpos($!iterators,$level,nqp::null),
                            nqp::bindpos($!next,$level,
                              nqp::atpos(
                                nqp::atpos($!reifieds,$level),
                                nqp::bindpos_i($!indices,$level,0)
                              )
                            )
                          ),

                          # new value, add to reified, update indices
                          nqp::stmts(
                            nqp::bindpos(
                              $!next,
                              $level,
                              nqp::bindpos(
                                nqp::atpos($!reifieds,$level),
                                nqp::bindpos_i(
                                  $!indices,
                                  $level,
                                  nqp::add_i(
                                    nqp::atpos_i($!indices,$level),
                                    1
                                  )
                                ),
                                $pulled
                              )
                            ),
                            ($level = -1)  # done searching
                          )
                        )
                      )
                    ),
                    nqp::if(
                      nqp::iseq_i($level,-1),
                      # was last iteration, free up everything now
                      ($!next := $!iterators := $!reifieds := $!indices :=
                        IterationEnd)
                    )
                  )
                ),
                $result
              )
            )
        }
        method is-lazy() { nqp::hllbool($!lazy) }
    }
    method CrossIterables(@iterables) {
        nqp::if(
          nqp::isgt_i((my int $n = @iterables.elems),1),  # reifies

          # actually need to do some crossing (probably)
          CrossIterables.new(@iterables),

          # simpler cases
          nqp::if(
            nqp::iseq_i($n,0),
            # nothing to cross, so return an empty list
            Rakudo::Iterator.Empty,
            # only 1 list to cross, which is the list itself
            nqp::atpos(nqp::getattr(@iterables,List,'$!reified'),0).iterator
          )
        )
    }

    # Return an iterator that will cross the given iterables and map
    # the result with the given mapper Callable.  Basically the
    # functionality of @a Xop @b (with the op  -> mapper functionality
    # to be supplied externally).
    my class CrossIterablesMap does Iterator {
        has $!iterators;  # iterator per iterable, if any
        has $!reifieds;   # cached values (either complete, or so far)
        has $!indices;    # indices of virtual matrix of crossed values
        has $!next;       # IterationBuffer with next values to return
        has $!mapper;     # Callable to do final result mapping
        has int $!lazy;   # whether the outer iterator is lazy
        has int $!top;    # index of top reified/iterator

        method !SET-SELF(\iterables,\mapper) {
            nqp::stmts(
              (my $iterables := nqp::getattr(iterables,List,'$!reified')),
              (my int $elems  = nqp::elems($iterables)),
              ($!iterators   := nqp::setelems(nqp::list,$elems)),
              ($!reifieds    := nqp::setelems(nqp::list,$elems)),
              ($!next := nqp::setelems(nqp::create(IterationBuffer),$elems)),

              # loop over all iterables
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),

                # set up initial value of index $i with...
                nqp::bindpos($!next,$i,nqp::if(
                  nqp::iscont(my $elem := nqp::atpos($iterables,$i))
                    || nqp::not_i(nqp::istype($elem,Iterable)),

                  # single value same as reified list of 1
                  nqp::bindpos(
                    nqp::bindpos($!reifieds,$i,nqp::list),
                    0,
                    $elem
                  ),

                  # something more elaborate
                  nqp::if(
                    nqp::istype($elem,List)
                      && nqp::not_i(nqp::isconcrete(
                           nqp::getattr($elem,List,'$!todo')
                    )),

                    # it's a List, may have a reified we can use directly
                    nqp::if(
                      nqp::isconcrete(
                        $elem := nqp::getattr($elem,List,'$!reified')
                      ) && nqp::isgt_i(nqp::elems($elem),0),

                      # use the available reified directly
                      nqp::stmts(
                        nqp::bindpos($!reifieds,$i,$elem),
                        nqp::atpos($elem,0)
                      ),

                      # cross with an empty list is always an empty list
                      (return Rakudo::Iterator.Empty)
                    ),

                    # need to set up an iterator
                    nqp::stmts(
                      nqp::if($elem.is-lazy,($!lazy = 1)),
                      nqp::if(
                        nqp::eqaddr(
                          (my $pulled :=
                            ($elem := $elem.iterator).pull-one),
                          IterationEnd
                        ),

                        # cross with an empty list is an empty list
                        (return Rakudo::Iterator.Empty),

                        # set up the iterator stuff
                        nqp::stmts(
                          nqp::bindpos($!iterators,$i,$elem),
                          nqp::bindpos($!reifieds,$i,nqp::list($pulled)),
                          $pulled
                        )
                      )
                    )
                  )
                ))
              ),

              # indices start with 0 xx $elems
              ($!indices := nqp::setelems(nqp::list_i,$elems)),
              ($!top = nqp::sub_i($elems,1)),
              ($!mapper := mapper),
              self
            )
        }
        method new(\its,\map) { nqp::create(self)!SET-SELF(its,map) }
        method pull-one() {
            nqp::if(
              nqp::eqaddr($!next,IterationEnd),
              IterationEnd,
              nqp::stmts(

                # set up result of this pull
                # we *MUST* clone here, because we cannot be sure
                # the mapper isn't going to throw the buffer away.
                (my $result := $!mapper(nqp::clone($!next))),

                # start working on next result
                nqp::unless(
                  nqp::isnull(nqp::atpos($!iterators,$!top)),

                  # top level is still iterator, fetch
                  nqp::if(
                    nqp::eqaddr(
                      (my $pulled :=
                        nqp::atpos($!iterators,$!top).pull-one),
                      IterationEnd
                    ),
                    # iterator no more
                    nqp::bindpos($!iterators,$!top,nqp::null),

                    # push value, let normal reifier handler handle
                    nqp::push(
                      nqp::atpos($!reifieds,$!top),
                      $pulled
                    )
                  )
                ),

                # no iterator, must use reified list
                nqp::if(
                  nqp::islt_i(
                    (my int $index =
                      nqp::add_i(nqp::atpos_i($!indices,$!top),1)),
                    nqp::elems(nqp::atpos($!reifieds,$!top))
                  ),

                  # within range, update next result and index
                  nqp::bindpos($!next,$!top,
                    nqp::atpos(
                      nqp::atpos($!reifieds,$!top),
                      nqp::bindpos_i($!indices,$!top,$index)
                    )
                  ),

                  # need to update lower levels
                  nqp::stmts(

                    # update topmost value (go back to first)
                    nqp::bindpos($!next,$!top,
                      nqp::atpos(
                        nqp::atpos($!reifieds,$!top),
                        nqp::bindpos_i($!indices,$!top,0)
                      )
                    ),

                    # until we're at the bottom
                    (my int $level = $!top),
                    nqp::while(
                      nqp::isge_i(($level = nqp::sub_i($level,1)),0),
                      nqp::if(
                        nqp::isnull(nqp::atpos($!iterators,$level)),

                        # can use reified list at this level
                        nqp::if(
                          nqp::islt_i(
                            ($index = nqp::add_i(
                              nqp::atpos_i($!indices,$level),1)),
                            nqp::elems(nqp::atpos($!reifieds,$level))
                          ),

                          # within range, update next result and index
                          nqp::stmts(
                            nqp::bindpos($!next,$level,
                              nqp::atpos(
                                nqp::atpos($!reifieds,$level),
                                nqp::bindpos_i($!indices,$level,$index)
                              )
                            ),
                            ($level = -1)  # done searching
                          ),

                          # reset this level
                          nqp::bindpos($!next,$level,
                            nqp::atpos(
                              nqp::atpos($!reifieds,$level),
                              nqp::bindpos_i($!indices,$level,0)
                            )
                          )
                        ),

                        # still an iterator at this level
                        nqp::if(
                          nqp::eqaddr(
                            ($pulled :=
                              nqp::atpos($!iterators,$level).pull-one),
                            IterationEnd
                          ),

                          # exhausted iterator, reset to reified
                          nqp::stmts(
                            nqp::bindpos($!iterators,$level,nqp::null),
                            nqp::bindpos($!next,$level,
                              nqp::atpos(
                                nqp::atpos($!reifieds,$level),
                                nqp::bindpos_i($!indices,$level,0)
                              )
                            )
                          ),

                          # new value, add to reified, update indices
                          nqp::stmts(
                            nqp::bindpos(
                              $!next,
                              $level,
                              nqp::bindpos(
                                nqp::atpos($!reifieds,$level),
                                nqp::bindpos_i(
                                  $!indices,
                                  $level,
                                  nqp::add_i(
                                    nqp::atpos_i($!indices,$level),
                                    1
                                  )
                                ),
                                $pulled
                              )
                            ),
                            ($level = -1)  # done searching
                          )
                        )
                      )
                    ),
                    nqp::if(
                      nqp::iseq_i($level,-1),
                      # was last iteration, free up everything now
                      ($!next := $!iterators := $!reifieds := $!indices :=
                        IterationEnd)
                    )
                  )
                ),
                $result
              )
            )
        }
        method is-lazy() { nqp::hllbool($!lazy) }
    }
    method CrossIterablesMap(@iterables,&mapper) {
        nqp::if(
          nqp::isgt_i((my int $n = @iterables.elems),1),  # reifies

          # actually need to do some crossing (probably)
          CrossIterablesMap.new(@iterables,&mapper),

          # simpler cases
          nqp::if(
            nqp::iseq_i($n,0),
            # nothing to cross, so return an empty list
            Rakudo::Iterator.Empty,
            # only 1 list to cross, which is the list itself
            nqp::atpos(nqp::getattr(@iterables,List,'$!reified'),0).iterator
          )
        )
    }

    # Return an iterator that will cross the given iterables and operator.
    # Basically the functionality of @a Z=> @b, with &[=>] being the op.
    method CrossIterablesOp(@iterables,\op) {
        nqp::if(
          nqp::eqaddr(op,&infix:<,>),
          Rakudo::Iterator.CrossIterables(@iterables),
          Rakudo::Iterator.CrossIterablesMap(
            @iterables,
            Rakudo::Metaops.MapperForOp(op)
          )
        )
    }

    # Returns an iterator that handles all properties of a -while- with
    # a condition.  Takes a Callable to be considered the body of the loop,
    # and a Callable for the condition..
    my class CStyleLoop does SlippyIterator {
        has &!body;
        has &!cond;
        has &!afterwards;
        has int $!seen-first;

        method !SET-SELF(\body,\cond,\afterwards) {
            nqp::stmts(
              (&!body := body),
              (&!cond := cond),
              (&!afterwards := afterwards),
              self
            )
        }
        method new(\body,\cond,\afterwards) {
            nqp::create(self)!SET-SELF(body,cond,afterwards)
        }

        method pull-one() {
            if $!slipping && nqp::not_i(
                nqp::eqaddr((my $result := self.slip-one),IterationEnd)
            ) {
                $result
            }
            else {
                nqp::stmts(
                  nqp::if(
                    $!seen-first,
                    &!afterwards(),
                    ($!seen-first = 1)
                  ),
                  nqp::if(
                    &!cond(),
                    nqp::stmts(
                      nqp::until(
                        (my int $stopped),
                        nqp::stmts(
                          ($stopped = 1),
                          nqp::handle(
                            nqp::if(
                              nqp::istype(($result := &!body()),Slip),
                              nqp::if(
                                nqp::eqaddr(
                                  ($result := self.start-slip($result)),
                                  IterationEnd
                                ),
                                nqp::stmts(
                                  &!afterwards(),
                                  ($stopped = nqp::if(&!cond(),0,1))
                                )
                              )
                            ),
                            'NEXT', nqp::stmts(
                              &!afterwards(),
                              ($stopped = nqp::if(&!cond(),0,1))
                            ),
                            'REDO', ($stopped = 0),
                            'LAST', ($result := IterationEnd)
                          )
                        ),
                        :nohandler
                      ),
                      $result
                    ),
                    IterationEnd
                  )
                )
            }
        }
    }
    method CStyleLoop(&body,&cond,&afterwards) {
        CStyleLoop.new(&body,&cond,&afterwards)
    }

    my role DelegateCountOnly[\iter] does PredictiveIterator {
        method count-only(--> Int:D) { iter.count-only }
    }

    # Takes two iterators and mixes in a role into the second iterator that
    # delegates .count-only method to the first iterator if the first is a
    # PredictiveIterator
    method delegate-iterator-opt-methods (Iterator:D \a, Iterator:D \b) {
        nqp::istype(a,PredictiveIterator)
          ?? b.^mixin(DelegateCountOnly[a])
          !! b
    }

    # Create an iterator from a source iterator that will repeat the
    # values of the source iterator indefinitely *unless* a Whatever
    # was encountered, in which case it will repeat the last seen value
    # indefinitely (even if the source iterator wasn't actually exhausted).
    # Only if the source iterator did not produce any values at all, then
    # the resulting iterator will not produce any either.
    my class DWIM does Iterator {
        has $!source;
        has $!buffer;
        has int $!ended;
        has int $!whatever;
        has int $!i;
        method !SET-SELF(\source) {
            $!source := source;
            $!buffer := nqp::create(IterationBuffer);
            self
        }
        method new(\source) { nqp::create(self)!SET-SELF(source) }

        method pull-one() is raw {
            nqp::if(
              $!ended,
              nqp::if(                          # source exhausted
                $!whatever,
                nqp::if(                        # seen a Whatever
                  nqp::elems($!buffer),
                  nqp::atpos($!buffer,          # last value seen
                    nqp::sub_i(nqp::elems($!buffer),1)),
                  Nil                           # no last value seen
                ),
                nqp::atpos($!buffer,            # not seen, so modulo repeat
                  nqp::mod_i(
                    nqp::sub_i(($!i = nqp::add_i($!i,1)),1),
                    nqp::elems($!buffer)
                  )
                )
              ),
              nqp::if(                          # source not exhausted
                nqp::eqaddr((my \value := $!source.pull-one),IterationEnd),
                nqp::stmts(                     # exhausted now
                  ($!ended = 1),
                  nqp::if(
                    nqp::iseq_i(nqp::elems($!buffer),0),
                    IterationEnd,               # nothing to repeat, done
                    self.pull-one               # last or repeat
                  )
                ),
                nqp::if(                        # got a value
                  nqp::istype(value,Whatever),
                  nqp::stmts(                   # done, repeat last value
                    ($!whatever = $!ended = 1),
                    self.pull-one,
                  ),
                  nqp::stmts(                   # save / return value
                    nqp::push($!buffer,value),
                    value
                  )
                )
              )
            )
        }

        # Is the source iterator considered exhausted?
        method ended() { nqp::hllbool($!ended) }

        # Eat the iterator trying to find out the number of elements
        # produced by the iterator.  Intended to provide information
        # for error messages.
        method count-elems() {
            nqp::if(
              $!ended,
              nqp::elems($!buffer),
              nqp::stmts(
                (my int $elems = nqp::elems($!buffer)),
                nqp::until(
                  nqp::eqaddr($!source.pull-one,IterationEnd),
                  $elems = nqp::add_i($elems,1)
                ),
                $elems
              )
            )
        }
    }
    method DWIM(\source) { DWIM.new(source) }

    # Returns a sentinel Iterator object that will never generate any value.
    # Does not take a parameter.
    my class Empty does PredictiveIterator {
        method new() { nqp::create(self) }
        method pull-one(--> IterationEnd)  { }
        method push-all($ --> IterationEnd) { }
        method sink-all(--> IterationEnd)  { }
        method skip-one(--> 0) { }
        method skip-at-least($ --> 0) { }
        method count-only(--> 0) { }
        method bool-only(--> False) { }
    }
    method Empty() { BEGIN Empty.new }

    # Returns at most N items, then calls .sink-all on source. Optionally,
    # executes a Callable when either N items were returned or original iterator
    # got exhausted. N can be negative to ask for "all values".
    # This is used in several places in IO::Handle, e.g. in
    # .lines to read N lines and then close the filehandle via .sink-all
    my class FirstNThenSinkAllN does Iterator {
        has $!source;
        has int $!n;
        has int $!i;
        has &!callable;
        method pull-one() is raw {
            nqp::if(
              nqp::isle_i($!n, ($!i = nqp::add_i($!i, 1))),
              nqp::if(
                nqp::iseq_i($!i, $!n),
                nqp::stmts(
                  (my \got1 := $!source.pull-one),
                  self!FINISH-UP(
                    nqp::isfalse(nqp::eqaddr(got1, IterationEnd))),
                  got1),
                nqp::stmts(
                  $!n || self!FINISH-UP(1),
                  IterationEnd)),
              nqp::if(
                nqp::eqaddr((my \got2 := $!source.pull-one),IterationEnd),
                nqp::stmts(
                  self!FINISH-UP(0),
                  IterationEnd),
                got2))
        }
        method sink-all(--> IterationEnd) { self.FINISH-UP }
        method new(\s,\n,\c) { nqp::create(self)!SET-SELF(s,n,c) }
        method !SET-SELF($!source,$!n,&!callable) { self }
        method !FINISH-UP(\do-sink) {
            do-sink    && $!source.sink-all;
            &!callable && &!callable();
            1
        }
    }
    my class FirstNThenSinkAllCallable  does Iterator {
        has $!source;
        has &!callable;
        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr((my \got := $!source.pull-one),IterationEnd)
                && (&!callable()||1),
              IterationEnd,
              got
            )
        }
        method sink-all(--> IterationEnd) {
            $!source.sink-all;
            &!callable();
        }
        method new(\s,\c) { nqp::create(self)!SET-SELF(s,c) }
        method !SET-SELF($!source,&!callable) { self }
    }
    method FirstNThenSinkAll(\source,\n,&callable?) {
        # XXX TODO: Make this code DRYer by moving common bits to a role,
        # but currently (2017-04) assigning to `int $!n` attribute from SET-SELF
        # signature complains about immutable ints if done in a role, and
        # private methods **in roles** are slow, so we duplicated stuff here
        nqp::if(
          nqp::isge_i(n, 0),
          # only want N pull's
          FirstNThenSinkAllN.new(source,n,&callable),
          nqp::if(
            # want it all
            &callable,
            # want it all with callable
            FirstNThenSinkAllCallable.new(source,&callable),
            # want it all without callable
            source
          )
        )
    }

    # Return an iterator that will cache a source iterator for the index
    # values that the index iterator provides, from a given offest in the
    # cached source iterator.  Values from the index iterator below the
    # offset, are considered to be illegal and will throw.  Also takes an
    # optional block to be called when an otherwise out-of-bounds index
    # value is given by the index iterator: if not given, Nil will be
    # returned for such index values.
    my class FromIndexes does Iterator {
        has $!source;
        has $!indexes;
        has int $!offset;
        has &!out;
        has $!cache;
        method !SET-SELF($!source,$!indexes,\offset,&!out) {
            $!cache := nqp::setelems(nqp::list,$!offset = offset);
            self
        }
        method new(\s,\i,\o,\out) { nqp::create(self)!SET-SELF(s,i,o,out) }
        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr((my $got := $!indexes.pull-one),IterationEnd),
              IterationEnd,
              nqp::if(
                nqp::istype(                      # doesn't look like int
                  (my $number = +$got),Failure),
                $number.throw,
                nqp::if(                          # out of range
                  nqp::islt_i((my int $index = $number.Int),$!offset),
                  X::OutOfRange.new(:$got,:range("$!offset..^Inf")).throw,
                  nqp::if(
                    nqp::existspos($!cache,$index),
                    nqp::atpos($!cache,$index),   # it's in the cache
                    nqp::if(
                      nqp::defined($!source),
                      nqp::stmts(                 # can still search it
                        nqp::until(
                          nqp::existspos($!cache,$index)
                            || nqp::eqaddr(
                                 (my \pulled := $!source.pull-one),
                                 IterationEnd
                               ),
                          nqp::push($!cache,pulled)
                        ),
                        nqp::if(
                          nqp::eqaddr(pulled,IterationEnd),
                          nqp::stmts(
                            ($!source := Mu),
                            nqp::if(
                              $!indexes.is-lazy,
                              IterationEnd,       # not going to be any more
                              nqp::stmts(         # didn't find it
                                nqp::if(&!out,&!out($index)),
                                Nil
                              )
                            )
                          ),
                          pulled                  # found it
                        )
                      ),
                      nqp::stmts(                 # cannot be found
                        nqp::if(&!out,&!out($index)),
                        Nil
                      )
                    )
                  )
                )
              )
            )
        }
        method is-lazy() { $!source.is-lazy && $!indexes.is-lazy }
    }
    method FromIndexes(\source,\indexes,\offset,&out?) {
        FromIndexes.new(source,indexes,offset,&out)
    }

    # Return an iterator for the basic "gather" functionality on the
    # given block.
    my class Gather does SlippyIterator {
        has &!resumption;
        has $!push-target;
        has int $!wanted;

        my constant PROMPT = nqp::create(Mu);

        method new(&block) {
            my \iter = nqp::create(self);
            my int $wanted;
            my $taken;
            my $taker := {
                nqp::stmts(
                  ($taken := nqp::getpayload(nqp::exception())),
                  nqp::if(
                    nqp::istype($taken, Slip),
                    nqp::stmts(
                      iter!start-slip-wanted($taken),
                      ($wanted = nqp::getattr_i(iter, self, '$!wanted'))
                    ),
                    nqp::stmts(  # doesn't sink
                      nqp::getattr(iter, self, '$!push-target').push($taken),
                      ($wanted = nqp::bindattr_i(iter,self,'$!wanted',
                        nqp::sub_i(nqp::getattr_i(iter,self,'$!wanted'),1)))
                    )
                  ),
                  nqp::if(
                    nqp::iseq_i($wanted,0),
                    nqp::continuationcontrol(0, PROMPT, -> Mu \c {
                        nqp::bindattr(iter, self, '&!resumption', c);
                    })
                  ),
                  nqp::resume(nqp::exception())
                )
            }
            nqp::bindattr(iter, self, '&!resumption', {
                nqp::stmts(  # doesn't sink
                  nqp::handle(&block(), 'TAKE', $taker()),
                  nqp::continuationcontrol(0, PROMPT, -> | {
                      nqp::bindattr(iter, self, '&!resumption', Callable)
                  })
                )
            });
            iter
        }

        method pull-one() is raw {
            nqp::if(
              $!slipping && nqp::not_i(
                nqp::eqaddr((my \result = self.slip-one),IterationEnd)
              ),
              result,
              nqp::stmts(
                nqp::unless(
                  nqp::isconcrete($!push-target),
                  ($!push-target := nqp::create(IterationBuffer))
                ),
                ($!wanted = 1),
                nqp::continuationreset(PROMPT, &!resumption),
                nqp::if(
                  nqp::isconcrete(&!resumption),
                  nqp::shift($!push-target),
                  IterationEnd
                )
              )
            )
        }

        method push-exactly(\target, int $n) {
            nqp::if(
              nqp::isgt_i($n,0),
              nqp::stmts(
                ($!wanted = $n),
                ($!push-target := target),
                nqp::if(
                  $!slipping && nqp::not_i(
                    nqp::eqaddr(self!slip-wanted,IterationEnd)
                  ),
                  nqp::stmts(
                    ($!push-target := nqp::null),
                    $n
                  ),
                  nqp::stmts(
                    nqp::continuationreset(PROMPT, &!resumption),
                    ($!push-target := nqp::null),
                    nqp::if(
                      nqp::isconcrete(&!resumption),
                      ($n - $!wanted),
                      IterationEnd
                    )
                  )
                )
              )
            )
        }

        method !start-slip-wanted(\slip --> Nil) {
            my $value := self.start-slip(slip);
            nqp::unless(
              nqp::eqaddr($value,IterationEnd),
              nqp::stmts(  # doesn't sink
                $!push-target.push($value),
                (my int $i = 0),
                (my int $n = $!wanted),
                nqp::while(  # doesn't sink
                  nqp::islt_i($i = nqp::add_i($i,1),$n),
                  nqp::if(
                    nqp::eqaddr(($value := self.slip-one),IterationEnd),
                    last
                  ),
                  $!push-target.push($value)
                ),
                ($!wanted = $!wanted - $i)
              )
            )
        }

        method !slip-wanted() {
            my int $i = -1;
            my int $n = $!wanted;
            my $value;
            nqp::while(
              nqp::islt_i($i = nqp::add_i($i,1),$n),
              nqp::stmts(  # doesn't sink
                nqp::if(
                  nqp::eqaddr(($value := self.slip-one),IterationEnd),
                  last
                ),
                $!push-target.push($value)
              )
            );
            $!wanted = nqp::sub_i($!wanted,$i);
            nqp::if(
              nqp::eqaddr($value,IterationEnd),
              IterationEnd,
              $n
            )
        }
    }
    method Gather(&block) { Gather.new(&block) }

    # Return an iterator for the given low/high integer value (inclusive).
    # Has dedicated .push-all for those cases one needs to fill a list
    # with consecutive numbers quickly.
    my class IntRange does PredictiveIterator {
        has int $!i;
        has int $!last;
        has $!is-lazy;

        method !SET-SELF(int $i, $last) {
            nqp::stmts(
              ($!i    = nqp::sub_i($i,1)),
              ($!last = nqp::if(
                ($!is-lazy := $last == Inf),
                int.Range.max,
                $last
              )),
              self
            )
        }
        method new(\f,\t) { nqp::create(self)!SET-SELF(f,t) }

        method pull-one() {
            nqp::if(
              nqp::isle_i(($!i = nqp::add_i($!i,1)),$!last),
              $!i,
              IterationEnd
            )
        }
        method skip-one() { nqp::isle_i(($!i = nqp::add_i($!i,1)),$!last) }
        method push-exactly(\target, int $batch-size) {
            nqp::stmts(
              (my int $todo = nqp::add_i($batch-size,1)),
              (my int $i    = $!i),      # lexicals are faster than attrs
              (my int $last = $!last),
              nqp::while(
                ($todo = nqp::sub_i($todo,1))
                  && nqp::isle_i(($i = nqp::add_i($i,1)),$last),
                target.push(nqp::p6box_i($i))
              ),
              ($!i = $i),                # make sure pull-one ends
              nqp::if(
                nqp::isgt_i($i,$last),
                IterationEnd,
                $batch-size
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              (my int $i    = $!i),      # lexicals are faster than attrs
              (my int $last = $!last),
              nqp::while(
                nqp::isle_i(($i = nqp::add_i($i,1)),$last),
                target.push(nqp::p6box_i($i))
              ),
              ($!i = $i),                # make sure pull-one ends
            )
        }
        method is-lazy(--> Bool:D) { $!is-lazy }
        method count-only(--> Int:D) {
            nqp::p6box_i($!last - $!i + nqp::isgt_i($!i,$!last))
        }
        method sink-all(--> IterationEnd) { $!i = $!last }
    }
    method IntRange(\from,\to) { IntRange.new(from,to) }

    # Return an iterator from a given iterator producing Pairs, in which
    # each .value is checked for iterability: if Iterable, produce Pairs
    # with the original key as its value, and key with the values produced
    # by the Iterable.  Otherwise, just produce an antipair.
    my class Invert does Iterator {
        has $!iterator;   # source iterator
        has $!value;      # original key to repeat for Iterable
        has $!slipper;    # iterator if Iterable value in source

        method new(\iterator) {
            nqp::p6bindattrinvres(
              nqp::create(self),self,'$!iterator',iterator)
        }
        method pull-one() {
            nqp::if(
              $!slipper,                            # we have a slipper
              nqp::if(
                nqp::eqaddr(
                  (my $pulled := $!slipper.pull-one),
                  IterationEnd
                ),
                nqp::stmts(                         # slipper exhausted
                  ($!slipper := nqp::null),         # deny all knowledge
                  self.pull-one                     # rinse and repeat
                ),
                Pair.new($pulled,$!value)           # not the end, slip it
              ),
              nqp::if(                              # no slipper
                nqp::eqaddr(
                  ($pulled := nqp::decont($!iterator.pull-one)),
                  IterationEnd
                ),
                IterationEnd,                       # source exhausted
                nqp::if(                            # still in business
                  nqp::istype($pulled,Pair),
                  nqp::if(                          # it's a Pair, whee!
                    nqp::istype(
                      (my $key := nqp::getattr($pulled,Pair,'$!value')),
                      Iterable
                    ),
                    nqp::stmts(                     # need to slip it!
                      ($!slipper := $key.iterator), # set up the slipper
                      ($!value := nqp::getattr($pulled,Pair,'$!key')),
                      self.pull-one                 # rinse and repeat
                    ),
                    Pair.new(                       # just needs swapping
                      $key,
                      nqp::getattr($pulled,Pair,'$!key')
                    )
                  ),
                  X::TypeCheck.new(                 # naughty, slap it!
                    operation => 'invert',
                    got       => $pulled,
                    expected  => Pair
                  ).throw
                )
              )
            )
        }
        method is-lazy() { $!iterator.is-lazy }
        method sink-all(--> IterationEnd) {
            nqp::until(
              nqp::eqaddr((my \pulled := $!iterator.pull-one),IterationEnd),
              nqp::unless(
                nqp::istype(pulled,Pair),
                X::TypeCheck.new(                   # naughty, slap it!
                  operation => 'invert',
                  got       => pulled,
                  expected  => Pair
                ).throw
              )
            )
        }
    }
    method Invert(\iterator) { Invert.new(iterator) }

    # Return an iterator that will alternately generate an index value,
    # and the value of the given iterator, basically the .kv functionality
    # for 1 dimensional lists.
    my class KeyValue does Iterator {
        has Mu $!iter;
        has Mu $!pulled;
        has int $!on-key;
        has int $!key;

        method !SET-SELF(\iter) { $!iter := iter; $!key = -1; self }
        method new(\iter) { nqp::create(self)!SET-SELF(iter) }

        method pull-one() is raw {
            nqp::if(
              ($!on-key = nqp::not_i($!on-key)),
              nqp::if(
                nqp::eqaddr(
                  ($!pulled := $!iter.pull-one),IterationEnd
                ),
                IterationEnd,
                nqp::p6box_i(($!key = nqp::add_i($!key,1))),
              ),
              $!pulled,
            )
        }
        method push-all(\target --> IterationEnd) {
            my int $key = -1;
            nqp::until(
              nqp::eqaddr(
                (my \pulled := $!iter.pull-one),
                IterationEnd
              ),
              nqp::stmts(
                target.push(nqp::p6box_i(($key = nqp::add_i($key,1)))),
                target.push(pulled),
              )
            )
        }
        method is-lazy() { $!iter.is-lazy }
    }
    method KeyValue(\iterator) { KeyValue.new(iterator) }

    # Create iterator for the last N values of a given iterator.  Needs
    # to specify the :action part of X::Cannot::Lazy in case the given
    # iterator is lazy.  Optionally returns an empty iterator if the
    # given iterator produced fewer than N values.
    my class LastNValues does Iterator {
        has $!iterator;
        has int $!size;
        has int $!full;
        has $!lastn;
        has int $!todo;
        has int $!index;
        method !SET-SELF(\iterator, \size, \full) {
            nqp::stmts(
              ($!iterator := iterator),
              ($!full = full),
              ($!lastn := nqp::setelems(nqp::list, $!size = size)),
              nqp::setelems($!lastn, 0),
              self
            )
        }
        method new(\iterator,\n,\action,\f) {
            nqp::if(
              iterator.is-lazy,
              X::Cannot::Lazy.new(:action(action)).throw,
              nqp::if(
                nqp::istype(n,Whatever),
                iterator,                   # * just give back itself
                nqp::if(
                  n <= 0,                   # must be HLL comparison
                  Rakudo::Iterator.Empty,   # negative is just nothing
                  nqp::if(
                    (nqp::istype(n,Int)
                      && nqp::isbig_I(nqp::decont(n)))
                      || n == Inf,
                    iterator,               # big value = itself
                    nqp::create(self)!SET-SELF(iterator,n,f)
                  )
                )
              )
            )
        }
        method next() is raw {
            nqp::stmts(
              (my int $index = $!index),
              ($!index = nqp::mod_i(nqp::add_i($!index,1),$!size)),
              ($!todo  = nqp::sub_i($!todo,1)),
              nqp::atpos($!lastn,$index)
            )
        }
        method pull-one() is raw {
            nqp::if(
              $!todo,
              self.next,
              nqp::if(
                nqp::defined($!iterator),
                nqp::stmts(
                  (my int $index),
                  (my int $size = $!size),
                  nqp::until(
                    nqp::eqaddr(
                      (my \pulled := $!iterator.pull-one),IterationEnd),
                    nqp::stmts(
                      nqp::bindpos($!lastn,$index,pulled),
                      ($index = nqp::mod_i(nqp::add_i($index,1),$size))
                    )
                  ),
                  nqp::if(
                    nqp::iseq_i(nqp::elems($!lastn),$size),   # full set
                    nqp::stmts(
                      ($!index = $index),
                      ($!todo  = $!size)
                    ),
                    ($!todo =       # not a full set, $!index still at 0
                      nqp::if($!full,0,nqp::elems($!lastn))),
                  ),
                  ($!iterator := Mu),                   # done iterating
                  nqp::if($!todo, self.next, IterationEnd)
                ),
                IterationEnd
              )
            )
        }
    }
    method LastNValues(\iterator, \n, \action, $full = 0) {
        LastNValues.new(iterator, n, action, $full)
    }

    # Return the last value of the given source iterator (if any).
    # Also needs the action string to be used in X::Cannot::Lazy if
    # the source iterator turns out to be lazy.
    method LastValue(\iterator, $action) is raw {
        nqp::if(
          iterator.is-lazy,
          X::Cannot::Lazy.new(:$action).throw,
          nqp::stmts(
            (my $result := IterationEnd),
            nqp::if(
              nqp::istype(iterator,PredictiveIterator),
              nqp::if(
                (my \count := iterator.count-only)
                  && iterator.skip-at-least(count - 1),
                $result := iterator.pull-one
              ),
              nqp::until(
                nqp::eqaddr((my \pulled := iterator.pull-one),IterationEnd),
                ($result := pulled)
              ),
            ),
            $result
          )
        )
    }

    # Return an iterator given a List and an iterator that generates
    # an IterationBuffer of indexes for each pull.  Each value is
    # is a List with the mapped elements.
    my class ListIndexes does Iterator {
        has $!list;
        has $!indexes;
        method !SET-SELF(\list,\indexes) {
            nqp::stmts(
              ($!list := nqp::getattr(list,List,'$!reified')),
              ($!indexes := indexes),
              self
            )
          }
        method new(\l,\i) { nqp::create(self)!SET-SELF(l,i) }
        method pull-one() {
            nqp::if(
              nqp::eqaddr(
                (my \buffer := $!indexes.pull-one),
                IterationEnd
              ),
              IterationEnd,
              nqp::stmts(
                (my int $elems = nqp::elems(buffer)),
                (my int $i = -1),
                nqp::while(           # repurpose buffer for result
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::bindpos(buffer,$i,
                    nqp::atpos($!list,nqp::atpos(buffer,$i))
                  )
                ),
                nqp::p6bindattrinvres(
                  nqp::create(List),List,'$!reified',buffer)
              )
            )
        }
    }
    method ListIndexes(\list,\indexes) {
        nqp::if(
          (my int $elems = list.elems),     # reifies
          ListIndexes.new(list,indexes),    # actually need to do some mapping
          Rakudo::Iterator.OneValue(nqp::create(List)) # only one
        )
    }

    # Returns an iterator that handles all properties of a bare -loop-
    # Takes a Callable to be considered the body of the loop.
    my class Loop does SlippyIterator {
        has &!body;

        method new(&body) {
            nqp::p6bindattrinvres(nqp::create(self),self,'&!body',&body)
        }

        method pull-one() {
            my $result;
            my int $stopped;
            nqp::if(
              $!slipping && nqp::not_i(
                nqp::eqaddr(($result := self.slip-one),IterationEnd)
              ),
              $result,
              nqp::stmts(
                nqp::until(
                  $stopped,
                  nqp::stmts(
                    ($stopped = 1),
                    nqp::handle(
                      nqp::if(
                        nqp::istype(($result := &!body()),Slip),
                        ($stopped = nqp::eqaddr(
                          ($result := self.start-slip($result)),
                          IterationEnd
                        ))
                      ),
                      'NEXT', ($stopped = 0),
                      'REDO', ($stopped = 0),
                      'LAST', ($result := IterationEnd)
                    )
                  ),
                  :nohandler
                ),
                $result
              )
            )
        }

        method is-lazy(--> True) { }
    }
    method Loop(&body) { Loop.new(&body) }

    # An often occurring use of the Mappy role to generate all of the
    # keys of a Map / Hash.  Takes a Map / Hash as the only parameter.
    my class Mappy-keys does Rakudo::Iterator::Mappy {
        method pull-one() {
            nqp::if(
              $!iter,
              nqp::iterkey_s(nqp::shift($!iter)),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!iter,
              target.push(nqp::iterkey_s(nqp::shift($!iter)))
            )
        }
    }
    method Mappy-keys(\map) { Mappy-keys.new(map) }

    # An often occurring use of the Mappy role to generate alternating
    # key and values of a Map/Hash in which each value is a Pair to
    # be interpreted as the actual key/value.  Takes a Map / Hash as
    # the only parameter.  Make sure class gets created at compile time,
    # to avoid global de-opt at run-time
    my class Mkvp does Mappy-kv-from-pairs { }
    method Mappy-kv-from-pairs(\map) { Mkvp.new(map) }

    # An often occurring use of the Mappy role to generate all of the
    # values of a Map / Hash.  Takes a Map / Hash as the only parameter.
    my class Mappy-values does Mappy {
        method pull-one() is raw {
            nqp::if(
              $!iter,
              nqp::iterval(nqp::shift($!iter)),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(  # doesn't sink
              $!iter,
              target.push(nqp::iterval(nqp::shift($!iter)))
            )
        }
    }
    method Mappy-values(\map) { Mappy-values.new(map) }

    # Return an iterator that will iterate over a source iterator and an
    # iterator generating monotonically increasing index values from a
    # given offset.  Optionally, call block if an out-of-sequence index
    # value is obtained, or simply ignore out of sequence index values.
    my class MonotonicIndexes does Iterator {
        has $!source;     # source iterator
        has $!indexes;    # iterator providing index values
        has int $!next;   # virtual index of next source value
        has &!out;        # callable for out of sequence values
        method !SET-SELF($!source,$!indexes,\offset,&!out) {
            $!next = offset;
            self
        }
        method new(\s,\i,\o,\out) { nqp::create(self)!SET-SELF(s,i,o,out) }
        method pull-one() is raw {
            nqp::stmts(
              nqp::until(
                nqp::eqaddr(
                  (my $got := $!indexes.pull-one),
                  IterationEnd
                ),
                nqp::if(
                  nqp::istype((my $number = +$got),Failure),
                  $number.throw,
                  nqp::if(
                    nqp::isle_i($!next,(my int $index = $number.Int)),
                    nqp::stmts(                      # possibly valid index
                      nqp::while(
                        nqp::islt_i($!next,$index) && $!source.skip-one,
                        ($!next = nqp::add_i($!next,1))
                      ),
                      (return-rw nqp::if(
                        nqp::iseq_i($!next,$index),
                        nqp::stmts(
                          ($!next = nqp::add_i($!next,1)),
                          $!source.pull-one
                        ),
                        IterationEnd
                      ))
                    ),
                    nqp::if(&!out,&!out($index,$!next)) # out of sequence
                  )
                )
              ),
              IterationEnd
            )
        }
    }
    method MonotonicIndexes(\source,\indexes,\offset,&out?) {
        MonotonicIndexes.new(source,indexes,offset,&out)
    }

    # Returns an iterator for the next N values of given iterator.
    my class NextNValues does Iterator {
        has $!iterator;
        has int $!times;
        method !SET-SELF($!iterator,$!times) { self }
        method new(\iterator,\times) {
            nqp::if(
              nqp::istype(times,Whatever),
              iterator,                   # * just give back itself
              nqp::if(
                times <= 0,               # must be HLL comparison
                Rakudo::Iterator.Empty,   # negative is just nothing
                nqp::if(
                  nqp::istype(times,Int),
                  nqp::if(
                    nqp::isbig_I(nqp::decont(times)),
                    iterator,             # big value = itself
                    nqp::create(self)!SET-SELF(iterator,times)
                  ),
                  nqp::if(
                    times == Inf,         # big value = itself
                    iterator,
                    nqp::create(self)!SET-SELF(iterator,times.Int)
                  )
                )
              )
            )
        }
        method pull-one() is raw {
            nqp::if(
              nqp::isgt_i($!times,0),
              nqp::if(
                nqp::eqaddr(
                  (my \pulled := $!iterator.pull-one),
                  IterationEnd
                ),
                nqp::stmts(
                  ($!times = 0),
                  IterationEnd
                ),
                nqp::stmts(
                  ($!times = nqp::sub_i($!times,1)),
                  pulled
                )
              ),
              IterationEnd
            )
        }
    }
    method NextNValues(\iterator,\times) { NextNValues.new(iterator,times) }

    # Return an iterator that only will return the given value once.
    # Basically the same as 42 xx 1.
    my class OneValue does Iterator {
        has Mu $!value;
        method new(Mu \value) {
            nqp::p6bindattrinvres(nqp::create(self),self,'$!value',value)
        }
        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr($!value,IterationEnd),
              IterationEnd,
              nqp::stmts(
                (my Mu $value := $!value),
                ($!value := IterationEnd),
                $value
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              nqp::unless(
                nqp::eqaddr($!value,IterationEnd),
                target.push($!value)
              ),
              ($!value := IterationEnd)
            )
        }
        method skip-one() {
            nqp::if(
              nqp::not_i(nqp::eqaddr($!value,IterationEnd)),
              nqp::isfalse($!value := IterationEnd)
            )
        }
        method sink-all(--> IterationEnd) {
            $!value := IterationEnd
        }
    }
    method OneValue(Mu \value) { OneValue.new(value) }

    # Return an iterator that only will return the given value for the
    # given number of times.  Basically the same as 42 xx N.
    my class OneValueTimes does PredictiveIterator {
        has Mu $!value;
        has Int $!times;
        has int $!is-lazy;

        method !SET-SELF(Mu \value,\times) {
            nqp::stmts(
              ($!value := value),
              ($!times  = times),
              ($!is-lazy = nqp::isbig_I(nqp::decont(times))),
              self
            )
        }
        method new(Mu \value,\times) {
            nqp::if(
              times > 0,
              nqp::create(self)!SET-SELF(value,times),
              Rakudo::Iterator.Empty
            )
        }
        method pull-one() is raw {
            nqp::if(
              $!times,
              nqp::stmts(
                --$!times,
                $!value
              ),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!times,
              nqp::stmts(
                --$!times,
                target.push($!value)
              )
            )
        }
        method skip-one() { nqp::if($!times,$!times--) }
        method is-lazy() { nqp::hllbool($!is-lazy) }
        method count-only(--> Int:D) { $!times }
        method sink-all(--> IterationEnd) { $!times = 0 }
    }
    method OneValueTimes(Mu \value,\times) { OneValueTimes.new(value,times) }

    # Return an iterator that will generate a pair with the index as the
    # key and as value the value of the given iterator, basically the
    # .pairs functionality on 1 dimensional lists.
    my class PairIterator does Iterator {
        has Mu $!iter;
        has int $!key;

        method !SET-SELF(\iter) { $!iter := iter; $!key = -1; self }
        method new(\iter) { nqp::create(self)!SET-SELF(iter) }

        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr((my $pulled := $!iter.pull-one),IterationEnd),
              IterationEnd,
              Pair.new(($!key = nqp::add_i($!key,1)),$pulled)
            )
        }
        method push-all(\target --> IterationEnd) {
            my $pulled;
            my int $key = -1;
            nqp::until(
              nqp::eqaddr(($pulled := $!iter.pull-one),IterationEnd),
              target.push(Pair.new(($key = nqp::add_i($key,1)),$pulled))
            )
        }
        method is-lazy() { $!iter.is-lazy }
    }
    method Pair(\iterator) { PairIterator.new(iterator) }

    # Return an iterator for a given number of permutations.  Also specify
    # whether an IterationBuffer should be returned for each iteration (1),
    # or a List (0).  Basically the workhorse of permutations.
    my class Permutations does PredictiveIterator {
        has int $!n;
        has int $!b;
        has int $!todo;
        has int $!elems;
        has $!next;
        method !SET-SELF(int $n, int $b) {
            nqp::stmts(
              ($!n = $n),
              ($!b = $b),
              ($!todo = 1),
              (my int $i = 1),
              nqp::while(
                nqp::isle_i(($i = nqp::add_i($i,1)),$n),
                ($!todo = nqp::mul_i($!todo,$i))
              ),
              ($!elems = $!todo),
              ($!next :=
                nqp::setelems(nqp::create(IterationBuffer),$n)),
              ($i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$n),
                nqp::bindpos($!next,$i,nqp::clone($i))
              ),
              self
            )
        }
        method new(\n,\b) { nqp::create(self)!SET-SELF(n,b) }
        method pull-one {
            nqp::if(
              nqp::isge_i(($!todo = nqp::sub_i($!todo,1)),0),
              nqp::stmts(
                (my $permuted := nqp::clone($!next)),
                nqp::if(
                  $!todo,     # need to calculate next one
                  nqp::stmts( # largest index k such that a[k] < a[k+1]
                    (my int $k = nqp::sub_i($!n,2)),
                    nqp::until(
                      nqp::islt_i(
                        nqp::atpos($!next,$k),
                        nqp::atpos($!next,nqp::add_i($k,1))
                      ),
                      ($k = nqp::sub_i($k,1)),
                    ),
                    (my int $l = nqp::sub_i($!n,1)),
                    nqp::until(
                      nqp::islt_i( # largest index l>k where a[k] < a[l]
                        nqp::atpos($!next,$k),
                        nqp::atpos($!next,$l)
                      ),
                      ($l = nqp::sub_i($l,1))
                    ),
                    (my $tmp := nqp::atpos($!next,$k)),
                    nqp::bindpos($!next,$k,nqp::atpos($!next,$l)),
                    nqp::bindpos($!next,$l,$tmp)
                  )
                ),
                ($l = $!n),
                nqp::until(
                  nqp::isge_i(
                    ($k = nqp::add_i($k,1)),
                    ($l = nqp::sub_i($l,1))
                  ),
                  nqp::stmts(
                    ($tmp := nqp::atpos($!next,$k)),
                    nqp::bindpos($!next,$k,nqp::atpos($!next,$l)),
                    nqp::bindpos($!next,$l,$tmp)
                  )
                ),
                nqp::if(
                  $!b,
                  $permuted,
                  $permuted.List
                )
              ),
              IterationEnd
            )
        }
        method count-only(--> Int:D) {
            nqp::p6box_i(nqp::isgt_i($!todo,0) && $!todo)
        }
    }
    method Permutations($n, int $b) {
        nqp::if(
          $n > nqp::if(nqp::iseq_i($?BITS,32),13,20),  # must be HLL comparison
          (die "Cowardly refusing to permutate more than {
              $?BITS == 32 ?? 13 !! 20
          } elements, tried $n"),
          nqp::if(
            $n < 1,                                    # must be HLL comparison
            Rakudo::Iterator.OneValue(
              nqp::create(nqp::if($b,IterationBuffer,List))
            ),
            # See:  L<https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order>
            Permutations.new($n,$b)
          )
        )
    }

    # Return an iterator for an Array that has been completely reified
    # already.  Returns a assignable container for elements don't exist
    # before the end of the reified array.
    my class ReifiedArrayIterator does PredictiveIterator {
        has $!reified;
        has $!descriptor;
        has int $!i;

        method !SET-SELF(\array, Mu \des) {
            nqp::stmts(
              ($!reified    := nqp::getattr(array, List,  '$!reified')),
              ($!descriptor := des),
              ($!i = -1),
              self
            )
        }
        method new(\arr, Mu \des) { nqp::create(self)!SET-SELF(arr, des) }

        method !hole(int $i) is raw {
            nqp::p6scalarfromdesc(ContainerDescriptor::BindArrayPos.new(
                $!descriptor, $!reified, $i))
        }
        method pull-one() is raw {
            nqp::ifnull(
              nqp::atpos($!reified,$!i = nqp::add_i($!i,1)),
              nqp::if(
                nqp::islt_i($!i,nqp::elems($!reified)), # found a hole
                self!hole($!i),
                IterationEnd
              )
            )
        }
        method push-exactly(\target, int $batch-size) {
            nqp::stmts(
              (my int $todo = nqp::add_i($batch-size,1)),
              (my int $i    = $!i),      # lexicals are faster than attrs
              (my int $elems = nqp::elems($!reified)),
              nqp::while(
                ($todo = nqp::sub_i($todo,1))
                  && nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                target.push(
                  nqp::ifnull(nqp::atpos($!reified,$i),self!hole($i))
                )
              ),
              ($!i = $i),                # make sure pull-one ends
              nqp::if(
                nqp::isge_i($i,$elems),
                IterationEnd,
                $batch-size
              )
            )
        }

        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              (my int $elems = nqp::elems($!reified)),
              (my int $i = $!i),
              nqp::while(   # doesn't sink
                nqp::islt_i($i = nqp::add_i($i,1),$elems),
                target.push(
                  nqp::ifnull(nqp::atpos($!reified,$i),self!hole($i))
                )
              ),
              ($!i = $i)
            )
        }
        method skip-one() {
            nqp::islt_i(
              ($!i = nqp::add_i($!i,1)),
              nqp::elems($!reified)
            )
        }
        method skip-at-least(Int:D $toskip) {
            nqp::unless(
              $toskip <= 0,  # must be HLL
              nqp::stmts(
                ($!i = nqp::if(
                  $!i + $toskip < nqp::elems($!reified),  # must be HLL
                  nqp::add_i($!i,$toskip),
                  nqp::elems($!reified)
                )),
                nqp::islt_i($!i,nqp::elems($!reified))
              )
            )
        }
        method count-only(--> Int:D) {
            nqp::p6box_i(
              nqp::elems($!reified)
                - $!i
                - nqp::islt_i($!i,nqp::elems($!reified))
            )
        }
        method sink-all(--> IterationEnd) { $!i = nqp::elems($!reified) }
    }
    method ReifiedArray(\array, Mu \descriptor) {
        ReifiedArrayIterator.new(array, descriptor)
    }

    # Return an iterator for a List that has been completely reified
    # already.  Returns an nqp::null for elements that don't exist
    # before the end of the reified list.
    my class ReifiedListIterator does Iterator {
        has $!reified;
        has int $!i;

        method !SET-SELF(\list) {
            nqp::stmts(
              ($!reified := nqp::if(
                nqp::istype(list,List),
                nqp::getattr(list,List,'$!reified'),
                list)),
              ($!i = -1),
              self
            )
        }
        method new(\list) { nqp::create(self)!SET-SELF(list) }

        method pull-one() is raw {
            nqp::ifnull(
              nqp::atpos($!reified,$!i = nqp::add_i($!i,1)),
              nqp::if(
                nqp::islt_i($!i,nqp::elems($!reified)), # found a hole
                nqp::null,                              # it's a hole
                IterationEnd                            # it's the end
              )
            )
        }
        method push-exactly(\target, int $batch-size) {
            nqp::stmts(
              (my int $todo = nqp::add_i($batch-size,1)),
              (my int $i    = $!i),      # lexicals are faster than attrs
              (my int $elems = nqp::elems($!reified)),
              nqp::while(
                ($todo = nqp::sub_i($todo,1))
                  && nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                target.push(nqp::atpos($!reified,$i))
              ),
              ($!i = $i),                # make sure pull-one ends
              nqp::if(
                nqp::isge_i($i,$elems),
                IterationEnd,
                $batch-size
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              (my int $elems = nqp::elems($!reified)),
              (my int $i = $!i), # lexicals are faster than attributes
              nqp::while(  # doesn't sink
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                target.push(nqp::atpos($!reified,$i))
              ),
              ($!i = $i)
            )
        }
        method skip-one() {
            nqp::islt_i(
              ($!i = nqp::add_i($!i,1)),
              nqp::elems($!reified)
            )
        }
        method skip-at-least(Int:D $toskip) {
            nqp::unless(
              $toskip <= 0,  # must be HLL
              nqp::stmts(
                ($!i = nqp::if(
                  $!i + $toskip < nqp::elems($!reified),  # must be HLL
                  nqp::add_i($!i,$toskip),
                  nqp::elems($!reified)
                )),
                nqp::islt_i($!i,nqp::elems($!reified))
              )
            )
        }
        method count-only(--> Int:D) {
            nqp::p6box_i(
              nqp::elems($!reified)
                - $!i
                - nqp::islt_i($!i,nqp::elems($!reified))
            )
        }
        method sink-all(--> IterationEnd) { $!i = nqp::elems($!reified) }
    }
    method ReifiedList(\list) {
        ReifiedListIterator.new(list)
    }

    # Return a lazy iterator that will repeat the values of a given
    # source iterator indefinitely.  Even when given a lazy iterator,
    # it will cache the values seen to handle case that the iterator
    # will exhaust after all.  Only if the source iterator did not
    # produce any values at all, then the returned iterator will not
    # produce any either.
    my class Repeat does Iterator {
        has $!iterator;
        has $!reified;
        has int $!i;
        method !SET-SELF(\iterator) {
            nqp::stmts(
              ($!iterator := iterator),
              ($!reified  := nqp::create(IterationBuffer)),
              self
            )
        }
        method new(\iter) { nqp::create(self)!SET-SELF(iter) }
        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr($!iterator,IterationEnd),
              nqp::atpos(                # supplying from cache
                $!reified,
                nqp::mod_i(
                  ($!i = nqp::add_i($!i,1)),
                  nqp::elems($!reified)
                )
              ),
              nqp::if(                   # supplying from iterator
                nqp::eqaddr(
                  (my \pulled := $!iterator.pull-one),
                  IterationEnd
                ),
                nqp::if(
                  nqp::elems($!reified),
                  nqp::stmts(            # exhausted, something in cache
                    ($!iterator := IterationEnd),
                    nqp::atpos($!reified,0)
                  ),
                  IterationEnd           # exhausted, nothing in cache
                ),
                nqp::push(               # cache and supply
                  $!reified,
                  pulled
                )
              )
            )
        }
        method is-lazy(--> True) { }     # we're lazy, always
    }
    method Repeat(\iterator) { Repeat.new(iterator) }

    # Returns an iterator that handles all properties of a -repeat- with
    # a condition.  Takes a Callable to be considered the body of the loop,
    # and a Callable for the condition..
    my class RepeatLoop does SlippyIterator {
        has $!body;
        has $!cond;
        has int $!skip;

        method !SET-SELF(\body,\cond) {
            nqp::stmts(
              ($!body := body),
              ($!cond := cond),
              ($!skip = 1),
              self
            )
        }
        method new(\body,\cond) {
            nqp::create(self)!SET-SELF(body,cond)
        }

        method pull-one() {
            if $!slipping && nqp::not_i(
                nqp::eqaddr((my $result := self.slip-one),IterationEnd)
            ) {
                $result
            }
            else {
                nqp::if(
                  $!skip || $!cond(),
                  nqp::stmts(
                    ($!skip = 0),
                    nqp::until(
                      (my int $stopped),
                      nqp::stmts(
                        ($stopped = 1),
                        nqp::handle(
                          nqp::if(
                            nqp::istype(($result := $!body()),Slip),
                            ($stopped = nqp::eqaddr(
                              ($result := self.start-slip($result)),
                              IterationEnd
                            ) && nqp::if($!cond(),0,1))
                          ),
                          'NEXT', ($stopped = nqp::if($!cond(),0,1)),
                          'REDO', ($stopped = 0),
                          'LAST', ($result := IterationEnd)
                        )
                      ),
                      :nohandler
                    ),
                    $result
                  ),
                  IterationEnd
                )
            }
        }
    }
    method RepeatLoop(&body, &cond) { RepeatLoop.new(&body,&cond) }

    # Return an iterator that rotorizes the given iterator with the
    # given cycle.  If the cycle is a Cool, then it is assumed to
    # be a single Int value to R:It.Batch with.  Otherwise it is
    # considered to be something Iterable that will be repeated
    # until the source iterator is exhausted.  The third parameter
    # indicates whether a partial result is acceptable when the
    # source iterator is exhausted.
    my class Rotor does Iterator {
        has $!iterator;
        has $!cycle;
        has $!buffer;
        has int $!complete;
        has int $!is-exhausted;
        method !SET-SELF(\iterator,\cycle,\partial) {
            nqp::stmts(
              ($!iterator := iterator),
              ($!cycle    := Rakudo::Iterator.Repeat(cycle.iterator)),
              ($!buffer   := nqp::create(IterationBuffer)),
              ($!complete  = !partial),
              self
            )
        }
        method new(\iterator,\cycle,\partial) {
            nqp::if(
              nqp::istype(cycle,Iterable),
              nqp::create(self)!SET-SELF(iterator,cycle,partial),
              Rakudo::Iterator.Batch(iterator,cycle,partial)
            )
        }
        method pull-one() is raw {
          nqp::if(
            $!is-exhausted,
            IterationEnd,
            nqp::stmts(
              nqp::if(
                nqp::istype((my $todo := $!cycle.pull-one),Pair),
                nqp::stmts(
                  (my $size := $todo.key),
                  nqp::if(
                    nqp::istype($size,Whatever),
                    nqp::stmts(                    # eat everything
                      (my int $elems = -1),
                      ($!complete = 0)
                    ),
                    nqp::if(
                      $size < 1,                   # must be HLL comparison
                      X::OutOfRange.new(
                        what    => "Rotorizing sublist length is",
                        got     => $size,
                        range   => "1..^Inf",
                      ).throw,
                      nqp::if(
                        $size == Inf || (
                          nqp::istype($size,Int)
                            && nqp::isbig_I(nqp::decont($size))
                        ),
                        nqp::stmts(                # eat everything
                          ($elems = -1),
                          ($!complete = 0)
                        ),
                        nqp::if(
                          nqp::isle_i(
                            nqp::add_i(
                              ($elems = $size.Int),
                              (my int $gap = $todo.value.Int)
                            ),
                            -1
                          ),
                          X::OutOfRange.new(       # gap out of range
                            what    => "Rotorizing gap is",
                            got     => $gap,
                            range   => "-$elems..^Inf",
                            comment => "\nEnsure a negative gap is not larger than the length of the sublist",
                          ).throw
                        )
                      )
                    )
                  )
                ),
                nqp::if(                           # just a size
                  nqp::istype($todo,Whatever),
                  nqp::stmts(                      # eat everything
                    ($elems = -1),
                    ($!complete = 0)
                  ),
                  nqp::if(
                    $todo < 1,                     # must be HLL comparison
                    X::OutOfRange.new(             # size out of range
                      what    => "Rotorizing sublist length is",
                      got     => $todo,
                      range   => "1..^Inf",
                      comment => "\nDid you mean to specify a Pair with => $todo?"
                    ).throw,
                    nqp::if(
                      (nqp::istype($todo,Int)
                        && nqp::isbig_I(nqp::decont($todo)))
                        || $todo == Inf,
                      nqp::stmts(                  # eat everything
                        ($elems = -1),
                        ($!complete = 0)
                      ),
                      ($elems = $todo.Int)
                    )
                  )
                )
              ),
              nqp::until(                          # fill the buffer
                (nqp::isge_i(nqp::elems($!buffer),$elems)
                  && nqp::isne_i($elems,-1))       # eat everything
                  || nqp::eqaddr(
                       (my $pulled := $!iterator.pull-one),
                       IterationEnd
                     ),
                nqp::push($!buffer,$pulled)
              ),
              nqp::if(
                  nqp::iseq_i($elems,-1),
                  ($elems = nqp::elems($!buffer))
              ),
              nqp::if(
                nqp::not_i(nqp::elems($!buffer))
                  || (nqp::eqaddr($pulled,IterationEnd)
                       && ($!is-exhausted = 1)
                       && $!complete
                       && nqp::islt_i(nqp::elems($!buffer),$elems)
                     ),
                IterationEnd,                      # done
                nqp::if(
                  nqp::islt_i($gap,0),
                  nqp::stmts(                      # keep some for next
                    (my $result := nqp::clone($!buffer).List),
                    nqp::if(
                      nqp::islt_i(nqp::elems($!buffer),$elems),
                      nqp::setelems($!buffer,0),   # was :partial, now done
                      nqp::splice($!buffer,$empty,0,nqp::add_i($elems,$gap))
                    ),
                    $result
                  ),
                  nqp::stmts(
                    nqp::if(
                      nqp::isgt_i($gap,0),
                      $!iterator.skip-at-least($gap) # need to skip a few
                    ),
                    nqp::if(
                      nqp::isle_i(nqp::elems($!buffer),$elems),
                      nqp::stmts(                    # whole buffer ok
                        ($result := $!buffer.List),
                        ($!buffer := nqp::create(IterationBuffer))
                      ),
                      nqp::stmts(                    # partial buffer ok
                        ($result := nqp::p6bindattrinvres(
                          nqp::create(List),List,'$!reified',
                          nqp::splice(
                            nqp::clone($!buffer),
                            $empty,
                            $elems,
                            nqp::sub_i(nqp::elems($!buffer),$elems)
                          )
                        )),
                        nqp::splice($!buffer,$empty,0,$elems)
                      )
                    ),
                    $result
                  )
                )
              )
            )
          )
        }
        method is-lazy() { $!iterator.is-lazy }
    }
    method Rotor(\iterator,\cycle,\partial) {
        Rotor.new(iterator,cycle,partial)
    }

    # Return an iterator that will roundrobin the given iterables
    # (with &[,]).  Basically the functionality of roundrobin(@a,@b)
    my class RoundrobinIterables does Iterator {
        has $!iters;
        has int $!lazy;
        method !SET-SELF(\iterables) {
            nqp::stmts(
              (my $iterables := nqp::getattr(iterables,List,'$!reified')),
              (my int $elems = nqp::elems($iterables)),
              ($!iters := nqp::setelems(nqp::list,$elems)),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos($!iters,$i,
                  nqp::if(
                    nqp::iscont(my $elem := nqp::atpos($iterables,$i)),
                    Rakudo::Iterator.OneValue($elem),
                    nqp::stmts(
                      nqp::if($elem.is-lazy,($!lazy = 1)),
                      $elem.iterator
                    )
                  )
                )
              ),
              self
            )
        }
        method new(\iterables) { nqp::create(self)!SET-SELF(iterables) }
        method pull-one() {
            nqp::if(
              nqp::eqaddr($!iters,IterationEnd),
              IterationEnd,
              nqp::stmts(
                (my int $i = -1),
                (my int $elems = nqp::elems($!iters)),
                (my $buf := nqp::create(IterationBuffer)),
                nqp::until(
                  nqp::iseq_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::eqaddr(
                      (my $pulled := nqp::atpos($!iters,$i).pull-one),
                      IterationEnd
                    ),
                    nqp::stmts(          # remove exhausted iterator
                      nqp::splice($!iters,$empty,$i,1),
                      ($i = nqp::sub_i($i,1)),
                      ($elems = nqp::sub_i($elems,1))
                    ),
                    nqp::push($buf,$pulled)
                  )
                ),
                nqp::if(
                  nqp::elems($buf),
                  $buf.List,
                  ($!iters := IterationEnd),            # we're done
                )
              )
            )
        }
        method is-lazy() { nqp::hllbool($!lazy) }
    }
    method RoundrobinIterables(@iterables) {
        nqp::if(
          nqp::isgt_i((my int $n = @iterables.elems),1),  # reifies
          RoundrobinIterables.new(@iterables),
          nqp::if(
            nqp::iseq_i($n,0),
            Rakudo::Iterator.Empty,
            nqp::atpos(nqp::getattr(@iterables,List,'$!reified'),0).iterator
          )
        )
    }

    # Return an iterator from a source iterator that is supposed to
    # generate iterators. As soon as an iterator is exhausted, the next iterator
    # will be fetched and iterated over until exhausted.
    my class SequentialIterators does Iterator {
        has $!source;
        has $!current;
        method !SET-SELF(\source) {
            nqp::stmts(
              ($!current := ($!source := source).pull-one),
              self
            )
        }
        method new(\source) { nqp::create(self)!SET-SELF(source) }
        method pull-one() {
            nqp::if(
              nqp::eqaddr($!current,IterationEnd),
              IterationEnd,
              nqp::if(
                nqp::eqaddr(
                  (my \pulled := $!current.pull-one),
                  IterationEnd
                ),
                nqp::stmts(
                  ($!current := $!source.pull-one),
                  self.pull-one
                ),
                pulled
              )
            )
        }
    }
    method SequentialIterators(\source) { SequentialIterators.new(source) }

    # Return an iterator that generates all possible keys of the
    # given shape.  Each value generated is a reified List.  This is
    # basically a copy of the internal engine of ShapeLeaf and
    # ShapeBranchi roles, but without any additional processing.
    # Intended for ad-hoc iterators that feed .AT-POS on shaped lists.
    my class ShapeIndex does Iterator {
        has $!dims;
        has $!indices;
        has int $!maxdim;
        has int $!max;

        method !SET-SELF(\shape) {
            nqp::stmts(
              ($!dims := nqp::getattr(nqp::decont(shape),List,'$!reified')),
              (my int $dims = nqp::elems($!dims)),
              ($!indices :=
                nqp::setelems(nqp::create(IterationBuffer),$dims)),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$dims),
                nqp::bindpos($!indices,$i,0)
              ),
              ($!maxdim = nqp::sub_i($dims,1)),
              ($!max    = nqp::atpos($!dims,$!maxdim)),
              self
            )
        }
        method new(\shape) { nqp::create(self)!SET-SELF(shape) }

        method pull-one() is raw {
            nqp::if(
              $!indices,
              nqp::stmts(                      # still iterating
                (my $buf := nqp::clone($!indices)),
                nqp::if(
                  nqp::islt_i(                        (my int $i =
                      nqp::add_i(nqp::atpos($!indices,$!maxdim),1)),
                    $!max
                  ),
                  nqp::bindpos($!indices,$!maxdim,$i),    # ready for next
                  nqp::stmts(                             # done for now
                    (my int $level = $!maxdim),
                    nqp::until(                           # update indices
                      nqp::islt_i(                        # exhausted ??
                        ($level = nqp::sub_i($level,1)),0)
                        || nqp::stmts(
                        nqp::bindpos($!indices,nqp::add_i($level,1),0),
                        nqp::islt_i(
                          nqp::bindpos($!indices,$level,
                            nqp::add_i(nqp::atpos($!indices,$level),1)),
                          nqp::atpos($!dims,$level)
                        ),
                      ),
                      nqp::null
                    ),
                    nqp::if(                   # this was the last value
                      nqp::islt_i($level,0),
                      $!indices := nqp::null
                    )
                  )
                ),
                $buf.List                      # what we found
              ),
              IterationEnd                     # done iterating
            )
        }
    }
    method ShapeIndex(\shape) {
        shape.elems == 1
          ?? IntRange.new(0,shape.AT-POS(0) - 1)
          !! ShapeIndex.new(shape)
    }

    # Returns an iterator for an unbounded sequence of generic values that
    # have a .succ method to indicate the next logical value.  Takes the
    # initial.
    my class SuccFromInf does Iterator {
        has Mu $!i;

        method !SET-SELF(Mu $!i) { self }
        method new(\i) { nqp::create(self)!SET-SELF(i) }

        method pull-one() {
            my Mu $i = $!i;
            $!i = $i.succ;
            $i
        }
        method is-lazy(--> True) { }
    }
    method SuccFromInf(\i) { SuccFromInf.new(i) }

    # Returns an iterator for a range of generic values that have a
    # .succ method to indicate the next logical value.  Takes the initial
    # value, whether the final value should be excluded and the final value.
    my class SuccFromTo does Iterator {
        has Mu $!i;
        has Mu $!e;
        has int $!exclude;

        method !SET-SELF(Mu $!i, Int() $!exclude, Mu $!e) { self }
        method new(\i,\exclude,\e) {
            nqp::create(self)!SET-SELF(i,exclude,e)
        }

        method pull-one() {
            if $!exclude ?? $!i before $!e !! not $!i after $!e {
                my Mu $i = $!i;
                $!i = $i.succ;
                $i
            }
            else {
                IterationEnd
            }
        }
        method push-all(\target --> IterationEnd) {
            my Mu $i = $!i;
            my Mu $e = $!e;
            if $!exclude {
                while $i before $e {
                    target.push(nqp::clone($i));
                    $i = $i.succ;
                }
            }
            else {
                while not $i after $e {
                    target.push(nqp::clone($i));
                    $i = $i.succ;
                }
            }
            $!i = $e.succ;
        }
        method sink-all(--> IterationEnd) { $!i = $!e.succ }
    }
    method SuccFromTo(\i,\exclude,\e) { SuccFromTo.new(i,exclude,e) }

    # Returns an iterator that takes a source iterator, an iterator producing
    # Callable blocks producing trueish/falsish values, and a flag indicating
    # the initial state.  Iteration begins with taking the next Callable from
    # the iterator taking Callables.  If there's no Callable found (anymore),
    # either the result iterator will end (if the state is falsish), or the
    # iterator will pass on all future values of the source iterator (if the
    # state is truish).  Then values from the source iterator will be taken
    # and fed to the block as long as the returned values matches the state.
    # If the state if trueish, then values will be passed along.  If the
    # state if falsish, then values will be dropped.  If the value returned
    # by the Callable does not match the state, the next Callable will be
    # taken (if any) and the process will be repeated until either the source
    # iterator is exhausted, or the Callable block iterator is.
    my class Toggle does Iterator {
        has $!iter;
        has $!conds;
        has int $!on;
        has $!current; # null if passing on
        has $!done;    # IterationEnd if done

        method !SET-SELF(\iter, \conds, \on) {
            nqp::stmts(
              ($!iter  := iter),
              ($!conds := conds),
              ($!on = nqp::istrue(on)),
              ($!done := nqp::null),
              nqp::if(
                nqp::eqaddr((my $next := conds.pull-one),IterationEnd),
                nqp::if(
                  $!on,
                  ($!current := nqp::null),
                  ($!done := IterationEnd)
                ),
                ($!current := $next)
              ),
              self
            )
        }
        method new(\iter, \conds, \on) {
            nqp::create(self)!SET-SELF(iter, conds, on)
        }

        method pull-one() is raw {
            nqp::ifnull(
              $!done,                        # done if not null

              nqp::if(                       # source not exhausted yet
                nqp::eqaddr((my $pulled := $!iter.pull-one),IterationEnd),
                ($!done := IterationEnd),    # source exhausted now

                nqp::if(
                  nqp::isnull($!current),
                  $pulled,                   # passing through rest

                  nqp::if(                   # need to check if ok
                    $!on,
                    nqp::if(                 # passing on until off
                      $!current($pulled),
                      $pulled,               # still on

                      nqp::if(               # was on, off now
                        nqp::eqaddr(
                          (my $next := $!conds.pull-one),
                          IterationEnd
                        ),
                        ($!done := IterationEnd), # no next checker, done

                        nqp::stmts(          # use next checker
                          nqp::until(
                           nqp::eqaddr(
                              ($pulled := $!iter.pull-one),
                              IterationEnd
                            ) || $next($pulled),
                            nqp::null
                          ),
                          nqp::if(           # ended looping, why?
                            nqp::eqaddr($pulled,IterationEnd),
                            ($!done := IterationEnd), # exhausted now

                            nqp::stmts(      # on, passed off, on again
                              ($!current := nqp::if(
                                nqp::eqaddr(
                                  ($next := $!conds.pull-one),
                                  IterationEnd
                                ),
                                nqp::null,   # pass rest on
                                $next        # set next checker
                              )),
                              $pulled

                            )
                          )
                        )
                      )
                    ),
                    nqp::if(                 # off now (first time)
                      $!current($pulled),
                      nqp::stmts(
                        nqp::if(             # on for first elem
                          nqp::eqaddr(
                            ($!current := $!conds.pull-one),
                            IterationEnd
                          ),
                          ($!current := nqp::null), # no next, passing on
                          ($!on = 1)         # there's next, keep going
                        ),
                        $pulled              # first hit is ok

                      ),
                      nqp::stmts(            # still off for first
                        nqp::until(
                         nqp::eqaddr(
                            ($pulled := $!iter.pull-one),
                            IterationEnd
                          ) || $!current($pulled),
                          nqp::null
                        ),
                        nqp::if(             # ended looping, why?
                          nqp::eqaddr($pulled,IterationEnd),
                          ($!done := IterationEnd), # exhausted now

                          nqp::stmts(        # found ok
                            nqp::if(
                              nqp::eqaddr(
                                ($!current := $!conds.pull-one),
                                IterationEnd
                              ),
                              ($!current := nqp::null), # no next, pass on
                              ($!on = 1)     # there's next, keep going
                            ),
                            $pulled

                          )
                        )
                      )
                    )
                  )
                )
              )
            )
        }
        method sink-all(--> IterationEnd) { $!iter.sink-all }
    }
    method Toggle(\iter, \conds, $on) { Toggle.new(iter, conds, $on) }

    # Return an iterator that only will return the two given values.
    my class TwoValues does Iterator {
        has Mu $!val1;
        has Mu $!val2;
        method new(Mu \val1, Mu \val2) {
            nqp::p6bindattrinvres(
              nqp::p6bindattrinvres(
                nqp::create(self),self,'$!val1',val1),
              self,'$!val2',val2
            )
        }
        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr($!val1,IterationEnd),
              nqp::if(
                nqp::eqaddr($!val2,IterationEnd),
                IterationEnd,
                nqp::stmts(
                  (my Mu $val2 := $!val2),
                  ($!val2 := IterationEnd),
                  $val2
                )
              ),
              nqp::stmts(
                (my $val1 := $!val1),
                ($!val1 := IterationEnd),
                $val1
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              nqp::if(
                nqp::eqaddr($!val1,IterationEnd),
                nqp::unless(nqp::eqaddr($!val2,Mu),target.push($!val2)),
                nqp::stmts(
                  target.push($!val1),
                  target.push($!val2)
                )
              ),
              ($!val1 := $!val2 := IterationEnd)
            )
        }
        method skip-one() {
            nqp::if(
              nqp::not_i(nqp::eqaddr($!val1,IterationEnd)),
              nqp::isfalse($!val1 := IterationEnd),
              nqp::if(
                nqp::not_i(nqp::eqaddr($!val2,IterationEnd)),
                nqp::isfalse($!val2 := IterationEnd)
              )
            )
        }
        method sink-all(--> IterationEnd) {
            $!val1 := $!val2 := IterationEnd
        }
    }
    method TwoValues(Mu \val1, Mu \val2) { TwoValues.new(val1, val2) }

    # Return a lazy iterator that will keep producing the given value.
    # Basically the functionality of 42 xx *
    my class UnendingValue does PredictiveIterator {
        has Mu $!value;
        method new(Mu \value) {
            nqp::p6bindattrinvres(nqp::create(self),self,'$!value',value)
        }
        method pull-one() is raw { $!value }
        method skip-one(--> True) { }
        method sink-all(--> IterationEnd) { }
        method count-only(--> Inf) { }
        method bool-only(--> True) { }
        method is-lazy(--> True) { }
    }
    method UnendingValue(Mu \value) { UnendingValue.new(value) }

    # Return an iterator from a given iterator with a given mapper callable
    # and a compare callable, producing values either with unique or repeated
    # semantics.
    my class UniqueRepeatedAsWith does Iterator {
        has Mu $!iter;
        has &!as;
        has &!with;
        has int $!unique;
        has $!seen;
        method !SET-SELF(\iterator, \as, \with, \unique) {
            nqp::stmts(
              ($!iter := iterator),
              (&!as := as),
              (&!with := with),
              ($!unique = nqp::istrue(unique)),
              ($!seen := nqp::list),
              self
            )
        }
        method new( \iterator, \as, \with, \union) {
            nqp::create(self)!SET-SELF(iterator, as, with, union)
        }
        method pull-one() is raw {
            nqp::stmts(
              (my &as := &!as),      # lexicals are faster than attributes
              (my &with := &!with),
              (my $seen := $!seen),
              nqp::until(
                nqp::eqaddr((my $needle := $!iter.pull-one),IterationEnd),
                nqp::stmts(
                  (my int $i = -1),
                  (my int $elems = nqp::elems($!seen)),
                  (my $target := as($needle)),
                  nqp::until(
                    nqp::iseq_i(($i = nqp::add_i($i,1)),$elems)
                      || with($target,nqp::atpos($seen,$i)),
                    nqp::null
                  ),
                  nqp::if(                         # done searching
                    $!unique,
                    nqp::if(                       # need unique semantics
                      nqp::iseq_i($i,$elems),
                      nqp::stmts(                  # new, so add and produce
                        nqp::push($!seen,$target),
                        (return-rw $needle)
                      )
                    ),
                    nqp::if(                       # need repeated semantics
                      nqp::iseq_i($i,$elems),
                      nqp::push($!seen,$target),   # new, just add
                      (return-rw $needle)          # not new, produce
                    )
                  )
                )
              ),
              IterationEnd
            )
        }
        method is-lazy() { $!iter.is-lazy }
        method sink-all(--> IterationEnd) { $!iter.sink-all }
    }
    method UniqueRepeatedAsWith(\iterator, \as, \with, \unique) {
        UniqueRepeatedAsWith.new(iterator, as, with, unique)
    }

    # Return an iterator from a given iterator with a given compare
    # callable, producing values either with unique or repeated semantics.
    my class UniqueRepeatedWith does Iterator {
        has Mu $!iter;
        has &!with;
        has int $!unique;
        has $!seen;
        method !SET-SELF(\iterator, \with, \unique) {
            nqp::stmts(
              ($!iter := iterator),
              (&!with := with),
              ($!unique = nqp::istrue(unique)),
              ($!seen := nqp::list),
              self
            )
        }
        method new( \iterator, \with, \union) {
            nqp::create(self)!SET-SELF(iterator, with, union)
        }
        method pull-one() is raw {
            nqp::stmts(
              (my &with := &!with),  # lexicals are faster than attributes
              (my $seen := $!seen),
              nqp::until(
                nqp::eqaddr((my $needle := $!iter.pull-one),IterationEnd),
                nqp::stmts(
                  (my int $i = -1),
                  (my int $elems = nqp::elems($!seen)),
                  nqp::until(
                    nqp::iseq_i(($i = nqp::add_i($i,1)),$elems)
                      || with($needle,nqp::atpos($seen,$i)),
                    nqp::null
                  ),
                  nqp::if(                         # done searching
                    $!unique,
                    nqp::if(                       # need unique semantics
                      nqp::iseq_i($i,$elems),
                      nqp::stmts(                  # new, so add and produce
                        nqp::push($!seen,$needle),
                        (return-rw $needle)
                      )
                    ),
                    nqp::if(                       # need repeated semantics
                      nqp::iseq_i($i,$elems),
                      nqp::push($!seen,$needle),   # new, just add
                      (return-rw $needle)          # not new, produce
                    )
                  )
                )
              ),
              IterationEnd
            )
        }
        method is-lazy() { $!iter.is-lazy }
        method sink-all(--> IterationEnd) { $!iter.sink-all }
    }
    method UniqueRepeatedWith(\iterator, \with, \unique) {
        UniqueRepeatedWith.new(iterator, with, unique)
    }

    # Returns an iterator that takes a source iterator and a Callable.  It
    # passes on all values from the source iterator from the moment the
    # Callable returns a trueish value.
    my class Until does Iterator {
        has $!iter;
        has $!cond;

        method !SET-SELF(\iter, \cond) {
            $!iter := iter;
            $!cond := cond;
            self
        }
        method new(\iter,\cond) { nqp::create(self)!SET-SELF(iter,cond) }

        method pull-one() is raw {
            nqp::if(
              $!cond,
              nqp::stmts(
                nqp::until(
                  nqp::eqaddr((my $pulled := $!iter.pull-one),IterationEnd)
                    || $!cond($pulled),
                  nqp::null
                ),
                ($!cond := nqp::null),
                $pulled
              ),
              $!iter.pull-one
            )
        }
        method sink-all(--> IterationEnd) { $!iter.sink-all }
    }
    method Until(\iter, &cond) { Until.new(iter, &cond) }

    # Returns an iterator from a given iterator where the occurrence of
    # a Whatever value indicates that last value seen from the source
    # iterator should be repeated indefinitely until either another
    # non-Whatever value is seen from the source iterator, or the source
    # iterator is exhausted.
    my class WhateverIterator does Iterator {
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
    }
    method Whatever(\source) { WhateverIterator.new(source) }

    # Returns an iterator that takes a source iterator and a Callable.  It
    # passes on values from the source iterator while the Callable returns
    # a trueish value.  Once a falsish value is returned, the iterator ends.
    my class While does Iterator {
        has $!iter;
        has &!cond;
        has $!done;

        method !SET-SELF(\iter, \cond) {
            $!iter := iter;
            &!cond := cond;
            $!done := nqp::null;
            self
        }
        method new(\iter,\cond) { nqp::create(self)!SET-SELF(iter,cond) }

        method pull-one() is raw {
            nqp::ifnull(
              $!done,
              nqp::if(
                nqp::eqaddr((my \pulled := $!iter.pull-one),IterationEnd)
                  || nqp::isfalse(&!cond(pulled)),
                ($!done := IterationEnd),
                pulled
              )
            )
        }
        method sink-all(--> IterationEnd) { $!iter.sink-all }
    }
    method While(\iter, &cond) { While.new(iter, &cond) }

    # Returns an iterator that handles all properties of a -while- with
    # a condition.  Takes a Callable to be considered the body of the loop,
    # and a Callable for the condition.
    my class WhileLoop does SlippyIterator {
        has $!body;
        has $!cond;

        method !SET-SELF(\body,\cond) {
            nqp::stmts(
              ($!body := body),
              ($!cond := cond),
              self
            )
        }
        method new(\body,\cond) {
            nqp::create(self)!SET-SELF(body,cond)
        }

        method pull-one() {
            if $!slipping && nqp::not_i(
                nqp::eqaddr((my $result := self.slip-one),IterationEnd)
            ) {
                $result
            }
            else {
                nqp::if(
                  $!cond(),
                  nqp::stmts(
                    nqp::until(
                      (my int $stopped),
                      nqp::stmts(
                        ($stopped = 1),
                        nqp::handle(
                          nqp::if(
                            nqp::istype(($result := $!body()),Slip),
                            ($stopped = nqp::eqaddr(
                              ($result := self.start-slip($result)),
                              IterationEnd
                            ) && nqp::if($!cond(),0,1))
                          ),
                          'NEXT', ($stopped = nqp::if($!cond(),0,1)),
                          'REDO', ($stopped = 0),
                          'LAST', ($result := IterationEnd)
                        )
                      ),
                      :nohandler
                    ),
                    $result
                  ),
                  IterationEnd
                )
            }
        }
    }
    method WhileLoop(&body, &cond) { WhileLoop.new(&body,&cond) }

    # Return an iterator that will zip the given iterables (with &[,])
    # Basically the functionality of @a Z @b
    my class ZipIterables does Iterator {
        has $!iters;
        has int $!lazy;
        method !SET-SELF(\iters) {
            nqp::stmts(
              (my \iterables := nqp::getattr(iters,List,'$!reified')),
              (my int $elems = nqp::elems(iterables)),
              ($!iters := nqp::setelems(nqp::list,$elems)),
              ($!lazy = 1),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos($!iters,$i,
                  nqp::if(
                    nqp::iscont(my \elem := nqp::atpos(iterables,$i)),
                    nqp::stmts(
                      ($!lazy = 0),
                      Rakudo::Iterator.OneValue(elem)
                    ),
                    nqp::stmts(
                      nqp::unless(elem.is-lazy,($!lazy = 0)),
                      Rakudo::Iterator.Whatever(elem.iterator)
                    )
                  )
                )
              ),
              self
            )
        }
        method new(\iterables) { nqp::create(self)!SET-SELF(iterables) }
        method pull-one() {
            nqp::if(
              nqp::eqaddr($!iters,IterationEnd),
              IterationEnd,
              nqp::stmts(
                (my int $i = -1),
                (my int $elems = nqp::elems($!iters)),
                (my int $is_iterend = 0),
                (my \buf := nqp::setelems(nqp::create(IterationBuffer),$elems)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::eqaddr(
                      (my \pulled := nqp::atpos($!iters,$i).pull-one), IterationEnd
                    ),
                    $is_iterend = 1,
                    nqp::bindpos(buf,$i,pulled)
                  )
                ),
                nqp::if(
                  $is_iterend,  # at least one exhausted
                  nqp::stmts(
                    ($!iters := IterationEnd),
                    IterationEnd
                  ),
                  buf.List
                )
              )
            )
        }
        method is-lazy() { nqp::hllbool($!lazy) }
    }
    method ZipIterables(@iterables) {
        nqp::if(
          nqp::isgt_i((my int $n = @iterables.elems),1),  # reifies
          ZipIterables.new(@iterables),
          nqp::if(
            nqp::iseq_i($n,0),
            Rakudo::Iterator.Empty,
            nqp::atpos(nqp::getattr(@iterables,List,'$!reified'),0).iterator
          )
        )
    }

    # Same as ZipIterablesOp, but takes a mapper Callable instead of
    # an op.  This is the underlying workhorse of ZipIterablesOp.
    my class ZipIterablesMap does Iterator {
        has $!iters;
        has $!mapper;
        has int $!lazy;
        method !SET-SELF(\iters,\mapper) {
            nqp::stmts(
              (my \iterables := nqp::getattr(iters,List,'$!reified')),
              (my int $elems = nqp::elems(iterables)),
              ($!iters  := nqp::setelems(nqp::list,$elems)),
              ($!mapper := mapper),
              ($!lazy = 1),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos($!iters,$i,
                  nqp::if(
                    nqp::iscont(my \elem := nqp::atpos(iterables,$i)),
                    nqp::stmts(
                      ($!lazy = 0),
                      Rakudo::Iterator.OneValue(elem)
                    ),
                    nqp::stmts(
                      nqp::unless(elem.is-lazy,($!lazy = 0)),
                      Rakudo::Iterator.Whatever(elem.iterator)
                    )
                  )
                )
              ),
              self
            )
        }
        method new(\iters,\map) { nqp::create(self)!SET-SELF(iters,map) }
        method pull-one() {
            nqp::if(
              nqp::eqaddr($!iters,IterationEnd),
              IterationEnd,
              nqp::stmts(
                (my int $i = -1),
                (my int $elems = nqp::elems($!iters)),
                (my int $is_iterend = 0),
                (my $list :=
                  nqp::setelems(nqp::create(IterationBuffer),$elems)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::eqaddr(
                      (my \pulled := nqp::atpos($!iters,$i).pull-one),
                      IterationEnd
                    ),
                    $is_iterend = 1,
                    nqp::bindpos($list,$i,pulled)
                  )
                ),
                nqp::if(
                  $is_iterend,  # at least one exhausted
                  ($!iters := IterationEnd),
                  $!mapper($list)
                )
              )
            )
        }
        method is-lazy() { nqp::hllbool($!lazy) }
    }
    method ZipIterablesMap(@iterables,&mapper) {
        nqp::if(
          nqp::isgt_i((my int $n = @iterables.elems),1),  # reifies
          ZipIterablesMap.new(@iterables,&mapper),
          nqp::if(
            nqp::iseq_i($n,0),
            Rakudo::Iterator.Empty,
            nqp::atpos(nqp::getattr(@iterables,List,'$!reified'),0).iterator
          )
        )
    }

    # Return an iterator that will zip the given iterables and operator.
    # Basically the functionality of @a Z=> @b, with &[=>] being the op.
    method ZipIterablesOp(@iterables,\op) {
        nqp::if(
          nqp::eqaddr(op,&infix:<,>),
          Rakudo::Iterator.ZipIterables(@iterables),
          Rakudo::Iterator.ZipIterablesMap(
            @iterables,
            Rakudo::Metaops.MapperForOp(op)
          )
        )
    }
}

# vim: ft=perl6 expandtab sw=4
