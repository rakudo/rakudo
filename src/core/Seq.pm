my class X::Seq::Consumed { ... }
my class X::Seq::NotIndexable { ... }
my class Seq is Cool does Iterable does Sequence {
    # The underlying iterator that iterating this sequence will work its
    # way through. Can only be obtained once.
    has Iterator $!iter;

    # The only valid way to create a Seq directly is by giving it the
    # iterator it will consume and maybe memoize.
    method new(Iterator:D $iter) {
        nqp::p6bindattrinvres(nqp::create(self),Seq,'$!iter',nqp::decont($iter))
    }

    method new-consumed() {
        self.bless;
    }

    method iterator(Seq:D:) {
        nqp::if(
          (my \iter = $!iter).DEFINITE,
          nqp::stmts(
            ($!iter := Iterator),
            iter
          ),
          nqp::if(
            $!list.DEFINITE,
            $!list.iterator,
            X::Seq::Consumed.new.throw
          )
        )
    }

    multi method is-lazy(Seq:D:) {
        nqp::if(
          $!iter.DEFINITE,
          $!iter.is-lazy,
          nqp::if(
            $!list.DEFINITE,
            $!list.is-lazy,
            X::Seq::Consumed.new.throw
          )
        )
    }

    multi method Seq(Seq:D:)   { self }

    method Capture() {
        self.List.Capture
    }

    method elems() {
        nqp::if(
          self.is-lazy,
          Failure.new(X::Cannot::Lazy.new(action => '.elems')),
          nqp::if(
            ($!iter.DEFINITE && nqp::can($!iter,'count-only')),
            $!iter.count-only,
            self.cache.elems
          )
        )
    }

    method Numeric() {
        nqp::if(
          ($!iter.DEFINITE && nqp::can($!iter,'count-only')),
          $!iter.count-only,
          self.cache.Numeric
        )
    }

    method Int() {
        nqp::if(
          ($!iter.DEFINITE && nqp::can($!iter,'count-only')),
          $!iter.count-only,
          self.cache.Int
        )
    }

    method Bool(Seq:D:) {
        nqp::if(
          $!iter.DEFINITE,
          nqp::if(
            nqp::can($!iter,'bool-only'),
            $!iter.bool-only,
            nqp::if(
              nqp::can($!iter,'count-only'),
              ?$!iter.count-only,
              self.cache.Bool
            )
          ),
          self.cache.Bool
        )
    }

    multi method perl(Seq:D \SELF:) {
        # If we don't have an iterator, someone grabbed it already;
        # Check for cached $!list; if that's missing too, we're consumed
        my $perl;
        if not $!iter.DEFINITE and not $!list.DEFINITE {
            # cannot call .cache on a Seq that's already been iterated,
            # so we need to produce a string that, when EVAL'd, reproduces
            # an already iterated Seq.
            # compare RT #127492
            $perl = self.^name ~ '.new-consumed()';
        }
        else { $perl = self.cache.perl ~ '.Seq' }
        nqp::iscont(SELF) ?? '$(' ~ $perl ~ ')' !! $perl
    }

    method join(Seq:D: $separator = '' --> Str:D) {
        nqp::if(
          (my $iterator := self.iterator).is-lazy,
          '...',
          nqp::stmts(
            (my $strings  := nqp::list_s),
            nqp::until(
              nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
              nqp::push_s($strings,nqp::unbox_s(
                nqp::if(
                  nqp::isconcrete($pulled) && nqp::istype($pulled,Str),
                  $pulled,
                  nqp::if(
                    nqp::can($pulled,'Str'),
                    $pulled.Str,
                    nqp::box_s($pulled,Str)
                  )
                )
              ))
            ),
            nqp::box_s(nqp::join(nqp::unbox_s($separator.Str),$strings),Str)
          )
        )
    }

    method sink(--> Nil) {
        nqp::if(
          $!iter.DEFINITE,
          nqp::stmts(
            $!iter.sink-all,
            ($!iter := Iterator)
          ),
          nqp::if(
            $!list.DEFINITE,
            $!list.sink
          )
        )
    }

    proto method from-loop(|) { * }
    multi method from-loop(&body) {
        Seq.new(Rakudo::Iterator.Loop(&body))
    }
    multi method from-loop(&body, &cond, :$repeat!) {
        Seq.new($repeat
          ?? Rakudo::Iterator.RepeatLoop(&body, &cond)
          !! Rakudo::Iterator.WhileLoop(&body, &cond)
        )
    }
    multi method from-loop(&body, &cond) {
        Seq.new(Rakudo::Iterator.WhileLoop(&body, &cond))
    }
    multi method from-loop(&body, &cond, &afterwards) {
        Seq.new(Rakudo::Iterator.CStyleLoop(&body, &cond, &afterwards))
    }

    multi method skip()         { nqp::stmts( $!iter.skip-one, self) }
    multi method skip(Int() $n) { nqp::stmts( $!iter.skip-at-least($n), self) }
}

sub GATHER(&block) {
    Seq.new(class :: does SlippyIterator {
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
                  $!push-target.DEFINITE,
                  ($!push-target := nqp::create(IterationBuffer))
                ),
                ($!wanted = 1),
                nqp::continuationreset(PROMPT, &!resumption),
                nqp::if(
                  &!resumption.DEFINITE,
                  nqp::shift($!push-target),
                  IterationEnd
                )
              )
            )
        }

        method push-exactly($target, int $n) {
            nqp::if(
              nqp::isgt_i($n,0),
              nqp::stmts(
                ($!wanted = $n),
                ($!push-target := $target),
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
                      &!resumption.DEFINITE,
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
    }.new(&block))
}

multi sub infix:<eqv>(Seq:D \a, Seq:D \b) {
    nqp::p6bool(
      nqp::unless(
        nqp::eqaddr(a,b),
        nqp::if(
          nqp::eqaddr(a.WHAT,b.WHAT),
          nqp::if(
            nqp::iseq_i(
              (my \ia := a.iterator).is-lazy,
              (my \ib := b.iterator).is-lazy
            ),
            nqp::if(
              ia.is-lazy,
              die(X::Cannot::Lazy.new: :action<eqv>),
              nqp::stmts(
                nqp::until(
                  nqp::stmts(
                    (my \pa := ia.pull-one),
                    (my \pb := ib.pull-one),
                    nqp::eqaddr(pa,IterationEnd)
                      || nqp::eqaddr(pb,IterationEnd)
                      || nqp::not_i(pa eqv pb)
                  ),
                  nqp::null
                ),
                nqp::eqaddr(pa,pb)  # exhausted if both IterationEnd
              )
            )
          )
        )
      )
    )
}

# vim: ft=perl6 expandtab sw=4
