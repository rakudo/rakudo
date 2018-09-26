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
          nqp::isconcrete(my \iter = $!iter),
          nqp::stmts(
            ($!iter := Iterator),
            iter
          ),
          nqp::if(
            nqp::isconcrete($!list),
            $!list.iterator,
            X::Seq::Consumed.new.throw
          )
        )
    }

    multi method is-lazy(Seq:D:) {
        nqp::if(
          nqp::isconcrete($!iter),
          $!iter.is-lazy,
          nqp::if(
            nqp::isconcrete($!list),
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
            nqp::isconcrete($!iter) && nqp::istype($!iter,PredictiveIterator),
            $!iter.count-only,
            self.cache.elems
          )
        )
    }

    method Numeric() {
        nqp::if(
          nqp::isconcrete($!iter) && nqp::istype($!iter,PredictiveIterator),
          $!iter.count-only,
          self.cache.Numeric
        )
    }

    method Int() {
        nqp::if(
          nqp::isconcrete($!iter) && nqp::istype($!iter,PredictiveIterator),
          $!iter.count-only,
          self.cache.Int
        )
    }

    method Bool(Seq:D:) {
        nqp::if(
          nqp::isconcrete($!iter) && nqp::istype($!iter,PredictiveIterator),
          $!iter.bool-only,
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
          nqp::isconcrete($!iter),
          nqp::stmts(
            $!iter.sink-all,
            ($!iter := Iterator)
          ),
          nqp::if(
            nqp::isconcrete($!list),
            $!list.sink
          )
        )
    }

    proto method from-loop(|) {*}
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

    multi method ACCEPTS(Seq:D: Iterable:D \iterable --> Bool:D) {
        nqp::if(
          (my \liter := self.iterator).is-lazy,
          False,
          nqp::if(
            (my \riter := iterable.iterator).is-lazy,
            False,
            nqp::stmts(
              nqp::until(
                nqp::eqaddr((my \left := liter.pull-one),IterationEnd),
                nqp::if(
                  nqp::eqaddr((my \right := riter.pull-one),IterationEnd)
                    || nqp::not_i(right.ACCEPTS(left)),
                  (return False)
                )
              ),
              nqp::hllbool(nqp::eqaddr(riter.pull-one,IterationEnd))
            )
          )
        )
    }
}

sub GATHER(&block) { Seq.new(Rakudo::Iterator.Gather(&block)) }

multi sub infix:<eqv>(Seq:D \a, Seq:D \b) {
    nqp::hllbool(
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
