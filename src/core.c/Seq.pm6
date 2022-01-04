my class X::Seq::Consumed { ... }
my class X::Seq::NotIndexable { ... }
my class Seq is Cool does Iterable does Sequence {
    # The underlying iterator that iterating this sequence will work its
    # way through. Can only be obtained once.
    has Iterator $!iter;

    # The only valid way to create a Seq directly is by giving it the
    # iterator it will consume and maybe memoize.
    proto method new(Seq: |) {*}
    multi method new(Seq: Iterator:D $iter) {
        nqp::p6bindattrinvres(nqp::create(self),Seq,'$!iter',nqp::decont($iter))
    }
    # This candidate exists purely for being able to EVAL a .raku
    # representation of a Seq of which the iterator has already been taken,
    multi method new(Seq:) { nqp::create(self) }

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
        nqp::isconcrete($!iter)
          ?? $!iter.is-lazy
          !! nqp::isconcrete($!list)
            ?? $!list.is-lazy
            !! X::Seq::Consumed.new.throw
    }

    multi method Seq(Seq:D:)   { self }

    method Capture() {
        self.List.Capture
    }

    method elems() {
        self.is-lazy
          ?? self.fail-iterator-cannot-be-lazy('.elems')
          !! nqp::isconcrete($!iter) && nqp::istype($!iter,PredictiveIterator)
            ?? $!iter.count-only
            !! self.cache.elems
    }

    method Numeric() { self.elems }
    method Int()     { self.elems }

    method Bool(Seq:D:) {
        nqp::isconcrete($!iter) && nqp::istype($!iter,PredictiveIterator)
          ?? $!iter.bool-only
          !! self.cache.Bool
    }

    multi method raku(Seq:D \SELF:) {
        # If we don't have an iterator, someone grabbed it already;
        # Check for cached $!list; if that's missing too, we're consumed
        my $raku;
        if not $!iter.DEFINITE and not $!list.DEFINITE {
            # cannot call .cache on a Seq that's already been iterated,
            # so we need to produce a string that, when EVAL'd, reproduces
            # an already iterated Seq.
            # compare https://github.com/Raku/old-issue-tracker/issues/5124
            $raku = self.^name ~ '.new()';
        }
        else { $raku = self.cache.raku ~ '.Seq' }
        nqp::iscont(SELF) ?? '$(' ~ $raku ~ ')' !! $raku
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

    method reverse(--> Seq:D) is nodal {
        nqp::if(
          (my $iterator := self.iterator).is-lazy,
          self.fail-iterator-cannot-be-lazy('.reverse'),
          nqp::stmts(
            $iterator.push-all(my \buffer := nqp::create(IterationBuffer)),
            Seq.new: Rakudo::Iterator.ReifiedReverse(buffer, Mu)
          )
        )
    }

    method rotate(Int(Cool) $rotate = 1 --> Seq:D) is nodal {
        nqp::if(
          (my $iterator := self.iterator).is-lazy,
          self.fail-iterator-cannot-be-lazy('.rotate'),
          nqp::if(
            $rotate,
            Seq.new( nqp::if(
              $rotate > 0,
              Rakudo::Iterator.Rotate($rotate, $iterator),
              nqp::stmts(
                $iterator.push-all(my \buffer := nqp::create(IterationBuffer)),
                Rakudo::Iterator.ReifiedRotate($rotate, buffer, Mu)
              )
            )),
            self
          )
        )
    }

    multi method slice(Seq:D: Iterable:D \iterable --> Seq:D) {
        Seq.new(
          Rakudo::Iterator.MonotonicIndexes(
            self.iterator,
            iterable.iterator,
            0,
            -> $index, $next {
                die "Provided index $index, which is lower than $next";
            }
          )
        )
    }
    multi method slice(Seq:D: *@indices --> Seq:D) { self.slice(@indices) }

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

    # This method is mainly called from Actions.nqp
    proto method from-loop(|) {*}
    multi method from-loop(&body, :$label) {
        Seq.new: Rakudo::Iterator.Loop(&body, $label)
    }
    multi method from-loop(&body, &cond, :$repeat!, :$label) {
        Seq.new: $repeat
          ?? Rakudo::Iterator.RepeatLoop(&body, &cond, $label)
          !! Rakudo::Iterator.WhileLoop(&body, &cond, $label)
    }
    multi method from-loop(&body, &cond, :$label) {
        Seq.new: Rakudo::Iterator.WhileLoop(&body, &cond, $label)
    }
    multi method from-loop(&body, &cond, &afterwards, :$label) {
        Seq.new: Rakudo::Iterator.CStyleLoop(&body, &cond, &afterwards, $label)
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

sub GATHER(&block) is implementation-detail {
    Seq.new(Rakudo::Iterator.Gather(&block))
}

# vim: expandtab shiftwidth=4
