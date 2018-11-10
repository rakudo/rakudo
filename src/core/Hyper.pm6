# A class to perform hyper operations of the form left op right

class Hyper {
    has $.operator;         # for some reason we cant make this a &.operator
    has int8 $.dwim-left;   # left side wont end
    has int8 $.dwim-right;  # right side wont end
    has int8 $.assigns;     # assigns to left side

    method new(\op, Bool() :$dwim-left, Bool() :$dwim-right) {
        self.bless(
          :operator(op),
          :$dwim-left,
          :$dwim-right,
          :assigns(op.name.ends-with("=>")),
        )
    }

    proto method infix(|) {*}

    # x >>op<< y
    multi method infix(\left, \right) is raw { $!operator(left,right) }

    # %x >>op<< %y
    multi method infix(
      Associative:D \left,
      Associative:D \right
      --> Associative:D
    ) is default {
        nqp::eqaddr(left.keyof,Str(Any))
          && nqp::eqaddr(right.keyof,Str(Any))
          ?? self!str-associatives(left,right)
          !! self!obj-associatives(left,right)
    }

    # %x >>op<< ...
    multi method infix(Associative:D \left, Iterable:D \right) {
        die "{left.^name} {$!operator.name} {right.^name} can never work
  reliably: the order of keys in {left.^name} is indeterminate"
    }

    # %x >>op<< y
    multi method infix(Associative:D \left, \right --> Associative:D) {
        my @keys = left.keys;
        my \result := left.WHAT.new.STORE(
            Seq.new(
              Rakudo::Iterator.RoundrobinIterablesFlat(
                (@keys, self.infix(left{@keys}, right))
              )
            )
        );
        nqp::iscont(left) ?? result.item !! result;
    }

    # ... >>op<< %y
    multi method infix(Iterable:D \left, Associative:D \right) {
        die "{left.^name} {$!operator.name} {right.^name} can never work
  reliably: the order of keys in {right.^name} is indeterminate"
    }

    # x >>op<< %y
    multi method infix(\left, Associative:D \right --> Associative:D) {
        my @keys = right.keys;
        my \result := right.WHAT.new.STORE(
            Seq.new(
              Rakudo::Iterator.RoundrobinIterablesFlat(
                (@keys, self.infix(left, right{@keys}))
              )
            )
        );
        nqp::iscont(right) ?? result.item !! result;
    }

    # [x] >>op<< y
    multi method infix(Positional:D \left, \right --> Positional:D) {
        X::HyperOp::Infinite.new(:side<left>, :$!operator).throw
          if left.is-lazy;

        my int $left-elems = left.elems;
        X::HyperOp::NonDWIM.new(
          :$!operator, :$left-elems, :right-elems(1), :recursing
        ).throw
          unless $left-elems == 1
            or $left-elems  > 1 and $!dwim-right
            or $left-elems == 0 and $!dwim-left || $!dwim-right;

        my \values := nqp::create(IterationBuffer);
        my \iterator := left.iterator;

        nqp::until(
          nqp::eqaddr((my \value := iterator.pull-one),IterationEnd),
          nqp::push(values, self.infix(value,right))
        );

        my \result := nqp::p6bindattrinvres(
          nqp::create(
            nqp::istype(left,List) ?? left.WHAT !! List  # keep subtype
          ),
          List, '$!reified', values
        );
        nqp::iscont(left) ?? result.item !! result
    }

    # x >>op<< [y]
    multi method infix(\left, Positional:D \right --> Positional:D) {
        X::HyperOp::Infinite.new(:side<right>, :$!operator).throw
          if right.is-lazy;

        my int $right-elems = right.elems;
        X::HyperOp::NonDWIM.new(
          :$!operator, :left-elems(1), :$right-elems, :recursing
        ).throw
          unless $right-elems == 1
            or $right-elems  > 1 and $!dwim-left
            or $right-elems == 0 and $!dwim-left || $!dwim-right;

        my \values := nqp::create(IterationBuffer);
        my \iterator := right.iterator;

        nqp::until(
          nqp::eqaddr((my \value := iterator.pull-one),IterationEnd),
          nqp::push(values, self.infix(left,value))
        );

        my \result := nqp::p6bindattrinvres(
          nqp::create(
            nqp::istype(right,List) ?? right.WHAT !! List  # keep subtype
          ),
          List, '$!reified', values
        );
        nqp::iscont(right) ?? result.item !! result
    }

    # ... >>op<< ...
    multi method infix(
      Iterable:D \left,
      Iterable:D \right
      --> Iterable:D
    ) {
        my \left-iterator  = left.iterator;
        my \right-iterator = right.iterator;

        # Check whether any side is lazy. They must not be to proceed.
        if left-iterator.is-lazy {
            X::HyperOp::Infinite.new(:side<both>, :$!operator).throw
              if right-iterator.is-lazy;
            X::HyperOp::Infinite.new(:side<left>, :$!operator).throw
              if nqp::not_i($!dwim-left) || $!dwim-right;
        }
        X::HyperOp::Infinite.new(:side<right>, :$!operator).throw
          if right-iterator.is-lazy
          and (nqp::not_i($!dwim-right) || $!dwim-left);

        my \values :=
          $!dwim-left
            ?? $!dwim-right
              ?? self!iterables-left-right(left-iterator,right-iterator)
              !! self!iterables-left(left-iterator,right-iterator)
            !! $!dwim-right
              ?? self!iterables-right(left-iterator,right-iterator)
              !! self!iterables(left-iterator,right-iterator)
        ;
        my \result := nqp::p6bindattrinvres(
          nqp::create(
            nqp::istype(left,List) ?? left.WHAT !! List  # keep subtype
          ),
          List, '$!reified', values
        );
        nqp::iscont(left) ?? result.item !! result;
    }

    # using an infix on a one element list in a meta op
    multi method infix(\object) {
        nqp::if(
          nqp::can($!operator,"nodal"),
          nodemap($!operator,object),
          deepmap($!operator,object)
        )
    }

#--- Private helper methods ----------------------------------------------------

    # ... >>op<< ...
    method !iterables(Iterator:D \left, Iterator:D \right) {
        my \result := nqp::create(IterationBuffer);

        nqp::until(
          nqp::eqaddr((my \leftv := left.pull-one),IterationEnd)
            || nqp::eqaddr((my \rightv := right.pull-one),IterationEnd),
          nqp::push(result, self.infix(leftv, rightv))
        );

        my int $left-elems = my int $right-elems = nqp::elems(result);
        nqp::if(
          nqp::eqaddr(rightv,IterationEnd),
          self!right-exhausted(left,nqp::elems(result)),
          nqp::unless(
            nqp::eqaddr(right.pull-one,IterationEnd),
            self!left-exhausted(right,nqp::elems(result))
          )
        );

        result
    }

    # ... <<op<< ...
    method !iterables-left(Iterator:D \left, Iterator:D \right) {
        my \lefti  := Rakudo::Iterator.DWIM(left);
        my \result := nqp::create(IterationBuffer);

        my \leftv := lefti.pull-one;
        nqp::unless(
          lefti.ended,
          nqp::until(
            nqp::eqaddr((my \rightv := right.pull-one),IterationEnd),
            nqp::stmts(
              nqp::push(result,self.infix(leftv,rightv)),
              nqp::bind(leftv,lefti.pull-one)
            )
          )
        );

        result
    }

    # ... >>op>> ...
    method !iterables-right(Iterator:D \left, Iterator:D \right) {
        my \righti := Rakudo::Iterator.DWIM(right);
        my \result := nqp::create(IterationBuffer);

        my \rightv := righti.pull-one;
        nqp::unless(
          righti.ended,
          nqp::until(
            nqp::eqaddr((my \leftv := left.pull-one),IterationEnd),
            nqp::stmts(
              nqp::push(result, self.infix(leftv,rightv)),
              nqp::bind(rightv,righti.pull-one)
            )
          )
        );

        result
    }

    # ... <<op>> ...
    method !iterables-left-right(Iterator:D \left, Iterator:D \right) {
        my \lefti  := Rakudo::Iterator.DWIM(left);
        my \righti := Rakudo::Iterator.DWIM(right);
        my \result := nqp::create(IterationBuffer);

        my \leftv  := lefti.pull-one;
        my \rightv := righti.pull-one;
        nqp::unless(
          lefti.ended || righti.ended,
          nqp::until(
            lefti.ended && righti.ended,
            nqp::stmts(
              nqp::push(result,self.infix(leftv,rightv)),
              nqp::bind(leftv, lefti.pull-one),
              nqp::bind(rightv,righti.pull-one)
            )
          )
        );
        
        result
    }

    # handle normal hashes
    method !str-associatives(\left, \right) {
        my $keys := nqp::hash;
        if $!dwim-left {
            nqp::bindkey($keys,$_,1) if right.EXISTS-KEY($_) for left.keys;
        }
        else {
            nqp::bindkey($keys,$_,1) for left.keys;
        }
        if nqp::not_i($!dwim-right) {
            nqp::bindkey($keys,$_,1) for right.keys;
        }

        # create HLL version of keys
        my @keys =
          nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$keys).keys;

        # run with the left/right values
        my \result := left.WHAT.new.STORE(
            Seq.new(
              Rakudo::Iterator.RoundrobinIterablesFlat(
                (@keys, quietly self.infix(left{@keys}, right{@keys}))
              )
            )
        );
        nqp::iscont(left) ?? result.item !! result;
    }

    # handle object hashes / QuantHashes
    method !obj-associatives(\left, \right) {
        my $keys := nqp::hash;
        if $!dwim-left {
            nqp::bindkey($keys,.WHICH,$_) if right.EXISTS-KEY($_) for left.keys;
        }
        else {
            nqp::bindkey($keys,.WHICH,$_) for left.keys;
        }
        if nqp::not_i($!dwim-right) {
            nqp::bindkey($keys,.WHICH,$_) for right.keys;
        }

        # create HLL version of keys
        my @keys =
          nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$keys).values;

        # run with the left/right values
        my \result := left.WHAT.new.STORE(
            Seq.new(
              Rakudo::Iterator.RoundrobinIterablesFlat(
                (@keys, quietly self.infix(left{@keys}, right{$keys}))
              )
            )
        );
        nqp::iscont(left) ?? result.item !! result;
    }

    # error when left side of non-DWIM exhausted
    method !left-exhausted(Iterator:D \iterator, int $left-elems) {
        my int $right-elems = $left-elems + 1;
        nqp::until(
          nqp::eqaddr(iterator.pull-one,IterationEnd),
          ++$right-elems
        );
        X::HyperOp::NonDWIM.new(
          :$!operator, :$left-elems, :$right-elems, :recursing
        ).throw;
    }

    # error when right side of non-DWIM exhausted
    method !right-exhausted(Iterator:D \iterator, int $right-elems) {
        my int $left-elems = $right-elems + 1;
        nqp::until(
          nqp::eqaddr(iterator.pull-one,IterationEnd),
          ++$left-elems
        );
        X::HyperOp::NonDWIM.new(
          :$!operator, :$left-elems, :$right-elems, :recursing
        ).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
