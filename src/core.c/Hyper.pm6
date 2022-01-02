# A class to perform hyper operations of the form left op right

class Hyper {
    has $.operator is built(:bind);  # for some reason this cant be &.operator
    has int8 $.dwim-left;   # left side wont end
    has int8 $.dwim-right;  # right side wont end
    has int8 $.assigns;     # assigns to left side

    method new(\op, Bool() :$dwim-left, Bool() :$dwim-right) {
        self.bless(
          :operator(op),
          :$dwim-left,
          :$dwim-right,
          :assigns(op.name.ends-with(' + {assigning}')),
        )
    }

    # for error messages
    method name() {
        my str $name = $!operator.name || 'infix:<op>';
        my int $start = nqp::index($name,"«");
        $start = nqp::index($name,"<") if $start == -1;
        my int $end = nqp::index($name,"»");
        $end = nqp::index($name,">") if $end == -1;

        ($!dwim-left ?? '<<' !! '>>')
          ~ nqp::substr($name,$start + 1,$end - $start -1)
          ~ ($!dwim-right ?? '>>' !! '<<')
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
        nqp::istype(left,Pair)
          ?? nqp::istype(right,Pair)
            ?? self!pair-pair(left,right)
            !! self!pair-mu(left,right)
          !! nqp::istype(right,Pair)
            ?? self!mu-pair(left,right)
            !! nqp::istype(left,Hash::Object)
              || nqp::istype(right,Hash::Object)
              ?? self!obj-associatives(left,right)
              !! self!str-associatives(left,right)
    }

    # %x >>op<< ...
    multi method infix(Associative:D \left, List:D \right) {
        die "{left.^name} $.name {right.^name} can never work reliably: order of keys in {left.^name} is indeterminate"
    }

    # %x >>op<< y
    multi method infix(Associative:D \left, \right --> Associative:D) {
        return self!pair-mu(left,right) if nqp::istype(left,Pair);

        if $!assigns {
            self.infix(left.values,right);
            left
        }
        else {
            my \result := nqp::create(left.WHAT).STORE(
              left.keys,
              self.infix(left.values,right),
              :INITIALIZE
            );
            nqp::iscont(left) ?? result.item !! result;
        }
    }

    # ... >>op<< %y
    multi method infix(List:D \left, Associative:D \right) {
        die "{left.^name} $.name {right.^name} can never work reliably: order of keys in {right.^name} is indeterminate"
    }

    # x >>op<< %y
    multi method infix(\left, Associative:D \right --> Associative:D) {
        return self!mu-pair(left,right) if nqp::istype(right,Pair);

        my \result := nqp::create(right.WHAT).STORE(
          right.keys,
          self.infix(left,right.values),
          :INITIALIZE
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

        my \iterator := left.iterator;
        if $!assigns {
            nqp::until(
              nqp::eqaddr((my \value := iterator.pull-one),IterationEnd),
              self.infix(value,right)
            );
            left
        }
        else {
            my \values := nqp::create(IterationBuffer);
            nqp::until(
              nqp::eqaddr((my \value := iterator.pull-one),IterationEnd),
              nqp::push(values, self.infix(value,right))
            );

            my \result := nqp::eqaddr(left.WHAT,List)
              || nqp::eqaddr(left.WHAT,Slip)
              ?? nqp::p6bindattrinvres(
                   nqp::create(left.WHAT),List,'$!reified',values
                 )
              !! nqp::can(left,"STORE")
                ?? left.WHAT.new(nqp::p6bindattrinvres(
                     nqp::create(List),List,'$!reified',values
                   ))
                !! nqp::p6bindattrinvres(
                     nqp::create(List),List,'$!reified',values
                   );
            nqp::iscont(left) ?? result.item !! result
        }
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

        my \result := nqp::eqaddr(right.WHAT,List)
          || nqp::eqaddr(right.WHAT,Slip)
          ?? nqp::p6bindattrinvres(                         # List or Slip
               nqp::create(right.WHAT),List,'$!reified',values
             )
          !! nqp::can(right,"STORE")
            ?? right.WHAT.new(nqp::p6bindattrinvres(
                 nqp::create(List),List,'$!reified',values
               ))
            !! nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',values);
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
              ?? self!iterators-left-right(left-iterator,right-iterator)
              !! self!iterators-left(left-iterator,right-iterator)
            !! $!dwim-right
              ?? self!iterators-right(left-iterator,right-iterator)
              !! nqp::istype(left-iterator,PredictiveIterator)
                   && nqp::istype(right-iterator,PredictiveIterator)
                ?? self!predictive-iterators(left-iterator,right-iterator)
                !! self!iterators(left-iterator,right-iterator)
        ;
        my \result := nqp::p6bindattrinvres(
          nqp::create(
            nqp::istype(left,List) ?? left.WHAT !! List  # keep subtype
          ),
          List, '$!reified', values
        );
        nqp::iscont(left) ?? result.item !! result;
    }

    # :x >>op<< y
    method !pair-mu(\left,\right) {
#    multi method infix(Pair:D \left, \right) {
        nqp::p6bindattrinvres(
          nqp::clone(left),
          Pair,
          '$!value',
          self.infix(nqp::getattr(left,Pair,'$!value'), right)
        )
    }

    # x >>op<< :y
    method !mu-pair(\left,\right) {
#    multi method infix(\left, Pair:D \right) {
        nqp::p6bindattrinvres(
          nqp::clone(right),
          Pair,
          '$!value',
          self.infix(left, nqp::getattr(right,Pair,'$!value'))
        )
    }

    # :x >>op<< :y
    method !pair-pair(\left, \right) {
#    multi method infix(Pair:D \left, Pair:D \right) {
        nqp::getattr(left,Pair,'$!key').WHICH
          eq nqp::getattr(right,Pair,'$!key').WHICH
          ?? nqp::p6bindattrinvres(
               nqp::clone(left),Pair,'$!value',self.infix(
                 nqp::getattr(left, Pair,'$!value'),
                 nqp::getattr(right,Pair,'$!value')
               )
             )
          !! Nil
    }

    # using an infix on a one element list in a meta op
    multi method infix(\object) {
        nqp::can($!operator,"nodal")
          ?? object.nodemap($!operator)
          !! object.deepmap($!operator)
    }

#--- Private helper methods ----------------------------------------------------

    # ... >>op<< ...
    method !predictive-iterators(
      PredictiveIterator:D \left,
      PredictiveIterator:D \right,
    ) {
        X::HyperOp::NonDWIM.new(
          :$!operator,
          :left-elems(left.count-only),
          :right-elems(right.count-only),
          :recursing
        ).throw
          if left.count-only != right.count-only;

        # sure they have same number of elems, so only need to check one
        my \result := nqp::create(IterationBuffer);
        nqp::until(
          nqp::eqaddr((my \leftv := left.pull-one),IterationEnd),
          nqp::push(result,self.infix(leftv,right.pull-one))
        );
        result
    }

    # ... >>op<< ...
    method !iterators(Iterator:D \left, Iterator:D \right) {
        my \result := nqp::create(IterationBuffer);

        nqp::until(
          nqp::eqaddr((my \leftv := left.pull-one),IterationEnd)
            || nqp::eqaddr((my \rightv := right.pull-one),IterationEnd),
          nqp::push(result, self.infix(leftv, rightv))
        );

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
    method !iterators-left(Iterator:D \left, Iterator:D \right) {
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
    method !iterators-right(Iterator:D \left, Iterator:D \right) {
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
    method !iterators-left-right(Iterator:D \left, Iterator:D \right) {
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
        my @keys is List =
          nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$keys).keys;
        self!associatives(@keys, left, right)
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
        my @keys is List =
          nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$keys).values;
        self!associatives(@keys, left, right)
    }

    # actually handle 2 associatives
    method !associatives(@keys, \left, \right) {
        my \values := nqp::p6bindattrinvres(
          nqp::create(List),List,'$!reified',
          quietly self!iterators(
            Rakudo::Iterator.AssociativeIterableKeys(left, @keys),
            Rakudo::Iterator.AssociativeIterableKeys(right,@keys),
          )
        );
        if $!assigns {
            left
        }
        else {
            my \result :=
              nqp::create(left.WHAT).STORE(@keys, values, :INITIALIZE);
            nqp::iscont(left) ?? result.item !! result
        }
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

# vim: expandtab shiftwidth=4
