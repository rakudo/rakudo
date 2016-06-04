# for our tantrums
my class X::TypeCheck::Splice { ... }
my class Supply { ... }
my class Supplier { ... }

my sub combinations(Int() $n, Int() $k) {
    return () if $k < 0;
    return ((),) if $n < 1 || $k < 1;

    fail X::OutOfRange.new(
      :what("First parameter"),
      :got($n),
      :range("1..2147483647"),
    ) if nqp::isbig_I(nqp::decont($n));
    fail X::OutOfRange.new(
      :what("Second parameter"),
      :got($k),
      :range("1..2147483647"),
    ) if nqp::isbig_I(nqp::decont($k));

    Seq.new(class :: does Iterator {
        has int $!n;
        has int $!k;
        has Mu $!stack;
        has Mu $!combination;
        method !SET-SELF(\n,\k) {
            $!n = n;
            $!k = k;
            $!stack       := nqp::list_i(0);
            $!combination := nqp::list();
            self
        }
        method new(\n,\k) { nqp::create(self)!SET-SELF(n,k) }

        method pull-one() {
            my int $n = $!n;
            my int $k = $!k;

            while (my int $elems = nqp::elems($!stack)) {
                my int $index = $elems - 1;
                my int $value = nqp::pop_i($!stack);

                while nqp::islt_i($value, $n) && nqp::islt_i($index, $k) {
                    nqp::bindpos($!combination, $index, +$value);
#?if jvm
# temporary fix for RT #128123
                    $index++;
                    $value++;
#?endif
#?if !jvm
                    ++$index;
                    ++$value;
#?endif
                    nqp::push_i($!stack, $value);
                }
                return nqp::clone($!combination) if nqp::iseq_i($index, $k);
            }
            IterationEnd
        }
        method count-only { ([*] ($!n ... 0) Z/ 1 .. min($!n - $!k, $!k)).Int }
    }.new($n, $k))
}

sub find-reducer-for-op($op) {
    try my %prec := $op.prec;
    return &METAOP_REDUCE_LEFT if (nqp::isnull(%prec) or ! %prec);
    my $reducer = %prec<prec> eq 'f='
        ?? 'listinfix'
        !! %prec<assoc> // 'left';
    ::('&METAOP_REDUCE_' ~ $reducer.uc);
}

# A List is a (potentially infite) immutable list. The immutability is not
# deep; a List may contain Scalar containers that can be assigned to. However,
# it is not possible to shift/unshift/push/pop/splice/bind. A List is also
# Positional, and so may be indexed.
my class List does Iterable does Positional { # declared in BOOTSTRAP
    # class List is Cool {
    #   The reified elements in the list so far (that is, those that we already
    #   have produced the values for).
    #   has $!reified;
    # 
    #   Object that reifies the rest of the list. We don't just inline it into
    #   the List class itself, because a STORE on Array can clear things and
    #   upset an ongoing iteration. (An easy way to create such a case is to
    #   assign an array with lazy parts into itself.)
    #   has $!todo;

    # The object that goes into $!todo.
    class Reifier {
        # Our copy of the reified elements in the list so far.
        has $!reified;

        # The current iterator, if any, that we're working our way through in
        # order to lazily reify values. Must be depleted before $!future is
        # considered.
        has Iterator $!current-iter;

        # The (possibly lazy) values we've not yet incorporated into the list. The
        # only thing we can't simply copy from $!future into $!reified is a Slip
        # (and so the only reason to have a $!future is that there is at least one
        # Slip).
        has $!future;

        # The reification target (what .reify-* will .push to). Exists so we can
        # share the reification code between List/Array. List just uses its own
        # $!reified buffer; the Array one shoves stuff into Scalar containers
        # first.
        has $!reification-target;

        method reify-at-least(int $elems) {
            $!current-iter := Iterator
              if $!current-iter.DEFINITE 
              && $!current-iter.push-at-least($!reification-target,
                   nqp::sub_i($elems,nqp::elems($!reified))) =:= IterationEnd;

            # there is a future
            if $!future.DEFINITE {

                # still need and can get something from the future
                while nqp::islt_i(nqp::elems($!reified),$elems)
                  && nqp::elems($!future) {
                    my \current := nqp::shift($!future);
                    if nqp::istype(current, Slip) && nqp::isconcrete(current) {
                        my \iter := current.iterator;

                        # The iterator produced enough values to fill the need,
                        # but did not reach its end. We save it for next time.
                        # We know we'll exit the loop, since the < $elems check
                        # must be False (unless the iterator broke contract).
                        $!current-iter := iter
                          unless iter.push-at-least(
                            $!reification-target,
                            nqp::sub_i($elems,nqp::elems($!reified))
                          ) =:= IterationEnd;
                    }
                    else {
                        my $no-sink := $!reification-target.push(current);
                    }
                }

                # that was the future
                $!future := Mu unless nqp::elems($!future);
            }

            nqp::elems($!reified);
        }

        method reify-until-lazy() {
            $!current-iter := Iterator
              if $!current-iter.DEFINITE
              && $!current-iter.push-until-lazy($!reification-target)
                   =:= IterationEnd;

            if $!future.DEFINITE && !$!current-iter.DEFINITE {
                while nqp::elems($!future) {
                    my \current = nqp::shift($!future);
                    if nqp::istype(current, Slip) && nqp::isconcrete(current) {
                        my \iter = current.iterator;
                        unless iter.push-until-lazy($!reification-target) =:= IterationEnd {
                            $!current-iter := iter;
                            last;
                        }
                    }
                    else {
                        my $ = $!reification-target.push(current);
                    }
                }
                $!future := Mu unless nqp::elems($!future);
            }
            nqp::elems($!reified);
        }

        method reify-all() {
            if $!current-iter.DEFINITE {
                $!current-iter.push-all($!reification-target);
                $!current-iter := Iterator;
            }
            if $!future.DEFINITE {
                while nqp::elems($!future) {
                    my \current = nqp::shift($!future);
                    nqp::istype(current, Slip) && nqp::isconcrete(current)
                        ?? current.iterator.push-all($!reification-target)
                        !! my $ = $!reification-target.push(current);
                }
                $!future := Mu;
            }
            nqp::elems($!reified);
        }

        method fully-reified() {
            !($!current-iter.DEFINITE || $!future.DEFINITE)
        }

        method is-lazy() {
            $!current-iter.DEFINITE ?? $!current-iter.is-lazy !! False
        }
    }

    method from-iterator(List:U: Iterator $iter) {
        my \result := nqp::create(self);
        my \buffer := nqp::create(IterationBuffer);
        my \todo := nqp::create(Reifier);
        nqp::bindattr(result, List, '$!reified', buffer);
        nqp::bindattr(result, List, '$!todo', todo);
        nqp::bindattr(todo, Reifier, '$!reified', buffer);
        nqp::bindattr(todo, Reifier, '$!current-iter', $iter);
        nqp::bindattr(todo, Reifier, '$!reification-target',
            result.reification-target());
        result
    }

    method from-slurpy(|) {
        my Mu \vm-tuple = nqp::captureposarg(nqp::usecapture(), 1);
        my \result := nqp::create(self);
        my \buffer := nqp::create(IterationBuffer);
        my \todo := nqp::create(List::Reifier);
        nqp::bindattr(result, List, '$!reified', buffer);
        nqp::bindattr(result, List, '$!todo', todo);
        nqp::bindattr(todo, List::Reifier, '$!reified', buffer);
        nqp::bindattr(todo, List::Reifier, '$!future', vm-tuple)
          if nqp::elems(vm-tuple);
        nqp::bindattr(todo, List::Reifier, '$!reification-target',
            result.reification-target());
        result
    }

    method from-slurpy-onearg(|c) {
        my Mu \vm-tuple = nqp::captureposarg(nqp::usecapture(), 1);
        if nqp::elems(vm-tuple) != 1 {
            self.from-slurpy(|c);
        }
        else {
            my \consider = nqp::atpos(vm-tuple, 0);
            if nqp::istype(consider, Seq) {
                nqp::istype(self,Array) ?? consider.cache !! consider;
            }
            else {
                my \result := nqp::create(self);
                my \buffer := nqp::create(IterationBuffer);
                my \todo := nqp::create(List::Reifier);
                nqp::bindattr(result, List, '$!reified', buffer);
                nqp::bindattr(result, List, '$!todo', todo);
                nqp::bindattr(todo, List::Reifier, '$!reified', buffer);
                nqp::bindattr(todo, List::Reifier, '$!future',
                    nqp::iscont(consider) || !nqp::istype(consider, Iterable) || !consider.DEFINITE
                        ?? vm-tuple
                        !! nqp::list(consider.list.Slip)
                );
                nqp::bindattr(todo, List::Reifier, '$!reification-target',
                    result.reification-target());
                result
            }
        }
    }

    method from-slurpy-flat(|) {
        my \result := nqp::create(self);

        # only create a future if we need one
        my Mu \vm-tuple = nqp::captureposarg(nqp::usecapture(), 1);
        if nqp::elems(vm-tuple) -> int $elems {
            my \buffer := nqp::create(IterationBuffer);
            my \todo := nqp::create(List::Reifier);
            nqp::bindattr(result, List, '$!reified', buffer);
            nqp::bindattr(result, List, '$!todo', todo);
            nqp::bindattr(todo, List::Reifier, '$!reified', buffer);
            nqp::bindattr(todo, List::Reifier, '$!reification-target',
                result.reification-target());
            my \future := nqp::setelems(nqp::create(IterationBuffer),$elems);
            my int $i = -1;

            STATEMENT_LIST(
              my \consider := nqp::atpos(vm-tuple, $i);
              nqp::bindpos(future, $i, nqp::iscont(consider)
                ?? consider
                !! nqp::istype(consider, Iterable) && consider.DEFINITE
                  ?? (nqp::istype(consider, PositionalBindFailover)
                    ?? consider.cache
                    !! consider
                     ).flat.Slip
                  !! consider
              )
            ) while nqp::islt_i($i = nqp::add_i($i,1),$elems);

            nqp::bindattr(todo, List::Reifier, '$!future', future);
        }

        result
    }

    method new(**@things) {
        my \list = nqp::create(self);
        my \iterbuffer = nqp::create(IterationBuffer);
        nqp::bindattr(list, List, '$!reified', iterbuffer);

        my int $elems = +@things;  # reify
        my int $i     = -1;
        my $reified  := nqp::getattr(@things,List,'$!reified');
        my $no-sink;
        $no-sink := nqp::bindpos(iterbuffer,$i,(nqp::atpos($reified,$i)))
          while nqp::islt_i($i = nqp::add_i($i,1),$elems);
        list
    }

    method !ensure-allocated(--> Nil) {
        $!reified := nqp::create(IterationBuffer) unless $!reified.DEFINITE;
    }

    multi method Bool(List:D:) {
        self!ensure-allocated;
        so nqp::elems($!reified) ||
            $!todo.DEFINITE && $!todo.reify-at-least(1)
    }
    multi method Int(List:D:)     { self.elems }
    multi method end(List:D:)     { self.elems - 1 }
    multi method Numeric(List:D:) { self.elems }
    multi method Str(List:D:)     { self.join(' ') }

    # Pretend we're a Match assuming we're a list of Matches
    method to()      { self.elems ?? self[self.end].to !! Nil }
    method from()    { self.elems ?? self[0].from !! Nil }

    method sum(--> Numeric) {
        my int $elems = self.elems;
        my $list := nqp::getattr(self,List,'$!reified');
        my Numeric $sum = 0;
        my int $i = -1;
        $sum += nqp::ifnull(nqp::atpos($list,$i),0)
          while nqp::islt_i(++$i,$elems);
        $sum
    }

    method fmt($format = '%s', $separator = ' ') {
        self.map({ .fmt($format) }).join($separator);
    }

    multi method elems(List:D:) is nodal {
        self!ensure-allocated;
        if $!todo.DEFINITE {
            $!todo.reify-until-lazy();
            $!todo.fully-reified
              ?? ($!todo := Mu)
              !! fail X::Cannot::Lazy.new(:action('.elems'));
        }
        nqp::elems($!reified)
    }

    multi method AT-POS(List:D: Int $pos) is raw {
        self!ensure-allocated;
        my int $ipos = nqp::unbox_i($pos);
        nqp::islt_i($ipos, nqp::elems($!reified)) && nqp::isge_i($ipos, 0)
            ?? nqp::atpos($!reified, $ipos)
            !! self!AT-POS-SLOWPATH($ipos);
    }

    multi method AT-POS(List:D: int $pos) is raw {
        self!ensure-allocated;
        nqp::islt_i($pos, nqp::elems($!reified)) && nqp::isge_i($pos, 0)
            ?? nqp::atpos($!reified, $pos)
            !! self!AT-POS-SLOWPATH($pos);
    }

    method !AT-POS-SLOWPATH(int $pos) is raw {
        nqp::islt_i($pos, 0)
          ?? Failure.new(X::OutOfRange.new(
               :what($*INDEX // 'Index'), :got($pos), :range<0..Inf>))
          !! $!todo.DEFINITE && nqp::isgt_i($!todo.reify-at-least($pos + 1), $pos)
            ?? nqp::atpos($!reified, $pos)
            !! Nil
    }

    method BIND-POS(List:D: Int \pos, \what) is raw {
        nqp::iscont(self.AT-POS(pos))
          ?? nqp::bindpos(
               nqp::getattr(self,List,'$!reified'),nqp::unbox_i(pos),what)
          !! X::Bind.new.throw
    }

    multi method EXISTS-POS(List:D: int $pos) {
        self!ensure-allocated;
        $!todo.reify-at-least($pos + 1) if $!todo.DEFINITE;
        nqp::p6bool(nqp::not_i(
          nqp::islt_i($pos, 0) || nqp::isnull(nqp::atpos($!reified, $pos))))
    }
    multi method EXISTS-POS(List:D: Int:D $pos) {
        self!ensure-allocated;
        $!todo.reify-at-least($pos + 1) if $!todo.DEFINITE;
        nqp::p6bool(nqp::not_i(
          $pos < 0 || nqp::isnull(nqp::atpos($!reified, $pos))))
    }

    method reification-target(List:D:) {
        self!ensure-allocated;
        $!reified
    }

    method iterator(List:D:) {

        # something to iterate over in the future
        if $!todo.DEFINITE {
            self!ensure-allocated;
            class :: does Iterator {
                has int $!i;
                has $!list;
                has $!reified;
                has $!todo;

                method !SET-SELF(\list) {
                    $!i        = -1;
                    $!list    := list;
                    $!reified := nqp::getattr(list, List, '$!reified');
                    $!todo    := nqp::getattr(list, List, '$!todo');
                    self
                }
                method new(\list) { nqp::create(self)!SET-SELF(list) }

                method pull-one() is raw {
                    nqp::ifnull(
                      nqp::atpos($!reified,$!i = nqp::add_i($!i,1)),
                      $!todo.DEFINITE
                        ?? nqp::islt_i($!i,$!todo.reify-at-least(nqp::add_i($!i,1)))
                          ?? nqp::atpos($!reified,$!i)
                          !! self!done
                        !! IterationEnd
                    )
                }
                method !done() is raw {
                    $!todo := nqp::bindattr($!list,List,'$!todo',Mu);
                    IterationEnd
                }

                method push-until-lazy($target) {
                    if $!todo.DEFINITE {
                        my int $elems = $!todo.reify-until-lazy;
                        my $no-sink;
                        $no-sink := $target.push(nqp::atpos($!reified,$!i))
                          while nqp::islt_i($!i = nqp::add_i($!i,1),$elems);
                        $!todo.fully-reified
                          ?? self!done
                          !! STATEMENT_LIST($!i = $elems - 1; Mu)
                    }
                    else {
                        my int $elems = nqp::elems($!reified);
                        my $no-sink;
                        $no-sink := $target.push(nqp::atpos($!reified,$!i))
                          while nqp::islt_i($!i = nqp::add_i($!i,1),$elems);
                        IterationEnd
                    }
                }

                method is-lazy() { $!todo.DEFINITE && $!todo.is-lazy }
            }.new(self)
        }

        # everything we need is already there
        elsif $!reified.DEFINITE {
            class :: does Iterator {
                has $!reified;
                has int $!i;

                method !SET-SELF(\list) {
                    $!reified := nqp::getattr(list,List,'$!reified');
                    $!i        = -1;
                    self
                }
                method new(\list) { nqp::create(self)!SET-SELF(list) }

                method pull-one() is raw {
                    # lists cannot have holes, so null indicates the end
                    nqp::ifnull(
                      nqp::atpos($!reified,$!i = nqp::add_i($!i,1)),
                      IterationEnd
                    )
                }
                method push-all($target) {
                    my int $elems = nqp::elems($!reified);
                    my $no-sink;
                    $no-sink := $target.push(nqp::ifnull(nqp::atpos($!reified,$!i),Nil))
                      while nqp::islt_i($!i = nqp::add_i($!i,1),$elems);
                    IterationEnd
                }
            }.new(self)
        }

        # nothing now or in the future to iterate over
        else {
            Rakudo::Internals.EmptyIterator
        }
    }

    multi method ACCEPTS(List:D: $topic) {
        unless nqp::istype($topic, Iterable) {
            return self unless self.elems;
            return self if nqp::istype(self[0], Match);
            return False;
        }
        my $sseq = self;
        my $tseq = $topic;

        sub tailmatch($s,$t) {
            my int $spos = $s;
            my int $tpos = $t;
            while $spos < $sseq {
                # if the next element is Whatever
                if nqp::istype($sseq[$spos], HyperWhatever) {
                    # skip over all of the Whatevers
                    $spos = $spos + 1
                        while $spos <= $sseq && nqp::istype($sseq[$spos], HyperWhatever);
                    # if nothing left, we're done
                    return True if $spos == $sseq;
                    # find a target matching our new target
                    while $tpos < $tseq {
                        my $result = tailmatch($spos,$tpos);
                        return True if $result;
                        $tpos = $tpos + 1
                    }
                    # return false if we ran out
                    return False;
                }
                elsif $tpos == $tseq or not $sseq[$spos].ACCEPTS($tseq[$tpos] ) {
                    return False;
                }
                # skip matching elements
                $spos = $spos + 1;
                $tpos = $tpos + 1;
            }
            # If nothing left to match, we're successful.
            $tpos >= $tseq;
        }

        tailmatch(0,0);
    }

    multi method list(List:D:) { self }

    proto method Seq(|) is nodal { * }
    multi method Seq(List:D:) { Seq.new(self.iterator) }

    method sink(--> Nil) { }

    multi method values(List:D:) {
        Seq.new(self.iterator)
    }
    multi method keys(List:D:) {
        self.is-lazy
          ?? self.values.map: { (state $)++ }
          !! Range.new( 0, self.elems - 1 )
    }
    multi method kv(List:D:) {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has Mu $!pulled;
            has int $!on-key;
            has int $!key;

            method !SET-SELF(\iter) { $!iter := iter; $!on-key = 1; self }
            method new(\iter)   { nqp::create(self)!SET-SELF(iter) }

            method pull-one() is raw {
                if $!on-key {
                    my $pulled;
                    if ($pulled := $!iter.pull-one) =:= IterationEnd {
                        IterationEnd
                    }
                    else {
                        $!pulled := $pulled;
                        $!on-key  = 0;
                        $!key++
                    }
                }
                else {
                    $!on-key = 1;
                    $!pulled
                }
            }
            method push-all($target) {
                my $pulled;
                my int $key;
                until ($pulled := $!iter.pull-one) =:= IterationEnd {
                    $target.push(nqp::p6box_i($key));
                    $target.push($pulled);
                    $key = $key + 1;
                }
                IterationEnd
            }
        }.new(self.iterator))
    }
    multi method pairs(List:D:) {
        self.values.map: { (state $)++ => $_ }
    }
    multi method antipairs(List:D:) {
        self.values.map: { $_ => (state $)++ }
    }
    multi method invert(List:D:) {
        self.map({ nqp::decont(.value) »=>» .key }).flat
    }

    # Store in List targets containers with in the list. This handles list
    # assignemnts, like ($a, $b) = foo().
    proto method STORE(|) { * }
    multi method STORE(List:D: Iterable:D \iterable) {
        # First pass -- scan lhs containers and pick out scalar versus list
        # assignment. This also reifies the RHS values we need, and deconts
        # them. The decont is needed so that we can do ($a, $b) = ($b, $a).
        my \cv = nqp::list();
        my \lhs-iter = self.iterator;
        my \rhs-iter = iterable.iterator;
        my int $rhs-done;
        my Mu $v;
        until (my Mu \c := lhs-iter.pull-one) =:= IterationEnd {
            if nqp::iscont(c) {
                # Container: scalar assignment
                nqp::push(cv, c);
                nqp::push(cv, $rhs-done
                  || ($rhs-done = ($v := rhs-iter.pull-one) =:= IterationEnd)
                  ?? Nil
                  !! nqp::decont($v)
                );
            }
            elsif nqp::istype(c, Whatever) {
                # Whatever: skip assigning value
                $rhs-done = 1
                  if !$rhs-done && rhs-iter.pull-one =:= IterationEnd;
            }
            elsif nqp::istype(c, List) and not nqp::istype(c, Array) {
                # List splice into current lhs
                my \subiter := c.iterator;
                until (my \sc = subiter.pull-one) =:= IterationEnd {
                    nqp::push(cv, sc);
                    $v := rhs-iter.pull-one;
                    nqp::push(cv, ($rhs-done = ($v =:= IterationEnd))
                      ?? Nil
                      !! nqp::decont($v)
                    );
                }
            }
            else {
                # Non-container: store entire remaining rhs
                nqp::push(cv, c);
                nqp::push(cv, List.from-iterator(rhs-iter));
                $rhs-done = 1;
            }
        }

        # Second pass, perform the assignments.
        nqp::shift(cv) = nqp::shift(cv) while nqp::elems(cv);

        self
    }
    multi method STORE(List:D: Mu \item) {
        self.STORE((item,));
    }

    multi method gist(List:D:) {
        self.gistseen('List', {
            '(' ~ self.map( -> $elem {
                given ++$ {
                    when 101 { '...' }
                    when 102 { last }
                    default  { $elem.gist }
                }
            }).join(' ') ~ ')'
        })
    }

    multi method perl(List:D \SELF:) {
        SELF.perlseen('List', {
            '$' x nqp::iscont(SELF) ~ '('
            ~ (self.elems == 1 ?? self[0].perl ~ ',' !! self.map({.perl}).join(', '))
            ~ ')'
        })
    }

    multi method List(List:D:) { self }

    multi method Slip(List:D:) {
        if $!todo.DEFINITE {
            # We're not fully reified, and so have internal mutability still.
            # The safe thing to do is to take an iterator of ourself and build
            # the Slip out of that.
            Slip.from-iterator(self.iterator)
        }
        else {
            # We're fully reified - and so immutable inside and out! Just make
            # a Slip that shares our reified buffer.
            my \result := nqp::create(Slip);
            nqp::bindattr(result, List, '$!reified', $!reified);
            result
        }
    }

    multi method Array(List:D:) {
        # We need to populate the Array slots with Scalar containers, so no
        # shortcuts (and no special casing is likely worth it; iterators can
        # batch up the work too).
        Array.from-iterator(self.iterator)
    }
    method eager {
        $!todo.reify-all() if $!todo.DEFINITE;
        self;
    }

    method Capture() {
        fail X::Cannot::Lazy.new(:action('create a Capture from'))
            if self.is-lazy;

        # we have something to work with
        if $!reified.DEFINITE && nqp::elems($!reified) -> int $elems {
            my $capture := nqp::create(Capture);
            my $list := nqp::list;
            my $hash := nqp::hash;
            my int $i = -1;
            my $v;
            nqp::istype(($v := nqp::atpos($!reified, $i)),Pair)
              ?? nqp::bindkey($hash, nqp::unbox_s($v.key), $v.value)
              !! nqp::push($list,$v)
              while nqp::islt_i($i = nqp::add_i($i,1),$elems);
            nqp::bindattr($capture,Capture,'$!list',$list) if nqp::elems($list);
            nqp::bindattr($capture,Capture,'$!hash',$hash) if nqp::elems($hash);
            $capture
        }

        # nothing to work with
        else {
            nqp::create(Capture)
        }
    }
    method FLATTENABLE_LIST() {
        $!todo.DEFINITE
          ?? STATEMENT_LIST($!todo.reify-all; $!reified)
          !! $!reified.DEFINITE
            ?? $!reified
            !! nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer))
    }
    method FLATTENABLE_HASH() { nqp::hash() }

    method Supply(List:D:) { Supply.from-list(self) }

    method CALL-ME(List:U: |c) {
        self.new(|c);
    }

    method is-lazy() {
        $!todo.DEFINITE
          && STATEMENT_LIST($!todo.reify-until-lazy; !$!todo.fully-reified)
    }

    proto method pick(|) is nodal { * }
    multi method pick(List:D:) {
        self.is-lazy
         ?? Failure.new(X::Cannot::Lazy.new(:action('.pick from')))
         !! (my Int $elems = self.elems)
           ?? nqp::atpos($!reified, $elems.rand.floor)
           !! Nil
    }
    multi method pick(List:D: $number is copy) {
        fail X::Cannot::Lazy.new(:action('.pick from')) if self.is-lazy;
        my Int $elems = self.elems;
        return () unless $elems;

        $number = nqp::istype($number,Whatever) || $number == Inf
          ?? $elems
          !! $number.Int min $elems;
        Seq.new(class :: does Iterator {
            has $!list;
            has Int $!elems;
            has int $!number;

            method !SET-SELF(\list,$!elems,\number) {
                $!list  := nqp::clone(nqp::getattr(list,List,'$!reified'));
                $!number = number;
                self
            }
            method new(\list,\elems,\number) {
                nqp::create(self)!SET-SELF(list,elems,number)
            }
            method pull-one() {
                my int $i;
                if $!number {
                    my \tmp = nqp::atpos($!list,$i = $!elems.rand.floor);
                    nqp::bindpos(
                      $!list,$i,nqp::atpos($!list,nqp::unbox_i(--$!elems)));
                    $!number = $!number - 1;
                    tmp
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target) {
                my int $i;
                my $no-sink;
                while $!number {
                    $no-sink :=
                      $target.push(nqp::atpos($!list,$i = $!elems.rand.floor));
                    nqp::bindpos(
                      $!list,$i,nqp::atpos($!list,nqp::unbox_i(--$!elems)));
                    $!number = $!number - 1;
                }
                IterationEnd
            }
        }.new(self,$elems,$number))
    }

    proto method roll(|) is nodal { * }
    multi method roll() {
        self.is-lazy
          ?? Failure.new(X::Cannot::Lazy.new(:action('.roll from')))
          !! (my Int $elems = self.elems)
            ?? nqp::atpos($!reified, $elems.rand.floor)
            !! Nil
    }
    multi method roll(Whatever) {
        self.is-lazy
          ?? Failure.new(X::Cannot::Lazy.new(:action('.roll from')))
          !! (my Int $elems = self.elems)
            ?? Seq.from-loop({nqp::atpos($!reified, $elems.rand.floor)})
            !! ()
    }
    multi method roll(\number) {
        number == Inf
          ?? self.roll(*)
          !! self.is-lazy
            ?? Failure.new(X::Cannot::Lazy.new(:action('.roll from')))
            !! self.elems   # this allocates/reifies
              ?? Seq.new(class :: does Iterator {
                     has $!list;
                     has Int $!elems;
                     has int $!todo;
                     method !SET-SELF(\list,\todo) {
                         $!list := nqp::getattr(list,List,'$!reified');
                         $!elems = nqp::elems($!list);
                         $!todo  = todo;
                         self
                     }
                     method new(\list,\todo) {
                         nqp::create(self)!SET-SELF(list,todo)
                     }
                     method pull-one() is raw {
                         if $!todo {
                             $!todo = $!todo - 1;
                             nqp::atpos($!list,$!elems.rand.floor)
                         }
                         else {
                             IterationEnd
                         }
                     }
                 }.new(self,number.Int))
              !! ()
    }

    method reverse() is nodal {
        fail X::Cannot::Lazy.new(:action<reverse>) if self.is-lazy;
        my $rlist   := nqp::create(self);
        my $reified := $!reified;
        if $reified {
            my int $i     = -1;
            my int $elems = nqp::elems($reified);
            my int $last  = $elems - 1;
            my $reversed := nqp::list;
            nqp::setelems($reversed,$elems);
            nqp::bindpos($reversed, $last - $i, nqp::atpos($reified, $i))
                while ($i = $i + 1) < $elems;
            nqp::bindattr($rlist, List, '$!reified', $reversed);
        }
        $rlist
    }

    method rotate(Int(Cool) $rotate = 1) is nodal {
        fail X::Cannot::Lazy.new(:action<rotate>) if self.is-lazy;
        my int $elems = self.elems;  # this allocates/reifies
        my $rotated := nqp::create(self);
        if $elems {
            my int $n = $rotate % $elems;
            my $list := nqp::clone($!reified);
            if $n > 0 {
                $n = $n + 1;
                nqp::push($list, nqp::shift($list)) while $n = $n - 1;
            }
            elsif $n < 0 {
                $n = $n - 1;
                nqp::unshift($list, nqp::pop($list)) while $n = $n + 1;
            }
            nqp::bindattr($rotated,List,'$!reified',$list);
        }
        $rotated;
    }

    method rotor(List:D: *@cycle, :$partial) is nodal {
        self!ensure-allocated;
        die "Must specify *how* to rotor a List"
          unless @cycle.is-lazy || @cycle;

        my $finished = 0;
        # (Note, the xx should be harmless if the cycle is already infinite by accident.)
        my @c := @cycle.is-lazy ?? @cycle !! (@cycle xx *).cache;
        gather for flat @c -> $s {
            my $elems;
            my $gap;
            if $s ~~ Pair {
                $elems = +$s.key;
                $gap   = +$s.value;
            }
            elsif $s < 1 {
                die "Cannot have elems < 1, did you mean to specify a Pair with => $s?";
            }
            else {
                $elems = +$s;
                $gap   = 0;
            }

            $!todo.reify-at-least($finished + $elems) if $!todo.DEFINITE;
            if $finished + $elems <= nqp::elems($!reified) {
                take self[$finished ..^ $finished + $elems];
                $finished += $elems + $gap;
            }
            else {
                take self[$finished .. *]
                  if $partial and $finished < self.elems;
                last;
            }
        }
    }

    proto method combinations($?) is nodal {*}
    multi method combinations( Int $of ) {
        combinations(self.elems, $of).map: { self[@$_] }
    }
    multi method combinations( Range $ofrange = 0 .. * ) {
        my $over := ($ofrange.first max 0)
                 .. (($ofrange.first(:end) // -1) min self.elems);

        $over.map: { |combinations(self.elems, $_).map: { self[@$_] } }
    }

    proto method permutations(|) is nodal {*}
    multi method permutations() is nodal {
        permutations(self.elems).map: { self[@$_] }
    }

    method join(List:D: $separator = '') is nodal {
        my int $infinite;
        if $!todo.DEFINITE {
            $!todo.reify-until-lazy;
            $!todo.fully-reified
              ?? ($!todo := Mu)
              !! ($infinite = 1);
        }

        # something to join
        if $!reified.DEFINITE && nqp::elems($!reified) -> int $elems {
            my Mu $strings := nqp::setelems(nqp::list_s,$elems + $infinite);
            my int $i = -1;

            my $tmp;
            nqp::bindpos_s($strings,$i,
              nqp::isnull($tmp := nqp::atpos($!reified,$i))
              ?? ''
              !! nqp::unbox_s(nqp::isconcrete($tmp) && nqp::istype($tmp,Str)
                  ?? $tmp
                  !! nqp::can($tmp,'Str')
                    ?? $tmp.Str
                    !! nqp::box_s($tmp,Str)
                 )
            ) while nqp::islt_i(++$i,$elems);

            nqp::bindpos_s($strings,$i,'...') if $infinite;
            nqp::join(nqp::unbox_s($separator.Str),$strings)
        }

        # nothing to join
        else {
            $infinite ?? '...' !! ''
        }
    }

    method push(|) is nodal {
        X::Immutable.new(:typename<List>,:method<push>).throw
    }
    method append(|) is nodal {
        X::Immutable.new(:typename<List>,:method<append>).throw
    }
    method unshift(|) is nodal {
        X::Immutable.new(:typename<List>,:method<unshift>).throw
    }
    method prepend(|) is nodal {
        X::Immutable.new(:typename<List>,:method<prepend>).throw
    }
    method shift(|) is nodal {
        X::Immutable.new(:typename<List>,:method<shift>).throw
    }
    method pop(|) is nodal {
        X::Immutable.new(:typename<List>, :method<pop>).throw
    }
}

# The , operator produces a List.
proto sub infix:<,>(|) is pure {*}
multi sub infix:<,>() { nqp::create(List) }
multi sub infix:<,>(|) {
    my \result  = nqp::create(List);
    my \in      = nqp::p6argvmarray();
    my \reified = nqp::create(IterationBuffer);
    nqp::bindattr(result, List, '$!reified', reified);
    nqp::istype(nqp::atpos(in, 0), Slip)
      ?? STATEMENT_LIST(
           # We saw a Slip, so we'll lazily deal with the rest of the things
           # (as the Slip may expand to something lazy).
           my \todo := nqp::create(List::Reifier);
           nqp::bindattr(result, List, '$!todo', todo);
           nqp::bindattr(todo, List::Reifier, '$!reified', reified);
           nqp::bindattr(todo, List::Reifier, '$!future', in);
           nqp::bindattr(todo, List::Reifier, '$!reification-target',
             result.reification-target());
           last;
         )
      !! STATEMENT_LIST(
           nqp::push(reified, nqp::shift(in));
           Nil;
         )
      while nqp::elems(in); # don't Sink the thing above

    result
}

sub list(+l) { l }

# Use **@list and then .flat it, otherwise we'll end up remembering all the
# things we flatten, which would be different semantics to .flat which gives
# back a Seq.
sub flat(**@list is raw) {
    @list.flat
}

sub cache(+@l) { @l }

role XX-Whatever does Iterator {
    has Mu $!x;
    method !SET-SELF($!x) { self }
    method new(\x) { nqp::create(self)!SET-SELF(x) }
    method is-lazy() { True }
}

proto sub infix:<xx>(Mu $, $, *%) { * }
multi sub infix:<xx>() { Failure.new("No zero-arg meaning for infix:<xx>") }
multi sub infix:<xx>(Mu \x) { x }
multi sub infix:<xx>(&x, Num $n) {
    infix:<xx>(&x, $n == Inf ?? Whatever !! $n.Int);
}
multi sub infix:<xx>(&x, Whatever) {
    Seq.new(class :: does XX-Whatever {
        has @!slipped;
        method pull-one() {
            if @!slipped {
                @!slipped.shift
            }
            else {
                my $pulled := $!x.();
                if nqp::istype($pulled,Slip) {
                    @!slipped = $pulled;
                    @!slipped.shift
                }
                elsif nqp::istype($pulled, Seq) {
                    $pulled.cache
                }
                else {
                    $pulled
                }
            }
        }
    }.new(&x))
}
multi sub infix:<xx>(&x, Int() $n) {
    my int $todo = $n;
    my Mu $pulled;
    my Mu $list := nqp::list();
    while $todo > 0 {
        $pulled := &x.();
        if nqp::istype($pulled,Slip) {
            nqp::push($list, $_) for $pulled;
        }
        elsif nqp::istype($pulled,Seq) {
            nqp::push($list, $pulled.cache);
        }
        else {
            nqp::push($list, $pulled);
        }
        $todo = $todo - 1;
    }
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $list)
}
multi sub infix:<xx>(Mu \x, Num $n) {
    infix:<xx>(x, $n == Inf ?? Whatever !! $n.Int);
}
multi sub infix:<xx>(Mu \x, Whatever) {
    Seq.new(class :: does XX-Whatever {
        method pull-one() { $!x }
    }.new(x))
}
multi sub infix:<xx>(Mu \x, Int() $n) is pure {
    my int $elems = $n;
    my Mu $list := nqp::list();
    if $elems > 0 {
        nqp::setelems($list, $elems);  # presize
        my int $i;
        while $i < $elems {
            nqp::bindpos($list, $i, x);
            $i = $i + 1;
        }
    }
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $list)
}

proto sub reverse(|)   { * }
multi sub reverse(@a)  { @a.reverse }
multi sub reverse(+@a) { @a.reverse }

sub rotate(@a, Int $n = 1)  { @a.rotate($n) }

sub prefix:<|>(\x) { x.Slip }

multi sub infix:<cmp>(@a, @b) {
    (@a Zcmp @b).first(&prefix:<?>) || @a <=> @b
}

proto sub infix:<X>(|) is pure {*}
multi sub infix:<X>(+lol, :$with!) {
    METAOP_CROSS($with, find-reducer-for-op($with))(|lol.list);
}
multi sub infix:<X>(+lol) {
    my int $n = lol.elems - 1;
    my $laze = False;
    my @l = do for 0..$n -> $i {
        my \elem = lol[$i];
        if nqp::iscont(elem) {
            (elem,)
        }
        else {
            $laze = True if $i and elem.is-lazy;
            elem.list
        }
    }

    my Mu $v := nqp::list();
    my int $i = 0;

    if $laze {  # general case treats all lists as potentially lazy
        return gather {
            my @i = @l.map: *.iterator;
            while $i >= 0 {
                my \e = @i[$i].pull-one();
                if !(e =:= IterationEnd) {
                    nqp::bindpos($v, $i, e);
                    if $i >= $n { take nqp::clone($v) }
                    else {
                        $i = $i + 1;
                        my \elem = lol[$i];
                        @l[$i] = nqp::istype(elem, Iterable) ?? elem !! elem.list;
                    }
                }
                else { $i = $i - 1 }
            }
        }.lazy;
    }

    # eagerize 2nd and subsequent lists if finite
    my Mu $end := nqp::list_i();
    for 1 .. $n -> $i {
        nqp::bindpos_i($end,$i,@l[$i].elems);
    }
    $laze = True if @l[0].is-lazy;  # check pass-thru on the 1st one too

    # optimize for 2D and 3D crosses
    if $n == 1 { # 2-dimensional
        gather {
            my int $e = nqp::atpos_i($end,1);
            my $l0 = @l[0];
            my $l1 = @l[1];
            my \source = $l0.iterator;
            until (my \value = source.pull-one) =:= IterationEnd {
                nqp::bindpos($v, 0, value);
                loop (my int $j = 0; $j < $e; $j = $j + 1) {
                    nqp::bindpos($v, 1, $l1[$j]);
                    take nqp::clone($v);
                }
            }
        }.lazy-if($laze);
    }
    elsif $n == 2 { # 3-dimensional
        gather {
            my int $e1 = nqp::atpos_i($end,1);
            my int $e2 = nqp::atpos_i($end,2);
            my $l0 = @l[0];
            my $l1 = @l[1];
            my $l2 = @l[2];
            my \source = $l0.iterator;
            until (my \value = source.pull-one) =:= IterationEnd {
                nqp::bindpos($v, 0, value);
                loop (my int $j = 0; $j < $e1; $j = $j + 1) {
                    nqp::bindpos($v, 1, $l1[$j]);
                    loop (my int $k = 0; $k < $e2; $k = $k + 1) {
                        nqp::bindpos($v, 2, $l2[$k]);
                        take nqp::clone($v);
                    }
                }
            }
        }.lazy-if($laze);
    }
    else { # more than 3 dimensions
        my Mu $jsave := nqp::list_i();
        my \source = @l[0].iterator;
        gather {
            while $i == 0 {
                my \e = source.pull-one;
                if !(e =:= IterationEnd) {
                    nqp::bindpos($v, $i, e);

                    if $i >= $n { take nqp::clone($v) }
                    else { $i = $i + 1; }

                    my int $j = 0;
                    while $i >= 1 {
                        if $j < nqp::atpos_i($end,$i) {
                            nqp::bindpos($v, $i, @l[$i][$j]);
                            $j = $j + 1;

                            if $i >= $n { take nqp::clone($v) }
                            else {
                                nqp::bindpos_i($jsave, $i, $j);
                                $i = $i + 1;
                                $j = 0;
                            }
                        }
                        else {
                            $i = $i - 1;
                            $j = nqp::atpos_i($jsave,$i);
                        }
                    }
                }
                else { $i = $i - 1 }
            }
        }.lazy-if($laze);
    }
}

my &cross = &infix:<X>;

proto sub infix:<Z>(|) is pure {*}
multi sub infix:<Z>(+lol, :$with!) {
    METAOP_ZIP($with, find-reducer-for-op($with))(|lol.list);
}
multi sub infix:<Z>(+lol) {
    my $arity = lol.elems;
    my $laze = True;
    return () if $arity == 0;
    eager my @l = (^$arity).map: -> $i {
        my \elem = lol[$i];
        if nqp::iscont(elem) {
            $laze = False;
            Rakudo::Internals::WhateverIterator.new((elem,).iterator)
        }
        else {
            $laze = False unless elem.is-lazy;
            Rakudo::Internals::WhateverIterator.new(elem.iterator)
        }
    };

    gather {
        loop {
            my \p = @l.map: {
                my \val = .pull-one;
                last if val =:= IterationEnd;
                val
            }
            my \l = p.list;
            last if l.elems < $arity;
            take-rw l;
        }
    }.lazy-if($laze);
}

my &zip := &infix:<Z>;

sub roundrobin(**@lol is raw) {
    my $laze = False;
    my @iters = do for @lol -> \elem {
        if nqp::iscont(elem) {
            (elem,).iterator
        }
        else {
            $laze = True if elem.is-lazy;
            elem.iterator
        }
    }
    gather {
        while @iters {
            my @new-iters;
            my @values;
            for @iters -> $i {
                my \v = $i.pull-one;
                unless v =:= IterationEnd {
                    @values.push: v;
                    @new-iters.push: $i;
                }
            }
            take @values.List if @values;
            @iters = @new-iters;
        }
    }.lazy-if($laze);
}

# vim: ft=perl6 expandtab sw=4
