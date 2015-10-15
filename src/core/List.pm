# for our tantrums
my class X::TypeCheck { ... }
my class X::TypeCheck::Splice { ... }
my class X::Cannot::Lazy { ... }
my class X::Cannot::Empty { ... }
my class X::Immutable { ... }
my role Supply { ... }

my sub combinations($n, $k) {
    my @result;
    my @stack;

    return ((),) unless $k;

    @stack.push(0);
    gather while @stack {
        my $index = @stack - 1;
        my $value = @stack.pop;

        while $value < $n {
            @result[$index++] = $value++;
            @stack.push($value);
            if $index == $k {
                my @copy = @result.clone;
                take infix:<,>(|@copy);
                # take @result.map(-> $x { $x }).list;
                $value = $n;  # fake a last
            }
        }
    }
}

# XXX This next function is only checked in to help 
#     debug a fairly insane bug.  It is not intended 
#     to be a long term part of rakudo.
sub very-broken-combinations($n, $k) {
    my @result;
    my $value = 1;

    gather for 0..4 {
        for 0..1 {
            @result[$_] = $value++;
        }
        take @result;
    }
}

my sub permutations(Int $n) {
    $n == 1 ?? ( (0,), ) !!
    gather for ^$n -> $i {
        my @i = flat 0 ..^ $i, $i ^..^ $n;
        for permutations($n - 1) {
            take ($i, |@i[@$_])
        }
    }
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
            if $!current-iter.DEFINITE {
                if $!current-iter.push-at-least($!reification-target,
                        $elems - nqp::elems($!reified)) =:= IterationEnd {
                    $!current-iter := Iterator;
                }
            }
            while nqp::elems($!reified) < $elems &&
                    $!future.DEFINITE && nqp::elems($!future) {
                my \current = nqp::shift($!future);
                $!future := Mu unless nqp::elems($!future);
                if nqp::istype(current, Slip) && nqp::isconcrete(current) {
                    my \iter = current.iterator;
                    my int $deficit = $elems - nqp::elems($!reified);
                    unless iter.push-at-least($!reification-target, $deficit) =:= IterationEnd {
                        # The iterator produced enough values to fill the need,
                        # but did not reach its end. We save it for next time. We
                        # know we'll exit the loop, since the < $elems check must
                        # come out False (unless the iterator broke contract).
                        $!current-iter := iter;
                    }
                }
                else {
                    my $ = $!reification-target.push(current);
                }
            }
            nqp::elems($!reified);
        }

        method reify-until-lazy() {
            if $!current-iter.DEFINITE {
                if $!current-iter.push-until-lazy($!reification-target) =:= IterationEnd {
                    $!current-iter := Iterator;
                }
            }
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
            !$!current-iter.DEFINITE && !$!future.DEFINITE
        }

        method is-lazy() {
            $!current-iter.DEFINITE ?? $!current-iter.is-lazy !! False
        }
    }

    method from-iterator(List:U: Iterator $iter) {
        my \result := self.CREATE;
        my \buffer := IterationBuffer.CREATE;
        my \todo := Reifier.CREATE;
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
        my \result := self.CREATE;
        my \buffer := IterationBuffer.CREATE;
        my \todo := List::Reifier.CREATE;
        nqp::bindattr(result, List, '$!reified', buffer);
        nqp::bindattr(result, List, '$!todo', todo);
        nqp::bindattr(todo, List::Reifier, '$!reified', buffer);
        nqp::bindattr(todo, List::Reifier, '$!future', vm-tuple);
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
                my \result := self.CREATE;
                my \buffer := IterationBuffer.CREATE;
                my \todo := List::Reifier.CREATE;
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
        my Mu \vm-tuple = nqp::captureposarg(nqp::usecapture(), 1);
        my \future = IterationBuffer.CREATE;
        my int $i = 0;
        my int $n = nqp::elems(vm-tuple);
        while $i < $n {
            my \consider = nqp::atpos(vm-tuple, $i);
            my $no-sink := nqp::push(future, nqp::iscont(consider)
                ?? consider
                !! nqp::istype(consider, Iterable) && consider.DEFINITE
                    ?? (nqp::istype(consider, PositionalBindFailover)
                            ?? consider.cache
                            !! consider
                        ).flat.Slip
                    !! consider);
            $i = $i + 1;
        }

        my \result := self.CREATE;
        my \buffer := IterationBuffer.CREATE;
        my \todo := List::Reifier.CREATE;
        nqp::bindattr(result, List, '$!reified', buffer);
        nqp::bindattr(result, List, '$!todo', todo);
        nqp::bindattr(todo, List::Reifier, '$!reified', buffer);
        nqp::bindattr(todo, List::Reifier, '$!future', future);
        nqp::bindattr(todo, List::Reifier, '$!reification-target',
            result.reification-target());
        result
    }

    method new(**@things) {
        my \list = self.CREATE;
        my \iterbuffer = IterationBuffer.CREATE;
        nqp::bindattr(list, List, '$!reified', iterbuffer);
        for @things {
            my $no-sink := iterbuffer.push($_);
        }
        list
    }

    method !ensure-allocated() {
        $!reified := IterationBuffer.CREATE unless $!reified.DEFINITE;
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
    method to()         { self.elems ?? self[self.end].to !! Nil }
    method from()       { self.elems ?? self[0].from !! Nil }

    method fmt($format = '%s', $separator = ' ') {
        self.map({ .fmt($format) }).join($separator);
    }

    multi method elems(List:D:) is nodal {
        self!ensure-allocated;
        if $!todo.DEFINITE {
            $!todo.reify-until-lazy();
            if $!todo.fully-reified {
                $!todo := Mu;
            }
            else {
                fail X::Cannot::Lazy.new(:action('.elems'));
            }
        }
        nqp::elems($!reified)
    }

    multi method AT-POS(List:D: Int $pos) is raw {
        self!ensure-allocated;
        my int $ipos = nqp::unbox_i($pos);
        $ipos < nqp::elems($!reified) && $ipos >= 0
            ?? nqp::atpos($!reified, $ipos)
            !! self!AT-POS-SLOWPATH($ipos);
    }

    multi method AT-POS(List:D: int $pos) is raw {
        self!ensure-allocated;
        $pos < nqp::elems($!reified) && $pos >= 0
            ?? nqp::atpos($!reified, $pos)
            !! self!AT-POS-SLOWPATH($pos);
    }

    method !AT-POS-SLOWPATH(int $pos) is raw {
        fail X::OutOfRange.new(:what<Index>, :got($pos), :range<0..Inf>)
            if $pos < 0;
        $!todo.DEFINITE && $!todo.reify-at-least($pos + 1) > $pos
            ?? nqp::atpos($!reified, $pos)
            !! Nil
    }

    multi method EXISTS-POS(List:D: int $pos) {
        self!ensure-allocated;
        $!todo.reify-at-least($pos + 1) if $!todo.DEFINITE;
        nqp::islt_i($pos, 0) || nqp::isnull(nqp::atpos($!reified, $pos))
            ?? False
            !! True
    }
    multi method EXISTS-POS(List:D: Int:D $pos) {
        self!ensure-allocated;
        $!todo.reify-at-least($pos + 1) if $!todo.DEFINITE;
        $pos < 0 || nqp::isnull(nqp::atpos($!reified, $pos))
            ?? False
            !! True
    }

    method reification-target(List:D:) {
        self!ensure-allocated;
        $!reified
    }

    method iterator(List:D:) {
        self!ensure-allocated;
        class :: does Iterator {
            has int $!i;
            has $!reified;
            has $!todo;
            has $!oftype;

            method BUILD(\list, Mu \oftype) {
                $!reified := nqp::getattr(list, List, '$!reified');
                $!todo    := nqp::getattr(list, List, '$!todo');
                $!oftype  := oftype =:= Mu ?? Any !! oftype;
                self
            }
            method new(\list) { nqp::create(self).BUILD(list,list.of) }

            method pull-one() is raw {
                my int $i = $!i;
                $i < nqp::elems($!reified)
                    ?? nqp::ifnull(nqp::atpos($!reified, ($!i = $i + 1) - 1), $!oftype)
                    !! self!reify-and-pull-one()
            }

            method !reify-and-pull-one() is raw {
                my int $i = $!i;
                $!todo.DEFINITE && $i < $!todo.reify-at-least($i + 1)
                    ?? nqp::ifnull(nqp::atpos($!reified, ($!i = $i + 1) - 1), $!oftype)
                    !! IterationEnd
            }

            method push-until-lazy($target) {
                my int $n = $!todo.DEFINITE
                    ?? $!todo.reify-until-lazy()
                    !! nqp::elems($!reified);
                my int $i = $!i;
                my $no-sink;
                while $i < $n {
                    $no-sink := $target.push(nqp::ifnull(nqp::atpos($!reified, $i), $!oftype));
                    $i = $i + 1;
                }
                $!i = $n;
                !$!todo.DEFINITE || $!todo.fully-reified ?? IterationEnd !! Mu
            }

            method is-lazy() {
                $!todo.DEFINITE ?? $!todo.is-lazy !! False
            }
        }.new(self)
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
        gather self.values.map: {
            take (state $)++;
            take-rw $_;
        }
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
        my int $rhs-done = 0;
        loop {
            my Mu \c := lhs-iter.pull-one;
            last if c =:= IterationEnd;
            if nqp::iscont(c) {
                # Container: scalar assignment
                nqp::push(cv, c);
                if $rhs-done {
                    nqp::push(cv, Nil);
                }
                elsif (my \v = rhs-iter.pull-one) =:= IterationEnd {
                    nqp::push(cv, Nil);
                    $rhs-done = 1;
                }
                else {
                    nqp::push(cv, nqp::iscont(v) ?? nqp::decont(v) !! v);
                }
            }
            elsif nqp::istype(c, Whatever) {
                # Whatever: skip assigning value
                unless $rhs-done {
                    my \v = rhs-iter.pull-one;
                    $rhs-done = 1 if v =:= IterationEnd;
                }
            }
            elsif nqp::istype(c, List) and not nqp::istype(c, Array) {
                # List splice into current lhs
                my \subiter := c.iterator;
                until (my \sc = subiter.pull-one) =:= IterationEnd {
                    nqp::push(cv, sc);
                    if (my \v = rhs-iter.pull-one) =:= IterationEnd {
                        nqp::push(cv, Nil);
                        $rhs-done = 1;
                    }
                    else {
                        nqp::push(cv, nqp::iscont(v) ?? nqp::decont(v) !! v);
                    }
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
        while nqp::elems(cv) {
            my \c := nqp::shift(cv);
            c = nqp::shift(cv);
        }

        self
    }
    multi method STORE(List:D: \item) {
        self.STORE((item,));
    }

    multi method gist(List:D:) {
        if not %*gistseen<TOP> { my %*gistseen = :TOP ; return self.gist }
        if %*gistseen{self.WHICH} { %*gistseen{self.WHICH} = 2; return "List_{self.WHERE}" }
        %*gistseen{self.WHICH} = 1;
        my $result = '(' ~
            self.map( -> $elem {
                given ++$ {
                    when 101 { '...' }
                    when 102 { last }
                    default  { $elem.gist }
                }
            }).join(' ')
            ~ ')';
        $result = "(\\List_{self.WHERE} = $result)" if %*gistseen{self.WHICH}:delete == 2;
        $result;
    }

    multi method perl(List:D \SELF:) {
        if not %*perlseen<TOP> { my %*perlseen = :TOP ; return SELF.perl }
        if %*perlseen{self.WHICH} { %*perlseen{self.WHICH} = 2; return "List_{self.WHERE}" }
        %*perlseen{self.WHICH} = 1;
        my $result =
            '$' x nqp::iscont(SELF) ~ '('
            ~ (self.elems == 1 ?? self[0].perl ~ ',' !! self.map({.perl}).join(', '))
            ~ ')';
        $result = "(my \\List_{self.WHERE} = $result)" if %*perlseen{self.WHICH}:delete == 2;
        $result;
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
            my \result := Slip.CREATE;
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
        $!todo.reify-all() if $!todo.DEFINITE;
        my $cap := nqp::create(Capture);
        nqp::bindattr($cap, Capture, '$!list', $!reified);

        my \positional := IterationBuffer.CREATE;
        my Mu $hash := nqp::hash();
        my int $c = nqp::elems($!reified);
        my int $i = 0;
        while $i < $c {
            my $v := nqp::atpos($!reified, $i);
            nqp::istype($v, Pair)
                ??  nqp::bindkey($hash, nqp::unbox_s($v.key), $v.value)
                !!  positional.push($v);
            $i = $i + 1;
        }
        nqp::bindattr($cap, Capture, '$!list', positional);
        nqp::bindattr($cap, Capture, '$!hash', $hash);
        $cap
    }
    method FLATTENABLE_LIST() {
        self!ensure-allocated;
        $!todo.reify-all() if $!todo.DEFINITE;
        $!reified
    }
    method FLATTENABLE_HASH() { nqp::hash() }

    method Supply(List:D:) { Supply.from-list(self) }

    method CALL-ME(List:U: |c) {
        self.new(|c);
    }

    method is-lazy() {
        if $!todo.DEFINITE {
            $!todo.reify-until-lazy();
            !$!todo.fully-reified
        }
        else {
            False
        }
    }

    proto method pick(|) is nodal { * }
    multi method pick(List:D:) {
        fail X::Cannot::Lazy.new(:action('.pick from'))
            if self.is-lazy;
        my $elems = self.elems;
        $elems ?? nqp::atpos($!reified, $elems.rand.floor) !! Nil;
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

            submethod BUILD(\list,$!elems,\number) {
                $!list  := nqp::clone(nqp::getattr(list,List,'$!reified'));
                $!number = number;
                self
            }
            method new(\list,\elems,\number) {
                nqp::create(self).BUILD(list,elems,number)
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
        fail X::Cannot::Lazy.new(:action('.roll from'))
            if self.is-lazy;
        my $elems = self.elems;
        $elems ?? nqp::atpos($!reified, $elems.rand.floor) !! Nil;
    }
    multi method roll(Whatever) {
        fail X::Cannot::Lazy.new(:action('.roll from')) if self.is-lazy;
        my $elems = self.elems;
        return () unless $elems;

        # directly use Seq.from-loop so that the result is known infinite
        Seq.from-loop({nqp::atpos($!reified, $elems.rand.floor)});
    }
    multi method roll(\number) {
        return self.roll(*) if number == Inf;

        fail X::Cannot::Lazy.new(:action('.roll from')) if self.is-lazy;
        my $elems = self.elems;
        return () unless $elems;

        my int $n = number.Int;

        gather while $n > 0 {
            take nqp::atpos($!reified,$elems.rand.floor);
            $n = $n - 1;
        }
    }

    method reverse() is nodal {
        self!ensure-allocated;
        fail X::Cannot::Lazy.new(:action<reverse>) if self.is-lazy;
        my $reversed := IterationBuffer.new;
        my $reified  := $!reified;
        my int $i    = 0;
        my int $n    = nqp::elems($reified);
        while $i < $n {
            nqp::bindpos($reversed, $n - ($i + 1), nqp::atpos($reified, $i));
            $i = $i + 1;
        }
        my $rlist := nqp::create(self.WHAT);
        nqp::bindattr($rlist, List, '$!reified', $reversed);
        $rlist;
    }

    method rotate(Int(Cool) $n is copy = 1) is nodal {
        self!ensure-allocated;
        fail X::Cannot::Lazy.new(:action<rotate>) if self.is-lazy;
        my $elems = self.elems;
        return () unless $elems;

        $n %= $elems;
        return self if $n == 0;

        my $rot := nqp::clone($!reified);
        if $n > 0 {
            nqp::push($rot, nqp::shift($rot)) while $n--;
        }
        elsif $n < 0 {
            nqp::unshift($rot, nqp::pop($rot)) while $n++;
        }
        my $rlist := nqp::create(self.WHAT);
        nqp::bindattr($rlist, List, '$!reified', $rot);
        $rlist;
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
        gather for $ofrange.min .. ($ofrange.max min self.elems) -> $of {
            for combinations(self.elems, $of) {
                take self[@$_]
            }
        }
    }

    proto method permutations(|) is nodal {*}
    multi method permutations() is nodal {
        # need block on Moar because of RT#121830
        permutations(self.elems).map: { self[@$_] }
    }

    method join($separator = '') is nodal {
        self!ensure-allocated;
        my $infinite = False;
        if $!todo.DEFINITE {
            $!todo.reify-until-lazy();
            $infinite = !$!todo.fully-reified()
        }
        my Mu $rsa := nqp::list_s();
        unless $infinite {
            nqp::setelems($rsa, nqp::unbox_i(self.elems));
            nqp::setelems($rsa, 0);
        }
        my $tmp;
        my int $i = 0;
        my int $n = nqp::elems($!reified);
        while $i < $n {
            $tmp := nqp::ifnull(nqp::atpos($!reified, $i), Any);
            # Not sure why this works 
            nqp::push_s($rsa, nqp::unbox_s(nqp::istype($tmp, Str) && nqp::isconcrete($tmp) ?? 
                        $tmp !! 
                        nqp::can($tmp, 'Str') ?? $tmp.Str !! nqp::box_s($tmp, Str) ));
            $i = $i + 1;
        }
        nqp::push_s($rsa, '...') if $infinite;
        nqp::p6box_s(nqp::join(nqp::unbox_s($separator.Str), $rsa))
    }

    method push(|) is nodal {
        X::Immutable.new( method => 'push',    typename => 'List' ).throw;
    }
    method pop(|) is nodal {
        X::Immutable.new( method => 'pop',     typename => 'List' ).throw;
    }
    method shift(|) is nodal {
        X::Immutable.new( method => 'shift',   typename => 'List' ).throw;
    }
    method unshift(|) is nodal {
        X::Immutable.new( method => 'unshift', typename => 'List' ).throw;
    }
}

# The , operator produces a List.
proto sub infix:<,>(|) is pure {*}
multi sub infix:<,>() {
    my \result = List.CREATE;
    nqp::bindattr(result, List, '$!reified', BEGIN IterationBuffer.CREATE);
    result
}
multi sub infix:<,>(|) {
    my \result  = List.CREATE;
    my \in      = nqp::p6argvmarray();
    my \reified = IterationBuffer.CREATE;
    nqp::bindattr(result, List, '$!reified', reified);
    while nqp::elems(in) {
        if nqp::istype(nqp::atpos(in, 0), Slip) {
            # We saw a Slip, so we'll lazily deal with the rest of the things
            # (as the Slip may expand to something lazy).
            my \todo := List::Reifier.CREATE;
            nqp::bindattr(result, List, '$!todo', todo);
            nqp::bindattr(todo, List::Reifier, '$!reified', reified);
            nqp::bindattr(todo, List::Reifier, '$!future', in);
            nqp::bindattr(todo, List::Reifier, '$!reification-target',
                result.reification-target());
            last;
        }
        else {
            nqp::push(reified, nqp::shift(in));
            Nil # don't Sink the thing above
        }
    }
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

proto sub infix:<xx>(Mu $, $, *%) { * }
multi sub infix:<xx>(Mu \x, Num $n, :$thunked!) {
    infix:<xx>(x, $n == Inf ?? Whatever !! $n.Int, :$thunked);
}
multi sub infix:<xx>(Mu \x, Whatever, :$thunked!) {
    GATHER({ loop { take x.() } }).lazy
}
multi sub infix:<xx>(Mu \x, Int() $n, :$thunked!) {
    my int $todo = $n;
    GATHER({ take x.() while ($todo = $todo - 1) >= 0 })
}
multi sub infix:<xx>(Mu \x, Num $n) {
    infix:<xx>(x, $n == Inf ?? Whatever !! $n.Int);
}
multi sub infix:<xx>(Mu \x, Whatever) {
    GATHER({ loop { take x } }).lazy
}
multi sub infix:<xx>(Mu \x, Int() $n) {
    my int $elems = $n;
    my Mu $list := nqp::list();
    if $elems > 0 {
        my int $i;
        nqp::setelems($list, $elems);
        while $i < $elems {
            nqp::bindpos($list, $i, x);
            $i = $i + 1;
        }
    }
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $list)
}

sub reverse(+@a)            { @a.reverse }
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
        $laze = True if $i and elem.is-lazy;
        elem.list
    }

    my Mu $v := nqp::list();
    my int $i = 0;

    if $laze {  # general case treats all lists as potentially lazy
        return gather {
            my @i = @l.map: *.iterator;
            while $i >= 0 {
                my \e = @i[$i].pull-one();
                if e !=:= IterationEnd {
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
                if e !=:= IterationEnd {
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
        $laze = False unless elem.is-lazy;
        Rakudo::Internals::WhateverIterator.new(
            nqp::istype(elem, Iterable)
                ?? elem.iterator
                !! elem.list.iterator
        )
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

sub roundrobin(**@lol) {
    my @iters = @lol.map: { nqp::istype($_, Iterable) ?? .iterator !! .list.iterator };
    my $laze = so any(@lol).is-lazy;
    gather {
        while @iters {
            my @new-iters;
            my @values;
            for @iters -> $i {
                my \v = $i.pull-one;
                if v !=:= IterationEnd {
                    @values.push: v;
                    @new-iters.push: $i;
                }
            }
            take @values if @values;
            @iters = @new-iters;
        }
    }.lazy-if($laze);
}

# vim: ft=perl6 expandtab sw=4
