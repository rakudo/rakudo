my class X::TypeCheck { ... };
my class X::Subscript::Negative { ... };

# An Array is a List that ensures every item added to it is in a Scalar
# container. It also supports push, pop, shift, unshift, splice, BIND-POS,
# and so forth.
my class Array { # declared in BOOTSTRAP
    # class Array is List {
    #     has Mu $!descriptor;

    my class ArrayReificationTarget {
        has $!target;
        has $!descriptor;

        method new(\target, Mu \descriptor) {
            my \rt = self.CREATE;
            nqp::bindattr(rt, self, '$!target', target);
            nqp::bindattr(rt, self, '$!descriptor', descriptor);
            rt
        }

        method push(Mu \value) {
            nqp::push($!target,
                nqp::assign(nqp::p6scalarfromdesc($!descriptor), value));
        }
    }

    method from-iterator(Array:U: Iterator $iter) {
        my \result := self.CREATE;
        my \buffer := IterationBuffer.CREATE;
        my \todo := List::Reifier.CREATE;
        nqp::bindattr(result, List, '$!reified', buffer);
        nqp::bindattr(result, List, '$!todo', todo);
        nqp::bindattr(todo, List::Reifier, '$!reified', buffer);
        nqp::bindattr(todo, List::Reifier, '$!current-iter', $iter);
        nqp::bindattr(todo, List::Reifier, '$!reification-target',
            result.reification-target());
        todo.reify-until-lazy();
        result
    }

    method new(**@values is rw) {
        my \arr = nqp::create(self);
        arr.STORE(@values);
        arr
    }

    method !ensure-allocated() {
        nqp::bindattr(self, List, '$!reified', IterationBuffer.CREATE)
            unless nqp::getattr(self, List, '$!reified').DEFINITE;
    }

    method is-lazy() {
        my $todo := nqp::getattr(self, List, '$!todo');
        if $todo.DEFINITE {
            $todo.reify-until-lazy();
            if $todo.fully-reified {
                nqp::bindattr(self, List, '$!todo', Mu);
                False;
            }
            else {
                True;
            }
        }
        else {
            False
        }
    }

    proto method STORE(|) { * }
    multi method STORE(Array:D: Iterable:D \iterable) {
        nqp::iscont(iterable)
            ?? self!STORE-ONE(iterable)
            !! self!STORE-ITERABLE(iterable)
    }
    multi method STORE(Array:D: \item) {
        self!STORE-ONE(item)
    }
    method !STORE-ITERABLE(\iterable) {
        my \new-storage = IterationBuffer.CREATE;
        my \iter = iterable.iterator;
        my \target = ArrayReificationTarget.new(new-storage,
            nqp::decont($!descriptor));
        if iter.push-until-lazy(target) =:= IterationEnd {
            nqp::bindattr(self, List, '$!todo', Mu);
        }
        else {
            my \new-todo = List::Reifier.CREATE;
            nqp::bindattr(new-todo, List::Reifier, '$!reified', new-storage);
            nqp::bindattr(new-todo, List::Reifier, '$!current-iter', iter);
            nqp::bindattr(new-todo, List::Reifier, '$!reification-target', target);
            nqp::bindattr(self, List, '$!todo', new-todo);
        }
        nqp::bindattr(self, List, '$!reified', new-storage);
        self
    }
    method !STORE-ONE(\item) {
        my \new-storage = IterationBuffer.CREATE;
        nqp::push(new-storage, item);
        nqp::bindattr(self, List, '$!reified', new-storage);
        nqp::bindattr(self, List, '$!todo', Mu);
        self
    }

    method reification-target() {
        ArrayReificationTarget.new(
            nqp::getattr(self, List, '$!reified'),
            nqp::decont($!descriptor))
    }

    multi method flat(Array:D:) { Seq.new(self.iterator) }

    multi method AT-POS(Array:D: int $ipos) is rw {
        my Mu \reified := nqp::getattr(self, List, '$!reified');
        reified.DEFINITE && $ipos < nqp::elems(reified) && $ipos >= 0
            ?? nqp::ifnull(nqp::atpos(reified, $ipos),
                    self!AT-POS-SLOWPATH($ipos))
            !! self!AT-POS-SLOWPATH($ipos)
    }
    multi method AT-POS(Array:D: Int:D $pos) is rw {
        my int $ipos = nqp::unbox_i($pos);
        my Mu \reified := nqp::getattr(self, List, '$!reified');
        reified.DEFINITE && $ipos < nqp::elems(reified) && $ipos >= 0
            ?? nqp::ifnull(nqp::atpos(reified, $ipos),
                    self!AT-POS-SLOWPATH($ipos))
            !! self!AT-POS-SLOWPATH($ipos)
    }
    method !AT-POS-SLOWPATH(int $ipos) is rw {
        fail X::OutOfRange.new(:what<Index>,:got($ipos),:range<0..Inf>)
          if nqp::islt_i($ipos, 0);
        self!ensure-allocated();
        my $todo := nqp::getattr(self, List, '$!todo');
        if $todo.DEFINITE {
            $todo.reify-at-least($ipos + 1);
        }
        my Mu \reified := nqp::getattr(self, List, '$!reified');
        $ipos >= nqp::elems(reified) || nqp::isnull(my \value = nqp::atpos(reified, $ipos))
            ?? nqp::p6bindattrinvres(
                    (my \v := nqp::p6scalarfromdesc($!descriptor)),
                    Scalar,
                    '$!whence',
                    -> { nqp::bindpos(reified, $ipos, v) }
                )
            !! value
    }

    # XXX GLR
    #multi method ASSIGN-POS(Array:D: int \pos, Mu \assignee) is rw {
    #    X::OutOfRange.new(:what<Index>,:got(pos),:range<0..Inf>).throw
    #      if nqp::islt_i(pos,0);
    #    my \items := nqp::p6listitems(self);
    #    nqp::existspos(items,pos)
    #      || nqp::isconcrete(nqp::getattr(self,List,'$!nextiter'))
    #      && self.EXISTS-POS(pos)
    #        ?? (nqp::atpos(items,pos) = assignee)
    #        !! (nqp::bindpos(items,pos,nqp::p6scalarfromdesc($!descriptor)) = assignee)
    #}
    #multi method ASSIGN-POS(Array:D: Int:D \pos, Mu \assignee) is rw {
    #    my int $pos = nqp::unbox_i(pos);
    #    X::OutOfRange.new(:what<Index>,:got(pos),:range<0..Inf>).throw
    #      if nqp::islt_i($pos,0);
    #    my \items := nqp::p6listitems(self);
    #    nqp::existspos(items,$pos)
    #      || nqp::isconcrete(nqp::getattr(self,List,'$!nextiter'))
    #      && self.EXISTS-POS($pos)
    #        ?? (nqp::atpos(items,$pos) = assignee)
    #        !! (nqp::bindpos(items,$pos,nqp::p6scalarfromdesc($!descriptor)) = assignee)
    #}

    proto method BIND-POS(|) { * }
    multi method BIND-POS(Int $pos, Mu \bindval) is rw {
        my int $ipos = $pos;
        my $todo := nqp::getattr(self, List, '$!todo');
        $todo.reify-at-least($ipos + 1) if $todo.DEFINITE;
        nqp::bindpos(nqp::getattr(self, List, '$!reified'), $ipos, bindval);
    }
    multi method BIND-POS(int $pos, Mu \bindval) is rw {
        my $todo := nqp::getattr(self, List, '$!todo');
        $todo.reify-at-least($pos + 1) if $todo.DEFINITE;
        nqp::bindpos(nqp::getattr(self, List, '$!reified'), $pos, bindval);
    }

    method DELETE-POS(\pos, :$SINK) {
        fail X::Subscript::Negative.new(index => pos, type => self.WHAT) if pos < 0;

        my $value := self.AT-POS(pos); # needed for reification
        my $items := nqp::getattr(self, List, '$!reified');
        my $end   := self.end;

        pos <= $end
          ?? nqp::bindpos($items, pos, nqp::null())
          !! return self.default;

        if pos == $end {
            my int $pos = pos;
            nqp::pop($items);
            nqp::pop($items)
              while ($pos = $pos - 1) >= 0
                && nqp::isnull(nqp::atpos($items,$pos));
        }
        $value;
    }

    multi method push(Array:D: \value) {
        self!ensure-allocated();
        if nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable)) {
            fail X::Cannot::Lazy.new(action => 'push to') if self.is-lazy;

            nqp::push(
                nqp::getattr(self, List, '$!reified'),
                nqp::assign(nqp::p6scalarfromdesc($!descriptor), value)
            );
            self
        }
        else {
            self!push-list(value.list)
        }
    }
    multi method push(Array:D: **@values is rw) {
        self!ensure-allocated();
        self!push-list(@values)
   }
   method !push-list(@values) {
        fail X::Cannot::Lazy.new(action => 'push to') if self.is-lazy;

        my \values-iter = @values.iterator;
        my $reified := nqp::getattr(self, List, '$!reified');
        unless values-iter.push-until-lazy($reified) =:= IterationEnd {
            fail X::Cannot::Lazy.new(:action<push>, :what(self.^name));
        }
        self
    }

    multi method unshift(Array:D: \value) {
        if nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable)) {
            self!ensure-allocated();

            nqp::unshift(
                nqp::getattr(self, List, '$!reified'),
                nqp::assign(nqp::p6scalarfromdesc($!descriptor), value)
            );
            self
        }
        else {
            self!unshift-list(value.list)
        }
    }
    multi method unshift(Array:D: **@values is rw) {
        self!unshift-list(@values)
    }
    method !unshift-list(@values) {
        self!ensure-allocated();

        # unshift is always eager
        @values.elems;
        nqp::unshift(
            nqp::getattr(self, List, '$!reified'),
            nqp::assign(nqp::p6scalarfromdesc($!descriptor), @values.pop)
        );
        self;
    }

    method pop(Array:D:) is parcel is nodal {
        self!ensure-allocated();
        fail X::Cannot::Lazy.new(action => 'pop from') if self.is-lazy;

        my $reified := nqp::getattr(self, List, '$!reified');
        nqp::elems($reified)
            ?? nqp::pop($reified)
            !! fail X::Cannot::Empty.new(:action<pop>, :what(self.^name));
    }

    method shift(Array:D:) is parcel is nodal {
        # make sure we have at least one item, then shift+return it
        self!ensure-allocated();
        my $todo := nqp::getattr(self, List, '$!todo');
        my $reified := nqp::getattr(self, List, '$!reified');
        nqp::existspos($reified, 0) || $todo.DEFINITE && $todo.reify-at-least(1)
            ?? nqp::shift($reified)
            !! fail X::Cannot::Empty.new(:action<shift>, :what(self.^name));
    }

    # XXX GLR
    #method plan(Array:D: |args) is nodal {
    #    nqp::p6listitems(self);
    #    my $elems = self.gimme(*);
    #    fail X::Cannot::Lazy.new(:action('.plan to')) if $!nextiter.defined;
    #
#   #     # need type checks?
#   #     my $of := self.of;
#   #
#   #     unless $of =:= Mu {
#   #         X::TypeCheck.new(
#   #           operation => '.push',
#   #           expected  => $of,
#   #           got       => $_,
#   #         ).throw unless nqp::istype($_, $of) for @values;
#   #     }
    #
    #    nqp::bindattr(self, List, '$!nextiter', nqp::p6listiter(nqp::list(args.list), self));
    #    Nil;
    #}

    # XXX GLR
    proto method splice(|) is nodal { * }
    multi method splice(Array:D \SELF: :$SINK) {
        if $SINK {
            SELF.STORE(());
            Nil
        }
        else {
            my @ret := SELF.of =:= Mu ?? Array.new !! Array[SELF.of].new;
            @ret.STORE(SELF);
            SELF.STORE(());
            @ret
        }
    }
    multi method splice(Array:D: $offset=0, $size=Whatever, @values?, :$SINK) {
        self!splice-list($offset, $size, @values, :$SINK)
    }
    multi method splice(Array:D: $offset=0, $size=Whatever, **@values, :$SINK) {
        self!splice-list($offset, $size, @values, :$SINK)
    }
    method !splice-list($offset, $size, @values, :$SINK) {
        my \splice-buffer = IterationBuffer.new;
        unless @values.iterator.push-until-lazy(splice-buffer) =:= IterationEnd {
            fail X::Cannot::Lazy.new(:action('splice in'));
        }

        my $todo = nqp::getattr(self, List, '$!todo');
        my $lazy;
        if $todo.DEFINITE {
            $lazy = $todo.reify-until-lazy() !=:= IterationEnd;
        }

        my int $o = nqp::istype($offset,Callable)
          ?? $offset(self.elems)
          !! nqp::istype($offset,Whatever)
            ?? self.elems
            !! $offset.Int;
        X::OutOfRange.new(
          :what('Offset argument to splice'),
          :got($o),
          :range("0..{self.elems}"),
        ).fail if $o < 0 || (!$lazy && $o > self.elems); # one after list allowed for "push"
    
        my int $s = nqp::istype($size, Callable)
          ?? $size(self.elems - $o)
          !! !defined($size) || nqp::istype($size,Whatever)
             ?? self.elems - ($o min self.elems)
             !! $size.Int;
        X::OutOfRange.new(
          :what('Size argument to splice'),
          :got($s),
          :range("0..^{self.elems - $o}"),
        ).fail if $s < 0;

        # need to enforce type checking
        my $expected := self.of;
        if self.of !=:= Mu {
            my int $i = 0;
            my int $n = nqp::elems(splice-buffer);
            while $i < $n {
                unless nqp::istype(nqp::atpos(splice-buffer, $i), $expected) {
                    X::TypeCheck::Splice.new(
                        :action<splice>,
                        :got($_.WHAT),
                        :$expected,
                    ).fail;
                }
                $i = $i + 1;
            }
        }

        $todo.reify-at-least($o + $s) if $lazy;
        if $SINK {
            nqp::splice(nqp::getattr(self, List, '$!reified'),
                splice-buffer, $o, $s);
            Nil;
        }
        else {
            my @ret := $expected =:= Mu ?? Array.new !! Array[$expected].new;
            @ret = self[$o..($o + $s - 1)] if $s;
            nqp::splice(nqp::getattr(self, List, '$!reified'),
                splice-buffer, $o, $s);
            @ret;
        }
    }

    multi method ACCEPTS(Array:D: $topic) {
        my $sseq = self;
        my $tseq = $topic.list;

        my int $spos = 0;
        my int $tpos = 0;
        while $spos < +$sseq {
            # if the next element is Whatever
            if nqp::istype($sseq[$spos], Whatever) {
                # skip over all of the Whatevers
                $spos = $spos + 1
                    while $spos <= +$sseq && nqp::istype($sseq[$spos], Whatever);
                # if nothing left, we're done
                return True if !($spos < +$sseq);
                # find a target matching our new target
                $tpos = $tpos + 1
                    while ($tpos < +$tseq) && $tseq[$tpos] !== $sseq[$spos];
                # return false if we ran out
                return False if !($tpos < +$tseq);
            }
            elsif $tpos >= +$tseq || $tseq[$tpos] !=== $sseq[$spos] {
                return False;
            }
            # skip matching elements
            $spos = $spos + 1;
            $tpos = $tpos + 1;
        }
        # If nothing left to match, we're successful.
        $tpos >= +$tseq;
    }

    # introspection
    method name() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Str !! $d.name()
    }
    method of() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Mu !! $d.of;
    }
    method default() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Any !! $d.default;
    }
    method dynamic() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Bool !! so $d.dynamic;
    }
    multi method perl(Array:D \SELF:) {
        '$' x nqp::iscont(SELF) ~
        '[' ~ self.map({.perl}).join(', ') ~ ']';
    }

    # XXX GLR
    #my role TypedArray[::TValue] does Positional[TValue] {
    #    method new(|) {
    #        my Mu $args := nqp::p6argvmarray();
    #        nqp::shift($args);
    #
    #        my $list := nqp::p6list($args, self.WHAT, Bool::True);
    #
    #        my $of = self.of;
    #        if ( $of !=:= Mu ) {
    #            for @$list {
    #                if $_ !~~ $of {
    #                    X::TypeCheck.new(
    #                      operation => '.new',
    #                      expected  => $of,
    #                      got       => $_,
    #                    ).throw;
    #                }
    #            }
    #        }
    #
    #        $list;
    #    }
    #    multi method AT-POS(Int() $pos) is rw {
    #        if self.EXISTS-POS($pos) {
    #            nqp::atpos(
    #              nqp::getattr(self, List, '$!items'), nqp::unbox_i($pos)
    #            );
    #        }
    #        else {
    #            nqp::p6bindattrinvres(
    #                (my \v := nqp::p6scalarfromdesc(nqp::getattr(self, Array, '$!descriptor'))),
    #                Scalar,
    #                '$!whence',
    #                -> { nqp::bindpos(
    #                  nqp::getattr(self,List,'$!items'), nqp::unbox_i($pos), v) }
    #            );
    #        }
    #    }
    #    multi method AT-POS(int $pos) is rw {
    #        if self.EXISTS-POS($pos) {
    #            nqp::atpos(nqp::getattr(self, List, '$!items'), $pos);
    #        }
    #        else {
    #            nqp::p6bindattrinvres(
    #                (my \v := nqp::p6scalarfromdesc(nqp::getattr(self, Array, '$!descriptor'))),
    #                Scalar,
    #                '$!whence',
    #                -> { nqp::bindpos(nqp::getattr(self, List,'$!items'), $pos, v)}
    #            );
    #        }
    #    }
    #    multi method BIND-POS(Int() $pos, TValue \bindval) is rw {
    #        self.gimme($pos + 1);
    #        nqp::bindpos(nqp::getattr(self, List, '$!items'), nqp::unbox_i($pos), bindval)
    #    }
    #    multi method BIND-POS(int $pos, TValue \bindval) is rw {
    #        self.gimme($pos + 1);
    #        nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, bindval)
    #    }
    #    multi method perl(::?CLASS:D \SELF:) {
    #        my $args = self.map({ ($_ // TValue).perl(:arglist)}).join(', ');
    #        'Array[' ~ TValue.perl ~ '].new(' ~ $args ~ ')';
    #    }
    #    # XXX some methods to come here...
    #}
    #method ^parameterize(Mu:U \arr, Mu:U \t, |c) {
    #    if c.elems == 0 {
    #        my $what := arr.^mixin(TypedArray[t]);
    #        # needs to be done in COMPOSE phaser when that works
    #        $what.^set_name("{arr.^name}[{t.^name}]");
    #        $what;
    #    }
    #    else {
    #        die "Can only type-constrain Array with [ValueType]"
    #    }
    #}
}

# The [...] term creates an Array.
proto circumfix:<[ ]>(|) { * }
multi circumfix:<[ ]>() {
    my \result = Array.CREATE;
    nqp::bindattr(result, List, '$!reified', IterationBuffer.CREATE);
    result
}
multi circumfix:<[ ]>(Iterable:D \iterable) {
    Array.from-iterator(iterable.iterator)
}
multi circumfix:<[ ]>(|) {
    my \in      = nqp::p6argvmarray();
    my \result  = Array.CREATE;
    my \reified = IterationBuffer.CREATE;
    nqp::bindattr(result, List, '$!reified', reified);
    while nqp::elems(in) {
        if nqp::istype(nqp::atpos(in, 0), Slip) {
            # We saw a Slip, which may expand to something lazy. Put all that
            # remains in the future, and let normal reification take care of
            # it.
            my \todo := List::Reifier.CREATE;
            nqp::bindattr(result, List, '$!todo', todo);
            nqp::bindattr(todo, List::Reifier, '$!reified', reified);
            nqp::bindattr(todo, List::Reifier, '$!future', in);
            nqp::bindattr(todo, List::Reifier, '$!reification-target',
                result.reification-target());
            todo.reify-until-lazy();
            last;
        }
        else {
            # Just an item, no need to go through the whole maybe-lazy
            # business.
            nqp::push(reified,
                nqp::assign(nqp::p6scalarfromdesc(nqp::null()), nqp::shift(in)));
        }
    }
    result
}

proto sub pop(@) {*}
multi sub pop(@a) { @a.pop }

proto sub shift(@) {*}
multi sub shift(@a) { @a.shift }

proto sub unshift(|) {*}
multi sub unshift(\a, \elem)   { a.unshift: elem }
multi sub unshift(\a, *@elems) { a.unshift: @elems }

proto sub push(|) {*}
multi sub push(\a, \elem)   { a.push: elem }
multi sub push(\a, *@elems) { a.push: @elems }
sub splice(@arr, |c)        { @arr.splice(|c) }

# vim: ft=perl6 expandtab sw=4
