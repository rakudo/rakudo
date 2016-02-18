my class X::TypeCheck { ... };
my class X::Subscript::Negative { ... };
my class X::NotEnoughDimensions { ... };
my class X::Assignment::ArrayShapeMismatch { ... };

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
            my \rt = nqp::create(self);
            nqp::bindattr(rt, self, '$!target', target);
            nqp::bindattr(rt, self, '$!descriptor', descriptor);
            rt
        }

        method push(Mu \value) {
            nqp::push($!target,
                nqp::assign(nqp::p6scalarfromdesc($!descriptor), value));
        }
    }

    my class ListReificationTarget {
        has $!target;

        method new(\target) {
            nqp::p6bindattrinvres(nqp::create(self), self, '$!target', target);
        }

        method push(Mu \value) {
            nqp::push($!target,
                nqp::decont(value));
        }
    }

    method from-iterator(Array:U: Iterator $iter) {
        my \result := nqp::create(self);
        my \buffer := nqp::create(IterationBuffer);
        my \todo := nqp::create(List::Reifier);
        nqp::bindattr(result, List, '$!reified', buffer);
        nqp::bindattr(result, List, '$!todo', todo);
        nqp::bindattr(todo, List::Reifier, '$!reified', buffer);
        nqp::bindattr(todo, List::Reifier, '$!current-iter', $iter);
        nqp::bindattr(todo, List::Reifier, '$!reification-target',
            result.reification-target());
        todo.reify-until-lazy();
        result
    }

    sub allocate-shaped-storage(\arr, @dims) {
        nqp::bindattr(arr, List, '$!reified',
            Rakudo::Internals.SHAPED-ARRAY-STORAGE(@dims, nqp::knowhow(), Mu));
        arr
    }

    my role ShapedArray[::TValue] does Positional[TValue] does Rakudo::Internals::ShapedArrayCommon {
        has $.shape;

        proto method AT-POS(|) is raw {*}
        multi method AT-POS(Array:U: |c) is raw {
            self.Any::AT-POS(|c)
        }
        multi method AT-POS(Array:D: **@indices) is raw {
            my Mu $storage := nqp::getattr(self, List, '$!reified');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            if $numind >= $numdims {
                my $idxs := nqp::list_i();
                while $numdims > 0 {
                    nqp::push_i($idxs, @indices.shift);
                    $numdims = $numdims - 1;
                }
                my \elem = nqp::ifnull(
                    nqp::atposnd($storage, $idxs),
                    nqp::p6bindattrinvres(
                        (my \v := nqp::p6scalarfromdesc(nqp::getattr(self, Array, '$!descriptor'))),
                        Scalar,
                        '$!whence',
                        -> { nqp::bindposnd($storage, $idxs, v) }));
                @indices ?? elem.AT-POS(|@indices) !! elem
            }
            else {
                X::NYI.new(feature => "Partially dimensioned views of arrays").throw
            }
        }

        proto method ASSIGN-POS(|) {*}
        multi method ASSIGN-POS(Array:U: |c) {
            self.Any::ASSIGN-POS(|c)
        }
        multi method ASSIGN-POS(**@indices) {
            my \value = @indices.pop;
            my Mu $storage := nqp::getattr(self, List, '$!reified');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            if $numind == $numdims {
                # Dimension counts match, so fast-path it
                my $idxs := nqp::list_i();
                while $numdims > 0 {
                    nqp::push_i($idxs, @indices.shift);
                    $numdims = $numdims - 1;
                }
                nqp::ifnull(
                    nqp::atposnd($storage, $idxs),
                    nqp::bindposnd($storage, $idxs,
                        nqp::p6scalarfromdesc(nqp::getattr(self, Array, '$!descriptor')))
                    ) = value
            }
            elsif $numind > $numdims {
                # More than enough dimensions; may work, fall to slow path
                self.AT-POS(@indices) = value
            }
            else {
                # Not enough dimensions, cannot possibly assign here
                X::NotEnoughDimensions.new(
                    operation => 'assign to',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
        }

        proto method EXISTS-POS(|) {*}
        multi method EXISTS-POS(Array:U: |c) {
            self.Any::EXISTS-POS(|c)
        }
        multi method EXISTS-POS(**@indices) {
            my Mu $storage := nqp::getattr(self, List, '$!reified');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            my $dims := nqp::dimensions($storage);
            if $numind >= $numdims {
                my $idxs := nqp::list_i();
                loop (my int $i = 0; $i < $numind; $i = $i + 1) {
                    my int $idx = @indices.shift;
                    return False if $idx >= nqp::atpos_i($dims, $i);
                    nqp::push_i($idxs, $idx);
                }
                if nqp::isnull(nqp::atposnd($storage, $idxs)) {
                    False
                }
                elsif @indices {
                    nqp::atposnd($storage, $idxs).EXISTS-POS(|@indices)
                }
                else {
                    True
                }
            }
            else {
                loop (my int $i = 0; $i < $numind; $i = $i + 1) {
                    return False if @indices[$i] >= nqp::atpos_i($dims, $i);
                }
                True
            }
        }

        proto method DELETE-POS(|) {*}
        multi method DELETE-POS(Array:U: |c) {
            self.Any::DELETE-POS(|c)
        }
        multi method DELETE-POS(**@indices) {
            my Mu $storage := nqp::getattr(self, List, '$!reified');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            if $numind >= $numdims {
                my $idxs := nqp::list_i();
                while $numdims > 0 {
                    nqp::push_i($idxs, @indices.shift);
                    $numdims = $numdims - 1;
                }
                my \value = nqp::ifnull(nqp::atposnd($storage, $idxs), Nil);
                if @indices {
                    value.DELETE-POS(|@indices)
                }
                else {
                    nqp::bindposnd($storage, $idxs, nqp::null());
                    value
                }
            }
            else {
                # Not enough dimensions, cannot delete
                X::NotEnoughDimensions.new(
                    operation => 'delete from',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
        }

        proto method BIND-POS(|) is raw {*}
        multi method BIND-POS(Array:U: |c) is raw {
            self.Any::BIND-POS(|c)
        }
        multi method BIND-POS(Array:D: **@indices is raw) is raw {
            my Mu $storage := nqp::getattr(self, List, '$!reified');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems - 1;
            my \value = @indices.AT-POS($numind);
            if $numind >= $numdims {
                # At least enough indices that binding will work out or we can
                # pass the bind target on down the chain.
                my $idxs := nqp::list_i();
                my int $i = 0;
                while $i < $numdims {
                    nqp::push_i($idxs, @indices.AT-POS($i));
                    $i = $i + 1;
                }
                $numind == $numdims
                    ?? nqp::bindposnd($storage, $idxs, value)
                    !! nqp::atposnd($storage, $idxs).BIND-POS(|@indices[$numdims..*])
            }
            else {
                # Not enough dimensions, cannot possibly assign here
                X::NotEnoughDimensions.new(
                    operation => 'assign to',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
        }

        proto method STORE(|) { * }
        multi method STORE(::?CLASS:D: Iterable:D \in) {
            allocate-shaped-storage(self, self.shape);
            my \in-shape = nqp::can(in, 'shape') ?? in.shape !! Nil;
            if in-shape && !nqp::istype(in-shape.AT-POS(0), Whatever) {
                if self.shape eqv in-shape {
                    # Can do a VM-supported memcpy-like thing in the future
                    for self.keys {
                        self.ASSIGN-POS(|$_, in.AT-POS(|$_))
                    }
                }
                else {
                    X::Assignment::ArrayShapeMismatch.new(
                        source-shape => in-shape,
                        target-shape => self.shape
                    ).throw
                }
            }
            else {
                self!STORE-PATH((), self.shape, in)
            }
        }
        multi method STORE(::?CLASS:D: Mu \item) {
            self.STORE((item,))
        }

        method reverse(::?CLASS:D:) {
            self.shape.elems == 1
                ?? self.new(:shape(self.shape), self.List.reverse())
                !! X::IllegalOnFixedDimensionArray.new(operation => 'reverse').throw
        }

        method rotate(::?CLASS:D: Cool \n) {
            self.shape.elems == 1
                ?? self.new(:shape(self.shape), self.List.rotate(n))
                !! X::IllegalOnFixedDimensionArray.new(operation => 'rotate').throw
        }

        # A shaped array isn't lazy, we these methods don't need to go looking
        # into the "todo".
        multi method elems(::?CLASS:D:) is nodal {
            nqp::elems(nqp::getattr(self, List, '$!reified'))
        }
        method eager() { self }
        method is-lazy() { False }
    }

    proto method new(|) { * }
    multi method new(Mu:D \values, :$shape) {
        self!new-internal(values, $shape)
    }

    multi method new(**@values is raw, :$shape) {
        self!new-internal(@values, $shape)
    }

    method !new-internal(\values, \shape) {
        my \arr = nqp::create(self);
        if shape.DEFINITE {
            my \list-shape = nqp::istype(shape, List) ?? shape !! shape.list;
            allocate-shaped-storage(arr, list-shape);
            arr does ShapedArray[Mu];
            arr.^set_name('Array');
            nqp::bindattr(arr, arr.WHAT, '$!shape', list-shape);
            arr.STORE(values) if values;
        }
        else {
            arr.STORE(values);
        }
        arr
    }

    method !ensure-allocated(--> Nil) {
        nqp::bindattr(self, List, '$!reified', nqp::create(IterationBuffer))
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
    multi method STORE(Array:D: Mu \item) {
        self!STORE-ONE(item)
    }
    method !STORE-ITERABLE(\iterable) {
        my \new-storage = nqp::create(IterationBuffer);
        my \iter = iterable.iterator;
        my \target = ArrayReificationTarget.new(new-storage,
            nqp::decont($!descriptor));
        if iter.push-until-lazy(target) =:= IterationEnd {
            nqp::bindattr(self, List, '$!todo', Mu);
        }
        else {
            my \new-todo = nqp::create(List::Reifier);
            nqp::bindattr(new-todo, List::Reifier, '$!reified', new-storage);
            nqp::bindattr(new-todo, List::Reifier, '$!current-iter', iter);
            nqp::bindattr(new-todo, List::Reifier, '$!reification-target', target);
            nqp::bindattr(self, List, '$!todo', new-todo);
        }
        nqp::bindattr(self, List, '$!reified', new-storage);
        self
    }
    method !STORE-ONE(Mu \item) {
        my \new-storage = nqp::create(IterationBuffer);
        nqp::push(new-storage,
            nqp::assign(nqp::p6scalarfromdesc($!descriptor), item));
        nqp::bindattr(self, List, '$!reified', new-storage);
        nqp::bindattr(self, List, '$!todo', Mu);
        self
    }

    method reification-target() {
        ArrayReificationTarget.new(
            nqp::getattr(self, List, '$!reified'),
            nqp::decont($!descriptor))
    }

    multi method flat(Array:U:) { self }
    multi method flat(Array:D:) { Seq.new(self.iterator) }

    multi method List(Array:D:) {
        self!ensure-allocated;
        X::Cannot::Lazy.new(:action<List>).throw if self.is-lazy;
        my \retval := nqp::create(List);
        my \reified := nqp::create(IterationBuffer);
        nqp::bindattr(retval, List, '$!reified', reified);
        my \target := ListReificationTarget.new(reified);
        self.iterator.push-all(target);
        retval
    }

    method shape() { (*,) }

    multi method AT-POS(Array:D: int $ipos) is raw {
        my Mu \reified := nqp::getattr(self, List, '$!reified');
        reified.DEFINITE && $ipos < nqp::elems(reified) && $ipos >= 0
            ?? nqp::ifnull(nqp::atpos(reified, $ipos),
                    self!AT-POS-SLOWPATH($ipos))
            !! self!AT-POS-SLOWPATH($ipos)
    }
    multi method AT-POS(Array:D: Int:D $pos) is raw {
        my int $ipos = nqp::unbox_i($pos);
        my Mu \reified := nqp::getattr(self, List, '$!reified');
        reified.DEFINITE && $ipos < nqp::elems(reified) && $ipos >= 0
            ?? nqp::ifnull(nqp::atpos(reified, $ipos),
                    self!AT-POS-SLOWPATH($ipos))
            !! self!AT-POS-SLOWPATH($ipos)
    }
    method !AT-POS-SLOWPATH(int $ipos) is raw {
        fail X::OutOfRange.new(
          :what($*INDEX // 'Index'),:got($ipos),:range<0..Inf>)
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

    multi method ASSIGN-POS(Array:D: int $ipos, Mu \assignee) {
        X::OutOfRange.new(
          :what($*INDEX // 'Index'),:got($ipos),:range<0..Inf>).throw
            if nqp::islt_i($ipos, 0);
        my Mu \reified := nqp::getattr(self, List, '$!reified');
        reified.DEFINITE && $ipos < nqp::elems(reified)
            ?? nqp::isnull(nqp::atpos(reified, $ipos))
                ?? (nqp::bindpos(reified, $ipos, nqp::p6scalarfromdesc($!descriptor)) = assignee)
                !! (nqp::atpos(reified, $ipos) = assignee)
            !! self!ASSIGN-POS-SLOWPATH($ipos, assignee)
    }
    multi method ASSIGN-POS(Array:D: Int:D $pos, Mu \assignee) {
        my int $ipos = nqp::unbox_i($pos);
        X::OutOfRange.new(
          :what($*INDEX // 'Index'),:got($pos),:range<0..Inf>).throw
            if nqp::islt_i($ipos, 0);
        my Mu \reified := nqp::getattr(self, List, '$!reified');
        reified.DEFINITE && $ipos < nqp::elems(reified)
            ?? nqp::isnull(nqp::atpos(reified, $ipos))
                ?? (nqp::bindpos(reified, $ipos, nqp::p6scalarfromdesc($!descriptor)) = assignee)
                !! (nqp::atpos(reified, $ipos) = assignee)
            !! self!ASSIGN-POS-SLOWPATH($ipos, assignee)
    }
    method !ASSIGN-POS-SLOWPATH(int $ipos, Mu \assignee) {
        fail X::OutOfRange.new(
          :what($*INDEX // 'Index'),:got($ipos),:range<0..Inf>)
            if nqp::islt_i($ipos, 0);
        self!ensure-allocated();
        my $todo := nqp::getattr(self, List, '$!todo');
        if $todo.DEFINITE {
            $todo.reify-at-least($ipos + 1);
        }
        my Mu \reified := nqp::getattr(self, List, '$!reified');
        $ipos >= nqp::elems(reified) || nqp::isnull(my \value = nqp::atpos(reified, $ipos))
            ?? (nqp::bindpos(reified, $ipos, nqp::p6scalarfromdesc($!descriptor)) = assignee)
            !! (nqp::atpos(reified, $ipos) = assignee)
    }

    multi method BIND-POS(Array:D: Int $pos, Mu \bindval) is raw {
        self!ensure-allocated();
        my int $ipos = $pos;
        my $todo := nqp::getattr(self, List, '$!todo');
        $todo.reify-at-least($ipos + 1) if $todo.DEFINITE;
        nqp::bindpos(nqp::getattr(self, List, '$!reified'), $ipos, bindval);
    }
    multi method BIND-POS(Array:D: int $pos, Mu \bindval) is raw {
        self!ensure-allocated();
        my $todo := nqp::getattr(self, List, '$!todo');
        $todo.reify-at-least($pos + 1) if $todo.DEFINITE;
        nqp::bindpos(nqp::getattr(self, List, '$!reified'), $pos, bindval);
    }

    multi method DELETE-POS(\pos, :$SINK) {
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

    # MUST have a separate Slip variant to have it slip
    multi method push(Array:D: Slip \value) {
        self!ensure-allocated();
        fail X::Cannot::Lazy.new(action => 'push to') if self.is-lazy;
        self!append-list(value);
    }
    multi method push(Array:D: \value) {
        self!ensure-allocated();
        fail X::Cannot::Lazy.new(action => 'push to') if self.is-lazy;

        nqp::push(
          nqp::getattr(self, List, '$!reified'),
          nqp::assign(nqp::p6scalarfromdesc($!descriptor), value)
        );
        self
    }
    multi method push(Array:D: **@values is raw) {
        self!ensure-allocated();
        fail X::Cannot::Lazy.new(action => 'push to') if self.is-lazy;
        self!append-list(@values)
    }

    multi method append(Array:D: \value) {
        self!ensure-allocated();
        fail X::Cannot::Lazy.new(action => 'append to') if self.is-lazy;
        if nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable)) {
            nqp::push(
                nqp::getattr(self, List, '$!reified'),
                nqp::assign(nqp::p6scalarfromdesc($!descriptor), value)
            );
            self
        }
        else {
            self!append-list(value.list)
        }
    }
    multi method append(Array:D: **@values is raw) {
        self!ensure-allocated();
        fail X::Cannot::Lazy.new(action => 'append to') if self.is-lazy;
        self!append-list(@values)
    }
    method !append-list(@values) {
        my \values-iter = @values.iterator;
        my \reified := nqp::getattr(self, List, '$!reified');
        my \target := ArrayReificationTarget.new(reified,
            nqp::decont($!descriptor));
        unless values-iter.push-until-lazy(target) =:= IterationEnd {
            fail X::Cannot::Lazy.new(:action<push>, :what(self.^name));
        }
        self
    }

    multi method unshift(Array:D: Slip \value) {
        self!ensure-allocated();
        self!prepend-list(value)
    }
    multi method unshift(Array:D: \value) {
        self!ensure-allocated();
        nqp::unshift(
            nqp::getattr(self, List, '$!reified'),
            nqp::assign(nqp::p6scalarfromdesc($!descriptor), value)
        );
        self
    }
    multi method unshift(Array:D: **@values is raw) {
        self!ensure-allocated();
        self!prepend-list(@values)
    }
    multi method prepend(Array:D: \value) {
        if nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable)) {
            self!ensure-allocated();

            nqp::unshift(
                nqp::getattr(self, List, '$!reified'),
                nqp::assign(nqp::p6scalarfromdesc($!descriptor), value)
            );
            self
        }
        else {
            self!prepend-list(value.list)
        }
    }
    multi method prepend(Array:D: **@values is raw) {
        self!prepend-list(@values)
    }
    method !prepend-list(@values) {
        my \containers := nqp::create(IterationBuffer);
        my \target := ArrayReificationTarget.new(containers,
            nqp::decont($!descriptor));

        my \iter := @values.iterator;
        iter.push-all(target);

        self!ensure-allocated();
        nqp::splice(nqp::getattr(self, List, '$!reified'),
                    containers, 0, 0);

        self;
    }

    method pop(Array:D:) is raw is nodal {
        self!ensure-allocated();
        fail X::Cannot::Lazy.new(action => 'pop from') if self.is-lazy;

        my $reified := nqp::getattr(self, List, '$!reified');
        nqp::elems($reified)
            ?? nqp::pop($reified)
            !! fail X::Cannot::Empty.new(:action<pop>, :what(self.^name));
    }

    method shift(Array:D:) is raw is nodal {
        # make sure we have at least one item, then shift+return it
        self!ensure-allocated();
        my $todo := nqp::getattr(self, List, '$!todo');
        my $reified := nqp::getattr(self, List, '$!reified');
        nqp::existspos($reified, 0) || $todo.DEFINITE && $todo.reify-at-least(1)
            ?? nqp::shift($reified)
            !! nqp::elems($reified)  # is it actually just sparse?
                ?? STATEMENT_LIST(nqp::shift($reified); Nil)
                !! fail X::Cannot::Empty.new(:action<shift>, :what(self.^name));
    }

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
        $lazy = !($todo.reify-until-lazy() =:= IterationEnd)
          if $todo.DEFINITE;

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
        unless self.of =:= Mu {
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
            @ret = self[lazy $o..($o + $s - 1)] if $s;
            nqp::splice(nqp::getattr(self, List, '$!reified'),
                splice-buffer, $o, $s);
            @ret;
        }
    }

    # introspection
    method name() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Nil !! $d.name()
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
        nqp::isnull($d) ?? Nil !! so $d.dynamic;
    }
    multi method perl(Array:D \SELF:) {
        SELF.perlseen('Array', {
             '$' x nqp::iscont(SELF)  # self is always deconted
             ~ '['
             ~ self.map({nqp::decont($_).perl}).join(', ')
             ~ ',' x (self.elems == 1 && nqp::istype(self.AT-POS(0),Iterable))
             ~ ']'
        })
    }

    multi method gist(Array:D:) {
        self.gistseen('Array', { '[' ~ self.map({.gist}).join(' ') ~ ']' } )
    }

    multi method WHICH(Array:D:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                nqp::objectid(self)
            ),
            ObjAt
        )
    }

    my role TypedArray[::TValue] does Positional[TValue] {
        proto method new(|) { * }
        multi method new(**@values is raw, :$shape) {
            self!new-internal(@values, $shape);
        }
        multi method new(\values, :$shape) {
            self!new-internal(values, $shape);
        }

        method !new-internal(\values, \shape) {
            my \arr = nqp::create(self);
            nqp::bindattr(
              arr,
              Array,
              '$!descriptor',
              Perl6::Metamodel::ContainerDescriptor.new(
                :of(TValue), :rw(1), :default(TValue))
            );
            if shape.DEFINITE {
                my \list-shape = nqp::istype(shape, List) ?? shape !! shape.list;
                allocate-shaped-storage(arr, list-shape);
                arr does ShapedArray[Mu];
                arr.^set_name('Array');
                nqp::bindattr(arr, arr.WHAT, '$!shape', list-shape);
                arr.STORE(values) if values;
            } else {
                arr.STORE(values);
            }
            arr
        }

        proto method BIND-POS(|) { * }
        multi method BIND-POS(Array:D: Int $pos, TValue \bindval) is raw {
            my int $ipos = $pos;
            my $todo := nqp::getattr(self, List, '$!todo');
            $todo.reify-at-least($ipos + 1) if $todo.DEFINITE;
            nqp::bindpos(nqp::getattr(self, List, '$!reified'), $ipos, bindval)
        }
        multi method BIND-POS(Array:D: int $pos, TValue \bindval) is raw {
            my $todo := nqp::getattr(self, List, '$!todo');
            $todo.reify-at-least($pos + 1) if $todo.DEFINITE;
            nqp::bindpos(nqp::getattr(self, List, '$!reified'), $pos, bindval)
        }
        multi method perl(::?CLASS:D \SELF:) {
            my $args = self.map({ ($_ // TValue).perl(:arglist) }).join(', ');
            'Array[' ~ TValue.perl ~ '].new(' ~ $args ~ ')';
        }
    }
    method ^parameterize(Mu:U \arr, Mu:U \t, |c) {
        if c.elems == 0 {
            my $what := arr.^mixin(TypedArray[t]);
            # needs to be done in COMPOSE phaser when that works
            $what.^set_name("{arr.^name}[{t.^name}]");
            $what;
        }
        else {
            die "Can only type-constrain Array with [ValueType]"
        }
    }
}

# The [...] term creates an Array.
proto circumfix:<[ ]>(|) { * }
multi circumfix:<[ ]>() {
    my \result = nqp::create(Array);
    nqp::bindattr(result, List, '$!reified', nqp::create(IterationBuffer));
    result
}
multi circumfix:<[ ]>(Iterable:D \iterable) {
    if nqp::iscont(iterable) {
        my \result = nqp::create(Array);
        my \buffer = nqp::create(IterationBuffer);
        buffer.push(iterable);
        nqp::bindattr(result, List, '$!reified', buffer);
        result
    }
    else {
        Array.from-iterator(iterable.iterator)
    }
}
multi circumfix:<[ ]>(|) {
    my \in      = nqp::p6argvmarray();
    my \result  = nqp::create(Array);
    my \reified = nqp::create(IterationBuffer);
    nqp::bindattr(result, List, '$!reified', reified);
    while nqp::elems(in) {
        if nqp::istype(nqp::atpos(in, 0), Slip) {
            # We saw a Slip, which may expand to something lazy. Put all that
            # remains in the future, and let normal reification take care of
            # it.
            my \todo := nqp::create(List::Reifier);
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

sub push   (\a, |elems) { a.push:    |elems }
sub append (\a, |elems) { a.append:  |elems }
sub unshift(\a, |elems) { a.unshift: |elems }
sub prepend(\a, |elems) { a.prepend: |elems }

sub splice(@arr, |c)         { @arr.splice(|c) }

# vim: ft=perl6 expandtab sw=4
