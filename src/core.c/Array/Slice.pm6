#- start of generated part of array slice access -------------------------------
#- Generated on 2021-02-22T20:46:50+01:00 by tools/build/makeARRAY_SLICE_ACCESS.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

# no actionable adverbs
my class Array::Slice::Access::none is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,$!iterable.AT-POS(pos));
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,$!iterable.AT-POS(pos))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :kv
my class Array::Slice::Access::kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,$!iterable.AT-POS(pos));
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,$!iterable.AT-POS(pos));
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!kv
my class Array::Slice::Access::not-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        nqp::push($!result,$!iterable.AT-POS(pos));
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,$!iterable.AT-POS(pos));
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :p
my class Array::Slice::Access::p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,$!iterable.AT-POS(pos)))
          if $!iterable.EXISTS-POS(pos);
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,$!iterable.AT-POS(pos)))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!p
my class Array::Slice::Access::not-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,$!iterable.AT-POS(pos)));
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,$!iterable.AT-POS(pos)))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :k
my class Array::Slice::Access::k is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos) if $!iterable.EXISTS-POS(pos);
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,pos)
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!k
my class Array::Slice::Access::not-k is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,pos)
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :v
my class Array::Slice::Access::v is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,$!iterable.AT-POS(pos))
          if $!iterable.EXISTS-POS(pos);
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,$!iterable.AT-POS(pos))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :exists
my class Array::Slice::Access::exists is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,$!iterable.EXISTS-POS(pos));
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,True)
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :exists:kv
my class Array::Slice::Access::exists-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :exists:!kv
my class Array::Slice::Access::exists-not-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        nqp::push($!result,$!iterable.EXISTS-POS(pos));
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :exists:p
my class Array::Slice::Access::exists-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,True))
          if $!iterable.EXISTS-POS(pos);
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,True))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :exists:!p
my class Array::Slice::Access::exists-not-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,$!iterable.EXISTS-POS(pos)));
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,True))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :exists:delete
my class Array::Slice::Access::exists-delete is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,True);
        }
        else {
            nqp::push($!result,False);
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,True);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :exists:delete:kv
my class Array::Slice::Access::exists-delete-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :exists:delete:!kv
my class Array::Slice::Access::exists-delete-not-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        self!delete(pos)
          if nqp::push($!result,$!iterable.EXISTS-POS(pos));
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :exists:delete:p
my class Array::Slice::Access::exists-delete-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,True));
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,True));
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :exists:delete:!p
my class Array::Slice::Access::exists-delete-not-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,True));
        }
        else {
            nqp::push($!result,Pair.new(pos,False));
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,True));
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!exists
my class Array::Slice::Access::not-exists is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,!$!iterable.EXISTS-POS(pos));
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,False)
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!exists:kv
my class Array::Slice::Access::not-exists-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!exists:!kv
my class Array::Slice::Access::not-exists-not-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        nqp::push($!result,!$!iterable.EXISTS-POS(pos));
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!exists:p
my class Array::Slice::Access::not-exists-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,False))
          if $!iterable.EXISTS-POS(pos);
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,False))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!exists:!p
my class Array::Slice::Access::not-exists-not-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,!$!iterable.EXISTS-POS(pos)));
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,False))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!exists:delete
my class Array::Slice::Access::not-exists-delete is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,False);
        }
        else {
            nqp::push($!result,True);
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,False);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!exists:delete:kv
my class Array::Slice::Access::not-exists-delete-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!exists:delete:!kv
my class Array::Slice::Access::not-exists-delete-not-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        self!delete(pos)
          unless nqp::push($!result,!$!iterable.EXISTS-POS(pos));
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!exists:delete:p
my class Array::Slice::Access::not-exists-delete-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,False));
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,False));
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :!exists:delete:!p
my class Array::Slice::Access::not-exists-delete-not-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,False));
        }
        else {
            nqp::push($!result,Pair.new(pos,True));
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,False));
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :delete
my class Array::Slice::Access::delete is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,self!delete(pos));
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,self!delete(pos))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :delete:kv
my class Array::Slice::Access::delete-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,self!delete(pos));
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,self!delete(pos));
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :delete:kv
my class Array::Slice::Access::delete-not-kv is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        nqp::push($!result,self!delete(pos));
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,self!delete(pos));
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :delete:p
my class Array::Slice::Access::delete-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,self!delete(pos)))
          if $!iterable.EXISTS-POS(pos);
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,self!delete(pos)))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :delete:!p
my class Array::Slice::Access::delete-not-p is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,self!delete(pos)));
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,self!delete(pos)))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :delete:k
my class Array::Slice::Access::delete-k is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
        }
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :delete:!k
my class Array::Slice::Access::delete-not-k is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        self!delete(pos) if $!iterable.EXISTS-POS(pos);
        nqp::push($!result,pos);
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

# :delete:v
my class Array::Slice::Access::delete-v is implementation-detail {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
        nqp::push($!result,self!delete(pos))
          if $!iterable.EXISTS-POS(pos);
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,self!delete(pos))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );

            # We're only done on this level, not generally
            $!done = 0;
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my $real := (pos)(self!elems)),Int)
              ?? self!accept-lazy($real)
              !! self!handle-nonInt-lazy($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of array slice access ---------------------------------

#- start of generated part of array slice assignment ---------------------------
#- Generated on 2021-02-23T11:58:40+01:00 by tools/build/makeARRAY_SLICE_ASSIGN.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

# no actionable adverbs
my class Array::Slice::Assign::none is implementation-detail {
    has $!result;   # IterationBuffer with result
    has $!lhs;      # IterationBuffer with containers
    has $!rhs;      # IterationBuffer with values
    has $!iterable; # Iterable to assign to
    has $!elems;    # Number of elements in iterable
    has $!values;   # Iterator producing values to assign
    has int $!done; # flag to indicate we're done

    method !accept(\pos --> Nil) {
        nqp::push($!result,nqp::push($!lhs,$!iterable.AT-POS(pos)));
        nqp::push($!rhs,$!values.pull-one);
    }
    method !accept-lazy(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,nqp::push($!lhs,$!iterable.AT-POS(pos)));
            nqp::push($!rhs,$!values.pull-one);
        }
        else {
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable, \values) {
        $!result   := nqp::create(IterationBuffer);
        $!lhs      := nqp::create(IterationBuffer);
        $!rhs      := nqp::create(IterationBuffer);
        $!iterable := iterable;
        $!elems    := nqp::null;
        $!values   := values;
        self
    }
    method new(\iterable, \values) {
        nqp::create(self)!SET-SELF(iterable, values)
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {

            # Set up alternate result handling
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my \real := (pos)(self!elems)),Int)
              ?? self!accept(real)
              !! self!handle-nonInt(real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my \real := (pos)(self!elems)),Int)
              ?? self!accept-lazy(real)
              !! self!handle-nonInt-lazy(real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method assign-slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Do the actual assignments until there's nothing to assign anymore
        my $lhs := $!lhs;
        my $rhs := $!rhs;
        nqp::while(
          nqp::elems($lhs),
          nqp::assign(nqp::shift($lhs),nqp::shift($rhs))
        );

        $!result.List
    }
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of array slice assignment -----------------------------

#- start of generated part of array slice binding ------------------------------
#- Generated on 2021-02-23T11:56:12+01:00 by tools/build/makeARRAY_SLICE_BIND.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

# no actionable adverbs
my class Array::Slice::Bind::none is implementation-detail {
    has $!result;   # IterationBuffer with result
    has $!iterable; # Iterable to assign to
    has $!elems;    # Number of elements in iterable
    has $!values;   # Iterator producing values to assign
    has int $!done; # flag to indicate we're done

    method !accept(\pos --> Nil) {
        nqp::push($!result,$!iterable.BIND-POS(pos,$!values.pull-one));
    }
    method !accept-lazy(\pos --> Nil) {
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,$!iterable.BIND-POS(pos,$!values.pull-one))
          !! ($!done = 1);
    }

    method !SET-SELF(\iterable, \values) {
        $!result   := nqp::create(IterationBuffer);
        $!iterable := iterable;
        $!elems    := nqp::null;
        $!values   := values;
        self
    }
    method new(\iterable, \values) {
        nqp::create(self)!SET-SELF(iterable, values)
    }
    method !elems() {
        nqp::ifnull($!elems,$!elems := $!iterable.elems)
    }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        # Lazy iterators should halt as soon as a non-existing element is seen
        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept-lazy(pos),
                self!handle-nonInt-lazy(pos)
              )
            );
        }

        # Fast path for non-lazy iterators
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        # Take what was added and push it as a List
        if nqp::isgt_i(nqp::elems($!result),$mark) {

            # Set up alternate result handling
            my $buffer;
            if $mark {
                $buffer :=
                  nqp::slice($!result,$mark,nqp::sub_i(nqp::elems($!result),1));
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer  := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions eagerly
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my \real := (pos)(self!elems)),Int)
              ?? self!accept(real)
              !! self!handle-nonInt(real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept(pos.Int)
    }

    # Handle anything non-integer in the generated positions lazily
    method !handle-nonInt-lazy(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept-lazy(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype((my \real := (pos)(self!elems)),Int)
              ?? self!accept-lazy(real)
              !! self!handle-nonInt-lazy(real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   Rakudo::Iterator.IntRange(0,nqp::sub_i(self!elems,1))
                 )
              !! self!accept-lazy(pos.Int)
    }

    # The actual building of the result
    method bind-slice(\iterator) {

        if iterator.is-lazy {
            nqp::until(
              $!done
                || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }
        else {
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                self!accept(pos),
                self!handle-nonInt(pos)
              )
            );
        }

        $!result.List
    }
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of array slice binding --------------------------------

# vim: expandtab shiftwidth=4
