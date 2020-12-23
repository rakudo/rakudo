#- start of generated part of array slice access -------------------------------
#- Generated on 2020-12-23T19:29:15+01:00 by ./tools/build/makeARRAY_SLICE_ACCESS.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

# no actionable adverbs
my class Array::Slice::Access::none {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,$!iterable.AT-POS(pos));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, no actionable adverbs
my class Array::Slice::Access::lazy-none {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :kv
my class Array::Slice::Access::kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,$!iterable.AT-POS(pos));
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :kv
my class Array::Slice::Access::lazy-kv {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!kv
my class Array::Slice::Access::not-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        nqp::push($!result,$!iterable.AT-POS(pos));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :p
my class Array::Slice::Access::p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,$!iterable.AT-POS(pos)))
          if $!iterable.EXISTS-POS(pos);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :p
my class Array::Slice::Access::lazy-p {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!p
my class Array::Slice::Access::not-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,$!iterable.AT-POS(pos)));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :k
my class Array::Slice::Access::k {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos) if $!iterable.EXISTS-POS(pos);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :k
my class Array::Slice::Access::lazy-k {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!k
my class Array::Slice::Access::not-k {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :v
my class Array::Slice::Access::v {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,$!iterable.AT-POS(pos))
          if $!iterable.EXISTS-POS(pos);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :v
my class Array::Slice::Access::lazy-v {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :exists
my class Array::Slice::Access::exists {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,$!iterable.EXISTS-POS(pos));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :exists
my class Array::Slice::Access::lazy-exists {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :exists:kv
my class Array::Slice::Access::exists-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :exists:kv
my class Array::Slice::Access::lazy-exists-kv {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :exists:!kv
my class Array::Slice::Access::exists-not-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        nqp::push($!result,$!iterable.EXISTS-POS(pos));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :exists:p
my class Array::Slice::Access::exists-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,True))
          if $!iterable.EXISTS-POS(pos);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :exists:p
my class Array::Slice::Access::lazy-exists-p {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :exists:!p
my class Array::Slice::Access::exists-not-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,$!iterable.EXISTS-POS(pos)));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :exists:delete
my class Array::Slice::Access::exists-delete {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,True);
        }
        else {
            nqp::push($!result,False);
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :exists:delete
my class Array::Slice::Access::lazy-exists-delete {
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
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :exists:delete:kv
my class Array::Slice::Access::exists-delete-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :exists:delete:kv
my class Array::Slice::Access::lazy-exists-delete-kv {
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :exists:delete:!kv
my class Array::Slice::Access::exists-delete-not-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        self!delete(pos)
          if nqp::push($!result,$!iterable.EXISTS-POS(pos));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :exists:delete:p
my class Array::Slice::Access::exists-delete-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,True));
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :exists:delete:p
my class Array::Slice::Access::lazy-exists-delete-p {
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
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :exists:delete:!p
my class Array::Slice::Access::exists-delete-not-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,True));
        }
        else {
            nqp::push($!result,Pair.new(pos,False));
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!exists
my class Array::Slice::Access::not-exists {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,!$!iterable.EXISTS-POS(pos));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :!exists
my class Array::Slice::Access::lazy-not-exists {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!exists:kv
my class Array::Slice::Access::not-exists-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :!exists:kv
my class Array::Slice::Access::lazy-not-exists-kv {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!exists:!kv
my class Array::Slice::Access::not-exists-not-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        nqp::push($!result,!$!iterable.EXISTS-POS(pos));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!exists:p
my class Array::Slice::Access::not-exists-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,False))
          if $!iterable.EXISTS-POS(pos);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :!exists:p
my class Array::Slice::Access::lazy-not-exists-p {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!exists:!p
my class Array::Slice::Access::not-exists-not-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,!$!iterable.EXISTS-POS(pos)));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }

    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!exists:delete
my class Array::Slice::Access::not-exists-delete {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,False);
        }
        else {
            nqp::push($!result,True);
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :!exists:delete
my class Array::Slice::Access::lazy-not-exists-delete {
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
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!exists:delete:kv
my class Array::Slice::Access::not-exists-delete-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :!exists:delete:kv
my class Array::Slice::Access::lazy-not-exists-delete-kv {
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!exists:delete:!kv
my class Array::Slice::Access::not-exists-delete-not-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        self!delete(pos)
          if nqp::push($!result,!$!iterable.EXISTS-POS(pos));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!exists:delete:p
my class Array::Slice::Access::not-exists-delete-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,False));
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :!exists:delete:p
my class Array::Slice::Access::lazy-not-exists-delete-p {
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
            $!done = 1;
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :!exists:delete:!p
my class Array::Slice::Access::not-exists-delete-not-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,False));
        }
        else {
            nqp::push($!result,Pair.new(pos,True));
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :delete
my class Array::Slice::Access::delete {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,self!delete(pos));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :delete
my class Array::Slice::Access::lazy-delete {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :delete:kv
my class Array::Slice::Access::delete-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,self!delete(pos));
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :delete:kv
my class Array::Slice::Access::lazy-delete-kv {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :delete:kv
my class Array::Slice::Access::delete-not-kv {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,pos);
        nqp::push($!result,self!delete(pos));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :delete:p
my class Array::Slice::Access::delete-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,self!delete(pos)))
          if $!iterable.EXISTS-POS(pos);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :delete:p
my class Array::Slice::Access::lazy-delete-p {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :delete:!p
my class Array::Slice::Access::delete-not-p {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,Pair.new(pos,self!delete(pos)));
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :delete:k
my class Array::Slice::Access::delete-k {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
        }
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :delete:k
my class Array::Slice::Access::lazy-delete-k {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :delete:!k
my class Array::Slice::Access::delete-not-k {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        self!delete(pos) if $!iterable.EXISTS-POS(pos);
        nqp::push($!result,pos);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# :delete:v
my class Array::Slice::Access::delete-v {
    has $!result;
    has $!elems;
    has $!iterable;

    method !accept(\pos --> Nil) {
        nqp::push($!result,self!delete(pos))
          if $!iterable.EXISTS-POS(pos);
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
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

        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

# lazy, :delete:v
my class Array::Slice::Access::lazy-delete-v {
    has $!result;
    has $!elems;
    has $!iterable;
    has int $!done;

    method !accept(\pos --> Nil) {
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

        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          $!done || nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of array slice access ---------------------------------

# vim: expandtab shiftwidth=4
