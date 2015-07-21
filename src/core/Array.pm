my class X::TypeCheck { ... };
my class X::Subscript::Negative { ... };
my class X::IllegalOnFixedDimensionArray { ... };
my class X::NotEnoughDimensions { ... };

class Array { # declared in BOOTSTRAP
    # class Array is List {
    #     has Mu $!descriptor;

    my constant \SHAPE-STORAGE-ROOT := do {
        my Mu $root := nqp::newtype(nqp::knowhow(), 'Uninstantiable');
        nqp::setparameterizer($root, -> $, $key {
            my Mu $args := nqp::p6argvmarray();
            my $dim_type := nqp::newtype(nqp::knowhow(), 'MultiDimArray');
            nqp::composetype($dim_type, nqp::hash('array',
                nqp::hash('dimensions', $key.elems)));
            nqp::settypehll($dim_type, 'perl6');
            $dim_type
        });
        nqp::settypehll($root, 'perl6');
        $root
    }
    sub allocate-shaped-storage(\arr, @dims) {
        my $key := nqp::list();
        my $dims := nqp::list_i();
        for @dims {
            if nqp::istype($_, Whatever) {
                X::NYI.new(feature => 'Jagged array shapes');
            }
            nqp::push($key, Mu);
            nqp::push_i($dims, $_.Int);
        }
        my $storage := nqp::create(nqp::parameterizetype(SHAPE-STORAGE-ROOT, $key));
        nqp::setdimensions($storage, $dims);
        nqp::bindattr(arr, List, '$!items', $storage);
        arr
    }

    my role TypedArray[::TValue] does Positional[TValue] {
        method new(|) {
            my Mu $args := nqp::p6argvmarray();
            nqp::shift($args);

            my $list := nqp::p6list($args, self.WHAT, Bool::True);

            my $of = self.of;
            if ( $of !=:= Mu ) {
                for @$list {
                    if $_ !~~ $of {
                        X::TypeCheck.new(
                          operation => '.new',
                          expected  => $of,
                          got       => $_,
                        ).throw;
                    }
                }
            }

            $list;
        }
        multi method AT-POS(Int() $pos) is rw {
            if self.EXISTS-POS($pos) {
                nqp::atpos(
                  nqp::getattr(self, List, '$!items'), nqp::unbox_i($pos)
                );
            }
            else {
                nqp::p6bindattrinvres(
                    (my \v := nqp::p6scalarfromdesc(nqp::getattr(self, Array, '$!descriptor'))),
                    Scalar,
                    '$!whence',
                    -> { nqp::bindpos(
                      nqp::getattr(self,List,'$!items'), nqp::unbox_i($pos), v) }
                );
            }
        }
        multi method AT-POS(int $pos) is rw {
            if self.EXISTS-POS($pos) {
                nqp::atpos(nqp::getattr(self, List, '$!items'), $pos);
            }
            else {
                nqp::p6bindattrinvres(
                    (my \v := nqp::p6scalarfromdesc(nqp::getattr(self, Array, '$!descriptor'))),
                    Scalar,
                    '$!whence',
                    -> { nqp::bindpos(nqp::getattr(self, List,'$!items'), $pos, v)}
                );
            }
        }
        multi method BIND-POS(Int() $pos, TValue \bindval) is rw {
            self.gimme($pos + 1);
            nqp::bindpos(nqp::getattr(self, List, '$!items'), nqp::unbox_i($pos), bindval)
        }
        multi method BIND-POS(int $pos, TValue \bindval) is rw {
            self.gimme($pos + 1);
            nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, bindval)
        }
        multi method perl(::?CLASS:D \SELF:) {
            my $args = self.map({ ($_ // TValue).perl(:arglist)}).join(', ');
            'Array[' ~ TValue.perl ~ '].new(' ~ $args ~ ')';
        }
        # XXX some methods to come here...
    }

    my role ShapedArray[::TValue] does Positional[TValue] {
        has $.shape;

        proto method AT-POS(|) is rw {*}
        multi method AT-POS(Array:U: |c) is rw {
            self.Any::AT-POS(|c)
        }
        multi method AT-POS(Array:D: **@indices) is rw {
            my Mu $storage := nqp::getattr(self, List, '$!items');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            if $numind >= $numdims {
                my $idxs := nqp::list_i();
                while $numdims > 0 {
                    nqp::push_i($idxs, @indices.shift.Int);
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
                X::NYI.new(feature => "Partially dimensions views of arrays").throw
            }
        }

        proto method ASSIGN-POS(|) is rw {*}
        multi method ASSIGN-POS(Array:U: |c) is rw {
            self.Any::ASSIGN-POS(|c)
        }
        multi method ASSIGN-POS(**@indices) is rw {
            my \value = @indices.pop;
            my Mu $storage := nqp::getattr(self, List, '$!items');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            if $numind == $numdims {
                # Dimension counts match, so fast-path it
                my $idxs := nqp::list_i();
                while $numdims > 0 {
                    nqp::push_i($idxs, @indices.shift.Int);
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
            my Mu $storage := nqp::getattr(self, List, '$!items');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            my $dims := nqp::dimensions($storage);
            if $numind >= $numdims {
                my $idxs := nqp::list_i();
                loop (my int $i = 0; $i < $numind; $i = $i + 1) {
                    my int $idx = @indices.shift.Int;
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

        proto method DELETE-POS(|) is rw {*}
        multi method DELETE-POS(Array:U: |c) {
            self.Any::DELETE-POS(|c)
        }
        multi method DELETE-POS(**@indices) {
            my Mu $storage := nqp::getattr(self, List, '$!items');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            if $numind >= $numdims {
                my $idxs := nqp::list_i();
                while $numdims > 0 {
                    nqp::push_i($idxs, @indices.shift.Int);
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

        method elems() is nodal {
            return 0 unless self.DEFINITE;
            return nqp::elems(nqp::getattr(self, List, '$!items'));
        }

        multi method push(::?CLASS:D: $) {
            X::IllegalOnFixedDimensionArray.new(operation => 'push').throw
        }
        multi method push(::?CLASS:D: *@) {
            X::IllegalOnFixedDimensionArray.new(operation => 'push').throw
        }

        multi method pop(::?CLASS:D:) {
            X::IllegalOnFixedDimensionArray.new(operation => 'pop').throw
        }

        multi method shift(::?CLASS:D:) {
            X::IllegalOnFixedDimensionArray.new(operation => 'shift').throw
        }

        multi method unshift(::?CLASS:D: $) {
            X::IllegalOnFixedDimensionArray.new(operation => 'unshift').throw
        }
        multi method unshift(::?CLASS:D: *@) {
            X::IllegalOnFixedDimensionArray.new(operation => 'unshift').throw
        }

        multi method splice(::?CLASS:D: *@) {
            X::IllegalOnFixedDimensionArray.new(operation => 'splice').throw
        }

        multi method plan(::?CLASS:D: *@) {
            X::IllegalOnFixedDimensionArray.new(operation => 'plan').throw
        }

        method sink() { Nil }
        method eager() { self }
        multi method infinite(Array:D:) { False }
    }

    method new(*@, :$shape) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        if $shape.DEFINITE {
            my \arr := nqp::create(self);
            my $lol-shape := nqp::istype($shape, LoL) ?? $shape !! lol($shape);
            allocate-shaped-storage(arr, $lol-shape);
            arr does ShapedArray[Mu];
            nqp::bindattr(arr, arr.WHAT, '$!shape', $lol-shape);
            if $args {
                arr.STORE($args)
            }
            arr
        }
        else {
            nqp::p6list($args, self.WHAT, Bool::True);
        }
    }

    method shape() { LoL.new(*) }

    multi method AT-POS(Array:D: int \pos) is rw {
        fail X::OutOfRange.new(:what<Index>,:got(pos),:range<0..Inf>)
          if nqp::islt_i(pos,0);
        my Mu \items := nqp::p6listitems(self);
        # hotpath check for element existence (RT #111848)
        if nqp::existspos(items,pos)
          || nqp::isconcrete(nqp::getattr(self,List,'$!nextiter'))
          && nqp::istrue(self.EXISTS-POS(pos)) {
            nqp::atpos(items,pos);
        }
        else {
            nqp::p6bindattrinvres(
                (my \v := nqp::p6scalarfromdesc($!descriptor)),
                Scalar,
                '$!whence',
                -> { nqp::bindpos(items,pos,v) }
            );
        }
    }
    multi method AT-POS(Array:D: Int:D \pos) is rw {
        my int $pos = nqp::unbox_i(pos.Int);
        fail X::OutOfRange.new(:what<Index>,:got(pos),:range<0..Inf>)
          if nqp::islt_i($pos,0);
        my Mu \items := nqp::p6listitems(self);
        # hotpath check for element existence (RT #111848)
        if nqp::existspos(items,$pos)
          || nqp::isconcrete(nqp::getattr(self,List,'$!nextiter'))
          && nqp::istrue(self.EXISTS-POS($pos)) {
            nqp::atpos(items,$pos);
        }
        else {
            nqp::p6bindattrinvres(
                (my \v := nqp::p6scalarfromdesc($!descriptor)),
                Scalar,
                '$!whence',
                -> { nqp::bindpos(items,$pos,v) }
            );
        }
    }

    multi method ASSIGN-POS(Array:D: int \pos, Mu \assignee) is rw {
        X::OutOfRange.new(:what<Index>,:got(pos),:range<0..Inf>).throw
          if nqp::islt_i(pos,0);
        my \items := nqp::p6listitems(self);
        nqp::existspos(items,pos)
          || nqp::isconcrete(nqp::getattr(self,List,'$!nextiter'))
          && self.EXISTS-POS(pos)
            ?? (nqp::atpos(items,pos) = assignee)
            !! (nqp::bindpos(items,pos,nqp::p6scalarfromdesc($!descriptor)) = assignee)
    }
    multi method ASSIGN-POS(Array:D: Int:D \pos, Mu \assignee) is rw {
        my int $pos = nqp::unbox_i(pos);
        X::OutOfRange.new(:what<Index>,:got(pos),:range<0..Inf>).throw
          if nqp::islt_i($pos,0);
        my \items := nqp::p6listitems(self);
        nqp::existspos(items,$pos)
          || nqp::isconcrete(nqp::getattr(self,List,'$!nextiter'))
          && self.EXISTS-POS($pos)
            ?? (nqp::atpos(items,$pos) = assignee)
            !! (nqp::bindpos(items,$pos,nqp::p6scalarfromdesc($!descriptor)) = assignee)
    }

    proto method BIND-POS(|) { * }
    multi method BIND-POS(Int() $pos, Mu \bindval) is rw {
        self.gimme($pos + 1);
        nqp::bindpos(nqp::getattr(self, List, '$!items'), nqp::unbox_i($pos), bindval);
    }
    multi method BIND-POS(int $pos, Mu \bindval) is rw {
        self.gimme($pos + 1);
        nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, bindval)
    }

    multi method DELETE-POS(\pos, :$SINK) {
        fail X::Subscript::Negative.new(index => pos, type => self.WHAT) if pos < 0;

        my $value := self.AT-POS(pos); # needed for reification
        my $items := nqp::getattr(self,List,'$!items');
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

    method flattens() { 1 }

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
        '['
        ~ (  # simplify arrays that look 2D (in first 3 elems anyway)
            nqp::istype(self[0],Parcel) || nqp::istype(self[1],Parcel) || nqp::istype(self[2],Parcel)
                ?? self.map({.list.map({.perl}).join(', ')}).join('; ')
                !! self.map({.perl}).join(', ')
        )
        ~ ']'
        ~ '<>' x !nqp::iscont(SELF);
    }

    method REIFY(Parcel \parcel, Mu \nextiter) {
        my Mu $rpa := nqp::getattr(parcel, Parcel, '$!storage');
        my Mu $iter := nqp::iterator($rpa);
        my int $i = 0;
        while $iter {
            nqp::bindpos($rpa, $i, nqp::p6scalarfromdesc($!descriptor) = nqp::shift($iter));
            $i = $i + 1;
        }
        nqp::findmethod(List, 'REIFY')(self, parcel, nextiter)
    }

    method STORE(|) {
        # get arguments, shift off invocant
        my $args := nqp::p6argvmarray();
        nqp::shift($args);
        # make an array from them (we can't just use ourself for this,
        # or @a = @a will go terribly wrong); make it eager
        my $list := nqp::p6list($args, Array, Mu);
        nqp::bindattr($list, List, '$!flattens', True);
        $list.eager;
        # clear our items and set our next iterator to be one over
        # the array we just created
        nqp::bindattr(self, List, '$!items', Mu);
        nqp::bindattr(self, List, '$!nextiter', nqp::p6listiter(nqp::list($list), self));
        self
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
    multi method ACCEPTS(Array:D: $topic) {
        my $sseq = self;
        my $tseq = $topic.list;

        my int $spos = 0;
        my int $tpos = 0;
        while $spos < +$sseq {
            # if the next element is Whatever
            if nqp::istype($sseq[$spos],Whatever) {
                # skip over all of the Whatevers
                $spos = $spos + 1
                  while $spos <= +$sseq && nqp::istype($sseq[$spos],Whatever);
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

    multi method push(Array:D: \value) {
        if nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable)) && nqp::not_i(nqp::istype(value, Parcel)) {
            fail X::Cannot::Infinite.new(:action('.push to'))
              if self.infinite;
            self.gimme(*);
            nqp::p6listitems(self);
            nqp::push(
              nqp::getattr(self,List,'$!items'),
              nqp::assign(nqp::p6scalarfromdesc($!descriptor), value)
            );
            self
        }
        else {
            callsame();
        }
    }

    multi method push(Array:D: *@values) {
        fail X::Cannot::Infinite.new(:action<push>, :what(self.^name))
          if @values.infinite;
        nqp::p6listitems(self);
        my $elems = self.gimme(*);
        fail X::Cannot::Infinite.new(:action('.push to'))
          if self.infinite;

        # push is always eager
        @values.gimme(*);

        self.gimme(*);
        nqp::push(
          nqp::getattr(self,List,'$!items'),
          nqp::assign(nqp::p6scalarfromdesc($!descriptor), $_)
        ) for @values;

        self;
    }

    multi method unshift(Array:D: \value) {
        if nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable)) && nqp::not_i(nqp::istype(value, Parcel)) {
            fail X::Cannot::Infinite.new(:action<push to>)
              if self.infinite;
            self.gimme(*);
            nqp::p6listitems(self);
            nqp::unshift(
              nqp::getattr(self,List,'$!items'),
              nqp::assign(nqp::p6scalarfromdesc($!descriptor), value)
            );
            self
        }
        else {
            callsame();
        }
    }

    multi method unshift(Array:D: *@values) {
        fail X::Cannot::Infinite.new(:action<push>, :what(self.^name))
          if @values.infinite;
        nqp::p6listitems(self);
        my $elems = self.gimme(*);
        fail X::Cannot::Infinite.new(:action('.push to'))
          if self.infinite;

        # push is always eager
        @values.gimme(*);

        self.gimme(*);
        while @values {
            nqp::unshift(
              nqp::getattr(self,List,'$!items'),
              nqp::assign(nqp::p6scalarfromdesc($!descriptor), @values.pop)
            );
        }

        self;
    }
}

sub circumfix:<[ ]>(*@elems) is rw { my $ = @elems.eager }

# vim: ft=perl6 expandtab sw=4
