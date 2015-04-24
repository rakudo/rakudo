my class X::TypeCheck { ... };
my class X::Subscript::Negative { ... };

class Array { # declared in BOOTSTRAP
    # class Array is List {
    #     has Mu $!descriptor;

    method new(|) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);

        nqp::p6list($args, self.WHAT, Bool::True);
    }

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

    method DELETE-POS(\pos, :$SINK) {
        fail X::Subscript::Negative.new(index => pos, type => self.WHAT) if pos < 0;

        my $value := self.AT-POS(pos); # needed for reification
        my $items := nqp::getattr(self,List,'$!items');
        my $end   := self.end;

        if pos == $end {
            my $pos = pos;
            nqp::pop($items);
            nqp::pop($items)
              while --$pos >= 0 && nqp::isnull(nqp::atpos($items,$pos));
        }
        elsif pos < $end {
            nqp::bindpos($items, pos, nqp::null());
        }
        else {
            return self.default;
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

}


sub circumfix:<[ ]>(*@elems) is rw { my $ = @elems.eager }

# vim: ft=perl6 expandtab sw=4
