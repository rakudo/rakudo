my class X::ArrayShapeMismatch { ... }

# The rows of a jagged array, each not made yet given as a container that
# makes it
my class Array::JaggedRows does Iterator is implementation-detail {
    has $!array;
    has int $!i;

    method new(\array) {
        my \iterator := nqp::create(self);
        nqp::bindattr(iterator,Array::JaggedRows,'$!array',array);
        nqp::bindattr_i(iterator,Array::JaggedRows,'$!i',-1);
        iterator
    }
    method pull-one() is raw {
        nqp::islt_i(
          ($!i = nqp::add_i($!i,1)),
          nqp::elems(nqp::getattr($!array,List,'$!reified'))
        ) ?? $!array.AT-POS(nqp::box_i($!i,Int)) !! IterationEnd
    }
}

# Gives a native array of no set length the type of its elements
my role Array::JaggedSlots[::T] is array_type(T) is implementation-detail { }

# The type of each array of the first dimension of a jagged array. One not
# made yet reads as this type object, from which a write through its
# container makes one of its shape, as the S09 design doc describes.
my role Array::JaggedRow[::Base, Str:D $key] is implementation-detail {
    my $shape := Rakudo::Internals.JAGGED-SHAPE-OF($key);
    my int $jagged = Rakudo::Internals.JAGGED-SHAPE($shape);
    my int $views  = nqp::isgt_i($shape.elems,1) && nqp::not_i($jagged);

    # An array of objects or of a jagged shape takes this type, a native one
    # of no set length is made of it, and a shaped native one keeps its
    # elements in storage that takes its methods from this type
    my int $make = nqp::istype(Base,Array) || $jagged
      ?? 0
      !! nqp::istype($shape.AT-POS(0),Whatever) ?? 1 !! 2;
    method !make() is raw {
        nqp::iseq_i($make,0)
          ?? $jagged
            ?? Rakudo::Internals.JAGGED-INSTANCE(self.WHAT)
            !! nqp::rebless(Base.new(:$shape), self.WHAT)
          !! nqp::iseq_i($make,1)
            ?? nqp::create(self.WHAT)
            !! Rakudo::Internals.SHAPED-ARRAY-STORAGE($shape, self.HOW, Base.of)
    }

    # A new array of this type, unless given a shape of its own
    method new(|c) {
        return Base.new(|c) if c.hash.EXISTS-KEY('shape');
        my \row    := self!make;
        my \values := c.list;
        row.STORE(nqp::iseq_i(values.elems,1) ?? values.head !! values)
          if values;
        row
    }
    method shape() { $shape }

    # The elements, keys and length of one not made yet are those of a new
    # one. Each of its elements makes it when assigned, as when read by index.
    method !pairs(\SELF) {
        my \row := self.new;
        List.from-iterator(row.keys.map({
            Pair.new($_, self!element(SELF, nqp::istype($_,List) ?? $_ !! ($_,), row))
        }).iterator)
    }
    method !elements(\SELF) {
        List.from-iterator(self!pairs(SELF).map({ .value }).iterator)
    }
    multi method elems(::?CLASS:U:)         { self.new.elems  }
    multi method end(::?CLASS:U:)           { self.new.end    }
    multi method keys(::?CLASS:U:)          { self.new.keys   }
    multi method List(::?CLASS:U \SELF:)    { self!elements(SELF) }
    multi method values(::?CLASS:U \SELF:)  { self!elements(SELF).Seq }
    multi method pairs(::?CLASS:U \SELF:)   { self!pairs(SELF).Seq }
    multi method kv(::?CLASS:U \SELF:) {
        my \kv := nqp::create(IterationBuffer);
        for self!pairs(SELF) -> \pair {
            nqp::push(kv,pair.key);
            nqp::push(kv,nqp::getattr(pair,Pair,'$!value'));
        }
        kv.List.Seq
    }
    # Not multis, as those of a native or shaped array are not
    method iterator(\SELF:) {
        nqp::isconcrete(SELF) ?? nextsame() !! self!elements(SELF).iterator
    }
    method list(\SELF:) {
        nqp::isconcrete(SELF) ?? nextsame() !! self!elements(SELF)
    }
    method Seq(\SELF:) {
        nqp::isconcrete(SELF) ?? nextsame() !! self!elements(SELF).Seq
    }

    # A change to an array not made yet is made to a new one, which then
    # takes its place, so a change it refuses makes nothing
    method !vivify(\SELF, &change) is raw {
        X::Assignment::RO.new(:value(SELF)).throw unless nqp::iscont(SELF);
        my \row    := self.new;
        my \result := change(row);
        nqp::istype_nd(result,Failure)
          ?? result
          !! nqp::stmts(self!put(SELF, row), nqp::decont(SELF))
    }

    # Assigns a row made in place of one not made yet, which the container
    # of that one binds rather than copies as it would any other value
    method !put(\SELF, \row --> Nil) {
        my $*JAGGED-CONTAINER := SELF;
        my $*JAGGED-ROW       := row;
        SELF = row;
    }

    # An element reads as that of a new array and makes one when assigned.
    # Fewer indices than the dimensions of a shaped array give a view.
    method !element(\SELF, \given, \new = self.new) is raw {
        my \indices := given.map({ nqp::decont($_) }).List;
        return Array::ShapedView.new(SELF, indices)
          if $views && nqp::islt_i(indices.elems,$shape.elems);

        my \element := new.AT-POS(|indices);
        return element if nqp::istype(element,Failure);
        my $proxy := Proxy.new(
          FETCH => -> $ {
              nqp::isconcrete(SELF)
                ?? nqp::decont(nqp::decont(SELF).AT-POS(|indices))
                !! nqp::iscont(element)
                  ?? nqp::decont(element)
                  !! element.WHAT
          },
          STORE => -> $, \value {
              my \made := Rakudo::Internals.JAGGED-MADE($proxy, value);
              nqp::isconcrete(SELF)
                ?? made
                  ?? nqp::decont(SELF).BIND-POS(|indices, nqp::decont(value))
                  !! nqp::decont(SELF).ASSIGN-POS(|indices, value)
                !! self!vivify(SELF, -> \row {
                       WRITTEN(row, made
                         ?? row.BIND-POS(|indices, nqp::decont(value))
                         !! row.ASSIGN-POS(|indices, value), value)
                   })
          }
        );
        $proxy
    }

    # The indices of a capture, without the value that ends it
    sub INDICES(\c) is raw {
        my \indices := nqp::clone(nqp::getattr(c,Capture,'@!list'));
        nqp::pop(indices);
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',indices)
    }

    multi method AT-POS(::?CLASS:U \SELF: Int:D \pos) is raw {
        self!element(SELF, (pos,))
    }
    multi method AT-POS(::?CLASS:U \SELF: |c) is raw is default {
        self!element(SELF, c.list)
    }
    # The row a write made, or the Failure it was refused with
    sub WRITTEN(\row, Mu \result, Mu \value) is raw {
        Rakudo::Internals.JAGGED-REFUSED(result, value) ?? result !! row
    }
    multi method ASSIGN-POS(::?CLASS:U \SELF: Int:D \pos, Mu \value) is raw {
        my \made := self!vivify(SELF, -> \row {
            WRITTEN(row, row.ASSIGN-POS(pos, value), value)
        });
        nqp::istype_nd(made,Failure) ?? made !! made.AT-POS(pos)
    }
    multi method ASSIGN-POS(::?CLASS:U \SELF: |c) is raw is default {
        my \args  := nqp::getattr(c,Capture,'@!list');
        my \value := nqp::atpos(args,nqp::sub_i(nqp::elems(args),1));
        my \made  := self!vivify(SELF, -> \row {
            WRITTEN(row, row.ASSIGN-POS(|c), value)
        });
        nqp::istype_nd(made,Failure) ?? made !! made.AT-POS(|INDICES(c))
    }
    multi method BIND-POS(::?CLASS:U \SELF: |c) is raw {
        my \args  := nqp::getattr(c,Capture,'@!list');
        my \value := nqp::atpos(args,nqp::sub_i(nqp::elems(args),1));
        my \made  := self!vivify(SELF, -> \row {
            WRITTEN(row, row.BIND-POS(|c), value)
        });
        nqp::istype_nd(made,Failure) ?? made !! made.BIND-POS(|c)
    }
    multi method EXISTS-POS(::?CLASS:U: | --> False) is default { }
    multi method DELETE-POS(::?CLASS:U: |c) is raw is default {
        self.new.DELETE-POS(|c)
    }

    # Not multis, as those of a shaped array are protos that do the work
    method push(\SELF: |values) is nodal {
        nqp::isconcrete(SELF)
          ?? nextsame()
          !! self!vivify(SELF, -> \row { row.push(|values) })
    }
    method append(\SELF: |values) is nodal {
        nqp::isconcrete(SELF)
          ?? nextsame()
          !! self!vivify(SELF, -> \row { row.append(|values) })
    }
    method unshift(\SELF: |values) is nodal {
        nqp::isconcrete(SELF)
          ?? nextsame()
          !! self!vivify(SELF, -> \row { row.unshift(|values) })
    }
    method prepend(\SELF: |values) is nodal {
        nqp::isconcrete(SELF)
          ?? nextsame()
          !! self!vivify(SELF, -> \row { row.prepend(|values) })
    }
}

# An array whose shape leaves the length of a dimension unset, so that
# dimension takes any index. Each element of its first dimension is an
# array of the dimensions it leaves, made up front for a set length.
my role Array::Jagged[::Base, Str:D $key, ::Row] is implementation-detail {
    my $shape     := Rakudo::Internals.JAGGED-SHAPE-OF($key);
    my int $dims   = $shape.elems;
    my int $fixed  = nqp::not_i(nqp::istype($shape.AT-POS(0),Whatever));
    my int $length = $fixed ?? $shape.AT-POS(0) !! 0;
    my $row-shape := Row.shape;
    my \DEFAULT   := nqp::istype(Base,Array)
      ?? nqp::decont(Base.new.AT-POS(0))
      !! nqp::iseq_i(nqp::objprimspec(Base.of),2)
        ?? 0e0
        !! nqp::iseq_i(nqp::objprimspec(Base.of),3) ?? '' !! 0;

    proto method new(|) {*}
    multi method new(::?CLASS: |c) { Base.new(:$shape, |c) }

    method shape()   { $shape }
    method of()      { Base.of }
    method default() { DEFAULT }
    method JAGGED-ROW()  is implementation-detail { Row }
    method JAGGED-BASE() is implementation-detail { Base }
    method JAGGED-ROWS() is implementation-detail { self!rows }
    method JAGGED-ROW-OF(\values) is implementation-detail { self!new-row(values) }

    # Whether a position is within the first dimension
    method POS-WITHIN(\pos) is implementation-detail {
        so 0 <= pos && (nqp::not_i($fixed) || pos < $length)
    }

    # Whether each index is within its dimension. When asked to, it dies for
    # the first outside a set length, and fails for a negative one of a
    # dimension of no set length as an array does.
    method !within(@indices, int $die) {
        my int $i = -1;
        for @indices -> \index {
            last if nqp::isge_i(++$i,$dims);
            my $pos := index.Int;
            $pos.throw if nqp::istype($pos,Failure);
            my \length := $shape.AT-POS($i);
            if nqp::istype(length,Whatever) {
                if $pos < 0 {
                    return False unless $die;
                    return X::OutOfRange.new(
                      :what($*INDEX // 'Index'), :got($pos), :range<0..^Inf>
                    ).Failure;
                }
            }
            elsif nqp::not_i(0 <= $pos < length) {
                return False unless $die;
                die "Index $pos for dimension {$i + 1} out of range (must be 0..{length - 1})";
            }
        }
        True
    }

    # The values a row takes from a value, even an item, as those of a new
    # row for one not made yet and none for another undefined positional.
    # A row of several dimensions takes each row of a shaped array as one.
    method !values(Mu \value) is raw {
        my \decont := nqp::decont(value);
        nqp::isconcrete(decont)
          ?? nqp::isgt_i($row-shape.elems,1)
            ?? self!rows-in(decont) // decont
            !! decont
          !! nqp::istype(decont,Array::JaggedRow)
            ?? decont.new
            !! nqp::istype(decont,Positional) ?? () !! value
    }

    # The arrays of the first dimension of a shaped array of more than one
    # dimension, as its iterator gives the elements of the last
    method !rows-in(\value) {
        (nqp::istype(value,Rakudo::Internals::ShapedArrayCommon)
          || nqp::istype(value,Array::ShapedView))
          && nqp::isgt_i(value.shape.elems,1)
          ?? (^value.shape.AT-POS(0)).map({ value.AT-POS($_) })
          !! Nil
    }

    # The values arguments give as rows, a single list giving its values as
    # +@ would, and a shaped array of several dimensions its rows
    method !rows-of(\c) {
        my \args := c.list;
        nqp::iseq_i(args.elems,1) && nqp::not_i(nqp::iscont(args.AT-POS(0)))
          ?? self!rows-in(nqp::decont(args.AT-POS(0)))
               // (nqp::istype(args.AT-POS(0),Iterable) ?? args.AT-POS(0) !! args)
          !! args
    }

    # A new element of the first dimension holding the values given
    method !new-row(Mu \values) is raw {
        my \row := Row.new;
        row.STORE(self!values(values));
        row
    }

    # An element of the first dimension not made yet reads as the type
    # object of one, and is made with the values assigned through it
    method !unmade(\given) is raw {
        my \array := self;
        my \pos   := nqp::decont(given);
        my $proxy := Proxy.new(
          FETCH => -> $ {
              my \row := array.Array::AT-POS(pos);
              nqp::isconcrete(row) ?? nqp::decont(row) !! Row
          },
          STORE => -> $, \value {
              Rakudo::Internals.JAGGED-MADE($proxy, value)
                ?? self!bind-row(pos, nqp::decont(value))
                !! array.ASSIGN-POS(pos, value)
          }
        );
        $proxy
    }

    # A row not made yet is given as the container of one
    multi method iterator(::?CLASS:D:) { Array::JaggedRows.new(self) }
    multi method reverse(::?CLASS:D:) { Seq.new(self.iterator).reverse }
    multi method rotate(::?CLASS:D: |c) { Seq.new(self.iterator).rotate(|c) }
    multi method tail(::?CLASS:D:)    { Seq.new(self.iterator).tail     }
    multi method tail(::?CLASS:D: $n) { Seq.new(self.iterator).tail($n) }
    multi method Slip(::?CLASS:D:) { Slip.from-iterator(self.iterator) }

    # Binds a row at a position. Each position it leaves between the last
    # row and it holds the type object of a row, as one not made yet.
    method !bind-row(\pos, \row) is raw {
        my \reified := nqp::getattr(self,List,'$!reified');
        my int $i    = nqp::sub_i(nqp::elems(reified),1);
        my \bound   := self.Array::BIND-POS(pos, row);
        unless nqp::istype(bound,Failure) {
            nqp::bindpos(reified,$i,Row) while nqp::islt_i(++$i,pos);
        }
        bound
    }

    proto method AT-POS(|) is raw {*}
    multi method AT-POS(::?CLASS:D: Int:D \pos) is raw {
        unless self.POS-WITHIN(pos) {
            my \refused := self!within((pos,), 1);
            return refused if nqp::istype(refused,Failure);
        }
        my \row := self.Array::AT-POS(pos);
        nqp::isconcrete(row) ?? row !! self!unmade(pos)
    }
    multi method AT-POS(::?CLASS:D: **@indices) is raw {
        my \refused := self!within(@indices, 1);
        return refused if nqp::istype(refused,Failure);
        my \row := self.AT-POS(@indices.shift.Int);
        @indices ?? row.AT-POS(|@indices) !! row
    }

    # The value is taken from the capture, as a slurpy that is not raw would
    # make Nil Any and take the value out of a container to bind
    proto method ASSIGN-POS(|) is raw {*}
    multi method ASSIGN-POS(::?CLASS:D: |c) is raw {
        my \args    := nqp::clone(nqp::getattr(c,Capture,'@!list'));
        my \value   := nqp::pop(args);
        my \indices := nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',args);
        my \refused := self!within(indices, 1);
        return refused if nqp::istype(refused,Failure);
        my \pos := nqp::shift(args).Int;
        my \row := self.Array::AT-POS(pos);
        nqp::elems(args)
          ?? nqp::isconcrete(row)
            ?? row.ASSIGN-POS(|indices, value)
            !! do {
                   my \new    := Row.new;
                   my \result := new.ASSIGN-POS(|indices, value);
                   self!bind-row(pos, new)
                     unless Rakudo::Internals.JAGGED-REFUSED(result, value);
                   result
               }
          !! nqp::isconcrete(row)
            ?? nqp::eqaddr(nqp::decont(value),row)
              ?? row
              !! row.STORE(self!values(value))
            !! self!bind-row(pos, self!new-row(value))
    }

    proto method BIND-POS(|) is raw {*}
    multi method BIND-POS(::?CLASS:D: |c) is raw {
        my \args    := nqp::clone(nqp::getattr(c,Capture,'@!list'));
        my \value   := nqp::pop(args);
        my \indices := nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',args);
        my \refused := self!within(indices, 1);
        return refused if nqp::istype(refused,Failure);
        my \pos := nqp::shift(args).Int;
        my \row := self.Array::AT-POS(pos);
        if nqp::elems(args) {
            nqp::isconcrete(row)
              ?? row.BIND-POS(|indices, value)
              !! do {
                     my \new    := Row.new;
                     my \result := new.BIND-POS(|indices, value);
                     self!bind-row(pos, new)
                       unless Rakudo::Internals.JAGGED-REFUSED(result, value);
                     result
                 }
        }
        else {
            # a native jagged array is an Array, so it is checked by its base type
            my \base := nqp::istype(value,Array::Jagged)
              ?? value.JAGGED-BASE
              !! value.WHAT;
            X::TypeCheck::Binding.new(
              :got(value), :expected(Row), :symbol("element {pos}")
            ).throw unless nqp::istype(base,Base);

            # the type object of an array empties the element as deleting it does
            if nqp::isconcrete(value) {
                X::ArrayShapeMismatch.new(
                  :action<bind>, :source-shape(value.shape), :target-shape($row-shape)
                ).throw unless value.shape.List eqv $row-shape;
                self!bind-row(pos, nqp::decont(value))
            }
            else {
                self.DELETE-POS(pos);
                value
            }
        }
    }

    proto method EXISTS-POS(|) {*}
    multi method EXISTS-POS(::?CLASS:D: **@indices) {
        return False unless self!within(@indices, 0);
        my \pos := @indices.shift.Int;
        my \row := nqp::decont(self.Array::AT-POS(pos));
        nqp::isconcrete(row)
          ?? (!@indices || row.EXISTS-POS(|@indices))
          !! False
    }

    proto method DELETE-POS(|) is raw {*}
    multi method DELETE-POS(::?CLASS:D: **@indices) is raw {
        my \refused := self!within(@indices, 1);
        return refused if nqp::istype(refused,Failure);
        my \pos := @indices.shift.Int;
        if @indices {
            self.AT-POS(pos).DELETE-POS(|@indices)
        }
        elsif $fixed {
            my \row := self.Array::AT-POS(pos);
            self.Array::BIND-POS(pos, Row.new);
            row
        }
        # a row deleted is not made, and rows not made that end the array go
        else {
            my \reified := nqp::getattr(self,List,'$!reified');
            if pos < nqp::elems(reified) {
                my \row := nqp::atpos(reified,pos);
                nqp::bindpos(reified,pos,Row);
                nqp::pop(reified)
                  while nqp::elems(reified) && nqp::not_i(nqp::isconcrete(
                    nqp::atpos(reified,nqp::sub_i(nqp::elems(reified),1))
                  ));
                nqp::isconcrete(row) ?? row !! Row
            }
            else {
                Row
            }
        }
    }

    proto method STORE(::?CLASS:D: |) {*}
    # Lazy values fill a set length, as they do a shaped array
    multi method STORE(::?CLASS:D: \values, *%) is raw {
        my int $lazy = nqp::istype(values,Iterable) && values.is-lazy;
        X::Cannot::Lazy.new(:action<store>, :what(self.^name)).throw
          if $lazy && nqp::not_i($fixed);
        my \rows := nqp::create(IterationBuffer);
        my int $i = -1;
        for self!rows-in(nqp::decont(values)) // values -> \row {
            if $fixed && nqp::isge_i(++$i,$length) {
                last if $lazy;
                die "Index $i for dimension 1 out of range (must be 0..{$length - 1})";
            }
            nqp::push(rows,self!new-row(nqp::decont(row)));
        }
        nqp::push(rows,Row.new) while nqp::islt_i(nqp::elems(rows),$length);
        nqp::bindattr(self,List,'$!reified',rows);
        self
    }

    # A copy has a copy of each element of the first dimension
    method clone(::?CLASS:D:) {
        my \copy := self.Array::clone;
        my int $i = -1;
        nqp::while(
          nqp::islt_i(++$i,self.Array::elems),
          nqp::if(
            nqp::isconcrete(my \row := self.Array::AT-POS($i)),
            copy.Array::BIND-POS($i, self!new-row(nqp::decont(row)))
          )
        );
        copy
    }

    method !illegal(str $operation) {
        X::IllegalOnFixedDimensionArray.new(:$operation).throw
    }
    proto method push(|) {*}
    multi method push(::?CLASS:D: **@values is raw) {
        self!illegal('push') if $fixed;
        self.Array::BIND-POS(self.Array::elems, self!new-row(nqp::decont($_)))
          for @values;
        self
    }
    proto method append(|) {*}
    multi method append(::?CLASS:D: |c) {
        self!illegal('append') if $fixed;
        my \values := self!rows-of(c);
        X::Cannot::Lazy.new(:action<append>, :what(self.^name)).throw
          if values.is-lazy;
        self.push(|values.list)
    }
    proto method unshift(|) {*}
    multi method unshift(::?CLASS:D: **@values is raw) {
        self!illegal('unshift') if $fixed;
        nqp::unshift(
          nqp::getattr(self,List,'$!reified'),
          self!new-row(nqp::decont($_))
        ) for @values.reverse;
        self
    }
    proto method prepend(|) {*}
    multi method prepend(::?CLASS:D: |c) {
        self!illegal('prepend') if $fixed;
        my \values := self!rows-of(c);
        X::Cannot::Lazy.new(:action<prepend>, :what(self.^name)).throw
          if values.is-lazy;
        self.unshift(|values.list)
    }
    proto method pop(|) {*}
    multi method pop(::?CLASS:D:) {
        $fixed ?? self!illegal('pop') !! self.Array::pop
    }
    proto method shift(|) {*}
    multi method shift(::?CLASS:D:) {
        $fixed ?? self!illegal('shift') !! self.Array::shift
    }
    proto method grab(|) {*}
    multi method grab(::?CLASS:D: |c) {
        $fixed ?? self!illegal('grab') !! self.Array::grab(|c)
    }
    # Splices a plain array of the rows, as the splice of Array would call
    # back into the methods of this one
    proto method splice(|) {*}
    multi method splice(::?CLASS:D: |c) {
        self!illegal('splice') if $fixed;
        my \args := c.list;
        my @rows = (^self.Array::elems).map({
            nqp::decont(self.Array::AT-POS($_)).item
        });

        # a single list of values gives the values of the rows, as it does
        # the elements of an array
        my \new := args.elems == 3
          && nqp::istype(args.AT-POS(2),Iterable)
          && nqp::not_i(nqp::iscont(args.AT-POS(2)))
          ?? self!rows-in(nqp::decont(args.AT-POS(2))) // args.AT-POS(2)
          !! args.skip(2);
        my \removed := args.elems > 2
          ?? @rows.splice(
               |args.head(2),
               new.map({ self!new-row(nqp::decont($_)).item }).List
             )
          !! @rows.splice(|args);
        my \reified := nqp::create(IterationBuffer);
        nqp::push(reified,nqp::decont($_)) for @rows;
        nqp::bindattr(self,List,'$!reified',reified);
        removed
    }

    # The elements of the first dimension, a new one for each not made yet
    method !rows() {
        (^self.elems).map({
            my \row := self.Array::AT-POS($_);
            nqp::isconcrete(row) ?? nqp::decont(row) !! Row.new
        })
    }

    multi method gist(::?CLASS:D:) {
        self.gistseen(self.^name, {
            '[' ~ self!rows.map(*.gist).join(' ') ~ ']'
        })
    }
    # The rows are given as one list, as a single row would be taken as
    # the list of the values of rows
    multi method raku(::?CLASS:D:) {
        my @rows = self!rows.map(*.raku);
        Base.^name
          ~ '.new(:shape('
          ~ $shape.map({ nqp::istype($_,Whatever) ?? '*' !! .raku }).join(', ')
          ~ '), ('
          ~ @rows.join(', ')
          ~ (',' if @rows == 1)
          ~ '))'
    }
}

# Assigns to each element of the first dimension of a jagged array that a
# slice takes, as assigning to it by its index does
my class Array::JaggedSlice is implementation-detail {
    has $!array;

    method new(\array) {
        nqp::p6bindattrinvres(nqp::create(self),Array::JaggedSlice,'$!array',array)
    }
    method elems() { $!array.elems }
    method EXISTS-POS(\pos) { $!array.EXISTS-POS(pos) }
    method AT-POS(\given) is raw {
        my \array := $!array;
        my \pos   := nqp::decont(given);
        array.AT-POS(pos);
        Proxy.new(
          FETCH => -> $ { nqp::decont(array.AT-POS(pos)) },
          STORE => -> $, \value { array.ASSIGN-POS(pos, value) }
        )
    }
}
multi sub postcircumfix:<[ ]>(
  Array::Jagged:D \SELF,
  Iterable:D \positions,
  \values
) is raw {
    nqp::iscont(positions)
      ?? SELF.ASSIGN-POS(positions.Int, values)
      !! Array::Slice::Assign::none.new(
           Array::JaggedSlice.new(SELF),
           Rakudo::Iterator.TailWith(
             values.map({
                 # a copy, as a row given may be one assigned to before it
                 nqp::isconcrete($_) && nqp::istype($_,Positional)
                   ?? SELF.JAGGED-ROW-OF($_) !! $_
             }).iterator,
             ()
           )
         ).assign-slice(positions.iterator)
}

# Jagged arrays are the same when their base types, shapes and values are,
# whatever the types of their rows, so one with a row bound to it is the
# same as one with that row assigned
multi sub infix:<eqv>(Array::Jagged:D \a, Array::Jagged:D \b --> Bool:D) {
    return True if nqp::eqaddr(nqp::decont(a),nqp::decont(b));
    return False
      unless nqp::eqaddr(a.JAGGED-BASE,b.JAGGED-BASE)
        && a.shape eqv b.shape
        && a.elems == b.elems;
    for a.JAGGED-ROWS Z b.JAGGED-ROWS -> (\x, \y) {
        return False unless nqp::istype(x,Array::Jagged)
          ?? x eqv y
          !! x.shape eqv y.shape && x.Seq.List eqv y.Seq.List;
    }
    True
}

# vim: expandtab shiftwidth=4
