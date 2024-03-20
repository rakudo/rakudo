my class ValueList
  is IterationBuffer   # get some low level functionality for free
  does Positional      # so we can bind into arrays
  does Iterable        # so it iterates automagically
  is repr('VMArray')   # needed to get nqp:: ops to work on self
{

    multi method WHICH(ValueList:D:) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,ValueList),
              'ValueList|',
              nqp::concat(self.^name,'|')
            ),
            nqp::sha1(
              nqp::join(
                '|',
                nqp::stmts(  # cannot use native str arrays early in setting
                  (my $strings  := nqp::list_s),
                  (my int $i     = -1),
                  (my int $elems = nqp::elems(self)),
                  nqp::while(
                    nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                    nqp::push_s($strings,nqp::atpos(self,$i).Str)
                  ),
                  $strings
                )
              )
            )
          ),
          ValueObjAt
        )
    }

    proto method new(|) {*}
    multi method new(ValueList: @args) {
        nqp::create(self)!SET-SELF: @args.iterator
    }
    multi method new(ValueList: +@args) {
        nqp::create(self)!SET-SELF: @args.iterator
    }
    method STORE(ValueList:D: \to_store, :$INITIALIZE) {
        $INITIALIZE
          ?? self!SET-SELF(to_store.iterator)
          !! X::Assignment::RO.new(value => self).throw
    }

    method !SET-SELF(\iterator) is raw {
        nqp::until(
          nqp::eqaddr((my \pulled := iterator.pull-one),IterationEnd),
          nqp::push(self, nqp::decont(pulled))
        );

        # make sure we containerize it to prevent it from being slipped
        # into a QuantHash
        my $valuelist = self
    }

    # set up stringification forms
    multi method raku(ValueList:D:) {
        (nqp::eqaddr(self.WHAT,ValueList) ?? 'ValueList' !! self.^name)
          ~ '.new('
          ~ self.List.map(*.raku).join(',')
          ~ ')'
    }
    multi method gist(ValueList:D:) { self.List.gist }
    multi method Str(ValueList:D:)  { self.raku }

    method list() { self.List }
    method FLATTENABLE_LIST() is implementation-detail { self }

    multi method roll(ValueList:D: |c) { self.List.roll: |c }
    multi method pick(ValueList:D: |c) { self.List.pick: |c }

    # coercions
    multi method Capture(ValueList:D:) {
        nqp::p6bindattrinvres(nqp::create(Capture),Capture,'@!list',self)
    }

    # methods that are not allowed on immutable things
    BEGIN for <
      ASSIGN-POS BIND-POS push append pop shift unshift prepend
    > -> $method {
        ValueList.^add_method: $method, method (ValueList:D: |) {
            X::Immutable.new(:$method, typename => self.^name).throw
        }
    }
}

# vim: expandtab shiftwidth=4
