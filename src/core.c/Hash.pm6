my class X::Invalid::ComputedValue { ... };

my class Hash { # declared in BOOTSTRAP
    # my class Hash is Map
    #     has Mu $!descriptor;

    multi method WHICH(Hash:D: --> ObjAt:D) { self.Mu::WHICH }
    multi method Hash(Hash:) {
        self
    }
    multi method Map(Hash:U:) { Map }
    multi method Map(Hash:D: :$view) {  # :view is implementation-detail
        $view
             # Agreeing that the Hash won't be changed after the .Map
          ?? nqp::p6bindattrinvres(
               nqp::create(Map), Map, '$!storage',
               nqp::getattr(self,Map,'$!storage')
             )
          !! nqp::create(Map).STORE(self, :INITIALIZE, :DECONT)
    }
    method clone(Hash:D:) is raw {
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(
            nqp::create(self),Map,'$!storage',
            nqp::clone(nqp::getattr(self,Map,'$!storage'))),
          Hash, '$!descriptor', nqp::clone($!descriptor))
    }

    method !AT_KEY_CONTAINER(Str:D \key) is raw {
        nqp::p6scalarfromcertaindesc(ContainerDescriptor::BindHashPos.new($!descriptor, self, key))
    }

    multi method AT-KEY(Hash:D: Str:D \key) is raw {
        nqp::ifnull(
          nqp::atkey(nqp::getattr(self,Map,'$!storage'),key),
          self!AT_KEY_CONTAINER(key)
        )
    }
    multi method AT-KEY(Hash:D: \key) is raw {
        nqp::ifnull(
          nqp::atkey(nqp::getattr(self,Map,'$!storage'),key.Str),
          self!AT_KEY_CONTAINER(key.Str)
        )
    }

    proto method STORE_AT_KEY(|) is implementation-detail {*}
    multi method STORE_AT_KEY(Str:D \key, Mu \x --> Nil) {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          nqp::unbox_s(key),
          nqp::p6scalarwithvalue($!descriptor, x),
        )
    }
    multi method STORE_AT_KEY(\key, Mu \x --> Nil) {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          nqp::unbox_s(key.Str),
          nqp::p6scalarwithvalue($!descriptor, x),
        )
    }
    method !STORE_MAP(\map --> Nil) {
        my $iter := nqp::iterator(nqp::getattr(map,Map,'$!storage'));
        nqp::while(
          $iter,
          self.STORE_AT_KEY(
            nqp::iterkey_s(nqp::shift($iter)),nqp::iterval($iter)
          )
        );
    }

    proto method STORE(|) {*}
    multi method STORE(Hash:D: \to_store) {
        my $temp := nqp::p6bindattrinvres(
          nqp::clone(self),   # make sure we get a possible descriptor as well
          Map,
          '$!storage',
          my $storage := nqp::hash
        );
        my $iter := to_store.iterator;
        my Mu $x;
        my Mu $y;

        nqp::until(
          nqp::eqaddr(($x := $iter.pull-one),IterationEnd),
          nqp::if(
            nqp::istype($x,Pair),
            $temp.STORE_AT_KEY(
              nqp::getattr(nqp::decont($x),Pair,'$!key'),
              nqp::getattr(nqp::decont($x),Pair,'$!value')
            ),
            nqp::if(
              (nqp::istype($x,Map) && nqp::not_i(nqp::iscont($x))),
              $temp!STORE_MAP($x),
              nqp::if(
                nqp::eqaddr(($y := $iter.pull-one),IterationEnd),
                nqp::if(
                  nqp::istype($x,Failure),
                  $x.throw,
                  X::Hash::Store::OddNumber.new(
                    found => nqp::add_i(nqp::mul_i(nqp::elems($storage),2),1),
                    last  => $x
                  ).throw
                ),
                $temp.STORE_AT_KEY($x,$y)
              )
            )
          )
        );

        nqp::p6bindattrinvres(self,Map,'$!storage',$storage)
    }
    multi method STORE(Hash:D: \keys, \values) {
        my \iterkeys   := keys.iterator;
        my \itervalues := values.iterator;
        nqp::bindattr(self,Map,'$!storage',nqp::hash);
        nqp::until(
          nqp::eqaddr((my \key := iterkeys.pull-one),IterationEnd),
          self.STORE_AT_KEY(key,itervalues.pull-one)
        );
        self
    }

    multi method ASSIGN-KEY(Hash:D: Str:D $key, Mu \assignval) is raw {
        my \storage := nqp::getattr(self,Map,'$!storage');
        nqp::p6assign(
          nqp::ifnull(
            nqp::atkey(storage, $key),
            nqp::bindkey(storage, $key,
              nqp::p6bindattrinvres(nqp::create(Scalar), Scalar, '$!descriptor', $!descriptor))),
          assignval)
    }
    multi method ASSIGN-KEY(Hash:D: \key, Mu \assignval) is raw {
        my str $key = key.Str;
        my \storage := nqp::getattr(self, Map, '$!storage');
        nqp::p6assign(
          nqp::ifnull(
            nqp::atkey(storage, $key),
            nqp::bindkey(storage, $key,
              nqp::p6bindattrinvres(nqp::create(Scalar), Scalar, '$!descriptor', $!descriptor))),
          assignval)
    }

    proto method BIND-KEY(|) {*}
    multi method BIND-KEY(Hash:D: \key, Mu \bindval) is raw {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          key.Str,
          bindval
        )
    }
    multi method BIND-KEY(Hash:D: Str:D \key, Mu \bindval) is raw {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          key,
          bindval
        )
    }

    multi method DELETE-KEY(Hash:U: --> Nil) { }
    multi method DELETE-KEY(Hash:D: Str:D \key) {
        nqp::if(
          nqp::isnull(my \value := nqp::atkey(
            nqp::getattr(self,Map,'$!storage'),
            key
          )),
          $!descriptor.default,
          nqp::stmts(
            nqp::deletekey(
              nqp::getattr(self,Map,'$!storage'),
              key
            ),
            value
          )
        )
    }
    multi method DELETE-KEY(Hash:D: \key) { self.DELETE-KEY(key.Str) }

    multi method raku(Hash:D \SELF:) {
        SELF.rakuseen(self.^name, {
            '$' x nqp::iscont(SELF)  # self is always deconted
            ~ '{' ~ self.sort.map({.raku}).join(', ') ~ '}'
        })
    }

    multi method gist(Hash:D:) {
        self.gistseen: self.^name, {
            '{'
              ~ self.sort.head(100).map(*.gist).join(', ')
              ~ (', ...' if self.elems > 100)
              ~ '}'
        }
    }

    multi method DUMP(
      Hash:D: :$indent-step = 4, :%ctx
    ) is implementation-detail {
        %ctx
          ?? self.DUMP-OBJECT-ATTRS(
               nqp::list(
                 '$!descriptor',$!descriptor,'$!storage',
                 nqp::getattr(self,Map,'$!storage')
               ),
               :$indent-step,
               :%ctx
             )
          !! DUMP(self, :$indent-step)
    }

    # introspection
    method keyof() { Str(Any) }  # overridden by Hash::Object

    proto method of() {*}
    multi method of(Hash:U:) { Mu }
    multi method of(Hash:D:) { $!descriptor.of }

    method name(Hash:D:)    { $!descriptor.name }
    method default(Hash:D:) { $!descriptor.default }
    method dynamic(Hash:D:) { nqp::hllbool($!descriptor.dynamic) }

    method push(+values) {
        return self.fail-iterator-cannot-be-lazy('.push')
          if values.is-lazy;

        my $previous;
        my int $has_previous = 0;

        nqp::if(
          $has_previous,
          nqp::stmts(
            self!_push_construct($previous,$_),
            ($has_previous = 0)
          ),
          nqp::if(
            nqp::istype($_,Pair),
            self!_push_construct(.key,.value),
            nqp::stmts(
              ($previous := $_),
              ($has_previous = 1)
            )
          )
        ) for values;

        warn "Trailing item in {self.^name}.push" if $has_previous;
        self
    }

    method append(+values) {
        return self.fail-iterator-cannot-be-lazy('.append')
          if values.is-lazy;

        my $previous;
        my int $has_previous = 0;

        nqp::if(
          $has_previous,
          nqp::stmts(
            self!_append_construct($previous,$_),
            ($has_previous = 0)
          ),
          nqp::if(
            nqp::istype($_,Pair),
            self!_append_construct(.key,.value),
            nqp::stmts(
              ($previous := $_),
              ($has_previous = 1)
            )
          )
        ) for values;

        warn "Trailing item in {self.^name}.append" if $has_previous;
        self
    }

    proto method classify-list(|) {*}
    multi method classify-list( &test, \list, :&as ) {
        return self.fail-iterator-cannot-be-lazy('classify')
          if list.is-lazy;

        my \iter = (nqp::istype(list, Iterable) ?? list !! list.list).iterator;
        my $value := iter.pull-one;
        unless $value =:= IterationEnd {
            my $tested := test($value);

            # multi-level classify
            if nqp::istype($tested, Iterable) {
                my $els = $tested.elems;
                loop {
                    my @keys = @$tested;
                    @keys == $els or X::Invalid::ComputedValue.new(
                            :name<mapper>,
                            :method<classify-list>,
                            :value('an item with different number of elements '
                                ~ 'in it than previous items'),
                            :reason('all values need to have the same number '
                                ~ 'of elements. Mixed-level classification is '
                                ~ 'not supported.'),
                        ).throw;
                    my $last := @keys.pop;
                    my $hash  = self;
                    $hash = $hash{$_} //= self.new for @keys;
                    $hash{$last}.push(&as ?? as($value) !! $value);
                    last if ($value := iter.pull-one) =:= IterationEnd;
                    $tested := test($value);
                };
            }
            # just a simple classify
            else {
                loop {
                    self{$tested}.push(&as ?? as($value) !! $value);
                    last if ($value := iter.pull-one) =:= IterationEnd;
                    nqp::istype(($tested := test($value)), Iterable)
                        and X::Invalid::ComputedValue.new(
                            :name<mapper>,
                            :method<classify-list>,
                            :value('an item with different number of elements '
                                ~ 'in it than previous items'),
                            :reason('all values need to have the same number '
                                ~ 'of elements. Mixed-level classification is '
                                ~ 'not supported.'),
                        ).throw;
                };
            }
        }
        self;
    }
    multi method classify-list( %test, |c ) {
        self.classify-list( { %test{$^a} }, |c );
    }
    multi method classify-list( @test, |c ) {
        self.classify-list( { @test[$^a] }, |c );
    }
    multi method classify-list(&test, **@list, |c) {
        self.classify-list(&test, @list, |c);
    }

    proto method categorize-list(|) {*}
    multi method categorize-list( &test, \list, :&as ) {
        return self.fail-iterator-cannot-be-lazy('.categorize')
          if list.is-lazy;

        my \iter = (nqp::istype(list, Iterable) ?? list !! list.list).iterator;
        my $value := iter.pull-one;
        unless $value =:= IterationEnd {
            my $tested := test($value);

            # multi-level categorize
            if nqp::istype($tested[0],Iterable) {
                my $els = $tested[0].elems;
                loop {
                    for $tested.cache -> $cat {
                       my @keys = @$cat or next;
                       my $last := @keys.pop;
                       my $hash  = self;
                       $hash = $hash{$_} //= self.new for @keys;
                       $hash{$last}.push(&as ?? as($value) !! $value);
                    }

                    last if ($value := iter.pull-one) =:= IterationEnd;
                    $tested := test($value);

                    nqp::istype($tested[0],Iterable)
                        and $els == $tested[0]
                        or X::Invalid::ComputedValue.new(
                            :name<mapper>,
                            :method<categorize-list>,
                            :value('an item with different number of elements '
                                ~ 'in it than previous items'),
                            :reason('all values need to have the same number '
                                ~ 'of elements. Mixed-level classification is '
                                ~ 'not supported.'),
                        ).throw;
                }
            }
            # simple categorize
            else {
                loop {
                    self{$_}.push(&as ?? as($value) !! $value)
                        for @$tested;
                    last if ($value := iter.pull-one) =:= IterationEnd;
                    nqp::istype(($tested := test($value))[0], Iterable)
                        and X::Invalid::ComputedValue.new(
                            :name<mapper>,
                            :method<categorize-list>,
                            :value('an item with different number of elements '
                                ~ 'in it than previous items'),
                            :reason('all values need to have the same number '
                                ~ 'of elements. Mixed-level classification is '
                                ~ 'not supported.'),
                        ).throw;
                };
            }
       }
       self;
    }
    multi method categorize-list( %test, |c ) {
        self.categorize-list( { %test{$^a} }, |c );
    }
    multi method categorize-list( @test, |c ) {
        self.categorize-list( { @test[$^a] }, |c );
    }
    multi method categorize-list( &test, **@list, |c ) {
        self.categorize-list( &test, @list, |c );
    }

    # push a value onto a hash slot, constructing an array if necessary
    method !_push_construct(Mu $key, Mu \value --> Nil) {
        nqp::istype((my \current := self.AT-KEY($key)),Array)
          ?? current.push(value)
          !! (current = self.EXISTS-KEY($key) ?? [current,value] !! value)
    }

    # append values into a hash slot, constructing an array if necessary
    method !_append_construct(Mu $key, Mu \value --> Nil) {
        nqp::istype((my \current := self.AT-KEY($key)),Array)
          ?? current.append(|value)
          !! (current = self.EXISTS-KEY($key) ?? [|current,|value] !! value)
    }

    method ^parameterize(Mu:U \hash, Mu \of, Mu \keyof = Str(Any)) {

        # fast path
        if nqp::eqaddr(of,Mu) && nqp::eqaddr(keyof,Str(Any)) {
            hash
        }

        # error checking
        elsif nqp::isconcrete(of) {
            "Can not parameterize {hash.^name} with {of.raku}"
        }

        # only constraint on type
        elsif nqp::eqaddr(keyof,Str(Any)) {
            my $what := hash.^mixin(Hash::Typed[of]);
             # needs to be done in COMPOSE phaser when that works
            $what.^set_name:
              hash.^name ~ '[' ~ of.^name ~ ']';
            $what
        }

        # error checking
        elsif nqp::isconcrete(keyof) {
            "Can not parameterize {hash.^name} with {keyof.raku}"
        }

        # no support for native types yet
        elsif nqp::objprimspec(keyof) {
            'Parameterization of hashes with native '
              ~ keyof.raku
              ~ ' not yet implemented. Sorry.'
        }

        # a true object hash
        else {
            my $what := hash.^mixin(Hash::Object[of, keyof]);
            # needs to be done in COMPOSE phaser when that works
            $what.^set_name:
              hash.^name ~ '[' ~ of.^name ~ ',' ~ keyof.^name ~ ']';
            $what
        }
    }
}

proto sub circumfix:<{ }>(|) {*}
multi sub circumfix:<{ }>(*@elems) { my % = @elems }

# XXX parse dies with 'don't change grammar in the setting, please!'
# with ordinary sub declaration
#sub circumfix:<:{ }>(*@elems) { Hash.^parameterize(Mu,Any).new(@elems) }
BEGIN my &circumfix:<:{ }> = sub (*@e) { Hash.^parameterize(Mu,Any).new(@e) }

proto sub hash(|) {*}
multi sub hash(*%h) { %h }
multi sub hash(*@a, *%h) { my % = flat @a, %h }

# vim: expandtab shiftwidth=4
