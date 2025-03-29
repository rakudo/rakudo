my class X::Invalid::ComputedValue { ... };

my class Hash { # declared in BOOTSTRAP
    # my class Hash is Map
    #     has Mu $!descriptor;

    multi method WHICH(Hash:D: --> ObjAt:D) { self.Mu::WHICH }
    multi method Hash(Hash:) {
        self
    }
    multi method Map(Hash:U:) { Map }
    multi method Map(Hash:D: :view($)!) is implementation-detail {
        nqp::p6bindattrinvres(
          nqp::create(Map), Map, '$!storage',
          nqp::getattr(self,Map,'$!storage')
        )
    }
    multi method Map(Hash:D:) {
        nqp::create(Map).STORE(self, :INITIALIZE, :DECONT)
    }

    multi method clone(Hash:D:) {
        my $iter := nqp::iterator(
          my $storage := nqp::clone(nqp::getattr(self,Map,'$!storage'))
        );

        # Only re-containerize the values that are containers
        nqp::while(
          $iter,
          nqp::if(
            nqp::iscont(my $value := nqp::iterval(nqp::shift($iter))),
            nqp::bindkey(
              $storage,nqp::iterkey_s($iter),nqp::clone_nd($value)
            )
          )
        );

        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::clone(self),Map,'$!storage',$storage),
          Hash, '$!descriptor', nqp::clone($!descriptor)
        )
    }

    method !AT_KEY_CONTAINER(str $key) is raw {
        nqp::p6scalarfromcertaindesc(
          ContainerDescriptor::BindHashKey.new($!descriptor,self,$key)
        )
    }

    multi method AT-KEY(Hash:D: Str:D $key) is raw {
        nqp::ifnull(
          nqp::atkey(nqp::getattr(self,Map,'$!storage'),$key),
          self!AT_KEY_CONTAINER($key)
        )
    }
    multi method AT-KEY(Hash:D: \key) is raw {
        nqp::ifnull(
          nqp::atkey(nqp::getattr(self,Map,'$!storage'),key.Str),
          self!AT_KEY_CONTAINER(key.Str)
        )
    }

    proto method STORE_AT_KEY(|) is implementation-detail {*}
    multi method STORE_AT_KEY(Str:D $key, Mu \value --> Nil) {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          $key,
          nqp::p6scalarwithvalue($!descriptor,value),
        )
    }
    multi method STORE_AT_KEY(\key, Mu \value --> Nil) {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          nqp::unbox_s(key.Str),
          nqp::p6scalarwithvalue($!descriptor,value),
        )
    }

    proto method STORE(|) {*}
    multi method STORE(Hash:D: \to_store) {
        $!descriptor := $!descriptor.next
            if nqp::eqaddr($!descriptor.WHAT, ContainerDescriptor::UninitializedAttribute);
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
              $x.PUSH_FROM_MAP($temp),
              nqp::if(
                nqp::eqaddr(($y := $iter.pull-one),IterationEnd),
                $temp.store-odd-number($x),
                $temp.STORE_AT_KEY($x,$y)
              )
            )
          )
        );

        nqp::p6bindattrinvres(self,Map,'$!storage',$storage)
    }
    multi method STORE(Hash:D: \keys, \values) {
        $!descriptor := $!descriptor.next
            if nqp::eqaddr($!descriptor.WHAT, ContainerDescriptor::UninitializedAttribute);
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
    multi method BIND-KEY(Hash:D: Str:D $key, Mu \bindval) is raw {
        nqp::bindkey(nqp::getattr(self,Map,'$!storage'),$key,bindval)
    }
    multi method BIND-KEY(Hash:D: \key, Mu \bindval) is raw {
        nqp::bindkey(nqp::getattr(self,Map,'$!storage'),key.Str,bindval)
    }

    multi method DELETE-KEY(Hash:U: --> Nil) { }
    multi method DELETE-KEY(Hash:D: Str:D $key) {
        nqp::if(
          nqp::isnull(my \value := nqp::atkey(
            nqp::getattr(self,Map,'$!storage'),
            $key
          )),
          $!descriptor.default,
          nqp::stmts(
            nqp::deletekey(nqp::getattr(self,Map,'$!storage'),$key),
            value
          )
        )
    }
    multi method DELETE-KEY(Hash:D: \key) { self.DELETE-KEY(key.Str) }

    multi method Str(Hash:D: --> Str:D) {
        self.gistseen: self.^name, {
          self.sort.join("\n")
        }
    }

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
            my Mu $tested := test($value);

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
            my Mu $tested := test($value);

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
                    my $tested-iter := $tested.iterator;
                    until ($_ := $tested-iter.pull-one) =:= IterationEnd {
                        self{$_}.push(&as ?? as($value) !! $value)
                    }
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

    my class LTHandle {
        has Mu $!storage;
        has Mu $!descriptor;
    }

    method TEMP-LET-GET-HANDLE() is raw is implementation-detail {
        my \handle = nqp::create(LTHandle);
        nqp::bindattr(handle, LTHandle, '$!storage', nqp::getattr(self, Map, '$!storage'));
        nqp::bindattr(handle, LTHandle, '$!descriptor', nqp::getattr(self, Hash, '$!descriptor'));
        handle
    }

    method TEMP-LET-LOCALIZE() is raw is implementation-detail {
        my \handle = self.TEMP-LET-GET-HANDLE;
        # Re-initialize self from the original state by taking into account conterization status of keys.
        my \iter = nqp::iterator(nqp::getattr(self, Map, '$!storage'));
        nqp::bindattr(self, Map, '$!storage', my \new-storage = nqp::hash);
        nqp::while(
            iter,
            nqp::stmts(
                nqp::shift(iter),
                (my \v = nqp::iterval(iter)),
                nqp::bindkey(
                    new-storage,
                    nqp::iterkey_s(iter),
                    nqp::if( nqp::isrwcont(v),
                             nqp::p6assign(nqp::p6scalarfromdesc(nqp::getattr(self, Hash, '$!descriptor')), v),
                             v ))));
        handle
    }

    method TEMP-LET-RESTORE(\handle --> Nil) is implementation-detail {
        nqp::bindattr(self, Hash, '$!descriptor', nqp::getattr(handle, LTHandle, '$!descriptor'));
        nqp::bindattr(self, Map, '$!storage', nqp::getattr(handle, LTHandle, '$!storage'));
    }

    proto method is-generic {*}
    multi method is-generic(::?CLASS:U:) { False }
    multi method is-generic(::?CLASS:D:) { nqp::hllbool($!descriptor.is_generic) }

    multi method INSTANTIATE-GENERIC(::?CLASS:D: TypeEnv:D \type-environment) is raw {
        $!descriptor.is_generic
            ??  nqp::p6bindattrinvres(
                    self.clone, Hash, '$!descriptor', type-environment.instantiate($!descriptor) )
            !! self.clone
    }

    method ^parameterize(
      Mu:U \hash,
      Mu \of,
      Mu \keyof = Str(Any),
      Mu \default = of
    ) {
        # fast path
        if nqp::eqaddr(of,Mu)
          && nqp::eqaddr(keyof,Str(Any))
          && nqp::eqaddr(default,Any) {
            hash
        }

        # error checking
        elsif nqp::isconcrete(of) {
            die "Can not parameterize {hash.^name} with {of.raku}"
        }

        # only constraint on type
        elsif nqp::eqaddr(keyof,Str(Any)) {
            my $what := hash.^mixin(Hash::Typed[of, Str(Any), default]);
             # needs to be done in COMPOSE phaser when that works
            my $name = hash.^name ~ '[' ~ of.^name;
            $name ~= (',Str(Any),' ~ default.^name)
              unless nqp::eqaddr(default,of);
            $what.^set_name: "$name]";
            $what
        }

        # error checking
        elsif nqp::isconcrete(keyof) {
            die "Can not parameterize {hash.^name} with {keyof.raku}"
        }

        # no support for native types yet
        elsif nqp::objprimspec(keyof) {
            die 'Parameterization of hashes with native '
              ~ keyof.raku
              ~ ' not yet implemented. Sorry.'
        }

        # a true object hash
        else {
            my $what := hash.^mixin(Hash::Object[of, keyof, default]);
            # needs to be done in COMPOSE phaser when that works
            my $name = hash.^name ~ '[' ~ of.^name ~ ',' ~ keyof.^name;
            $name ~= (',' ~ default.^name) unless nqp::eqaddr(default,of);
            $what.^set_name: "$name]";
            $what
        }
    }
}

proto sub circumfix:<{ }>(|) {*}
multi sub circumfix:<{ }>() is default { my % }
multi sub circumfix:<{ }>(*@elems) { my % = @elems }

sub circumfix:<:{ }>(*@elems) { Hash.^parameterize(Mu,Mu,Any).new(@elems) }

proto sub hash(|) {*}
#?if !jvm
multi sub hash() is default { my %h }
#?endif
multi sub hash(*%h) { %h }
multi sub hash(*@a, *%h) { my % = flat @a, %h }

# vim: expandtab shiftwidth=4
