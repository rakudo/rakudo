my class Hash { # declared in BOOTSTRAP
    # my class Hash is Map {
    #     has Mu $!descriptor;

    multi method WHICH(Hash:D:) { self.Mu::WHICH }
    multi method Hash(Hash:) {
        self
    }
    multi method Map(Hash:U:) { Map }
    multi method Map(Hash:D: :$view) {
        my $hash := nqp::getattr(self,Map,'$!storage');

        # empty
        if nqp::not_i(nqp::defined($hash)) {
            nqp::create(Map)
        }

        # view, assuming no change in hash
        elsif $view {
            nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$hash)
        }

        # make cow copy
        else {
            my $map  := nqp::hash;
            my \iter := nqp::iterator($hash);
            my str $key;
            nqp::while(
              iter,
              nqp::bindkey(
                $map,
                ($key = nqp::iterkey_s(nqp::shift(iter))),
                nqp::decont(nqp::atkey($hash,$key))
              )
            );
            nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$map)
        }
    }
    method clone(Hash:D:) is raw {
        nqp::p6bindattrinvres(
          nqp::create(self),Map,'$!storage',
          nqp::clone(nqp::getattr(self,Map,'$!storage'))
        )
    }

    multi method AT-KEY(Hash:D: Str:D \key) is raw {
        nqp::if(
          nqp::getattr(self,Map,'$!storage').DEFINITE,
          nqp::ifnull(
            nqp::atkey(nqp::getattr(self,Map,'$!storage'),
              nqp::unbox_s(key)),
            nqp::p6bindattrinvres(
              (my \v := nqp::p6scalarfromdesc($!descriptor)),
              Scalar,
              '$!whence',
              -> { nqp::bindkey(nqp::getattr(self,Map,'$!storage'),
                     nqp::unbox_s(key),v) }
            )
          ),
          nqp::p6bindattrinvres(
            (my \vv := nqp::p6scalarfromdesc($!descriptor)),
            Scalar,
            '$!whence',
            -> { nqp::bindkey(
                   nqp::if(
                     nqp::getattr(self,Map,'$!storage').DEFINITE,
                     nqp::getattr(self,Map,'$!storage'),
                     nqp::bindattr(self,Map,'$!storage',nqp::hash)
                   ),
                   nqp::unbox_s(key),vv)
               }
          )
        )
    }
    multi method AT-KEY(Hash:D: \key) is raw {
        nqp::if(
          nqp::getattr(self,Map,'$!storage').DEFINITE,
          nqp::ifnull(
            nqp::atkey(nqp::getattr(self,Map,'$!storage'),
              nqp::unbox_s(key.Str)),
            nqp::p6bindattrinvres(
              (my \v := nqp::p6scalarfromdesc($!descriptor)),
              Scalar,
              '$!whence',
              -> { nqp::bindkey(nqp::getattr(self,Map,'$!storage'),
                       nqp::unbox_s(key.Str),v) }
            )
          ),
          nqp::p6bindattrinvres(
            (my \vv := nqp::p6scalarfromdesc($!descriptor)),
            Scalar,
            '$!whence',
            -> { nqp::bindkey(
                   nqp::if(
                     nqp::getattr(self,Map,'$!storage').DEFINITE,
                     nqp::getattr(self,Map,'$!storage'),
                     nqp::bindattr(self,Map,'$!storage',nqp::hash)
                   ),
                   nqp::unbox_s(key.Str),vv)
               }
          )
        )
    }

    multi method STORE_AT_KEY(Str:D \key, Mu \x --> Nil) {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          nqp::unbox_s(key),
          (nqp::p6scalarfromdesc($!descriptor) = x),
        )
    }
    multi method STORE_AT_KEY(\key, Mu \x --> Nil) {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          nqp::unbox_s(key.Str),
          (nqp::p6scalarfromdesc($!descriptor) = x),
        )
    }

    multi method ASSIGN-KEY(Hash:D: Str:D \key, Mu \assignval) is raw {
        nqp::if(
          nqp::getattr(self,Map,'$!storage').DEFINITE,
          (nqp::ifnull(
             nqp::atkey(
               nqp::getattr(self,Map,'$!storage'),
               nqp::unbox_s(key)
             ),
             nqp::bindkey(
               nqp::getattr(self,Map,'$!storage'),
               nqp::unbox_s(key),
               nqp::p6scalarfromdesc($!descriptor)
             )
          ) = assignval),
          nqp::bindkey(
            nqp::bindattr(self,Map,'$!storage',nqp::hash),
            nqp::unbox_s(key),
            nqp::p6scalarfromdesc($!descriptor) = assignval
          )
        )
    }
    multi method ASSIGN-KEY(Hash:D: \key, Mu \assignval) is raw {
        nqp::if(
          nqp::getattr(self,Map,'$!storage').DEFINITE,
          (nqp::ifnull(
             nqp::atkey(
               nqp::getattr(self,Map,'$!storage'),
               nqp::unbox_s(key.Str)
             ),
             nqp::bindkey(
               nqp::getattr(self,Map,'$!storage'),
               nqp::unbox_s(key.Str),
               nqp::p6scalarfromdesc($!descriptor)
             )
          ) = assignval),
          nqp::bindkey(
            nqp::bindattr(self,Map,'$!storage',nqp::hash),
            nqp::unbox_s(key.Str),
            nqp::p6scalarfromdesc($!descriptor) = assignval
          )
        )
    }

    # for some reason, this can't be turned into a multi without
    # making setting compilation get very confused indeed
    method BIND-KEY(Hash:D: \key, Mu \bindval) is raw {
        nqp::bindattr(self,Map,'$!storage',nqp::hash)
          unless nqp::defined(nqp::getattr(self,Map,'$!storage'));
        nqp::bindkey(nqp::getattr(self,Map,'$!storage'),
          nqp::unbox_s(nqp::istype(key,Str) ?? key !! key.Str), bindval)
    }

    multi method DELETE-KEY(Hash:U: --> Nil) { }
    multi method DELETE-KEY(Hash:D: Str:D \key) {
        nqp::if(
          (nqp::getattr(self,Map,'$!storage').DEFINITE
            && nqp::existskey(nqp::getattr(self,Map,'$!storage'),
                 nqp::unbox_s(key))),
          nqp::stmts(
            (my $value = nqp::atkey(nqp::getattr(self,Map,'$!storage'),
               nqp::unbox_s(key))),
            nqp::deletekey(nqp::getattr(self,Map,'$!storage'),
              nqp::unbox_s(key)),
            $value
          ),
          nqp::p6scalarfromdesc($!descriptor)
        )
    }
    multi method DELETE-KEY(Hash:D: \key) {
        nqp::stmts(
          (my str $key = nqp::unbox_s(key.Str)),
          nqp::if(
            (nqp::getattr(self,Map,'$!storage').DEFINITE
              && nqp::existskey(nqp::getattr(self,Map,'$!storage'),$key)),
            nqp::stmts(
              (my $value = nqp::atkey(nqp::getattr(self,Map,'$!storage'),$key)),
              nqp::deletekey(nqp::getattr(self,Map,'$!storage'),$key),
              $value
            ),
            nqp::p6scalarfromdesc($!descriptor)
          )
        )
    }

    multi method perl(Hash:D \SELF:) {
        SELF.perlseen('Hash', {
            '$' x nqp::iscont(SELF)  # self is always deconted
            ~ '{' ~ self.pairs.sort.map({.perl}).join(', ') ~ '}'
        })
    }

    multi method gist(Hash:D:) {
        self.gistseen('Hash', {
            '{' ~
            self.pairs.sort.map( -> $elem {
                given ++$ {
                    when 101 { '...' }
                    when 102 { last }
                    default  { $elem.gist }
                }
            } ).join(', ')
            ~ '}'
        })
    }

    multi method DUMP(Hash:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my Mu $attrs := nqp::list();
        nqp::push($attrs, '$!descriptor');
        nqp::push($attrs,  $!descriptor );
        nqp::push($attrs, '$!storage'   );
        nqp::push($attrs,  nqp::getattr(nqp::decont(self), Map, '$!storage'));
        self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx);
    }

    # introspection
    method name() {
        nqp::isnull($!descriptor) ?? Nil !! $!descriptor.name
    }
    method keyof() {
        Str(Any)
    }
    method of() {
        nqp::isnull($!descriptor) ?? Mu !! $!descriptor.of
    }
    method default() {
        nqp::isnull($!descriptor) ?? Any !! $!descriptor.default
    }
    method dynamic() {
        nqp::isnull($!descriptor) ?? Nil !! nqp::p6bool($!descriptor.dynamic)
    }

    method push(+values) {
        fail X::Cannot::Lazy.new(:action<push>, :what(self.^name))
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
        fail X::Cannot::Lazy.new(:action<append>, :what(self.^name))
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

    proto method classify-list(|) { * }
    multi method classify-list( &test, \list, :&as ) {
        fail X::Cannot::Lazy.new(:action<classify>) if list.is-lazy;
        my \iter = (nqp::istype(list, Iterable) ?? list !! list.list).iterator;
        my $value := iter.pull-one;
        unless $value =:= IterationEnd {
            my $tested := test($value);

            # multi-level classify
            if nqp::istype($tested, Iterable) {
                loop {
                    my @keys  = $tested;
                    my $last := @keys.pop;
                    my $hash  = self;
                    $hash = $hash{$_} //= self.new for @keys;
                    ($hash{$last} //= []).push(
                      &as ?? as($value) !! $value);
                    last if ($value := iter.pull-one) =:= IterationEnd;
                    $tested := test($value);
                };
            }

            # simple classify to store a specific value
            elsif &as {
                loop {
                    (self{$tested} //= []).push(as($value));
                    last if ($value := iter.pull-one) =:= IterationEnd;
                    $tested := test($value);
                };
            }

            # just a simple classify
            else {
                loop {
                    (self{$tested} //= []).push($value);
                    last if ($value := iter.pull-one) =:= IterationEnd;
                    $tested := test($value);
                };
            }
        }
        self;
    }
    multi method classify-list( %test, $list, |c ) {
        self.classify-list( { %test{$^a} }, $list, |c );
    }
    multi method classify-list( @test, $list, |c ) {
        self.classify-list( { @test[$^a] }, $list, |c );
    }

    proto method categorize-list(|) { * }
    # XXX GLR possibly more efficient taking an Iterable, not a @list
    # XXX GLR replace p6listitems op use
    # XXX GLR I came up with simple workarounds but this can probably
    #         be done more efficiently better.
    multi method categorize-list( &test, @list, :&as ) {
       fail X::Cannot::Lazy.new(:action<categorize>) if @list.is-lazy;
       if @list {
           # multi-level categorize
           if nqp::istype(test(@list[0])[0],Iterable) {
               @list.map: -> $l {
                   my $value := &as ?? as($l) !! $l;
                   for test($l) -> $k {
                       my @keys = @($k);
                       my $last := @keys.pop;
                       my $hash  = self;
                       $hash = $hash{$_} //= self.new for @keys;
                       $hash{$last}.push: $value;
                   }
               }
           } else {    
           # just a simple categorize
               @list.map: -> $l {
                  my $value := &as ?? as($l) !! $l;
                  (self{$_} //= []).push: $value for test($l);
               }
               # more efficient (maybe?) nom version that might
               # yet be updated for GLR
               # @list.map: -> $l {
               #     my $value := &as ?? as($l) !! $l;
               #     nqp::push(
               #       nqp::p6listitems(nqp::decont(self{$_} //= [])), $value )
               #       for test($l);
           }
       }
       self;
    }
    multi method categorize-list( %test, $list ) {
        self.categorize-list( { %test{$^a} }, $list );
    }
    multi method categorize-list( @test, $list ) {
        self.categorize-list( { @test[$^a] }, $list );
    }

    # push a value onto a hash slot, constructing an array if necessary
    method !_push_construct(Mu $key, Mu \value --> Nil) {
        self.EXISTS-KEY($key)
          ?? self.AT-KEY($key).^isa(Array)
            ?? self.AT-KEY($key).push(value)
            !! self.ASSIGN-KEY($key,[self.AT-KEY($key),value])
          !! self.ASSIGN-KEY($key,value)
    }

    # append values into a hash slot, constructing an array if necessary
    method !_append_construct(Mu $key, Mu \value --> Nil) {
        self.EXISTS-KEY($key)
          ?? self.AT-KEY($key).^isa(Array)
            ?? self.AT-KEY($key).append(|value)
            !! self.ASSIGN-KEY($key,[|self.AT-KEY($key),|value])
          !! self.ASSIGN-KEY($key,value)
    }

    my role TypedHash[::TValue] does Associative[TValue] {
        # These ASSIGN-KEY candidates are only needed because of:
        #   my Int %h; try %h<a> = "foo"; dd %h
        # leaving an uninitialized Int for key <a> in the hash.  If
        # we could live with that, then these candidates can be
        # removed.  However, there are spectest covering this
        # eventuality, so to appease roast, we need these.
        multi method ASSIGN-KEY(::?CLASS:D: Str:D \key, Mu \assignval) is raw {
            nqp::if(
              nqp::getattr(self,Map,'$!storage').DEFINITE,
              nqp::if(
                nqp::existskey(
                  nqp::getattr(self,Map,'$!storage'),
                  nqp::unbox_s(key)
                ),
                (nqp::atkey(
                  nqp::getattr(self,Map,'$!storage'),
                  nqp::unbox_s(key)
                ) = assignval),
                nqp::bindkey(
                  nqp::getattr(self,Map,'$!storage'),
                  nqp::unbox_s(key),
                  nqp::p6scalarfromdesc(
                    nqp::getattr(self,Hash,'$!descriptor')) = assignval
                )
              ),
              nqp::bindkey(
                nqp::bindattr(self,Map,'$!storage',nqp::hash),
                nqp::unbox_s(key),
                nqp::p6scalarfromdesc(
                  nqp::getattr(self,Hash,'$!descriptor')) = assignval
              )
            )
        }
        multi method ASSIGN-KEY(::?CLASS:D: \key, Mu \assignval) is raw {
            nqp::stmts(
              (my str $key = nqp::unbox_s(key.Str)),
              nqp::if(
                nqp::getattr(self,Map,'$!storage').DEFINITE,
                nqp::if(
                  nqp::existskey(
                    nqp::getattr(self,Map,'$!storage'),
                    $key
                  ),
                  (nqp::atkey(
                    nqp::getattr(self,Map,'$!storage'),
                    $key
                  ) = assignval),
                  nqp::bindkey(
                    nqp::getattr(self,Map,'$!storage'),
                    nqp::unbox_s(key.Str),
                    nqp::p6scalarfromdesc(
                      nqp::getattr(self,Hash,'$!descriptor')) = assignval
                  )
                ),
                nqp::bindkey(
                  nqp::bindattr(self,Map,'$!storage',nqp::hash),
                  $key,
                  nqp::p6scalarfromdesc(
                    nqp::getattr(self,Hash,'$!descriptor')) = assignval
                )
              )
            )
        }
        multi method perl(::?CLASS:D \SELF:) {
            SELF.perlseen('Hash', {
                self.elems
                  ?? "(my {TValue.perl} % = {
                       self.pairs.sort.map({.perl}).join(', ')
                     })"
                  !! "(my {TValue.perl} %)"
            })
        }
    }
    my role TypedHash[::TValue, ::TKey] does Associative[TValue] {
        method keyof () { TKey }
        method AT-KEY(::?CLASS:D: TKey \key) is raw {
            nqp::if(
              nqp::getattr(self,Map,'$!storage').DEFINITE,
              nqp::if(
                nqp::existskey(nqp::getattr(self,Map,'$!storage'),
                  (my str $which = nqp::unbox_s(key.WHICH))),
                nqp::getattr(
                  nqp::atkey(nqp::getattr(self,Map,'$!storage'),$which),
                  Pair,'$!value'),
                nqp::p6bindattrinvres(
                  (my \v := nqp::p6scalarfromdesc(
                    nqp::getattr(self,Hash,'$!descriptor'))),
                  Scalar,
                  '$!whence',
                  -> { nqp::bindkey(nqp::getattr(self,Map,'$!storage'),
                         $which,Pair.new(key,v)); v }
                )
              ),
              nqp::p6bindattrinvres(
                (my \vv := nqp::p6scalarfromdesc(
                  nqp::getattr(self,Hash,'$!descriptor'))),
                Scalar,
                '$!whence',
                -> { nqp::bindkey(
                       nqp::if(
                         nqp::getattr(self,Map,'$!storage').DEFINITE,
                         nqp::getattr(self,Map,'$!storage'),
                         nqp::bindattr(self,Map,'$!storage',nqp::hash)
                       ),
                       nqp::unbox_s(key.WHICH), Pair.new(key,vv)); vv }
              )
            )
        }

        method STORE_AT_KEY(TKey \key, TValue \x --> Nil) {
            nqp::bindkey(
              nqp::getattr(self,Map,'$!storage'),
              nqp::unbox_s(key.WHICH),
              Pair.new(
                key,
                nqp::p6scalarfromdesc(nqp::getattr(self,Hash,'$!descriptor'))
                = x
              )
            )
        }

        method ASSIGN-KEY(::?CLASS:D: TKey \key, TValue \assignval) is raw {
            nqp::if(
              nqp::getattr(self,Map,'$!storage').DEFINITE,
              nqp::if(
                nqp::existskey(nqp::getattr(self,Map,'$!storage'),
                  my str $which = nqp::unbox_s(key.WHICH)),
                (nqp::getattr(
                  nqp::atkey(nqp::getattr(self,Map,'$!storage'),$which),
                  Pair,'$!value') = assignval),
                nqp::getattr(
                  (nqp::bindkey(nqp::getattr(self,Map,'$!storage'),$which,
                    Pair.new(key,nqp::p6scalarfromdesc(
                      nqp::getattr(self,Hash,'$!descriptor')) = assignval))),
                  Pair,'$!value')
              ),
              nqp::getattr(
                (nqp::bindkey(nqp::bindattr(self,Map,'$!storage',nqp::hash),
                  nqp::unbox_s(key.WHICH),
                  Pair.new(key,nqp::p6scalarfromdesc(
                    nqp::getattr(self,Hash,'$!descriptor')) = assignval))),
                Pair,'$!value')
            )
        }

        method BIND-KEY(TKey \key, TValue \bindval) is raw {
            nqp::getattr(
              nqp::if(
                nqp::getattr(self,Map,'$!storage').DEFINITE,
                nqp::bindkey(nqp::getattr(self,Map,'$!storage'),
                  nqp::unbox_s(key.WHICH),
                  Pair.new(key,bindval)),
                nqp::bindkey(nqp::bindattr(self,Map,'$!storage',nqp::hash),
                  nqp::unbox_s(key.WHICH),
                  Pair.new(key,bindval))
              ),
              Pair,'$!value'
            )
        }

        method EXISTS-KEY(TKey \key) {
            nqp::if(
              nqp::getattr(self,Map,'$!storage').DEFINITE,
              nqp::p6bool(nqp::existskey(
                nqp::getattr(self,Map,'$!storage'),nqp::unbox_s(key.WHICH)))
            )
        }

        method DELETE-KEY(TKey \key) {
            nqp::if(
              (nqp::getattr(self,Map,'$!storage').DEFINITE
                && nqp::existskey(nqp::getattr(self,Map,'$!storage'),
                     (my str $which = key.WHICH))),
              nqp::stmts(
                (my TValue $value =
                  nqp::getattr(
                    nqp::atkey(nqp::getattr(self,Map,'$!storage'),$which),
                    Pair,'$!value')),
                 nqp::deletekey(nqp::getattr(self,Map,'$!storage'),$which),
                 $value
              ),
              TValue
            )
        }

        method keys() {
            Seq.new(class :: does Rakudo::Internals::MappyIterator {
                method pull-one() {
                    nqp::if(
                      $!iter,
                      nqp::getattr(nqp::iterval(nqp::shift($!iter)),
                        Pair,'$!key'),
                      IterationEnd
                    )
                 }
            }.new(self))
        }
        method values() {
            Seq.new(class :: does Rakudo::Internals::MappyIterator {
                method pull-one() {
                    nqp::if(
                      $!iter,
                      nqp::getattr(nqp::iterval(nqp::shift($!iter)),
                        Pair,'$!value'),
                      IterationEnd
                    )
                 }
            }.new(self))
        }
        method kv() {
            Seq.new(class :: does Rakudo::Internals::MappyIterator {
                has $!pair;

                method pull-one() {
                    nqp::if(
                      $!pair,
                      nqp::stmts(
                        (my $value := nqp::getattr($!pair,Pair,'$!value')),
                        ($!pair := nqp::null),
                        $value
                      ),
                      nqp::if(
                        $!iter,
                        nqp::getattr(
                          ($!pair := nqp::iterval(nqp::shift($!iter))),
                          Pair,'$!key'),
                        IterationEnd
                      )
                    )
                }
            }.new(self))
        }
        method pairs() {
            Seq.new(Rakudo::Internals::MappyIterator-values.new(self))
        }
        method antipairs() {
            Seq.new(class :: does Rakudo::Internals::MappyIterator {
                method pull-one() {
                    nqp::if(
                      $!iter,
                      nqp::iterval(nqp::shift($!iter)).antipair,
                      IterationEnd
                    )
                 }
            }.new(self))
        }
        method invert() {
            self.map: { .value »=>» .key }
        }
        multi method perl(::?CLASS:D \SELF:) {
            SELF.perlseen('Hash', {
                my $TKey-perl   := TKey.perl;
                my $TValue-perl := TValue.perl;
                $TKey-perl eq 'Any' && $TValue-perl eq 'Mu'
                  ?? ':{' ~ SELF.pairs.sort.map({.perl}).join(', ') ~ '}'
                  !! "(my $TValue-perl %\{$TKey-perl\} = {
                      self.pairs.sort.map({.perl}).join(', ')
                    })"
            })
        }

        # gotta force capture keys to strings or binder fails
        method Capture() {
            nqp::defined(nqp::getattr(self,Map,'$!storage'))
              ?? do {
                     my $cap := nqp::create(Capture);
                     my $h := nqp::hash();
                     for self.kv -> \k, \v {
                         nqp::bindkey($h,
                           nqp::unbox_s(nqp::istype(k,Str) ?? k !! k.Str),
                           v)
                     }
                     nqp::bindattr($cap,Capture,'$!hash',$h);
                     $cap
                 }
              !! nqp::create(Capture)
        }
        method Map() { self.pairs.Map }
    }
    method ^parameterize(Mu:U \hash, Mu:U \t, |c) {
        if c.elems == 0 {
            my $what := hash.^mixin(TypedHash[t]);
            # needs to be done in COMPOSE phaser when that works
            $what.^set_name("{hash.^name}[{t.^name}]");
            $what;
        }
        elsif c.elems == 1 {
            my $what := hash.^mixin(TypedHash[t, c[0].WHAT]);
            # needs to be done in COMPOSE phaser when that works
            $what.^set_name("{hash.^name}[{t.^name},{c[0].^name}]");
            $what;
        }
        else {
            die "Can only type-constrain Hash with [ValueType] or [ValueType,KeyType]";
        }
    }
}


sub circumfix:<{ }>(*@elems) { my % = @elems }
sub hash(*@a, *%h) { my % = flat @a, %h }

# XXX parse hangs with ordinary sub declaration
BEGIN my &circumfix:<:{ }> = sub (*@elems) { Hash.^parameterize(Mu,Any).new(@elems) }

# vim: ft=perl6 expandtab sw=4
