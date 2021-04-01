my class X::Constructor::Positional  { ... }
my class X::Method::NotFound         { ... }
my class X::Method::InvalidQualifier { ... }
my class X::Attribute::Required      { ... }
my class WalkList                    { ... }

my class Mu { # declared in BOOTSTRAP

    method self { self }

    method sink(--> Nil) { }

    proto method perl(|) {*}
    multi method perl(Mu \SELF: |c) { SELF.raku(|c) }
    # although technically not a documented method, some module authors have
    # used this in the ecosystem.
    method perlseen(Mu \SELF: |c) { SELF.rakuseen(|c) }

    proto method ACCEPTS(|) {*}
    multi method ACCEPTS(Mu:U: Mu \topic) {
        nqp::hllbool(nqp::istype(topic, self))
    }
    # Typically, junctions shouldn't be typechecked literally. There are
    # exceptions though, such as Junction in particular, so this probably
    # shouldn't be handled by the compiler itself. Having a default ACCEPTS
    # candidate to handle junctions allows them to get threaded as they should
    # while preserving compatibility with existing code that has any ACCEPTS
    # candidates for Mu or Junction.
    multi method ACCEPTS(Mu:U \SELF: Junction:D \topic) is default {
        topic.THREAD: { SELF.ACCEPTS: $_ }
    }

    method WHERE() {
        nqp::p6box_i(nqp::where(self))
    }

    proto method WHICH(|) {*}
    multi method WHICH(Mu:U: --> ValueObjAt:D) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|U'),
                nqp::objectid(self)
            ),
            ValueObjAt
        )
    }
    multi method WHICH(Mu:D: --> ObjAt:D) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                nqp::objectid(self)
            ),
            ObjAt
        )
    }

    proto method iterator(|) {*}
    multi method iterator(Mu:) { Rakudo::Iterator.OneValue(self) }

    proto method split(|) {*}

    method emit {
        emit self;
    }
    method take {
        take self;
    }
    method return-rw(|) {  # same code as control.pm6's return-rw
        my $list := RETURN-LIST(nqp::p6argvmarray());
        nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $list);
        $list;
    }
    method return(|) {  # same code as control.pm6's return
        my $list := RETURN-LIST(nqp::p6argvmarray());
        nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, nqp::p6recont_ro($list));
        $list;
    }

    proto method WHY(|) {*}
    multi method WHY(Mu:) {
        my Mu $why;

        my role Suggestion[$name] {
            method gist {
                "No documentation available for type '$name'. Perhaps it can be found at https://docs.raku.org/type/$name".naive-word-wrapper
            }
        }

        if nqp::can(self.HOW, 'WHY') {
            $why := self.HOW.WHY;
        }

        if $why.defined && !$.defined #`(ie. we're a type object) {
            $why.set_docee(self);
        }
        $why // Nil but Suggestion[self.^name]
    }

    method set_why($why) {
        self.HOW.set_why($why);
    }

    proto method Bool() {*}
    multi method Bool(Mu:U: --> False) { }
    multi method Bool(Mu:D:) { self.defined }

    method so()  { self.Bool }
    method not() { self ?? False !! True }

    proto method defined(|) {*}
    multi method defined(Mu:U: --> False) { }
    multi method defined(Mu:D: --> True)  { }

    proto method new(|) {*}
    multi method new(*%attrinit) {
        nqp::eqaddr((my $bless := nqp::findmethod(self,'bless')),
                    nqp::findmethod(Mu,'bless'))
                ?? nqp::create(self).BUILDALL(Empty, %attrinit)
                !! $bless(self,|%attrinit)
    }
    multi method new($, *@) {
        X::Constructor::Positional.new(:type( self )).throw();
    }

    proto method is-lazy (|) {*}
    multi method is-lazy(Mu: --> False) { }

    method CREATE() {
        nqp::create(self)
    }

    method bless(*%attrinit) {
        nqp::create(self).BUILDALL(Empty, %attrinit);
    }

    method BUILDALL(Mu:D: @autovivs, %attrinit) {
        my $init := nqp::getattr(%attrinit,Map,'$!storage');
        # Get the build plan. Note that we do this "low level" to
        # avoid the NQP type getting mapped to a Rakudo one, which
        # would get expensive.
        my $bp := nqp::findmethod(self.HOW,'BUILDALLPLAN')(self.HOW, self);
        my int $count = nqp::elems($bp);
        my int $i = -1;

        nqp::while(
          nqp::islt_i($i = nqp::add_i($i,1),$count),

          nqp::if(
            nqp::istype((my $task := nqp::atpos($bp,$i)),Callable),
            nqp::if(                             # BUILD/TWEAK
              nqp::istype(
                (my $build := nqp::if(
                  nqp::elems($init),
                  $task(self,|%attrinit),
                  $task(self)
                )),
                Failure
              ),
              return $build
            ),

            nqp::if(                             # not just calling
              (my int $code = nqp::atpos($task,0)),

              nqp::if(                           # >0
                nqp::isle_i($code,3),
                nqp::if(                         # 1|2|3
                  nqp::existskey($init,nqp::atpos($task,3)),
                  nqp::if(                       # can initialize
                    nqp::iseq_i($code,1),
                    nqp::bindattr_i(self,        # 1
                      nqp::atpos($task,1),
                      nqp::atpos($task,2),
                      nqp::decont(%attrinit.AT-KEY(nqp::atpos($task,3)))
                    ),
                    nqp::if(
                      nqp::iseq_i($code,2),
                      nqp::bindattr_n(self,      # 2
                        nqp::atpos($task,1),
                        nqp::atpos($task,2),
                        nqp::decont(%attrinit.AT-KEY(nqp::atpos($task,3)))
                      ),
                      nqp::bindattr_s(self,      # 3
                        nqp::atpos($task,1),
                        nqp::atpos($task,2),
                        nqp::decont(%attrinit.AT-KEY(nqp::atpos($task,3)))
                      )
                    )
                  )
                ),

                nqp::if(
                  nqp::iseq_i($code,4),
                  nqp::unless(                   # 4
                    nqp::attrinited(self,
                      nqp::atpos($task,1),
                      nqp::atpos($task,2)
                    ),
                    nqp::if(
                      nqp::istype(nqp::atpos($task,3),Block),
                      nqp::stmts(
                        (my \attr := nqp::getattr(self,
                          nqp::atpos($task,1),
                          nqp::atpos($task,2)
                        )),
                        (attr = nqp::atpos($task,3)(self,attr))
                      ),
                      nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,2)) =
                        nqp::atpos($task,3)
                    )
                  ),

                  nqp::if(
                    nqp::iseq_i($code,5),
                    nqp::if(                     # 5
                      nqp::iseq_i(my int $int = nqp::getattr_i(self,
                        nqp::atpos($task,1),
                        nqp::atpos($task,2)
                      ), 0),
                      nqp::bindattr_i(self,
                        nqp::atpos($task,1),
                        nqp::atpos($task,2),
                        nqp::if(
                          nqp::istype(nqp::atpos($task,3),Block),
                          (nqp::atpos($task,3)(self,$int)),
                          nqp::atpos($task,3)
                        )
                      )
                    ),

                    nqp::if(
                      nqp::iseq_i($code,6),
                      nqp::if(                   # 6
                        nqp::iseq_n(my num $num = nqp::getattr_n(self,
                          nqp::atpos($task,1),
                          nqp::atpos($task,2)
                        ), 0e0),
                        nqp::bindattr_n(self,
                          nqp::atpos($task,1),
                          nqp::atpos($task,2),
                          nqp::if(
                            nqp::istype(nqp::atpos($task,3),Block),
                            (nqp::atpos($task,3)(self,$num)),
                            nqp::atpos($task,3)
                          )
                        )
                      ),

                      nqp::if(
                        nqp::iseq_i($code,7),
                        nqp::if(                 # 7
                          nqp::isnull_s(my str $str = nqp::getattr_s(self,
                            nqp::atpos($task,1),
                            nqp::atpos($task,2)
                          )),
                          nqp::bindattr_s(self,
                            nqp::atpos($task,1),
                            nqp::atpos($task,2),
                            nqp::if(
                              nqp::istype(nqp::atpos($task,3),Block),
                              (nqp::atpos($task,3)(self,$str)),
                              nqp::atpos($task,3)
                            )
                          )
                        ),

                      nqp::if(
                        nqp::iseq_i($code,8),
                        nqp::unless(             # 8
                          nqp::attrinited(self,
                            nqp::atpos($task,1),
                            nqp::atpos($task,2)
                          ),
                          X::Attribute::Required.new(
                            name => nqp::atpos($task,2),
                            why  => nqp::atpos($task,3)
                          ).throw
                        ),

                        nqp::if(
                          nqp::iseq_i($code,9),
                          nqp::bindattr(self,    # 9
                            nqp::atpos($task,1),
                            nqp::atpos($task,2),
                            (nqp::atpos($task,3)())
                          ),

                          nqp::if(
                            nqp::iseq_i($code,11),
                            nqp::if(             # 11
                              nqp::existskey($init,nqp::atpos($task,3)),
                              (nqp::getattr(self,
                                nqp::atpos($task,1),nqp::atpos($task,2))
                                = %attrinit.AT-KEY(nqp::atpos($task,3))),
                              nqp::bindattr(self,
                                nqp::atpos($task,1),nqp::atpos($task,2),
                                nqp::list
                              )
                            ),

                            nqp::if(
                              nqp::iseq_i($code,12),
                              nqp::if(           # 12
                                nqp::existskey($init,nqp::atpos($task,3)),
                                (nqp::getattr(self,
                                  nqp::atpos($task,1),nqp::atpos($task,2))
                                  = %attrinit.AT-KEY(nqp::atpos($task,3))),
                                nqp::bindattr(self,
                                  nqp::atpos($task,1),nqp::atpos($task,2),
                                  nqp::hash
                                )
                              ),

                              nqp::if(
                                nqp::iseq_i($code,13),
                                nqp::if(         # 13
                                  nqp::existskey($init,nqp::atpos($task,3)),
                                  nqp::bindattr(self,
                                    nqp::atpos($task,1),nqp::atpos($task,2),
                                    nqp::if(
                                      nqp::elems($task) == 5,
                                      nqp::p6bindassert(
                                        %attrinit.AT-KEY(nqp::atpos($task,3)),
                                        nqp::atpos($task,4)),
                                      %attrinit.AT-KEY(nqp::atpos($task,3))
                                    )
                                  )
                                ),

                                nqp::if(
                                  nqp::iseq_i($code,14),
                                  nqp::unless(   # 14
                                    nqp::attrinited(self,
                                      nqp::atpos($task,1),
                                      nqp::atpos($task,2)
                                    ),
                                    nqp::bindattr(self,
                                      nqp::atpos($task,1),
                                      nqp::atpos($task,2),
                                      nqp::if(
                                        nqp::istype(nqp::atpos($task,3),Block),
                                        nqp::if(
                                          nqp::elems($task) == 5,
                                          nqp::p6bindassert(
                                            nqp::atpos($task,3)(self,
                                              nqp::getattr(self,
                                              nqp::atpos($task,1),
                                              nqp::atpos($task,2)
                                            )),
                                            nqp::atpos($task,4)
                                          ),
                                          nqp::atpos($task,3)(self,
                                            nqp::getattr(self,
                                            nqp::atpos($task,1),
                                            nqp::atpos($task,2)
                                          )),
                                        ),
                                        nqp::atpos($task,3)
                                      )
                                    )
                                  ),
                                  die('Invalid ' ~ self.^name ~ ".BUILDALL plan: $code"),
                  ))))))))))),

                  nqp::if(                       # 0
                    nqp::existskey($init,nqp::atpos($task,3)),
                    (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,2))
                      = %attrinit.AT-KEY(nqp::atpos($task,3)))
                  )
                )
              )
            );
            self
        }

        method BUILD_LEAST_DERIVED(%attrinit) {
            my $init := nqp::getattr(%attrinit,Map,'$!storage');
            # Get the build plan for just this class.
            my $bp := nqp::findmethod(self.HOW,'BUILDPLAN')(self.HOW,self);
            my int $count = nqp::elems($bp);
            my int $i     = -1;

        nqp::while(
          nqp::islt_i($i = nqp::add_i($i,1),$count),

          nqp::if(
            nqp::istype((my $task := nqp::atpos($bp,$i)),Callable),
            nqp::if(                             # BUILD/TWEAK
              nqp::istype(
                (my $build := nqp::if(
                  nqp::elems($init),
                  $task(self,|%attrinit),
                  $task(self)
                )),
                Failure
              ),
              return $build
            ),

            nqp::if(                             # not just calling
              (my int $code = nqp::atpos($task,0)),

              nqp::if(                           # >0
                nqp::isle_i($code,3),
                nqp::if(                         # 1|2|3
                  nqp::existskey($init,nqp::atpos($task,3)),
                  nqp::if(                       # can initialize
                    nqp::iseq_i($code,1),
                    nqp::bindattr_i(self,        # 1
                      nqp::atpos($task,1),
                      nqp::atpos($task,2),
                      nqp::decont(%attrinit.AT-KEY(nqp::atpos($task,3)))
                    ),
                    nqp::if(
                      nqp::iseq_i($code,2),
                      nqp::bindattr_n(self,      # 2
                        nqp::atpos($task,1),
                        nqp::atpos($task,2),
                        nqp::decont(%attrinit.AT-KEY(nqp::atpos($task,3)))
                      ),
                      nqp::bindattr_s(self,      # 3
                        nqp::atpos($task,1),
                        nqp::atpos($task,2),
                        nqp::decont(%attrinit.AT-KEY(nqp::atpos($task,3)))
                      )
                    )
                  )
                ),

                nqp::if(
                  nqp::iseq_i($code,4),
                  nqp::unless(                   # 4
                    nqp::attrinited(self,
                      nqp::atpos($task,1),
                      nqp::atpos($task,2)
                    ),
                    nqp::if(
                      nqp::istype(nqp::atpos($task,3),Block),
                      nqp::stmts(
                        (my \attr := nqp::getattr(self,
                          nqp::atpos($task,1),
                          nqp::atpos($task,2)
                        )),
                        (attr = nqp::atpos($task,3)(self,attr))
                      ),
                      nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,2)) =
                        nqp::atpos($task,3)
                    )
                  ),

                  nqp::if(
                    nqp::iseq_i($code,5),
                    nqp::if(                     # 5
                      nqp::iseq_i(my int $int = nqp::getattr_i(self,
                        nqp::atpos($task,1),
                        nqp::atpos($task,2)
                      ), 0),
                      nqp::bindattr_i(self,
                        nqp::atpos($task,1),
                        nqp::atpos($task,2),
                        nqp::if(
                          nqp::istype(nqp::atpos($task,3),Block),
                          (nqp::atpos($task,3)(self,$int)),
                          nqp::atpos($task,3)
                        )
                      )
                    ),

                    nqp::if(
                      nqp::iseq_i($code,6),
                      nqp::if(                   # 6
                        nqp::iseq_n(my num $num = nqp::getattr_n(self,
                          nqp::atpos($task,1),
                          nqp::atpos($task,2)
                        ), 0e0),
                        nqp::bindattr_n(self,
                          nqp::atpos($task,1),
                          nqp::atpos($task,2),
                          nqp::if(
                            nqp::istype(nqp::atpos($task,3),Block),
                            (nqp::atpos($task,3)(self,$num)),
                            nqp::atpos($task,3)
                          )
                        )
                      ),

                      nqp::if(
                        nqp::iseq_i($code,7),
                        nqp::if(                 # 7
                          nqp::isnull_s(my str $str = nqp::getattr_s(self,
                            nqp::atpos($task,1),
                            nqp::atpos($task,2)
                          )),
                          nqp::bindattr_s(self,
                            nqp::atpos($task,1),
                            nqp::atpos($task,2),
                            nqp::if(
                              nqp::istype(nqp::atpos($task,3),Block),
                              (nqp::atpos($task,3)(self,$str)),
                              nqp::atpos($task,3)
                            )
                          )
                        ),

                      nqp::if(
                        nqp::iseq_i($code,8),
                        nqp::unless(             # 8
                          nqp::attrinited(self,
                            nqp::atpos($task,1),
                            nqp::atpos($task,2)
                          ),
                          X::Attribute::Required.new(
                            name => nqp::atpos($task,2),
                            why  => nqp::atpos($task,3)
                          ).throw
                        ),

                        nqp::if(
                          nqp::iseq_i($code,9),
                          nqp::bindattr(self,    # 9
                            nqp::atpos($task,1),
                            nqp::atpos($task,2),
                            (nqp::atpos($task,3)())
                          ),

                          nqp::if(
                            nqp::iseq_i($code,10),
                            # Force vivification, for the sake of meta-object
                            # mix-ins at compile time ending up with correctly
                            # shared containers.
                            nqp::stmts(          # 10
                              nqp::getattr(self,
                                nqp::atpos($task,1),
                                nqp::atpos($task,2)
                              ),
                              nqp::while(        # 10's flock together
                                nqp::islt_i(($i = nqp::add_i($i,1)),$count)
                                  && nqp::islist($task := nqp::atpos($bp,$i))
                                  && nqp::iseq_i(nqp::atpos($task,0),10),
                                nqp::getattr(self,
                                  nqp::atpos($task,1),
                                  nqp::atpos($task,2)
                                )
                              ),
                              ($i = nqp::sub_i($i,1))
                            ),

                            nqp::if(
                              nqp::iseq_i($code,11),
                              nqp::if(           # 11
                                nqp::existskey($init,nqp::atpos($task,3)),
                                (nqp::getattr(self,
                                  nqp::atpos($task,1),nqp::atpos($task,2))
                                  = %attrinit.AT-KEY(nqp::atpos($task,3))),
                                nqp::bindattr(self,
                                  nqp::atpos($task,1),nqp::atpos($task,2),
                                  nqp::list
                                )
                              ),

                              nqp::if(
                                nqp::iseq_i($code,12),
                                nqp::if(         # 12
                                  nqp::existskey($init,nqp::atpos($task,3)),
                                  (nqp::getattr(self,
                                    nqp::atpos($task,1),nqp::atpos($task,2))
                                    = %attrinit.AT-KEY(nqp::atpos($task,3))),
                                  nqp::bindattr(self,
                                    nqp::atpos($task,1),nqp::atpos($task,2),
                                    nqp::hash
                                  )
                                ),

                                nqp::if(
                                  nqp::iseq_i($code,13),
                                  nqp::if(       # 13
                                    nqp::existskey($init,nqp::atpos($task,3)),
                                    nqp::bindattr(self,
                                      nqp::atpos($task,1),nqp::atpos($task,2),
                                      nqp::if(
                                        nqp::elems($task) == 5,
                                        nqp::p6bindassert(
                                          %attrinit.AT-KEY(nqp::atpos($task,3)),
                                          nqp::atpos($task,4)),
                                        %attrinit.AT-KEY(nqp::atpos($task,3))
                                      )
                                    )
                                  ),

                                  nqp::if(
                                    nqp::iseq_i($code,14),
                                    nqp::unless( # 14
                                      nqp::attrinited(self,
                                        nqp::atpos($task,1),
                                        nqp::atpos($task,2)
                                      ),
                                      nqp::bindattr(self,
                                        nqp::atpos($task,1),nqp::atpos($task,2),
                                        nqp::if(
                                          nqp::istype(
                                            nqp::atpos($task,3),Block),
                                          nqp::if(
                                            nqp::elems($task) == 5,
                                            nqp::p6bindassert(
                                              nqp::atpos($task,3)(self,
                                                nqp::getattr(self,
                                                nqp::atpos($task,1),
                                                nqp::atpos($task,2)
                                              )),
                                              nqp::atpos($task,4)
                                            ),
                                            nqp::atpos($task,3)(self,
                                              nqp::getattr(self,
                                              nqp::atpos($task,1),
                                              nqp::atpos($task,2)
                                            )),
                                          ),
                                          nqp::atpos($task,3)
                                        )
                                      )
                                    ),
                                    die('Invalid ' ~ self.^name ~ ".BUILD_LEAST_DERIVED plan: $code"),
              )))))))))))),

              nqp::if(                           # 0
                nqp::existskey($init,nqp::atpos($task,3)),
                (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,2))
                  = %attrinit.AT-KEY(nqp::atpos($task,3))),
              )
            )
          )
        );
        self
    }

    proto method Numeric(|) {*}
    multi method Numeric(Mu:U \v:) {
        warn "Use of uninitialized value of type {self.^name} in numeric context";
        0
    }
    proto method Real(|) {*}
    multi method Real(Mu:U \v:) {
        warn "Use of uninitialized value of type {self.^name} in numeric context";
        0
    }

    proto method Str(|) {*}
    multi method Str(Mu:U \v:) {
        my $name = (defined($*VAR_NAME) ?? $*VAR_NAME !! try v.VAR.name) // '';
        $name   ~= ' ' if $name ne '';
        warn "Use of uninitialized value {$name}of type {self.^name} in string"
                ~ " context.\nMethods .^name, .raku, .gist, or .say can be"
                ~ " used to stringify it to something meaningful.";
        ''
    }
    multi method Str(Mu:D:) {
        nqp::eqaddr(self,IterationEnd)
          ?? "IterationEnd"
          !! self.^name ~ '<' ~ nqp::tostr_I(nqp::objectid(self)) ~ '>'
    }

    proto method Stringy(|) {*}
    multi method Stringy(Mu:U \v:) {
        my $*VAR_NAME = try v.VAR.name;
        self.Str
    }
    multi method Stringy(Mu:D $:) { self.Str }

    method item(Mu \item:) is raw { item }

    proto method say(|) {*}
    proto method put(|) {*}
    proto method note(|) {*}
    proto method print(|) {*}

    # Handle the typical "foo.say"
    multi method say() {

        my $method := self.^find_method("print");
        if nqp::not_i(nqp::istype($method,Mu))  # an NQP routine
          || nqp::eqaddr($method.package,Mu) {  # no own print method, use $*OUT
            $_ := $*OUT;
            .print(nqp::concat(self.gist,.nl-out))
        }

        # has its own .print, let it handle the empty case
        else {
            self.print(self.nl-out)
        }
    }

    # Fallback for classes that act as $*OUT / $*ERR, but which do not have
    # a .say method themselves.
    multi method say(\x) {
        self.print: nqp::concat(x.gist,self.nl-out)
    }
    multi method say(|) {
        my \args := nqp::p6argvmarray;
        nqp::shift(args);  # lose self
        my $parts := Rakudo::Internals.GistList2list_s(args);
        nqp::push_s($parts,self.nl-out);
        self.print: nqp::join("",$parts)
    }

    # Handle the typical "foo.put"
    multi method put() {

        my $method := self.^find_method("print");
        if nqp::not_i(nqp::istype($method,Mu))  # an NQP routine
          || nqp::eqaddr($method.package,Mu) {  # no own print method, use $*OUT
            $_ := $*OUT;
            .print(nqp::concat(self.Str,.nl-out))
        }

        # has its own .print, let it handle the empty case
        else {
            self.print(self.nl-out)
        }
    }

    # Fallback for classes that act as $*OUT / $*ERR, but which do not have
    # a .put method themselves.
    multi method put(\x) {
        self.print: nqp::concat(x.Str,self.nl-out)
    }
    multi method put(|) {
        my \args := nqp::p6argvmarray;
        nqp::shift(args);  # lose self
        my $parts := Rakudo::Internals.StrList2list_s(args);
        nqp::push_s($parts,self.nl-out);
        self.print: nqp::join("",$parts)
    }

    # Handle the typical "foo.note"
    multi method note() {

        my $method := self.^find_method("print");
        if nqp::not_i(nqp::istype($method,Mu))  # an NQP routine
          || nqp::eqaddr($method.package,Mu) {  # no own print method, use $*ERR
            $_ := $*ERR;
            .print(nqp::concat(self.gist,.nl-out))
        }

        # has its own .print, let it handle the empty case
        else {
            self.print(self.nl-out)
        }
    }

    # Handle the typical "foo.print"
    multi method print() {
        $*OUT.print: self.Str
    }

    method gistseen(Mu:D \SELF: $id, $gist, *%named) {
        if nqp::not_i(nqp::isnull(nqp::getlexdyn('$*gistseen'))) {
            my \sems := $*gistseen;
            my str $WHICH = nqp::unbox_s(self.WHICH);
            if nqp::existskey(sems,$WHICH) && nqp::atkey(sems,$WHICH) {
                nqp::bindkey(sems,$WHICH,2);
                "{$id}_{nqp::objectid(SELF)}";
            }
            else {
                nqp::bindkey(sems,$WHICH,1);
                my $result   := $gist(|%named);
                my int $value = nqp::atkey(sems,$WHICH);
                nqp::deletekey(sems,$WHICH);
                $value == 2
                  ?? "(\\{$id}_{nqp::objectid(SELF)} = $result)"
                  !! $result
            }
        }
        else {
            my $*gistseen := nqp::hash("TOP",1);
            SELF.gistseen($id,$gist,|%named)
        }
    }

    proto method gist(|) {*}
    multi method gist(Mu:U:) { '(' ~ self.^shortname ~ ')' }
    multi method gist(Mu:D:) { self.raku }

    method rakuseen(Mu:D \SELF: $id, $raku, *%named) {
        my $sigil = nqp::iseq_s($id, 'Array') ?? '@'
            !! nqp::iseq_s($id, 'Hash') ?? '%' !! '\\';
        if nqp::not_i(nqp::isnull(nqp::getlexdyn('$*rakuseen'))) {
            my \sems := $*rakuseen;
            my str $WHICH = nqp::unbox_s(self.WHICH);
            if nqp::existskey(sems,$WHICH) && nqp::atkey(sems,$WHICH) {
                nqp::bindkey(sems,$WHICH,2);
                $sigil x nqp::isne_s($sigil, '\\') ~ "{$id}_{nqp::objectid(SELF)}";
            }
            else {
                nqp::bindkey(sems,$WHICH,1);
                my $result := $raku(|%named);
                my int $value = nqp::atkey(sems,$WHICH);
                nqp::deletekey(sems,$WHICH);
                $value == 2
                  ?? nqp::iseq_s($sigil, '\\')
                    ??  "(my {$sigil}{$id}_{nqp::objectid(SELF)} = $result)"
                    !! "((my {$sigil}{$id}_{nqp::objectid(SELF)}) = $result)"
                  !! $result
            }
        }
        else {
            my $*rakuseen := nqp::hash("TOP",1);
            SELF.rakuseen($id,$raku,|%named)
        }
    }

    proto method raku(|) {*}
    multi method raku(Mu:U:) {
        nqp::eqaddr(self.^find_method("perl").package,Mu)
          ?? self.^name
          !! self.perl
    }
    multi method raku(Mu:D:) {
        nqp::eqaddr(self,IterationEnd)
          ?? "IterationEnd"
          !! nqp::iscont(self) # a Proxy object would have a conted `self`
            ?? nqp::decont(self).raku
            !! nqp::eqaddr((my $proto := self.^find_method("perl")).package,Mu)
                 && $proto.dispatchees == 1
              ?? self!default-raku
              !! self.perl    # class has dedicated old-style .perl
    }

    method !default-raku() {
        self.rakuseen: self.^name, {
            if self.^attributes.map( {
                nqp::concat(
                  nqp::substr(.Str,2),
                  nqp::concat(' => ',.get_value(self).raku)
                ) if .is_built;
            } ).join(', ') -> $attributes {
                self.^name ~ '.new(' ~ $attributes ~ ')'
            }
            else {
                self.^name ~ '.new'
            }
        }
    }

    proto method DUMP(|) {*}  # is implementation-detail
    multi method DUMP(Mu:U:) { self.raku }
    multi method DUMP(Mu:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my Mu $attrs := nqp::list();
        for self.^attributes.flat -> $attr {
            my str $name       = $attr.name;
            my str $acc_name   = nqp::substr($name, 2, nqp::chars($name) - 2);
            my str $build_name = $attr.has_accessor ?? $acc_name !! $name;

            my Mu $value;
            if    $attr.has_accessor {
                $value := self."$acc_name"();
            }
            elsif nqp::can($attr, 'get_value') {
                $value := $attr.get_value(self);
            }
            elsif nqp::can($attr, 'package') {
                my Mu $package := $attr.package;

                $value := do given nqp::p6box_i(nqp::objprimspec($attr.type)) {
                    when 0 {              nqp::getattr(  self,$package,$name)  }
                    when 1 { nqp::p6box_i(nqp::getattr_i(self,$package,$name)) }
                    when 2 { nqp::p6box_n(nqp::getattr_n(self,$package,$name)) }
                    when 3 { nqp::p6box_s(nqp::getattr_s(self,$package,$name)) }
                };
            }
            else {
                next;
            }

            nqp::push($attrs, $build_name);
            nqp::push($attrs, $value);
        }

        self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx);
    }
    method DUMP-PIECES(
      @pieces: $before, $after = ')', :$indent = @pieces > 1, :$indent-step
    ) {  # is implementation-detail
        $indent ?? $before ~ "\n" ~ @pieces.join(",\n").indent($indent-step) ~ "\n" ~ $after
                !! $before ~        @pieces.join(', ')                              ~ $after;
    }
    method DUMP-OBJECT-ATTRS(
      |args (*@args, :$indent-step, :%ctx, :$flags?)
    ) {  # is implementation-detail
        my Mu  $attrs := nqp::clone(nqp::captureposarg(nqp::usecapture(), 1));
        my str $where  = nqp::base_I(nqp::where(self), 16);
        my str $before = ($flags if defined $flags) ~ self.^name ~ '<' ~ %ctx{$where} ~ '>(';

        my @pieces;
        while $attrs {
            my str $name  = nqp::shift($attrs);
            my Mu $value := nqp::shift($attrs);
            @pieces.push: ':' ~ $name ~ '(' ~ DUMP($value, :$indent-step, :%ctx) ~ ')';
        }
        @pieces.DUMP-PIECES($before, :$indent-step);
    }

    proto method isa(|) {*}
    multi method isa(Mu \SELF: Mu $type --> Bool:D) {
        nqp::hllbool(SELF.^isa($type.WHAT))
    }
    multi method isa(Mu \SELF: Str:D $name --> Bool:D) {
        return True if .^name eq $name for SELF.^mro;
        False
    }

    method does(Mu \SELF: Mu $type) {
        nqp::hllbool(nqp::istype(SELF, $type.WHAT))
    }

    method can(Mu \SELF: $name) {
        SELF.^can($name)
    }

    proto method clone (|) {*}
    multi method clone(Mu:U: *%twiddles) {
        %twiddles and die 'Cannot set attribute values when cloning a type object';
        self
    }
    multi method clone(Mu:D: *%twiddles) {
        my $cloned := nqp::clone(self);
        if %twiddles.elems {
            for self.^attributes.flat -> $attr {
                my $name    := $attr.name;
                my $package := $attr.package;

                nqp::bindattr($cloned, $package, $name,
                  nqp::clone(nqp::getattr($cloned, $package, $name).VAR)
                ) if nqp::attrinited(self, $package, $name)
                    and nqp::not_i(nqp::objprimspec($attr.type));

                my $acc_name := substr($name,2);
                nqp::getattr($cloned, $package, $name) =
                  nqp::decont(%twiddles{$acc_name})
                  if $attr.has_accessor && %twiddles.EXISTS-KEY($acc_name);
            }
        }
        else {
            for self.^attributes.flat -> $attr {
                unless nqp::objprimspec($attr.type) {
                    my $name     := $attr.name;
                    my $package  := $attr.package;
                    if nqp::attrinited(self, $package, $name) {
                        my $attr_val := nqp::getattr($cloned, $package, $name);
                        nqp::bindattr($cloned,
                          $package, $name, nqp::clone($attr_val.VAR))
                            if nqp::iscont($attr_val);
                    }
                }
            }
        }
        $cloned
    }

    method Capture() {
        my $attrs := nqp::hash;
        for self.^attributes.flat -> $attr {
            if $attr.has_accessor {
                my str $name = substr($attr.name,2);
                nqp::bindkey($attrs,$name,self."$name"())
                  unless nqp::existskey($attrs,$name);
            }
        }
        my $capture := nqp::create(Capture);
        nqp::bindattr($capture,Capture,'%!hash',$attrs) if nqp::elems($attrs);
        $capture
    }

    # Various of the following dispatch methods are not called in situations
    # where the compiler can rewrite them into a cheaper form.

    # XXX TODO: Handle positional case.
    method dispatch:<var>(Mu \SELF: $var, |c) is raw {
        # We put a `return` here to make sure we do the right thing if $var
        # happens to be &fail.
        return $var(SELF, |c)
    }

    method dispatch:<::>(Mu \SELF: $name, Mu $type, |c) is raw {
        my $meth;
        my $ctx := nqp::ctxcaller(nqp::ctx());
        # Bypass wrapping thunk if redirected from spesh plugin
        $ctx := nqp::ctxcaller($ctx) if $*SPESH-THUNKED-DISPATCH;
        if nqp::istype(self, $type) {
            my $sym-found := 0;
            my $caller-type;
            repeat {
                my $pad := nqp::ctxlexpad($ctx);
                for <$?CONCRETIZATION $?CLASS> {
                    if nqp::existskey($pad, $_) {
                        $caller-type := nqp::atkey($pad, $_);
                        $sym-found := 1;
                        last;
                    }
                }
                $ctx := nqp::ctxouterskipthunks($ctx);
            } while $ctx && !$sym-found;
            $meth = $caller-type.^find_method_qualified($type, $name)
                if $sym-found && nqp::istype($caller-type, $type);
            $meth = self.^find_method_qualified($type, $name) unless $meth;
        }

        unless nqp::defined($meth) {
            X::Method::InvalidQualifier.new(
                    method          => $name,
                    invocant        => SELF,
                    qualifier-type  => $type,
            ).throw;
        }

        $meth(SELF, |c)
    }

    method dispatch:<!>(Mu \SELF: \name, Mu \type, |c) is raw {
        my $meth := type.^find_private_method(name);
        $meth ??
            $meth(SELF, |c) !!
            X::Method::NotFound.new(
              invocant => SELF,
              method   => name,
              typename => type.^name,
              :private,
              :in-class-call(nqp::eqaddr(nqp::what(SELF), nqp::getlexcaller('$?CLASS'))),
            ).throw;
    }

    method dispatch:<.=>(\mutate: Str() $name, |c) is raw {
        $/ := nqp::getlexcaller('$/');
        mutate = mutate."$name"(|c)
    }

    method dispatch:<.?>(Mu \SELF: Str() $name, |c) is raw {
        nqp::can(SELF,$name) ??
            SELF."$name"(|c) !!
            Nil
    }

    method !batch-call(Mu \SELF: \name, Capture:D \c, :$throw = False, :$reverse = False, :$roles = False) {
        my @mro := SELF.^mro(:$roles);
        my $results := nqp::create(IterationBuffer);
        my int $mro_high = $reverse ?? 0 !! @mro.elems - 1;
        my int $i = @mro.elems;
        while nqp::isge_i(--$i, 0) {
            my int $idx = nqp::abs_i($mro_high - $i);
            my Mu \type-obj = @mro[$idx];
            my $meth = (type-obj.^method_table){name} unless type-obj.HOW.archetypes.composable;
            $meth = (type-obj.^submethod_table){name} if !$meth;
            nqp::push($results,$meth(SELF, |c))    if $meth;
        }
        if $throw && $results.elems == 0 {
            X::Method::NotFound.new(
              invocant => SELF,
              method   => name,
              typename => SELF.^name,
            ).throw;
        }
        $results.List
    }

    method dispatch:<.+>(Mu \SELF: \name, |c) {
        SELF!batch-call(name, c, :throw);
    }

    method dispatch:<.*>(Mu \SELF: \name, |c) {
        SELF!batch-call(name, c)
    }

    method dispatch:<hyper>(Mu \SELF: $nodality, Str $meth-name, |c) {
        nqp::if(
          nqp::if(
            nqp::istype($nodality,Str),
            nqp::if(
              $nodality,
                 nqp::can(List,$nodality)
              && nqp::can(List.can($nodality ).AT-POS(0),'nodal'),
                 nqp::can(List,$meth-name)
              && nqp::can(List.can($meth-name).AT-POS(0),'nodal')),
            nqp::can($nodality, 'nodal')),
          nqp::if(
            c,
            HYPER( sub (\obj) is nodal { obj."$meth-name"(|c) }, SELF ),
            HYPER( sub (\obj) is nodal { obj."$meth-name"()   }, SELF )),
          nqp::if(
            c,
            HYPER( -> \obj { obj."$meth-name"(|c) }, SELF ),
            HYPER( -> \obj { obj."$meth-name"(  ) }, SELF )))
    }

    proto method WALK(|) {*}  # is implementation-detail
    multi method WALK(:$name!, :$canonical, :$ascendant, :$descendant, :$preorder, :$breadth,
                :$super, :$omit, :$include, :$roles, :$submethods = True, :$methods = True
                --> WalkList)
    {
        # First, build list of classes in the order we'll need them.

        my sub maybe-with-roles(Mu \typeobj) {
            flat typeobj.^parents(:local),
                 ($roles ?? typeobj.^roles(:local, :transitive, :mro) !! ())
        }

        my @classes;
        if $super {
            @classes = maybe-with-roles(self)
        }
        elsif $breadth {
            my @search_list = self.WHAT;
            while @search_list {
                append @classes, @search_list;
                my @new_search_list;
                for @search_list -> $current {
                    for maybe-with-roles($current) -> $next {
                        unless @new_search_list.grep({ $^c.WHAT =:= $next.WHAT }) {
                            push @new_search_list, $next;
                        }
                    }
                }
                @search_list = @new_search_list;
            }
        } elsif $ascendant | $preorder {
            sub build_ascendent(Mu $class) {
                unless @classes.grep({ $^c.WHAT =:= $class.WHAT }) {
                    push @classes, $class;
                    for maybe-with-roles($class) {
                        build_ascendent($^parent);
                    }
                }
            }
            build_ascendent(self.WHAT);
        } elsif $descendant {
            sub build_descendent(Mu $class) {
                unless @classes.grep({ $^c.WHAT =:= $class.WHAT }) {
                    for maybe-with-roles($class) {
                        build_descendent($^parent);
                    }
                    push @classes, $class;
                }
            }
            build_descendent(self.WHAT);
        } else {
            # Canonical, the default (just whatever the meta-class says) with us
            # on the start.
            @classes = self.^mro(:$roles);
        }

        # Now we have classes, build method list.
        my @methods;
        for @classes -> $class {
            if (!defined($include) || $include.ACCEPTS($class)) &&
              (!defined($omit) || !$omit.ACCEPTS($class)) {
                if $methods && !$class.HOW.archetypes.composable {
                    @methods.push: $_ with $class.^method_table{$name}
                }
                if $submethods && nqp::can($class.HOW, 'submethod_table') {
                    @methods.push: $_ with $class.^submethod_table{$name}
                }
            }
        }

        WalkList.new(|@methods).set_invocant(self)
    }

    multi method WALK(Str:D $name, *%n --> WalkList ) {
        samewith(:$name, |%n)
    }
}


proto sub defined(Mu, *%) is pure {*}
multi sub defined(Mu \x) { x.defined }

proto sub infix:<~~>(Mu, Mu, *%) {*}
multi sub infix:<~~>(Mu \topic, Mu \matcher) {
    matcher.ACCEPTS(topic).Bool;
}

proto sub infix:<!~~>(Mu, Mu, *%) {*}
multi sub infix:<!~~>(Mu \topic, Mu \matcher) {
    matcher.ACCEPTS(topic).not;
}

proto sub infix:<=:=>(Mu $?, Mu $?, *%) is pure {*}
multi sub infix:<=:=>($?)      { Bool::True }
multi sub infix:<=:=>(Mu \a, Mu \b) {
    nqp::hllbool(nqp::eqaddr(a, b));
}

proto sub infix:<eqv>(Any $?, Any $?, *%) is pure {*}
multi sub infix:<eqv>($?)            { Bool::True }

# Last ditch snapshot semantics.  We shouldn't come here too often, so
# please do not change this to be faster but wronger.  (Instead, add
# specialized multis for datatypes that can be tested piecemeal.)
multi sub infix:<eqv>(Any:U \a, Any:U \b) {
    nqp::hllbool(nqp::eqaddr(nqp::decont(a),nqp::decont(b)))
}
multi sub infix:<eqv>(Any:D \a, Any:U \b --> False) { }
multi sub infix:<eqv>(Any:U \a, Any:D \b --> False) { }
multi sub infix:<eqv>(Any:D \a, Any:D \b) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
        || (nqp::eqaddr(a.WHAT,b.WHAT) && nqp::iseq_s(a.raku,b.raku))
    )
}

sub DUMP(|args (*@args, :$indent-step = 4, :%ctx?)) { # is implementation-detail
    my Mu $capture := nqp::usecapture();
    my Mu $topic   := nqp::captureposarg($capture, 0);

    return "\x25b6" ~ DUMP(nqp::decont($topic), :$indent-step, :%ctx)
        if nqp::iscont($topic);
    return '(null)' if nqp::isnull($topic);

    my str $type  = $topic.^name;
    my str $where = nqp::base_I(nqp::where($topic), 16);

    if %ctx{$where} -> $obj_num {
        nqp::istype($topic, Bool) ?? $topic.DUMP(:$indent-step, :%ctx)  !!
        nqp::isconcrete($topic)   ?? '=' ~ $type ~ '<' ~ $obj_num ~ '>' !!
        nqp::can($topic, 'DUMP')  ?? $topic.DUMP(:$indent-step, :%ctx)  !!
                                     $type;
    }
    else {
        my int $obj_num = %ctx.elems + 1;
        %ctx{$where} = $obj_num;

        if    nqp::islist($topic) {
            my str $id = $type ~ '<' ~ $obj_num ~ '>';

            my @pieces;
            $topic := nqp::clone($topic);
            while $topic {
                my Mu $x := nqp::shift($topic);
                @pieces.push: DUMP($x, :$indent-step, :%ctx);
            }

            @pieces.DUMP-PIECES($id ~ '(', :$indent-step);
        }
        elsif nqp::ishash($topic) {
            my str $id = $type ~ '<' ~ $obj_num ~ '>';

            my @pieces;
            {
                for $topic.pairs {
                    @pieces.push: $_.key ~ ' => ' ~ DUMP($_.value, :$indent-step, :%ctx);
                }
                CATCH { default { @pieces.push: '...' } }
            }

            @pieces.DUMP-PIECES($id ~ '(', :$indent-step);
        }
        elsif nqp::can($topic, 'DUMP') {
            $topic.DUMP(:$indent-step, :%ctx);
        }
        else {
            given nqp::p6box_i(nqp::captureposprimspec($capture, 0)) {
                when 0 { $type ~ '<' ~ $obj_num ~ '>(...)' }
                when 1 { nqp::captureposarg_i($capture, 0).DUMP(:$indent-step, :%ctx) }
                when 2 { nqp::captureposarg_n($capture, 0).DUMP(:$indent-step, :%ctx) }
                when 3 { nqp::captureposarg_s($capture, 0).DUMP(:$indent-step, :%ctx) }
            }
        }
    }
}

# These must collapse Junctions
proto sub so(Mu, *%) {*}
multi sub so(Bool:U --> False) { }
multi sub so(Bool:D \x) { x }
multi sub so(Mu \x) { nqp::hllbool(nqp::istrue(x)) }

proto sub not(Mu, *%) {*}
multi sub not(Bool:U --> True) { }
multi sub not(Mu \x) { nqp::hllbool(nqp::isfalse(x)) }

# vim: expandtab shiftwidth=4
