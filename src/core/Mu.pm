my class X::Constructor::Positional  { ... }
my class X::Method::NotFound         { ... }
my class X::Method::InvalidQualifier { ... }
my class X::Attribute::Required      { ... }

# We use a sentinel value to mark the end of an iteration.
my constant IterationEnd = nqp::create(Mu);

my class Mu { # declared in BOOTSTRAP

    method self { self }

    method sink(--> Nil) { }

    proto method ACCEPTS(|) { * }
    multi method ACCEPTS(Mu:U: Any \topic) {
        nqp::p6bool(nqp::istype(topic, self))
    }
    multi method ACCEPTS(Mu:U: Mu:U \topic) {
        nqp::p6bool(nqp::istype(topic, self))
    }

    method WHERE() {
        nqp::p6box_i(nqp::where(self))
    }

    proto method WHICH(|) {*}
    multi method WHICH(Mu:U:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|U'),
                nqp::objectid(self)
            ),
            ObjAt
        )
    }
    multi method WHICH(Mu:D:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                nqp::objectid(self)
            ),
            ObjAt
        )
    }

    proto method split(|) { * }

    method emit {
        emit self;
    }
    method take {
        take self;
    }
    method return-rw(|) {  # same code as control.pm's return-rw
        my $list := RETURN-LIST(nqp::p6argvmarray());
        nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $list);
        $list;
    }
    method return(|) {  # same code as control.pm's return
        my $list := RETURN-LIST(nqp::p6argvmarray());
        nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, nqp::p6recont_ro($list));
        $list;
    }

    proto method WHY(|) { * }
    multi method WHY(Mu:) {
        my Mu $why;

        if nqp::can(self.HOW, 'WHY') {
            $why := self.HOW.WHY;
        }

        if $why.defined && !$.defined #`(ie. we're a type object) {
            $why.set_docee(self);
        }
        $why // Any
    }

    method set_why($why) {
        self.HOW.set_why($why);
    }

    proto method Bool() {*}
    multi method Bool(Mu:U: --> False) { }
    multi method Bool(Mu:D:) { self.defined }

    method so()  { self.Bool }
    method not() { self ?? False !! True }

    method defined() {
        nqp::p6bool(nqp::isconcrete(self))
    }

    proto method new(|) { * }
    multi method new(*%) {
        nqp::invokewithcapture(nqp::findmethod(self, 'bless'), nqp::usecapture())
    }
    multi method new($, *@) {
        X::Constructor::Positional.new(:type( self )).throw();
    }

    proto method is-lazy (|) { * }
    multi method is-lazy(Mu:) { False }

    method CREATE() {
        nqp::create(self)
    }

    method bless(*%attrinit) {
        nqp::create(self).BUILDALL(%attrinit);
    }

    proto method BUILDALL(|) { * }

    # This candidate provided for those modules that rely on the old
    # BUILDALL interface, such as Inline::Perl5
    multi method BUILDALL(@positional,%attrinit) {
        self.BUILDALL(%attrinit)
    }

    multi method BUILDALL(%attrinit) {
        my $init := nqp::getattr(%attrinit,Map,'$!storage');
        # Get the build plan. Note that we do this "low level" to
        # avoid the NQP type getting mapped to a Rakudo one, which
        # would get expensive.
        my $build_plan :=
          nqp::findmethod(self.HOW,'BUILDALLPLAN')(self.HOW, self);
        my int $count = nqp::elems($build_plan);
        my int $i     = -1;
        my $task;
        my $build;
        my int $code;
        my int $int;
        my num $num;
        my str $str;

        nqp::while(
          nqp::islt_i($i = nqp::add_i($i,1),$count),

          nqp::if( # 0     # Custom BUILD call.
            nqp::iseq_i(($code = nqp::atpos(
              ($task := nqp::atpos($build_plan,$i)),0
            )),0),
            nqp::if(
              nqp::istype(
                ($build := nqp::atpos($task,1)(self,|%attrinit)),Failure),
              return $build
            ),

            nqp::if( # 1
              nqp::iseq_i($code,1),
              nqp::if(
                nqp::existskey($init,nqp::atpos($task,2)),
                (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                  = %attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2))))
              ),

              nqp::if( # 2
                nqp::iseq_i($code,2),
                nqp::if(
                  nqp::existskey($init,nqp::atpos($task,2)),
                  (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                    = %attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))),
                  nqp::bindattr(self,nqp::atpos($task,1),nqp::atpos($task,3),
                    nqp::list)
                ),

                nqp::if( # 3
                  nqp::iseq_i($code,3),
                  nqp::if(
                    nqp::existskey($init,nqp::atpos($task,2)),
                    (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                       = %attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))),
                    nqp::bindattr(self,nqp::atpos($task,1),nqp::atpos($task,3),
                       nqp::hash)
                  ),

                  nqp::if( # 4
                    nqp::iseq_i($code,4),
                    nqp::unless(
                      nqp::attrinited(self,
                        nqp::atpos($task,1),
                        nqp::atpos($task,2)
                      ),
                      nqp::stmts(
                        (my \attr := nqp::getattr(self,
                          nqp::atpos($task,1),
                          nqp::atpos($task,2)
                        )),
                        (attr = nqp::atpos($task,3)(self,attr))
                      )
                    ),

                    nqp::if( # 5
                      nqp::iseq_i($code,5),
                      nqp::if(
                        nqp::existskey($init,nqp::atpos($task,2)),
                        nqp::bindattr_i(self,
                          nqp::atpos($task,1),
                          nqp::atpos($task,3),
                          nqp::decont(%attrinit.AT-KEY(
                            nqp::p6box_s(nqp::atpos($task,2))
                          ))
                        )
                      ),

                      nqp::if( # 6
                        nqp::iseq_i($code,6),
                        nqp::if(
                          nqp::existskey($init,nqp::atpos($task,2)),
                          nqp::bindattr_n(self,
                            nqp::atpos($task,1),
                            nqp::atpos($task,3),
                            nqp::decont(%attrinit.AT-KEY(
                              nqp::p6box_s(nqp::atpos($task,2))
                            ))
                          )
                        ),

                        nqp::if( # 7
                          nqp::iseq_i($code,7),
                          nqp::if(
                            nqp::existskey($init,nqp::atpos($task,2)),
                            nqp::bindattr_s(self,
                              nqp::atpos($task,1),
                              nqp::atpos($task,3),
                              nqp::decont(%attrinit.AT-KEY(
                                nqp::p6box_s(nqp::atpos($task,2))
                              ))
                            )
                          ),

                          nqp::if( # 8
                            nqp::iseq_i($code,8),
                            nqp::if(
                              nqp::iseq_i($int = nqp::getattr_i(self,
                                nqp::atpos($task,1),
                                nqp::atpos($task,2)
                              ), 0),
                              nqp::bindattr_i(self,
                                nqp::atpos($task,1),
                                nqp::atpos($task,2),
                                (nqp::atpos($task,3)(self,$int))
                              )
                            ),

                            nqp::if( # 9
                              nqp::iseq_i($code,9),
                              nqp::if(
                                nqp::iseq_n($num = nqp::getattr_n(self,
                                  nqp::atpos($task,1),
                                  nqp::atpos($task,2)
                                ), 0e0),
                                nqp::bindattr_n(self,
                                  nqp::atpos($task,1),
                                  nqp::atpos($task,2),
                                  (nqp::atpos($task,3)(self,$num))
                                )
                              ),

                              nqp::if( # 10
                                nqp::iseq_i($code,10),
                                nqp::if(
                                  nqp::isnull_s($str = nqp::getattr_s(self,
                                    nqp::atpos($task,1),
                                    nqp::atpos($task,2)
                                  )),
                                  nqp::bindattr_s(self,
                                    nqp::atpos($task,1),
                                    nqp::atpos($task,2),
                                    (nqp::atpos($task,3)(self,$str))
                                  )
                                ),

                              nqp::if( # 11
                                nqp::iseq_i($code,11),
                                nqp::unless(
                                  nqp::attrinited(self,
                                    nqp::atpos($task,1),
                                    nqp::atpos($task,2)
                                  ),
                                  X::Attribute::Required.new(
                                    name => nqp::atpos($task,2),
                                    why  => nqp::atpos($task,3)
                                  ).throw
                                ),

                                nqp::if( # 12
                                  nqp::iseq_i($code,12),
                                  nqp::bindattr(self,
                                    nqp::atpos($task,1),
                                    nqp::atpos($task,2),
                                    (nqp::atpos($task,3)())
                                  ),

                                  nqp::if( # 13
                                    nqp::isne_i($code,13),  # no-op
                                    die("Invalid BUILDALL plan")
                                  )
        ))))))))))))));
        self
    }

    method BUILD_LEAST_DERIVED(%attrinit) {
        my $init := nqp::getattr(%attrinit,Map,'$!storage');
        # Get the build plan for just this class.
        my $build_plan := nqp::findmethod(self.HOW,'BUILDPLAN')(self.HOW,self);
        my int $count = nqp::elems($build_plan);
        my int $i     = -1;
        my $task;
        my $build;
        my int $code;

        nqp::while(
          nqp::islt_i($i = nqp::add_i($i,1),$count),

          nqp::if( # 0     # Custom BUILD call.
            nqp::iseq_i(($code = nqp::atpos(
              ($task := nqp::atpos($build_plan,$i)),0
            )),0),
            nqp::if(
              nqp::istype(
                ($build := nqp::atpos($task,1)(self,|%attrinit)),Failure),
              return $build
            ),

            nqp::if( # 1
              nqp::iseq_i($code,1),
              nqp::if(
                nqp::existskey($init,nqp::atpos($task,2)),
                (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                  = nqp::decont(
                    %attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))
                  )
                )
              ),

              nqp::if( # 2
                nqp::iseq_i($code,2),
                nqp::if(
                  nqp::existskey($init,nqp::atpos($task,2)),
                  (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                    = nqp::decont(
                      %attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))
                    )
                  ),
                  nqp::bindattr(self,nqp::atpos($task,1),nqp::atpos($task,3),
                    nqp::list)
                ),

                nqp::if( # 3
                  nqp::iseq_i($code,3),
                  nqp::if(
                    nqp::existskey($init,nqp::atpos($task,2)),
                    (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                       = nqp::decont(
                         %attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))
                       )
                    ),
                    nqp::bindattr(self,nqp::atpos($task,1),nqp::atpos($task,3),
                       nqp::hash)
                  ),

                  nqp::if( # 4
                    nqp::iseq_i($code,4),
                    nqp::unless(
                      nqp::attrinited(self,
                        nqp::atpos($task,1),
                        nqp::atpos($task,2)
                      ),
                      nqp::stmts(
                        (my \attr := nqp::getattr(self,
                          nqp::atpos($task,1),
                          nqp::atpos($task,2)
                        )),
                        (attr = nqp::atpos($task,3)(self,attr))
                      )
                    ),

                    nqp::if( # 5
                      nqp::iseq_i($code,5),
                      nqp::if(
                        nqp::existskey($init,nqp::atpos($task,2)),
                        nqp::bindattr_i(self,
                          nqp::atpos($task,1),
                          nqp::atpos($task,3),
                          nqp::decont(%attrinit.AT-KEY(
                            nqp::p6box_s(nqp::atpos($task,2))
                          ))
                        )
                      ),

                      nqp::if( # 6
                        nqp::iseq_i($code,6),
                        nqp::if(
                          nqp::existskey($init,nqp::atpos($task,2)),
                          nqp::bindattr_n(self,
                            nqp::atpos($task,1),
                            nqp::atpos($task,3),
                            nqp::decont(%attrinit.AT-KEY(
                              nqp::p6box_s(nqp::atpos($task,2))
                            ))
                          )
                        ),

                        nqp::if( # 7
                          nqp::iseq_i($code,7),
                          nqp::if(
                            nqp::existskey($init,nqp::atpos($task,2)),
                            nqp::bindattr_s(self,
                              nqp::atpos($task,1),
                              nqp::atpos($task,3),
                              nqp::decont(%attrinit.AT-KEY(
                                nqp::p6box_s(nqp::atpos($task,2))
                              ))
                            )
                          ),

                          nqp::if( # 13
                            nqp::iseq_i($code,13),
                            nqp::getattr(self,  # not sure why this is needed
                              nqp::atpos($task,1),
                              nqp::atpos($task,2)
                            ),
                            die("Invalid BUILD_LEAST_DERIVED plan")
                          )
        )))))))));
        self
    }

    proto method Numeric(|) { * }
    multi method Numeric(Mu:U \v:) {
        warn "Use of uninitialized value of type {self.^name} in numeric context";
        0
    }
    proto method Real(|) { * }
    multi method Real(Mu:U \v:) {
        warn "Use of uninitialized value of type {self.^name} in numeric context";
        0
    }

    proto method Str(|) { * }
    multi method Str(Mu:U \v:) {
        my $name = (defined($*VAR_NAME) ?? $*VAR_NAME !! try v.VAR.?name) // '';
        $name   ~= ' ' if $name ne '';
        warn "Use of uninitialized value {$name}of type {self.^name} in string"
                ~ " context.\nMethods .^name, .perl, .gist, or .say can be"
                ~ " used to stringify it to something meaningful.";
        ''
    }
    multi method Str(Mu:D:) {
        nqp::if(
          nqp::eqaddr(self,IterationEnd),
          "IterationEnd",
          self.^name ~ '<' ~ nqp::tostr_I(nqp::objectid(self)) ~ '>'
        )
    }

    proto method Stringy(|) { * }
    multi method Stringy(Mu:U \v:) {
        my $*VAR_NAME = try v.VAR.?name;
        self.Str
    }
    multi method Stringy(Mu:D $:) { self.Str }

    method item(Mu \item:) is raw { item }

    proto method say(|) { * }
    multi method say() { say(self) }
    method print() { print(self) }
    method put() { put(self) }
    method note() { note(self) }

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

    proto method gist(|) { * }
    multi method gist(Mu:U:) { '(' ~ self.^shortname ~ ')' }
    multi method gist(Mu:D:) { self.perl }

    method perlseen(Mu:D \SELF: $id, $perl, *%named) {
        if nqp::not_i(nqp::isnull(nqp::getlexdyn('$*perlseen'))) {
            my \sems := $*perlseen;
            my str $WHICH = nqp::unbox_s(self.WHICH);
            if nqp::existskey(sems,$WHICH) && nqp::atkey(sems,$WHICH) {
                nqp::bindkey(sems,$WHICH,2);
                "{$id}_{nqp::objectid(SELF)}";
            }
            else {
                nqp::bindkey(sems,$WHICH,1);
                my $result := $perl(|%named);
                my int $value = nqp::atkey(sems,$WHICH);
                nqp::deletekey(sems,$WHICH);
                $value == 2
                  ?? "(my \\{$id}_{nqp::objectid(SELF)} = $result)"
                  !! $result
            }
        }
        else {
            my $*perlseen := nqp::hash("TOP",1);
            SELF.perlseen($id,$perl,|%named)
        }
    }

    proto method perl(|) { * }
    multi method perl(Mu:U:) { self.^name }
    multi method perl(Mu:D:) {
        nqp::if(
          nqp::eqaddr(self,IterationEnd),
          "IterationEnd",
          self.perlseen(self.^name, {
              my @attrs;
              for self.^attributes().flat.grep: { .has_accessor } -> $attr {
                  my $name := substr($attr.Str,2);
                  @attrs.push: $name ~ ' => ' ~ $attr.get_value(self).perl
              }
              self.^name ~ '.new' ~ ('(' ~ @attrs.join(', ') ~ ')' if @attrs)
          })
        )
    }

    proto method DUMP(|) { * }
    multi method DUMP(Mu:U:) { self.perl }
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
                my Mu $decont  := nqp::decont(self);
                my Mu $package := $attr.package;

                $value := do given nqp::p6box_i(nqp::objprimspec($attr.type)) {
                    when 0 {              nqp::getattr(  $decont, $package, $name)  }
                    when 1 { nqp::p6box_i(nqp::getattr_i($decont, $package, $name)) }
                    when 2 { nqp::p6box_n(nqp::getattr_n($decont, $package, $name)) }
                    when 3 { nqp::p6box_s(nqp::getattr_s($decont, $package, $name)) }
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
    method DUMP-PIECES(@pieces: $before, $after = ')', :$indent = @pieces > 1, :$indent-step) {
        $indent ?? $before ~ "\n" ~ @pieces.join(",\n").indent($indent-step) ~ "\n" ~ $after
                !! $before ~        @pieces.join(', ')                              ~ $after;
    }
    method DUMP-OBJECT-ATTRS(|args (*@args, :$indent-step, :%ctx, :$flags?)) {
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

    proto method isa(|) { * }
    multi method isa(Mu \SELF: Mu $type) {
        nqp::p6bool(SELF.^isa($type.WHAT))
    }
    multi method isa(Mu \SELF: Str:D $name) {
        my @mro = SELF.^mro;
        my int $mro_count = @mro.elems;
        my int $i = -1;

        return True
          if @mro[$i].^name eq $name
          while nqp::islt_i(++$i,$mro_count);

        False
    }

    method does(Mu \SELF: Mu $type) {
        nqp::p6bool(nqp::istype(SELF, $type.WHAT))
    }

    method can(Mu \SELF: $name) {
        SELF.^can($name)
    }

    method clone(*%twiddles) {
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

    # XXX TODO: Handle positional case.
    method dispatch:<var>(Mu \SELF: $var, |c) is raw {
        $var(SELF, |c)
    }

    method dispatch:<::>(Mu \SELF: $name, Mu $type, |c) is raw {
        unless nqp::istype(SELF, $type) {
            X::Method::InvalidQualifier.new(
                    method          => $name,
                    invocant        => SELF,
                    qualifier-type  => $type,

            ).throw;
        }
        self.^find_method_qualified($type, $name)(SELF, |c)
    }

    method dispatch:<!>(Mu \SELF: \name, Mu \type, |c) is raw {
        my $meth := type.^find_private_method(name);
        $meth ??
            $meth(SELF, |c) !!
            X::Method::NotFound.new(
              invocant => SELF,
              method   => '!' ~ name,
              typename => type.^name,
              :private,
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

    method dispatch:<.+>(Mu \SELF: $name, |c) {
        my @result := SELF.dispatch:<.*>($name, |c);
        if @result.elems == 0 {
            X::Method::NotFound.new(
              invocant => SELF,
              method   => $name,
              typename => SELF.^name,
            ).throw;
        }
        @result
    }

    method dispatch:<.*>(Mu \SELF: \name, |c) {
        my @mro = SELF.^mro;
        my int $mro_count = @mro.elems;
        my $results := nqp::list;
        my int $i = -1;
        while nqp::islt_i(++$i,$mro_count) {
            my $obj = @mro[$i];
            my $meth = ($obj.^method_table){name};
            $meth = ($obj.^submethod_table){name} if !$meth && $i == 0;
            nqp::push($results,$meth(SELF, |c))    if $meth;
        }
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$results)
    }

    method dispatch:<hyper>(Mu \SELF: Str() $name, |c) {
        nqp::if(
          nqp::can(List,$name) && nqp::can(List.can($name).AT-POS(0),"nodal"),
          nqp::if(
            c,
            HYPER( sub (\obj) is nodal { obj."$name"(|c) }, SELF ),
            HYPER( sub (\obj) is nodal { obj."$name"() }, SELF )
          ),
          nqp::if(
            c,
            HYPER( -> \obj { obj."$name"(|c) }, SELF ),
            HYPER( -> \obj { obj."$name"() }, SELF )
          )
        )
    }

    method WALK(:$name!, :$canonical, :$ascendant, :$descendant, :$preorder, :$breadth,
                :$super, :$omit, :$include) {
        # First, build list of classes in the order we'll need them.
        my @classes;
        if $super {
            @classes = self.^parents(:local);
        }
        elsif $breadth {
            my @search_list = self.WHAT;
            while @search_list {
                append @classes, @search_list;
                my @new_search_list;
                for @search_list -> $current {
                    for flat $current.^parents(:local) -> $next {
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
                    for flat $class.^parents(:local) {
                        build_ascendent($^parent);
                    }
                }
            }
            build_ascendent(self.WHAT);
        } elsif $descendant {
            sub build_descendent(Mu $class) {
                unless @classes.grep({ $^c.WHAT =:= $class.WHAT }) {
                    for flat $class.^parents(:local) {
                        build_descendent($^parent);
                    }
                    push @classes, $class;
                }
            }
            build_descendent(self.WHAT);
        } else {
            # Canonical, the default (just whatever the meta-class says) with us
            # on the start.
            @classes = self.^mro();
        }

        # Now we have classes, build method list.
        my @methods;
        for @classes -> $class {
            if (!defined($include) || $include.ACCEPTS($class)) &&
              (!defined($omit) || !$omit.ACCEPTS($class)) {
                try {
                    for flat $class.^methods(:local) -> $method {
                        my $check_name = $method.?name;
                        if $check_name.defined && $check_name eq $name {
                            @methods.push($method);
                        }
                    }
                    0;
                }
            }
        }

        @methods;
    }
}


proto sub defined(Mu) is pure { * }
multi sub defined(Mu \x) { x.defined }

proto sub infix:<~~>(Mu \topic, Mu \matcher) { * }
multi sub infix:<~~>(Mu \topic, Mu \matcher) {
    matcher.ACCEPTS(topic).Bool;
}

proto sub infix:<!~~>(Mu \topic, Mu \matcher) { * }
multi sub infix:<!~~>(Mu \topic, Mu \matcher) {
    matcher.ACCEPTS(topic).not;
}

proto sub infix:<=:=>(Mu $?, Mu $?) is pure { * }
multi sub infix:<=:=>($?)      { Bool::True }
multi sub infix:<=:=>(Mu \a, Mu \b) {
    nqp::p6bool(nqp::eqaddr(a, b));
}

proto sub infix:<eqv>(Any $?, Any $?) is pure { * }
multi sub infix:<eqv>($?)            { Bool::True }

# Last ditch snapshot semantics.  We shouldn't come here too often, so
# please do not change this to be faster but wronger.  (Instead, add
# specialized multis for datatypes that can be tested piecemeal.)
multi sub infix:<eqv>(Any:U \a, Any:U \b) {
    nqp::p6bool(nqp::eqaddr(nqp::decont(a),nqp::decont(b)))
}
multi sub infix:<eqv>(Any:D \a, Any:U \b) { False }
multi sub infix:<eqv>(Any:U \a, Any:D \b) { False }
multi sub infix:<eqv>(Any:D \a, Any:D \b) {
    nqp::p6bool(
      nqp::eqaddr(a,b)
        || (nqp::eqaddr(a.WHAT,b.WHAT) && nqp::iseq_s(a.perl,b.perl))
    )
}

multi sub infix:<eqv>(@a, @b) {
    nqp::p6bool(
      nqp::unless(
        nqp::eqaddr(@a,@b),                                    # identity
        nqp::if(
          nqp::eqaddr(@a.WHAT,@b.WHAT),                        # same type
          nqp::if(
            nqp::iseq_i((my int $elems = @a.elems),@b.elems),  # same # elems
            nqp::stmts(
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems)    # not exhausted
                  && @a.AT-POS($i) eqv @b.AT-POS($i),          # still same
                nqp::null
              ),
              nqp::iseq_i($i,$elems)                      # exhausted = success!
            )
          )
        )
      )
    )
}

sub DUMP(|args (*@args, :$indent-step = 4, :%ctx?)) {
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

# U+2212 minus (forward call to regular minus)
proto sub  infix:<−>(|)  is pure { * }
multi sub  infix:<−>(|c)         {  infix:<->(|c) }
proto sub prefix:<−>(|)  is pure { * }
multi sub prefix:<−>(|c)         { prefix:<->(|c) }

# These must collapse Junctions
proto sub so(Mu $) {*}
multi sub so(Mu $x)  { ?$x }
proto sub not(Mu $) {*}
multi sub not(Mu $x) { !$x }

Metamodel::ClassHOW.exclude_parent(Mu);

# vim: ft=perl6 expandtab sw=4
