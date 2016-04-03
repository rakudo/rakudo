my class X::Constructor::Positional  { ... }
my class X::Method::NotFound         { ... }
my class X::Method::InvalidQualifier { ... }
my class X::Attribute::Required      { ... }

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
        nqp::box_s(nqp::unbox_s(self.^name), ObjAt);
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

    method take {
        take self;
    }
    method return-rw(|) {  # same code as control.pm's return-rw
        my $list := RETURN-LIST(nqp::p6argvmarray());
        nqp::p6routinereturn($list);
        $list;
    }
    method return(|) {  # same code as control.pm's return
        my $list := RETURN-LIST(nqp::p6argvmarray());
        nqp::p6routinereturn(nqp::p6recont_ro($list));
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

    method bless(*@autovivs, *%attrinit) {
        nqp::create(self).BUILDALL(@autovivs, %attrinit);
    }

    method BUILDALL(@autovivs, %attrinit) {
        my $init := nqp::getattr(%attrinit,Map,'$!storage');
        # Get the build plan. Note that we do this "low level" to
        # avoid the NQP type getting mapped to a Rakudo one, which
        # would get expensive.
        my $build_plan := nqp::findmethod(self.HOW, 'BUILDALLPLAN')(self.HOW, self);
        my int $count   = nqp::elems($build_plan);
        my int $i       = -1;
        while nqp::islt_i($i = nqp::add_i($i,1), $count) {
            my $task := nqp::atpos($build_plan, $i);
            my int $code = nqp::atpos($task, 0);
            if nqp::iseq_i($code, 0) {
                # Custom BUILD call.
                nqp::atpos($task, 1)(self, |%attrinit);
            }
            elsif nqp::iseq_i($code, 1) {
                # See if we have a value to initialize this attr with.
                nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                  = %attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))
                  if nqp::existskey($init,nqp::atpos($task,2));
            }
            elsif nqp::iseq_i($code, 2) {
                nqp::existskey($init,nqp::atpos($task,2))
                  ?? (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                       = %attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2))))
                  !! nqp::bindattr(self,nqp::atpos($task,1),nqp::atpos($task,3),
                       nqp::list);
            }
            elsif nqp::iseq_i($code, 3) {
                nqp::existskey($init,nqp::atpos($task,2))
                  ?? (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                       = %attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2))))
                  !! nqp::bindattr(self,nqp::atpos($task,1),nqp::atpos($task,3),
                       nqp::hash);
            }
            elsif nqp::iseq_i($code, 4) {
                unless nqp::attrinited(self, nqp::atpos($task, 1), nqp::atpos($task, 2)) {
                    my \attr := nqp::getattr(self, nqp::atpos($task, 1), nqp::atpos($task, 2));
                    attr = nqp::atpos($task, 3)(self, attr);
                }
            }
            elsif nqp::iseq_i($code, 5) {
                nqp::bindattr_i(self,nqp::atpos($task,1),nqp::atpos($task,3),
                  nqp::decont(%attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))))
                  if nqp::existskey($init,nqp::atpos($task,2));
            }
            elsif nqp::iseq_i($code, 6) {
                my $something;  # avoid issue with JIT
                nqp::bindattr_n(self,nqp::atpos($task,1),nqp::atpos($task,3),
                  nqp::decont(%attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))))
                  if nqp::existskey($init,nqp::atpos($task,2));
            }
            elsif nqp::iseq_i($code, 7) {
                nqp::bindattr_s(self,nqp::atpos($task,1),nqp::atpos($task,3),
                  nqp::decont(%attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))))
                  if nqp::existskey($init,nqp::atpos($task,2));
            }
            elsif nqp::iseq_i($code, 8) {
                my int $cur_value = nqp::getattr_i(self, nqp::atpos($task, 1), nqp::atpos($task, 2));
                nqp::bindattr_i(self,nqp::atpos($task,1),nqp::atpos($task,2),
                  nqp::atpos($task, 3)(self, $cur_value))
                  if nqp::iseq_i($cur_value, 0);
            }
            elsif nqp::iseq_i($code, 9) {
                my num $cur_value = nqp::getattr_n(self, nqp::atpos($task, 1), nqp::atpos($task, 2));
                nqp::bindattr_n(self,nqp::atpos($task,1),nqp::atpos($task,2),
                  nqp::atpos($task,3)(self, $cur_value))
                  if nqp::iseq_n($cur_value, 0e0);
            }
            elsif nqp::iseq_i($code, 10) {
                my str $cur_value = nqp::getattr_s(self, nqp::atpos($task, 1), nqp::atpos($task, 2));
                nqp::bindattr_s(self,nqp::atpos($task,1),nqp::atpos($task,2),
                  nqp::atpos($task, 3)(self, $cur_value))
                  if nqp::isnull_s($cur_value);
            }
            elsif nqp::iseq_i($code, 11) {
                my str $attr_name = nqp::atpos($task, 2);
                X::Attribute::Required.new(name => $attr_name).throw
                  unless nqp::attrinited(self,nqp::atpos($task,1),$attr_name);
            }
            elsif nqp::iseq_i($code, 12) {
                nqp::bindattr(self, nqp::atpos($task, 1), nqp::atpos($task, 2),
                    nqp::atpos($task, 3)());
            }
            elsif nqp::isne_i($code, 13) { # no-op in BUILDALL
                die "Invalid BUILDALLPLAN";
            }
        }
        self
    }

    method BUILD_LEAST_DERIVED(%attrinit) {
        my $init := nqp::getattr(%attrinit,Map,'$!storage');
        # Get the build plan for just this class.
        my $build_plan := nqp::findmethod(self.HOW, 'BUILDPLAN')(self.HOW, self);
        my int $count   = nqp::elems($build_plan);
        my int $i       = -1;
        while nqp::islt_i($i = nqp::add_i($i,1), $count) {
            my $task := nqp::atpos($build_plan, $i);
            my int $code = nqp::atpos($task, 0);
            if nqp::iseq_i($code, 0) {
                # Custom BUILD call.
                nqp::atpos($task, 1)(self, |%attrinit);
            }
            elsif nqp::iseq_i($code, 1) {
                # See if we have a value to initialize this attr with.
                nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                  = nqp::decont(%attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2))))
                  if nqp::existskey($init,nqp::atpos($task,2));
            }
            elsif nqp::iseq_i($code, 2) {
                nqp::existskey($init,nqp::atpos($task,2))
                  ?? (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                       = nqp::decont(%attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))))
                  !! nqp::bindattr(self,nqp::atpos($task,1),nqp::atpos($task,3),
                       nqp::list);
            }
            elsif nqp::iseq_i($code, 3) {
                nqp::existskey($init,nqp::atpos($task,2))
                  ?? (nqp::getattr(self,nqp::atpos($task,1),nqp::atpos($task,3))
                       = nqp::decont(%attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))))
                  !! nqp::bindattr(self,nqp::atpos($task,1),nqp::atpos($task,3),
                       nqp::hash);
            }
            elsif nqp::iseq_i($code, 4) {
                unless nqp::attrinited(self, nqp::atpos($task, 1), nqp::atpos($task, 2)) {
                    my \attr := nqp::getattr(self, nqp::atpos($task, 1), nqp::atpos($task, 2));
                    attr = nqp::atpos($task, 3)(self, attr);
                }
            }
            elsif nqp::iseq_i($code, 5) {
                nqp::bindattr_i(self,nqp::atpos($task,1),nqp::atpos($task,3),
                  nqp::decont(%attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))))
                  if nqp::existskey($init,nqp::atpos($task,2));
            }
            elsif nqp::iseq_i($code, 6) {
                nqp::bindattr_n(self,nqp::atpos($task,1),nqp::atpos($task,3),
                  nqp::decont(%attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))))
                  if nqp::existskey($init,nqp::atpos($task,2));
            }
            elsif nqp::iseq_i($code, 7) {
                nqp::bindattr_s(self,nqp::atpos($task,1),nqp::atpos($task,3),
                  nqp::decont(%attrinit.AT-KEY(nqp::p6box_s(nqp::atpos($task,2)))))
                  if nqp::existskey($init,nqp::atpos($task,2));
            }
            elsif nqp::iseq_i($code, 13) {
                nqp::getattr(self, nqp::atpos($task, 1), nqp::atpos($task, 2));
            }
            else {
                die "Invalid BUILD_LEAST_DERIVED";
            }
        }
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
        warn "Use of uninitialized value {$name}of type {self.^name} in string context\n" ~
             "Any of .^name, .perl, .gist, or .say can stringify undefined things, if needed.";
        ''
    }
    multi method Str(Mu:D:) {
        self.^name ~ '<' ~ nqp::tostr_I(self.WHERE) ~ '>'
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
        if nqp::isnull(nqp::getlexdyn('$*gistseen')) == 0 {
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
        if nqp::isnull(nqp::getlexdyn('$*perlseen')) == 0 {
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
        self.perlseen(self.^name, {
            my @attrs;
            for self.^attributes().flat.grep: { .has_accessor } -> $attr {
                my $name := substr($attr.Str,2);
                @attrs.push: $name ~ ' => ' ~ $attr.get_value(self).perl
            }
            self.^name ~ '.new' ~ ('(' ~ @attrs.join(', ') ~ ')' if @attrs)
        })
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
        my int $mro_count = +@mro;
        my int $i = 0;
        while $i < $mro_count {
            my $obj = @mro[$i];
            if $obj.^name eq $name {
                return Bool::True;
            }
            $i = $i + 1;
        }
        Bool::False
    }

    method does(Mu \SELF: Mu $type) {
        nqp::p6bool(nqp::istype(SELF, $type.WHAT))
    }

    method can(Mu \SELF: $name) {
        SELF.^can($name)
    }

    method clone(*%twiddles) {
        my $cloned := nqp::clone(nqp::decont(self));
        if %twiddles.elems {
            for self.^attributes.flat -> $attr {
                my $name    := $attr.name;
                my $package := $attr.package;
#?if jvm
                unless nqp::objprimspec($attr.type) {
                    my $attr_val = nqp::getattr($cloned, $package, $name);
                    nqp::bindattr($cloned, $package, $name, nqp::clone($attr_val.VAR));
                }
#?endif
#?if !jvm
                nqp::bindattr($cloned, $package, $name,
                  nqp::clone(nqp::getattr($cloned, $package, $name).VAR)
                ) unless nqp::objprimspec($attr.type);
#?endif

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
                    my $attr_val := nqp::getattr($cloned, $package, $name);
                    nqp::bindattr($cloned,
                      $package, $name, nqp::clone($attr_val.VAR))
                        if nqp::iscont($attr_val);
                }
            }
        }
        $cloned
    }

    method Capture() {
        my %attrs;
        for self.^attributes.flat -> $attr {
            if $attr.has_accessor {
                my $name = substr($attr.name,2);
                unless %attrs.EXISTS-KEY($name) {
                    %attrs{$name} = self."$name"();
                }
            }
        }
        %attrs.Capture
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

    method dispatch:<!>(Mu \SELF: $name, Mu $type, |c) is raw {
        my $meth := $type.^find_private_method($name);
        $meth ??
            $meth(SELF, |c) !!
            X::Method::NotFound.new(
              invocant => SELF,
              method   => '!' ~ $name,
              typename => $type.^name,
              :private,
            ).throw;
    }

    method dispatch:<.^>(Mu \SELF: $name, |c) is raw {
        self.HOW."$name"(SELF, |c)
    }

    method dispatch:<.=>(\mutate: $name, |c) is raw {
        $/ := nqp::getlexcaller('$/');
        mutate = mutate."$name"(|c)
    }

    method dispatch:<.?>(Mu \SELF: $name, |c) is raw {
        nqp::can(SELF, $name) ??
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

    method dispatch:<.*>(Mu \SELF: $name, |c) {
        my @mro = SELF.^mro;
        my int $mro_count = +@mro;
        my @results;
        my int $i = 0;
        while $i < $mro_count {
            my $obj = @mro[$i];
            my $meth = ($obj.^method_table){$name};
            if !$meth && $i == 0 {
                $meth = ($obj.^submethod_table){$name};
            }
            if $meth {
                @results.push($meth(SELF, |c));
            }
            $i = $i + 1;
        }
        my $list := nqp::create(List);
        nqp::bindattr($list, List, '$!reified', nqp::getattr(@results, List, '$!reified'));
        $list
    }

    method dispatch:<hyper>(Mu \SELF: $name, |c) {
        my $listcan = List.can($name);
        if $listcan and $listcan[0].?nodal {
            c
                ?? HYPER( sub (\obj) is nodal { obj."$name"(|c) }, SELF )
                !! HYPER( sub (\obj) is nodal { obj."$name"() }, SELF )
        }
        else {
            c
                ?? HYPER( -> \obj { obj."$name"(|c) }, SELF )
                !! HYPER( -> \obj { obj."$name"() }, SELF )
        }
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
multi sub infix:<eqv>(Any $a, Any $b) {
    # Last ditch snapshot semantics.  We shouldn't come here too often, so
    # please do not change this to be faster but wronger.  (Instead, add
    # specialized multis for datatypes that can be tested piecemeal.)
    $a.WHAT === $b.WHAT and $a.perl eq $b.perl;
}

multi sub infix:<eqv>(@a, @b) {
    if @a.WHAT === @b.WHAT && (my int $n = @a.elems) == @b.elems {
        my int $i;
        while $i < $n {
            return Bool::False unless @a.AT-POS($i) eqv @b.AT-POS($i);
            $i = $i + 1;
        }
        Bool::True
    }
    else {
        Bool::False;
    }
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

# These must collapse Junctions
proto sub so(Mu $) {*}
multi sub so(Mu $x)  { ?$x }
proto sub not(Mu $) {*}
multi sub not(Mu $x) { !$x }

Metamodel::ClassHOW.exclude_parent(Mu);

# vim: ft=perl6 expandtab sw=4
