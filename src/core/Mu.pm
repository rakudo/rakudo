my class X::Constructor::Positional { ... }
my class X::Method::NotFound        { ... }
my class X::Method::InvalidQualifier { ... }

my class Mu {
    proto method ACCEPTS(|) { * }
    multi method ACCEPTS(Mu:U: Mu \topic) {
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
            nqp::concat_s(
                nqp::concat_s(nqp::unbox_s(self.^name), '|'),
                nqp::where(self)
            ),
            ObjAt
        )
    }

    method take {
        take self;
    }

    method WHY() {
        self.HOW.docs // Any
    }
    
    proto method Bool(|) {*}
    multi method Bool() {
        self.defined
    }

    method so()  { self.Bool }
    method not() { self ?? False !! True }
    
    method defined() {
        nqp::p6bool(nqp::isconcrete(self))
    }
    
    proto method new(|) { * }
    multi method new(*%attrinit) {
        self.bless(*, |%attrinit);
    }
    multi method new($, *@) {
        X::Constructor::Positional.new(:name(self.^name)).throw();
    }
    
    method CREATE() {
        nqp::create(self)
    }
    
    method bless(Mu \candidate, *@autovivs, *%attrinit) {
        # If we were passed *, then need to create a candidate.
        my $cand := nqp::istype(candidate, Whatever) ??
            nqp::create(self) !!
            candidate;
        $cand.BUILDALL(@autovivs, %attrinit);
    }
    
    method BUILDALL(@autovivs, %attrinit) {
        # Get the build plan. Note that we do this "low level" to
        # avoid the NQP type getting mapped to a Rakudo one, which
        # would get expensive.
        my $build_plan := nqp::findmethod(self.HOW, 'BUILDALLPLAN')(self.HOW, self);
        my int $count   = nqp::elems($build_plan);
        my int $i       = 0;
        while nqp::islt_i($i, $count) {
            my $task := nqp::atpos($build_plan, $i);
            my int $code = nqp::atpos_i($task, 0);
            $i = nqp::add_i($i, 1);
            if nqp::iseq_i($code, 0) {
                # Custom BUILD call.
                nqp::atpos($task, 1)(self, |%attrinit);
            }
            elsif nqp::iseq_i($code, 1) {
                # See if we have a value to initialize this attr
                # with.
                my $key_name := nqp::p6box_s(nqp::atpos_s($task, 2));
                if %attrinit.exists($key_name) {
                    # XXX Should not really need the decontainerize, but seems
                    # that slurpy hashes sometimes lead to double containers
                    # somehow...
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3)) = nqp::p6decont(%attrinit{$key_name});
                }
            }
            elsif nqp::iseq_i($code, 2) {
                my $key_name := nqp::p6box_s(nqp::atpos_s($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3)) = nqp::p6decont(%attrinit{$key_name});
                }
                else {
                    nqp::bindattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3), nqp::list())
                }
            }
            elsif nqp::iseq_i($code, 3) {
                my $key_name := nqp::p6box_s(nqp::atpos_s($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3)) = nqp::p6decont(%attrinit{$key_name});
                }
                else {
                    nqp::bindattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3), nqp::hash())
                }
            }
            elsif nqp::iseq_i($code, 4) {
                unless nqp::attrinited(self, nqp::atpos($task, 1), nqp::atpos_s($task, 2)) {
                    my $attr := nqp::getattr(self, nqp::atpos($task, 1), nqp::atpos_s($task, 2));
                    $attr = nqp::atpos($task, 3)(self, $attr);
                }
            }
            else {
                die "Invalid BUILDALLPLAN";
            }
        }
        self
    }
    
    method BUILD_LEAST_DERIVED(%attrinit) {
        # Get the build plan for just this class.
        my $build_plan := nqp::findmethod(self.HOW, 'BUILDPLAN')(self.HOW, self);
        my int $count   = nqp::elems($build_plan);
        my int $i       = 0;
        while nqp::islt_i($i, $count) {
            my $task := nqp::atpos($build_plan, $i);
            my int $code = nqp::atpos_i($task, 0);
            $i = nqp::add_i($i, 1);
            if nqp::iseq_i($code, 0) {
                # Custom BUILD call.
                nqp::atpos($task, 1)(self, |%attrinit);
            }
            elsif nqp::iseq_i($code, 1) {
                # See if we have a value to initialize this attr
                # with.
                my $key_name := nqp::p6box_s(nqp::atpos_s($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3)) = nqp::p6decont(%attrinit{$key_name});
                }
            }
            elsif nqp::iseq_i($code, 2) {
                my $key_name := nqp::p6box_s(nqp::atpos_s($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3)) = nqp::p6decont(%attrinit{$key_name});
                }
                else {
                    nqp::bindattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3), nqp::list())
                }
            }
            elsif nqp::iseq_i($code, 3) {
                my $key_name := nqp::p6box_s(nqp::atpos_s($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3)) = nqp::p6decont(%attrinit{$key_name});
                }
                else {
                    nqp::bindattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3), nqp::hash())
                }
            }
            elsif nqp::iseq_i($code, 4) {
                unless nqp::attrinited(self, nqp::atpos($task, 1), nqp::atpos_s($task, 2)) {
                    my $attr := nqp::getattr(self, nqp::atpos($task, 1), nqp::atpos_s($task, 2));
                    $attr = nqp::atpos($task, 3)(self, $attr);
                }
            }
            else {
                die "Invalid BUILDALLPLAN";
            }
        }
        self
    }
    
    proto method Numeric(|) { * }
    multi method Numeric(Mu:U \v:) {
        warn (nqp::iscont(v)
              ?? "use of uninitialized variable { v.VAR.name }"
              !! "use of uninitialized value")
            ~ " of type {self.^name} in numeric context";
        0
    }
    proto method Real(|) { * }
    multi method Real(Mu:U \v:) {
        warn (nqp::iscont(v)
              ?? "use of uninitialized variable { v.VAR.name }"
              !! "use of uninitialized value")
            ~ " of type {self.^name} in numeric context";
        0
    }
    
    proto method Str(|) { * }
    multi method Str(Mu:U \v:) {
        warn (nqp::iscont(v)
              ?? "use of uninitialized variable { v.VAR.name }"
              !! "use of uninitialized value")
            ~ " of type {self.^name} in string context";
        ''
    }
    multi method Str(Mu:D:) {
        self.HOW.name(self) ~ '<' ~ self.WHERE ~ '>'
    }

    method Stringy() { self.Str }
    
    method item(Mu \item:) is rw { item }
    
    proto method say(|) { * }
    multi method say() { say(self) }
    method print() { print(self) }

    proto method gist(|) { * }
    multi method gist(Mu:U:) { self.HOW.name(self) ~ '()' }
    multi method gist(Mu:D:) { self.perl }

    proto method perl(|) { * }
    multi method perl(Mu:U:) { self.HOW.name(self) }
    multi method perl(Mu:D:) {
        my @attrs;
        for self.^attributes().grep: { .has_accessor } -> $attr {
            my $name := $attr.Str.substr(2);
            @attrs.push: $name
                        ~ ' => '
                        ~ self."$name"().perl
        }
        self.^name() ~ '.new(' ~  @attrs.join(', ') ~ ')';
    }

    proto method DUMP(|) { * }
    multi method DUMP(Mu:D:) { self.perl }
    multi method DUMP(Mu:U:) { self.perl }
    method DUMP-ID() { self.HOW.name(self) ~ '<' ~ self.WHERE ~ '>' }
    
    proto method isa(|) { * }
    multi method isa(Mu \SELF: Mu $type) {
        nqp::p6bool(SELF.HOW.isa(SELF, $type.WHAT))
    }
    multi method isa(Mu \SELF: Str:D $name) {
        my @mro = SELF.HOW.mro(SELF);
        my int $mro_count = +@mro;
        my int $i = 0;
        while $i < $mro_count {
            my $obj = @mro[$i];
            if $obj.HOW.name($obj) eq $name {
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
        SELF.HOW.can(SELF, $name)
    }
    
    method clone(*%twiddles) {
        my $cloned := pir::repr_clone__PP(nqp::p6decont(self));
        for self.^attributes() -> $attr {
            my $name := $attr.name;
            my $package := $attr.package;
            unless pir::repr_get_primitive_type_spec__IP($attr.type) {
                my $attr_val := nqp::getattr($cloned, $package, $name);
                nqp::bindattr($cloned, $package, $name, pir::repr_clone__PP($attr_val.VAR))
                    if nqp::iscont($attr_val);
            }
            my $acc_name := $name.substr(2);
            if $attr.has-accessor && %twiddles.exists($acc_name) {
                nqp::getattr($cloned, $package, $name) = %twiddles{$acc_name};
            }
        }
        $cloned
    }
    
    method Capture() {
        my %attrs;
        for self.^attributes -> $attr {
            if $attr.has-accessor {
                my $name = $attr.name.substr(2);
                unless %attrs.exists($name) {
                    %attrs{$name} = self."$name"();
                }
            }
        }
        %attrs.Capture
    }
    
    # XXX TODO: Handle positional case.
    method dispatch:<var>(Mu \SELF: $var, |c) is rw is hidden_from_backtrace {
        $var(SELF, |c)
    }
    
    method dispatch:<::>(Mu \SELF: $name, Mu $type, |c) is rw {
        unless nqp::istype(SELF, $type) {
            X::Method::InvalidQualifier.new(
                    method          => $name,
                    invocant        => SELF,
                    qualifier-type  => $type,

            ).throw;
        }
        self.HOW.find_method_qualified(self, $type, $name)(SELF, |c)
    }
    
    method dispatch:<!>(Mu \SELF: $name, Mu $type, |c) is rw is hidden_from_backtrace {
        my $meth := $type.HOW.find_private_method($type, $name);
        $meth ??
            $meth(SELF, |c) !!
            X::Method::NotFound.new(
                    method   => '!' ~ $name,
                    typename => $type.HOW.name($type),
                    :private,
            ).throw;
    }
    
    method dispatch:<.^>(Mu \SELF: $name, |c) is rw is hidden_from_backtrace {
        self.HOW."$name"(SELF, |c)
    }
    
    method dispatch:<.=>(\mutate: $name, |c) is rw {
        mutate = mutate."$name"(|c)
    }
    
    method dispatch:<.?>(Mu \SELF: $name, |c) is rw is hidden_from_backtrace {
        nqp::can(SELF, $name) ??
            SELF."$name"(|c) !!
            Nil
    }
    
    method dispatch:<.+>(Mu \SELF: $name, |c) {
        my @result := SELF.dispatch:<.*>($name, |c);
        if @result.elems == 0 {
            X::Method::NotFound.new(
                    method   => $name,
                    typename => SELF.^name,
            ).throw;
        }
        @result
    }
    
    method dispatch:<.*>(Mu \SELF: $name, |c) {
        my @mro = SELF.HOW.mro(SELF);
        my int $mro_count = +@mro;
        my @results;
        my int $i = 0;
        while $i < $mro_count {
            my $obj = @mro[$i];
            my $meth = ($obj.HOW.method_table($obj)){$name};
            if !$meth && $i == 0 {
                $meth = ($obj.HOW.submethod_table($obj)){$name};
            }
            if $meth {
                @results.push($meth(SELF, |c));
            }
            $i = $i + 1;
        }
        &infix:<,>(|@results)
    }

    method dispatch:<hyper>(Mu \SELF: $name, |c) {
        hyper( -> \obj { obj."$name"(|c) }, SELF )
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
                push @classes, @search_list;
                my @new_search_list;
                for @search_list -> $current {
                    for $current.^parents(:local) -> $next {
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
                    for $class.^parents(:local) {
                        build_ascendent($^parent);
                    }
                }
            }
            build_ascendent(self.WHAT);
        } elsif $descendant {
            sub build_descendent(Mu $class) {
                unless @classes.grep({ $^c.WHAT =:= $class.WHAT }) {
                    for $class.^parents(:local) {
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
                    for $class.^methods(:local) -> $method {
                        my $check_name = $method.?name;
                        if $check_name.defined && $check_name eq $name {
                            @methods.push($method);
                        }
                    }
                    0;
                }
            }
        }

        return @methods;
    }
}


proto sub defined(Mu) { * }
multi sub defined(Mu \x) { x.defined }

proto sub infix:<~~>(|) { * }
multi sub infix:<~~>(Mu \topic, Mu \matcher) {
    matcher.ACCEPTS(topic).Bool;
}

proto sub infix:<=:=>(Mu $a?, Mu $b?) { * }
multi sub infix:<=:=>($a?)      { Bool::True }
multi sub infix:<=:=>(Mu \a, Mu \b) { 
    nqp::p6bool(nqp::iseq_i(nqp::where(a), nqp::where(b)));
}

proto sub infix:<eqv>(Any $?, Any $?) { * }
multi sub infix:<eqv>($a?)            { Bool::True }
multi sub infix:<eqv>(Any $a, Any $b) {
    $a.WHICH eq $b.WHICH
}

multi sub infix:<eqv>(@a, @b) {
    unless @a.WHAT === @b.WHAT && @a.elems == @b.elems {
        return Bool::False
    }
    for ^@a -> $i {
        unless @a[$i] eqv @b[$i] {
            return Bool::False;
        }
    }
    Bool::True
}

sub DUMP(|) {
    my Mu $args := pir::perl6_current_args_rpa__P();
    my Mu $topic  := nqp::shift($args);
    if nqp::isnull($topic) { '(null)' }
    elsif nqp::islist($topic) {
        my str $type = pir::typeof__SP($topic);
        $type = 'RPA' if $type eq 'ResizablePMCArray';
        my $s = $type ~ '<' ~ nqp::p6box_s(nqp::where($topic)) ~ '>(';
        my $t = '';
        $topic := nqp::clone($topic);
        while $topic {
            my Mu $x := nqp::shift($topic);
            $s = $s ~ $t ~ DUMP($x);
            $t = ', ';
        }
        $s ~ ')'
    }
    else { 
        nqp::iscont($topic)
          ?? "\x25b6" ~ $topic.DUMP() 
          !! $topic.DUMP()
    }
};
Metamodel::ClassHOW.exclude_parent(Mu);
