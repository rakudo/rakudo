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
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
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
        X::Constructor::Positional.new(:type( self )).throw();
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
        # would get expensive. Need to do it a bit differently on
        # Parrot; it's not so 6model-y as other backends.
        my $build_plan := nqp::findmethod(self.HOW, 'BUILDALLPLAN')(self.HOW, self);
        my int $count   = nqp::elems($build_plan);
        my int $i       = 0;
#?if parrot
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
                        nqp::atpos($task, 3)) = nqp::decont(%attrinit{$key_name});
                }
            }
            elsif nqp::iseq_i($code, 2) {
                my $key_name := nqp::p6box_s(nqp::atpos_s($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3)) = nqp::decont(%attrinit{$key_name});
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
                        nqp::atpos_s($task, 3)) = nqp::decont(%attrinit{$key_name});
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
#?endif
#?if !parrot
        while nqp::islt_i($i, $count) {
            my $task := nqp::atpos($build_plan, $i);
            my int $code = nqp::atpos($task, 0);
            $i = nqp::add_i($i, 1);
            if nqp::iseq_i($code, 0) {
                # Custom BUILD call.
                nqp::atpos($task, 1)(self, |%attrinit);
            }
            elsif nqp::iseq_i($code, 1) {
                # See if we have a value to initialize this attr
                # with.
                my $key_name := nqp::p6box_s(nqp::atpos($task, 2));
                if %attrinit.exists($key_name) {
                    # XXX Should not really need the decontainerize, but seems
                    # that slurpy hashes sometimes lead to double containers
                    # somehow...
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3)) = nqp::decont(%attrinit{$key_name});
                }
            }
            elsif nqp::iseq_i($code, 2) {
                my $key_name := nqp::p6box_s(nqp::atpos($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3)) = nqp::decont(%attrinit{$key_name});
                }
                else {
                    nqp::bindattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3), nqp::list())
                }
            }
            elsif nqp::iseq_i($code, 3) {
                my $key_name := nqp::p6box_s(nqp::atpos($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3)) = nqp::decont(%attrinit{$key_name});
                }
                else {
                    nqp::bindattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3), nqp::hash())
                }
            }
            elsif nqp::iseq_i($code, 4) {
                unless nqp::attrinited(self, nqp::atpos($task, 1), nqp::atpos($task, 2)) {
                    my $attr := nqp::getattr(self, nqp::atpos($task, 1), nqp::atpos($task, 2));
                    $attr = nqp::atpos($task, 3)(self, $attr);
                }
            }
            else {
                die "Invalid BUILDALLPLAN";
            }
        }
#?endif
        self
    }
    
    method BUILD_LEAST_DERIVED(%attrinit) {
        # Get the build plan for just this class. Need to do it a bit
        # differently on Parrot; it's not so 6model-y as other backends.
        my $build_plan := nqp::findmethod(self.HOW, 'BUILDPLAN')(self.HOW, self);
        my int $count   = nqp::elems($build_plan);
        my int $i       = 0;
#?if parrot
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
                        nqp::atpos_s($task, 3)) = nqp::decont(%attrinit{$key_name});
                }
            }
            elsif nqp::iseq_i($code, 2) {
                my $key_name := nqp::p6box_s(nqp::atpos_s($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos_s($task, 3)) = nqp::decont(%attrinit{$key_name});
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
                        nqp::atpos_s($task, 3)) = nqp::decont(%attrinit{$key_name});
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
#?endif
#?if !parrot
        while nqp::islt_i($i, $count) {
            my $task := nqp::atpos($build_plan, $i);
            my int $code = nqp::atpos($task, 0);
            $i = nqp::add_i($i, 1);
            if nqp::iseq_i($code, 0) {
                # Custom BUILD call.
                nqp::atpos($task, 1)(self, |%attrinit);
            }
            elsif nqp::iseq_i($code, 1) {
                # See if we have a value to initialize this attr
                # with.
                my $key_name := nqp::p6box_s(nqp::atpos($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3)) = nqp::decont(%attrinit{$key_name});
                }
            }
            elsif nqp::iseq_i($code, 2) {
                my $key_name := nqp::p6box_s(nqp::atpos($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3)) = nqp::decont(%attrinit{$key_name});
                }
                else {
                    nqp::bindattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3), nqp::list())
                }
            }
            elsif nqp::iseq_i($code, 3) {
                my $key_name := nqp::p6box_s(nqp::atpos($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3)) = nqp::decont(%attrinit{$key_name});
                }
                else {
                    nqp::bindattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3), nqp::hash())
                }
            }
            elsif nqp::iseq_i($code, 4) {
                unless nqp::attrinited(self, nqp::atpos($task, 1), nqp::atpos($task, 2)) {
                    my $attr := nqp::getattr(self, nqp::atpos($task, 1), nqp::atpos($task, 2));
                    $attr = nqp::atpos($task, 3)(self, $attr);
                }
            }
            else {
                die "Invalid BUILDALLPLAN";
            }
        }
#?endif
        self
    }
    
    proto method Numeric(|) { * }
    multi method Numeric(Mu:U \v:) {
        warn "use of uninitialized value of type {self.^name} in numeric context";
        0
    }
    proto method Real(|) { * }
    multi method Real(Mu:U \v:) {
        warn "use of uninitialized value of type {self.^name} in numeric context";
        0
    }
    
    proto method Str(|) { * }
    multi method Str(Mu:U \v:) {
        warn "use of uninitialized value of type {self.^name} in string context";
        ''
    }
    multi method Str(Mu:D:) {
        self.HOW.name(self) ~ '<' ~ nqp::tostr_I(self.WHERE) ~ '>'
    }

    proto method Stringy(|) { * }
    multi method Stringy() { self.Str }
    
    method item(Mu \item:) is rw { item }
    
    proto method say(|) { * }
    multi method say() { say(self) }
    method print() { print(self) }

    proto method gist(|) { * }
    multi method gist(Mu:U:) { '(' ~ self.HOW.name(self) ~ ')' }
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
    multi method DUMP(Mu:U:) { self.perl }
    multi method DUMP(Mu:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my Mu $attrs := nqp::list();
        for self.HOW.attributes(self) -> $attr {
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
        my str $before = ($flags if defined $flags) ~ self.HOW.name(self) ~ '<' ~ %ctx{$where} ~ '>(';

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
        my $cloned := nqp::clone(nqp::decont(self));
        for self.^attributes() -> $attr {
            my $name := $attr.name;
            my $package := $attr.package;
            unless nqp::objprimspec($attr.type) {
                my $attr_val := nqp::getattr($cloned, $package, $name);
                nqp::bindattr($cloned, $package, $name, nqp::clone($attr_val.VAR))
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


proto sub defined(Mu) is pure { * }
multi sub defined(Mu \x) { x.defined }

proto sub infix:<~~>(|) { * }
multi sub infix:<~~>(Mu \topic, Mu \matcher) {
    matcher.ACCEPTS(topic).Bool;
}

proto sub infix:<=:=>(Mu $a?, Mu $b?) { * }
multi sub infix:<=:=>($a?)      { Bool::True }
multi sub infix:<=:=>(Mu \a, Mu \b) { 
    nqp::p6bool(nqp::eqaddr(a, b));
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

sub DUMP(|args (*@args, :$indent-step = 4, :%ctx?)) {
    my Mu $capture := nqp::usecapture();
    my Mu $topic   := nqp::captureposarg($capture, 0);

    return "\x25b6" ~ DUMP(nqp::decont($topic), :$indent-step, :%ctx)
        if nqp::iscont($topic);
    return '(null)' if nqp::isnull($topic);

    # On Parrot, not everything is a 6model object, so use the typeof op to
    # get a real type name. On other platforms, .HOW.name(...) can be relied
    # on to work.
#?if parrot
    my str $type  = pir::typeof__SP($topic);
#?endif
#?if !parrot
    my str $type  = $topic.HOW.name($topic);
#?endif
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
#?if parrot
            $type = 'RPA' if $type eq 'ResizablePMCArray';
#?endif
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
                for $topic {
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

Metamodel::ClassHOW.exclude_parent(Mu);
