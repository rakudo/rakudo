my class Mu {
    proto method ACCEPTS(|$) { * }
    multi method ACCEPTS(Mu:U: Mu \$topic) {
        nqp::p6bool(nqp::istype($topic, self))
    }

    method WHERE() {
        nqp::p6box_i(nqp::where(self))
    }

    method WHICH() {
        nqp::p6box_i(nqp::where(self))
    }

    method WHY() {
        self.HOW.docs // Any
    }
    
    proto method Bool(|$) {*}
    multi method Bool() {
        self.defined
    }

    method so()  { self.Bool }
    method not() { self ?? False !! True }
    
    method defined() {
        nqp::p6bool(pir::repr_defined__IP(self))
    }
    
    proto method new(|$) { * }
    multi method new(*%attrinit) {
        self.bless(*, |%attrinit);
    }
    multi method new($, *@) {
        die "Default constructor only takes named arguments";
    }
    
    method CREATE() {
        nqp::create(self)
    }
    
    method bless(Mu \$candidate, *@autovivs, *%attrinit) {
        # If we were passed *, then need to create a candidate.
        my $cand := nqp::istype($candidate, Whatever) ??
            nqp::create(self) !!
            $candidate;
        $cand.BUILDALL(@autovivs, %attrinit);
    }
    
    method BUILDALL(@autovivs, %attrinit) {
        # Get the build plan. Note that we do this "low level" to
        # avoid the NQP type getting mapped to a Rakudo one, which
        # would get expensive.
        my $build_plan := pir::find_method__PPs(self.HOW, 'BUILDALLPLAN')(self.HOW, self);
        my int $count   = nqp::elems($build_plan);
        my int $i       = 0;
        while nqp::islt_i($i, $count) {
            my $task := nqp::atpos($build_plan, $i);
            $i = nqp::add_i($i, 1);
            if nqp::iseq_i(nqp::atpos($task, 0), 0) {
                # Custom BUILD call.
                nqp::atpos($task, 1)(self, |%attrinit);
            }
            elsif nqp::iseq_i(nqp::atpos($task, 0), 1) {
                # See if we have a value to initialize this attr
                # with.
                my $key_name := nqp::p6box_s(nqp::atpos($task, 2));
                if %attrinit.exists($key_name) {
                    # XXX Should not really need the decontainerize, but seems
                    # that slurpy hashes sometimes lead to double containers
                    # somehow...
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3)) = pir::nqp_decontainerize__PP(%attrinit{$key_name});
                }
            }
            elsif nqp::iseq_i(nqp::atpos($task, 0), 2) {
                unless nqp::attrinited(self, nqp::atpos($task, 1), nqp::atpos($task, 2)) {
                    my $attr := nqp::getattr(self, nqp::atpos($task, 1), nqp::atpos($task, 2));
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
        my $build_plan := pir::find_method__PPs(self.HOW, 'BUILDPLAN')(self.HOW, self);
        my int $count   = nqp::elems($build_plan);
        my int $i       = 0;
        while nqp::islt_i($i, $count) {
            my $task := nqp::atpos($build_plan, $i);
            $i = nqp::add_i($i, 1);
            if nqp::iseq_i(nqp::atpos($task, 0), 0) {
                # Custom BUILD call.
                nqp::atpos($task, 1)(self, |%attrinit);
            }
            elsif nqp::iseq_i(nqp::atpos($task, 0), 1) {
                # See if we have a value to initialize this attr
                # with.
                my $key_name := nqp::p6box_s(nqp::atpos($task, 2));
                if %attrinit.exists($key_name) {
                    nqp::getattr(self, nqp::atpos($task, 1),
                        nqp::atpos($task, 3)) = pir::nqp_decontainerize__PP(%attrinit{$key_name});
                }
            }
            elsif nqp::iseq_i(nqp::atpos($task, 0), 2) {
                unless nqp::attrinited(self, nqp::atpos($task, 1), nqp::atpos($task, 2)) {
                    my $attr := nqp::getattr(self, nqp::atpos($task, 1), nqp::atpos($task, 2));
                    $attr = nqp::atpos($task, 3)(self, $attr);
                }
            }
            else {
                die "Invalid BUILDALLPLAN";
            }
        }
        self
    }
    
    proto method Numeric(|$) { * }
    multi method Numeric(Mu:U:) {
        note 'Use of uninitialized value in numeric context';
        0
    }
    
    proto method Str(|$) { * }
    multi method Str(Mu:U:) {
        note 'Use of uninitialized value in string context';
        ''
    }
    multi method Str(Mu:D:) {
        self.HOW.name(self) ~ '<' ~ self.WHERE ~ '>'
    }

    method Stringy() { self.Str }
    
    method item() { self }
    
    method say() { say(self) }
    method print() { print(self) }

    proto method gist(|$) { * }
    multi method gist(Mu:U:) { self.HOW.name(self) ~ '()' }
    multi method gist(Mu:D:) { self.perl }

    proto method perl(|$) { * }
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

    proto method DUMP(|$) { * }
    multi method DUMP(Mu:D:) { self.perl }
    multi method DUMP(Mu:U:) { self.perl }
    method DUMP-ID() { self.HOW.name(self) ~ '<' ~ self.WHERE ~ '>' }
    
    proto method isa(|$) { * }
    multi method isa(Mu $type) {
        nqp::p6bool(self.HOW.isa(self, $type.WHAT))
    }
    multi method isa(Str:D $name) {
        my @mro = self.HOW.mro(self);
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
    
    method does(Mu $type) {
        nqp::p6bool(nqp::istype(self, $type.WHAT))
    }
    
    method can($name) {
        self.HOW.can(self, $name)
    }
    
    method clone() {
        my $cloned := pir::repr_clone__PP(pir::perl6_decontainerize__PP(self));
        # XXX Probably need to clone containery things a level deeper.
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
    method dispatch:<var>($var, *@pos, *%named) {
        $var(self, |@pos, |%named)
    }
    
    method dispatch:<::>($name, Mu $type, *@pos, *%named) {
        unless nqp::istype(self, $type) {
            die "Cannot dispatch to a method on " ~ $type.WHAT.perl ~
                " because it is not inherited or done by " ~
                self.WHAT.perl;
        }
        pir::find_method__PPS($type, $name)(self, |@pos, |%named)
    }
    
    method dispatch:<!>($name, Mu $type, *@pos, *%named) {
        my $meth := $type.HOW.find_private_method($type, $name);
        $meth ??
            $meth(self, |@pos, |%named) !!
            die("Private method '$name' not found on type " ~ $type.HOW.name($type))
            
    }
    
    method dispatch:<.^>($name, *@pos, *%named) {
        self.HOW."$name"(self, |@pos, |%named)
    }
    
    method dispatch:<.=>(\$mutate: $name, *@pos, *%named) {
        $mutate = $mutate."$name"(|@pos, |%named)
    }
    
    method dispatch:<.?>($name, *@pos, *%named) {
        pir::can__IPS(self, $name) ??
            self."$name"(|@pos, |%named) !!
            Nil
    }
    
    method dispatch:<.+>($name, *@pos, *%named) {
        my @result := self.dispatch:<.*>($name, |@pos, |%named);
        if @result.elems == 0 {
            die "Method '$name' not found for invocant of type '" ~
                self.WHAT.perl ~ "'";
        }
        @result
    }
    
    method dispatch:<.*>($name, *@pos, *%named) {
        my @mro = self.HOW.mro(self);
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
                @results.push($meth(self, |@pos, |%named));
            }
            $i = $i + 1;
        }
        &infix:<,>(|@results)
    }

    method dispatch:<hyper>($name, *@pos, *%named) {
        hyper( -> \$obj { $obj."$name"(|@pos, |%named) }, self )
    }
}


proto sub prefix:<defined>(|$) { * }
multi sub prefix:<defined>(Mu \$x) { $x.defined }

proto sub infix:<~~>(|$) { * }
multi sub infix:<~~>(Mu \$topic, Mu \$matcher) {
    $matcher.ACCEPTS($topic).Bool;
}

sub infix:<=:=>(Mu \$x, Mu \$y) { 
    nqp::p6bool(nqp::iseq_i(nqp::where($x), nqp::where($y)));
}

proto sub infix:<eqv>(Mu $, Mu $) { * }
multi sub infix:<eqv>(Mu $a, Mu $b) {
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

sub DUMP(|$) {
    my Mu $args := pir::perl6_current_args_rpa__P();
    my Mu $topic  := nqp::shift($args);
    if nqp::isnull($topic) { '(null)' }
    elsif pir::isa__IPs($topic, 'ResizablePMCArray') {
        my $s = 'RPA<' ~ nqp::p6box_s(nqp::where($topic)) ~ '>(';
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

