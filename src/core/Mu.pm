my class Mu {
    method new() {
        self.bless(self.CREATE());
    }
    
    proto method ACCEPTS(|$) { * }
    multi method ACCEPTS(Mu:U: \$topic) {
        nqp::p6bool(pir::type_check__IPP($topic, self))
    }

    method WHERE() {
        nqp::p6box_i(pir::get_addr__IP(self))
    }
    
    method Bool() {
        self.defined
    }
    
    method defined() {
        nqp::p6bool(pir::repr_defined__IP(self))
    }
    
    method CREATE() {
        pir::repr_instance_of__PP(self.WHAT)
    }
    
    method bless(Mu \$candidate) {
        $candidate
    }
    
    proto method Str(|$) { * }
    multi method Str(Mu:U:) {
        ''   # TODO: should be a warning of some sort
    }
    multi method Str(Mu:D:) {
        self.HOW.name(self) ~ '<' ~ self.WHERE ~ '>'
    }

    method Stringy() { self.Str }
    
    method item() { self }
    
    method say() { say(self) }

    proto method gist(|$) { * }
    multi method gist(Mu:U:) { self.HOW.name(self) ~ '()' }
    multi method gist(Mu:D:) { self.Stringy }

    proto method perl(|$) { * }
    multi method perl(Mu:D:) { self.Str }
    multi method perl(Mu:U:) { self.HOW.name(self) }

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
        my $i = 0;
        while $i < +@mro {
            my $obj = @mro[$i];
            if $obj.HOW.name($obj) eq $name {
                return nqp::p6bool(1);
            }
            $i++;
        }
        nqp::p6bool(0)
    }
    
    method does(Mu $type) {
        nqp::p6bool(pir::type_check__IPP(self, $type))
    }
    
    # XXX TODO: Handle positional case.
    method dispatch:<var>($var, *@pos, *%named) {
        $var(self, |@pos, |%named)
    }
    
    method dispatch:<::>($name, *@pos, *%named) {
        die "multi-part method name lookups not yet implemented"
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
        my @results;
        my $i = 0;
        while $i < +@mro {
            my $obj = @mro[$i];
            my $meth = ($obj.HOW.method_table($obj)){$name};
            if !$meth && $i == 0 {
                $meth = ($obj.HOW.submethod_table($obj)){$name};
            }
            if $meth {
                @results.push($meth($obj, |@pos, |%named));
            }
            $i++;
        }
        &infix:<,>(|@results)
    }
}


proto sub prefix:<defined>(|$) { * }
multi sub prefix:<defined>(Mu \$x) { $x.defined }

proto sub infix:<~~>(|$) { * }
multi sub infix:<~~>(Mu \$topic, Mu \$matcher) {
    $matcher.ACCEPTS($topic).Bool;
}


sub DUMP(|$) {
    my Mu $args := pir::perl6_current_args_rpa__P();
    my Mu $topic  := nqp::shift($args);
    if pir::isnull__IP($topic) { '(null)' }
    elsif pir::isa__IPs($topic, 'ResizablePMCArray') {
        my $s = 'RPA<' ~ nqp::p6box_s(pir::get_addr__IP($topic)) ~ '>(';
        my $t = '';
        $topic := pir::clone__PP($topic);
        while $topic {
            my Mu $x := nqp::shift($topic);
            $s = $s ~ $t ~ DUMP($x);
            $t = ', ';
        }
        $s ~ ')'
    }
    else { 
        pir::is_container__IP($topic)
          ?? "\x25b6" ~ $topic.DUMP() 
          !! $topic.DUMP()
    }
};

