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
    
}


sub DUMP(|$) {
    my Mu $args := pir::perl6_current_args_rpa__P();
    my Mu $topic  := pir::shift__PP($args);
    if pir::isnull__IP($topic) { '(null)' }
    elsif pir::isa__IPs($topic, 'ResizablePMCArray') {
        my $s = 'RPA<' ~ nqp::p6box_s(pir::get_addr__IP($topic)) ~ '>(';
        my $t = '';
        $topic := pir::clone__PP($topic);
        while $topic {
            my Mu $x := pir::shift__PP($topic);
            $s = $s ~ $t ~ DUMP($x);
            $t = ', ';
        }
        $s ~ ')'
    }
    else { 
        pir::is_container__IP($topic)
          ?? '▶' ~ $topic.DUMP() 
          !! $topic.DUMP()
    }
};

