my class Cursor does NQPCursorRole {
    has $!ast; # Need it to survive re-creations of the match object.
    
    # Some bits to support <prior>
    trusts Regex;
    my $last_match;
    method !set_last_match($m) { $last_match = $m }

    method MATCH() {
        my $match := nqp::getattr(self, Cursor, '$!match');
        return $match if nqp::istype($match, Match) && nqp::isconcrete($match);
        $match := nqp::create(Match);
        nqp::bindattr($match, Match, '$!orig', nqp::getattr(self, Cursor, '$!orig'));
        nqp::bindattr($match, Match, '$!from', nqp::p6box_i(nqp::getattr_i(self, Cursor, '$!from')));
        nqp::bindattr($match, Match, '$!to', nqp::p6box_i(nqp::getattr_i(self, Cursor, '$!pos')));
        nqp::bindattr($match, Match, '$!ast', nqp::getattr(self, Cursor, '$!ast'));
        nqp::bindattr($match, Match, '$!CURSOR', self);
        my Mu $list := nqp::list();
        my Mu $hash := nqp::hash();
        if $match.Bool {
            my Mu $caphash := pir::find_method__PPs(Cursor, 'CAPHASH')(self);
            my Mu $capiter := nqp::iterator($caphash);
            while $capiter {
                my Mu $pair := nqp::shift($capiter);
                my str $key = $pair.key;
                my Mu $value := $pair.value;
                $value := nqp::p6list($value, List, Mu)
                    if pir::isa__IPs($value, 'ResizablePMCArray');
                nqp::iscclass(pir::const::CCLASS_NUMERIC, $key, 0)
                  ?? nqp::bindpos($list, $key, $value)
                  !! nqp::bindkey($hash, $key, $value);
            }
        }
        nqp::bindattr($match, Capture, '$!list', $list);
        nqp::bindattr($match, Capture, '$!hash', $hash);
        nqp::bindattr(self, Cursor, '$!match', $match);
        $match;
    }

    method INTERPOLATE($var) {
        nqp::isconcrete($var) ??
            ($var ~~ Callable ?? $var(self) !! self."!LITERAL"(nqp::unbox_s($var.Str))) !!
            self."!cursor_start"()
    }
    
    method OTHERGRAMMAR($grammar, $name, |$args) {
        my $lang_cursor := $grammar.'!cursor_init'(self.target(), :p(self.pos()));
        $lang_cursor."$name"(); 
    }
    
    method prior() {
        nqp::isconcrete($last_match) ??
            self."!LITERAL"(nqp::unbox_s(~$last_match)) !!
            self."!cursor_start"()
    }
}

sub MAKE_REGEX($arg) {
    $arg ~~ Regex ?? $arg !! eval("my \$x = anon regex \{ $arg \}")
}


