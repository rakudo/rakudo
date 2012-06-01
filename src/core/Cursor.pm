my class Cursor does NQPCursorRole {
    has $!ast; # Need it to survive re-creations of the match object.
    
    # Some bits to support <prior>
    trusts Regex;
    my $last_match;
    method !set_last_match($m) { $last_match = $m }
    
    # For <( and )>
    has $!explicit_from;
    has $!explicit_to;
    method MARK_FROM() {
        my int $pos = nqp::getattr_i(self, Cursor, '$!pos');
        $!explicit_from = $pos;
        my $cur := self.'!cursor_start'();
        $cur.'!cursor_pass'($pos);
        $cur
    }
    method MARK_TO() {
        my int $pos = nqp::getattr_i(self, Cursor, '$!pos');
        $!explicit_to = $pos;
        my $cur := self.'!cursor_start'();
        $cur.'!cursor_pass'($pos);
        $cur
    }

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
            my Mu $caphash := nqp::findmethod(Cursor, 'CAPHASH')(self);
            my Mu $capiter := nqp::iterator($caphash);
            while $capiter {
                my Mu $pair := nqp::shift($capiter);
                my str $key = $pair.key;
                my Mu $value := $pair.value;
                $value := nqp::p6list($value, List, Mu)
                    if nqp::islist($value);
                nqp::iscclass(pir::const::CCLASS_NUMERIC, $key, 0)
                  ?? nqp::bindpos($list, $key, $value)
                  !! nqp::bindkey($hash, $key, $value);
            }
            if $!explicit_from.DEFINITE {
                nqp::bindattr($match, Match, '$!from', $!explicit_from);
            }
            if $!explicit_to.DEFINITE {
                nqp::bindattr($match, Match, '$!to',  $!explicit_to);
            }
        }
        nqp::bindattr($match, Capture, '$!list', $list);
        nqp::bindattr($match, Capture, '$!hash', $hash);
        nqp::bindattr(self, Cursor, '$!match', $match);
        $match;
    }

    method INTERPOLATE($var, $i = 0) {
        nqp::isconcrete($var) ??
            ($var ~~ Callable ?? $var(self) !! self."!LITERAL"(nqp::unbox_s($var.Str), $i)) !!
            self."!cursor_start"()
    }
    
    method OTHERGRAMMAR($grammar, $name, |$args) {
        my $lang_cursor := $grammar.'!cursor_init'(self.target(), :p(self.pos()));
        $lang_cursor."$name"(); 
    }
    
    method RECURSE() {
        pir::find_dynamic_lex__Ps('$?REGEX')(self)
    }
    
    method prior() {
        nqp::isconcrete($last_match) ??
            self."!LITERAL"(nqp::unbox_s(~$last_match)) !!
            self."!cursor_start"()
    }
}

sub MAKE_REGEX($arg) {
    my role CachedCompiledRegex {
        has $.regex;
    }
    if $arg ~~ Regex {
        $arg
    }
    elsif nqp::istype($arg, CachedCompiledRegex) {
        $arg.regex
    }
    else {
        my $rx := eval("my \$x = anon regex \{ $arg \}");
        $arg does CachedCompiledRegex($rx);
        $rx
    }
}


