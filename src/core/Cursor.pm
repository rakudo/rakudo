my class Cursor does NQPCursorRole {
    has $!ast; # Need it to survive re-creations of the match object.
    
    # Some bits to support <prior>
    my $last_match;
    
    method MATCH() {
        my $match := nqp::getattr(self, Cursor, '$!match');
        return $match if nqp::istype($match, Match) && nqp::isconcrete($match);
        $match := nqp::create(Match);
        nqp::bindattr($match, Match, '$!orig', nqp::findmethod(self, 'orig')(self));
        nqp::bindattr_i($match, Match, '$!from', nqp::getattr_i(self, Cursor, '$!from'));
        nqp::bindattr_i($match, Match, '$!to', nqp::getattr_i(self, Cursor, '$!pos'));
        nqp::bindattr($match, Match, '$!ast', nqp::getattr(self, Cursor, '$!ast'));
        nqp::bindattr($match, Match, '$!CURSOR', self);
        my Mu $list := nqp::list();
        my Mu $hash := nqp::hash();
        if $match.Bool {
            my Mu $caphash := nqp::findmethod(Cursor, 'CAPHASH')(self);
            my Mu $capiter := nqp::iterator($caphash);
            while $capiter {
                my str $key = nqp::shift_s($capiter);
                my Mu $value := nqp::p6type(nqp::atkey($caphash, $key));
                if $key eq '$!from' || $key eq '$!to' {
                    nqp::bindattr_i($match, Match, $key, $value.from);
                }
                else {
                    $value := nqp::p6list($value, List, Mu)
                        if nqp::islist($value);
                    nqp::iscclass(nqp::const::CCLASS_NUMERIC, $key, 0)
                      ?? nqp::bindpos($list, $key, $value)
                      !! nqp::bindkey($hash, $key, $value);
                }
            }
        }
        nqp::bindattr($match, Capture, '$!list', $list);
        nqp::bindattr($match, Capture, '$!hash', $hash);
        nqp::bindattr(self, Cursor, '$!match', $match);
        $match;
    }

    method MATCH_SAVE() {
        my $match := self.MATCH();
        $last_match := $match if $match;
        $match;
    }

    method INTERPOLATE($var, $i = 0, $s = 0) {
        if nqp::isconcrete($var) {
            if nqp::istype($var, Positional) # for array-likes
            || nqp::istype($var, Capture) {  # for references to arrays
                my $maxlen := -1;
                my $cur := self.'!cursor_start_cur'();
                my $pos := nqp::getattr_i($cur, $?CLASS, '$!from');
                my $tgt := $cur.target;
                my $eos := nqp::chars($tgt);
                for $var.list {
                    my $topic := $_ ~~ Callable ?? $_(self) !! $_;
                    my $len := nqp::chars($topic);
                    if $len > $maxlen && $pos + $len <= $eos
                        && nqp::substr($tgt, $pos, $len) eq $topic {
                        $maxlen := $len;
                        last if $s; # stop here for sequential alternation
                    }
                }
                $cur.'!cursor_pass'($pos + $maxlen, '') if $maxlen >= 0;
                $cur
            }
            else {
                $var ~~ Callable ?? $var(self) !! self."!LITERAL"(nqp::unbox_s($var.Str), $i)
            }
        }
        else {
            self."!cursor_start_cur"()
        }
    }
    
    method OTHERGRAMMAR($grammar, $name, |) {
        my $lang_cursor := $grammar.'!cursor_init'(self.target(), :p(self.pos()));
        $lang_cursor."$name"(); 
    }
    
    method RECURSE() {
        pir::find_dynamic_lex__Ps('$?REGEX')(self)
    }
    
    method prior() {
        nqp::isconcrete($last_match) ??
            self."!LITERAL"(nqp::unbox_s(~$last_match)) !!
            self."!cursor_start_cur"()
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


