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

    # INTERPOLATE will iterate over the string $tgt beginning at position 0.
    # If it can't match against pattern $var (or any element of $var if it is an array)
    # it will increment $pos and try again. Therefor it is important to only match
    # against the current position.
    # $i is case insensitive flag
    # $s is for sequential matching instead of junctive
    # $a is true if we are in an assertion
    method INTERPOLATE($var, $i = 0, $s = 0, $a = 0) {
        if nqp::isconcrete($var) {
            # Call it if it is a routine. This will capture if requested.
            return $var(self) if $var ~~ Callable;
            my $maxlen := -1;
            my $cur := self.'!cursor_start_cur'();
            my $pos := nqp::getattr_i($cur, $?CLASS, '$!from');
            my $tgt := $cur.target;
            my $eos := nqp::chars($tgt);

            for nqp::istype($var, Positional) || nqp::istype($var, Capture)
                ?? $var.list !! $var -> $topic {
                my $match;
                my $len;
                
                # We are in a regex assertion, the strings we get will be treated as
                # regex rules.
                if $a {
                    my $rx := eval("my \$x = anon regex \{ ^$topic \}");
                    $match := (nqp::substr($tgt, $pos, $eos - $pos) ~~ $rx).Str;
                    $len   := nqp::chars( $match );
                }
                # A Regex already.
                elsif $topic ~~ Regex {
                    $match := nqp::substr($tgt, $pos, $eos - $pos) ~~ $topic;
                    # In order to return the correct result we need to match from the
                    # current position only.
                    next if $match.from;
                    $match := ~$match;
                    $len   := nqp::chars( $match );
                }
                # The pattern is a string.
                else {
                    $len   := nqp::chars( $topic );
                    $match := $len < 1
                            ||  ($i ?? nqp::lc(nqp::substr($tgt, $pos, $len)) eq nqp::lc($topic)
                                    !! nqp::substr($tgt, $pos, $len) eq $topic);
                }

                if $match && $len > $maxlen && $pos + $len <= $eos {
                    $maxlen := $len;
                    last if $s; # stop here for sequential alternation
                }
            }

            $cur.'!cursor_pass'($pos + $maxlen, '') if $maxlen >= 0;
            $cur
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
        nqp::getlexdyn('$?REGEX')(self)
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


