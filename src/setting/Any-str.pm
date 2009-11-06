class Any is also {
    our Int multi method bytes() is export {
        Q:PIR {
            $S0 = self
            $I0 = bytelength $S0
            %r = box $I0
        }
    }

    our Str multi method capitalize() is export {
        self.lc.subst(/\w+/, { .ucfirst }, :global)
    }

    our Str multi method chop() is export {
        self.substr(0, -1)
    }

    our Str multi method fmt(Str $format = '%s') {
        sprintf($format, self)
    }

    our Str multi method lc() is export {
        Q:PIR {
            $S0 = self
            downcase $S0
            %r = 'prefix:~'($S0)
        }
    }

    our Str multi method lcfirst() is export {
        self gt '' ?? self.substr(0,1).lc ~ self.substr(1) !! ""
    }

    our Int multi method ord() is export {
        fail('Can not take ord of empty string') if self.chars == 0;
        Q:PIR {
            $S0 = self
            $I0 = ord $S0
            %r = box $I0
        }
    }

    our Int multi method p5chomp() is export {
        my $num = 0;

        for @.list -> $str is rw {
            if $str ~~ /\x0a$/ {
                $str = $str.substr(0, $str.chars - 1);
                $num++;
            }
        }

        $num;
    }

    # TODO: Return type should be a Char once that is supported.
    our Str multi method p5chop() is export {
        my $char = '';

        for @.list -> $str is rw {
            if $str gt '' {
                $char = $str.substr($str.chars - 1, 1);
                $str  = $str.chop;
            }
        }

        $char
    }

    our Str multi method samecase(Str $pattern) is export {
        my @pattern = $pattern.split('');
        [~] gather {
            my $p = "";
            for (~self).split('') -> $s {
                $p = @pattern.shift if @pattern;
                given $p {
                    when /<upper>/ { take $s.uc }
                    when /<lower>/ { take $s.lc }
                    default { take $s }
                }
            }
        }
    }

=begin item split

 our List multi Str::split ( Str $delimiter ,  Str $input = $+_, Int $limit = inf )
 our List multi Str::split ( Rule $delimiter = /\s+/,  Str $input = $+_, Int $limit = inf )
 our List multi Str::split ( Str $input :  Str $delimiter          , Int $limit = inf )
 our List multi Str::split ( Str $input : Rule $delimiter          , Int $limit = inf )

String delimiters must not be treated as rules but as constants.  The
default is no longer S<' '> since that would be interpreted as a constant.
P5's C<< split('S< >') >> will translate to C<.words> or some such.  Null trailing fields
are no longer trimmed by default.  We might add some kind of :trim flag or
introduce a trimlist function of some sort.

B<Note:> partial implementation only

=end item

    our List multi method split(Code $delimiter, $limit = *, :$all) {
        my $s = ~self;
        my $l = $limit ~~ Whatever ?? Inf !! $limit;
        my $keep = '';
        return gather {
            while $l > 1 && $s ~~ $delimiter {
                take $keep ~ $s.substr(0, $/.from);
                if $/.from == $/.to {
                    $keep = $s.substr($/.to, 1);
                    $s.=substr($/.to + 1);
                } else {
                    $keep = '';
                    $s.=substr($/.to)
                }
                $l--;
                next if $l < 1;
                if $all {
                    $l--;
                    take $/;
                }
            }
            take $keep ~ $s if $l > 0;
        }
    }

    multi method flip() is export {
        (~self).split('').reverse().join;
    }

    # TODO: substitute with '$delimiter as Str' once coercion is implemented
    our List multi method split($delimiter, $limit = *) {
        my Int $prev = 0;
        my $l = $limit ~~ Whatever ?? Inf !! $limit;
        my $s = ~self;
        if $delimiter eq '' {
            return gather {
                take $s.substr($_, 1) for 0 .. ($s.chars - 1 min $l - 2);
                if $l <= $s.chars {
                    take $s.substr($l - 1 );
                };
            }
        }
        return gather {
            my $pos = 0;
            while $l > 1
                  && $pos < $s.chars
                  && defined ($pos = $s.index($delimiter, $prev)) {
                take $s.substr($prev, $pos - $prev);
                $prev = [max] 1 + $prev, $pos + (~$delimiter).chars;
                $l--;
            }
            take $s.substr($prev) if $l > 0;
        }
    }

    our List multi method comb (Regex $matcher = /./, $limit = *) {
        my $l = $limit ~~ Whatever ?? Inf !! $limit;
        # currently we use a copy of self and destroy it piece by piece.
        # the preferred way of doing it is using self, not destroying it,
        # and use the :pos modifier to the regex. That way the offsets into
        # self will be right
        my $s = ~self;
        return gather {
            while $l > 0 && $s ~~ $matcher {
                # if we have captures, return the actual match object
                take @($/) || %($/) ?? $/.clone !! ~$/;
                $l--;
                $s.=substr([max] 1, $/.to);
            }
        }
    }

    our List multi method words($limit = *) {
        self.comb(/\S+/, $limit);
    }

    # TODO: signature not fully specced in S32 yet
    our Str multi method trim() is export {
        (~self).subst(/(^\s+)|(\s+$)/, "", :g)
    }

    our Str multi method uc() is export {
        Q:PIR {
            $S0 = self
            upcase $S0
            %r = 'prefix:~'($S0)
        }
    }

    our Str multi method ucfirst() is export {
        self gt '' ?? self.substr(0,1).uc ~ self.substr(1) !! ""
    }
}

multi sub split($delimiter, $target, $limit = *, :$all) {
    $target.split($delimiter, $limit, :$all);
}

# TODO: '$filename as Str' once support for that is in place
multi sub lines(Str $filename,
                :$bin = False,
                :$enc = 'Unicode',
                :$nl = "\n",
                :$chomp = True) {

    my $filehandle = open($filename, :r);
    return lines($filehandle, :$bin, :$enc, :$nl, :$chomp);
}

sub unpack($template, $target) {
    $template.trans(/\s+/ => '') ~~ / ((<[Ax]>)(\d+))* /
        or return (); # unknown syntax
    my $pos = 0;
    return gather for $0.values -> $chunk {
        my ($operation, $count) = $chunk.[0, 1];
        given $chunk.[0] {
            when 'A' { take $target.substr($pos, $count); }
            when 'x' { } # just skip
        }
        $pos += $count;
    }
}

multi sub infix:<x>($str, $n) {
    Q:PIR {
        $P1 = find_lex '$n'
        $I0 = $P1
        if $I0 > 0 goto do_repeat
        $S0 = ''
        goto done
      do_repeat:
        $P0 = find_lex '$str'
        $S0 = $P0
        $S0 = repeat $S0, $I0
      done:
        %r = box $S0
    }
}

# vim: ft=perl6
