augment class Cool {

    our Int multi method bytes() is export {
        pir::box__PI(pir::bytelength__IS(self))
    }

    # The spec has a more elegant approach for this,
    # but this one works now.
    our Str multi method capitalize() {
        self.lc.split(/\w+/, :all).map({ .Str.ucfirst }).join('');
    }

    our Int multi method chars() is export {
        pir::length__IS(self);
    }

    our multi method chomp() is export {
        # in PIR for speed
        Q:PIR {
            $P0 = find_lex 'self'
            $S0 = $P0
            unless $S0 > '' goto done
            $I0 = ord $S0, -1
            unless $I0 == 10 goto done
            $S0 = chopn $S0, 1
            unless $S0 > '' goto done
            $I0 = ord $S0, -1
            unless $I0 == 13 goto done
            $S0 = chopn $S0, 1
          done:
            %r = new ['Str']
            assign %r, $S0
        }
    }

    multi method subst($matcher, $replacement,
                       :ii(:$samecase), :ss(:$samespace), *%options) {
        my @matches = self.match($matcher, |%options);
        return self unless @matches;
        return self if @matches == 1 && !@matches[0];
        my $prev = 0;
        my $result = '';
        for @matches -> $m {
            $result ~= self.substr($prev, $m.from - $prev);

            my $real_replacement = ~($replacement ~~ Callable ?? $replacement($m) !! $replacement);
            $real_replacement    = $real_replacement.samecase(~$m) if $samecase;
            $real_replacement    = $real_replacement.samespace(~$m) if $samespace;
            $result ~= $real_replacement;
            $prev = $m.to;
        }
        my $last = @matches.pop;
        $result ~= self.substr($last.to);
        $result;
    }

    multi method comb(Regex $matcher = /./, $limit = *, :$match) {
        my $self-string = ~self;
        my $c = 0;
        my $l = $limit ~~ ::Whatever ?? Inf !! $limit;
        gather while $l > 0 && (my $m = $self-string.match($matcher, :c($c))) {
            if $match {
                my $m-clone = $m;
                take $m-clone;
            } else {
                take ~$m;
            }
            $c = $m.from == $m.to ?? $m.to + 1 !! $m.to;
            --$l;
        }
    }

    multi method samecase(Cool $pattern) is export {
        my $result = '';
        my $p = '';
        my @pattern = $pattern.comb;
        for self.comb -> $s {
            $p = @pattern.shift if @pattern;
            if $p ~~ /<.upper>/ {
                $result ~= $s.uc;
            } elsif $p ~~ /<.lower>/ {
                $result ~= $s.lc;
            } else {
                $result ~= $s;
            }
        }
        $result;
    }

    method samespace($other as Str) {
        my @pieces = self.split(/\s+/);
        my @new_spaces = $other.comb(/\s+/, @pieces-1);

        my @new = @pieces[0..^@new_spaces] Z @new_spaces;

        if @new_spaces < @pieces-1 {
            my $remainder = self ~~ m:nth(@pieces-1)/\s+/;
            @new.push(self.substr($remainder.from-1));
        }
        else {
            @new.push(@pieces[*-1]);
        }

        return @new.join;
    }


    multi method split(Regex $matcher, $limit = *, :$all) {
        my $c = 0;
        my $l = $limit ~~ ::Whatever ?? Inf !! $limit - 1;
        my $prev-pos = 0;
        return if $l < 0;
        return self.list if $l == 0;
        gather {
            for @.match($matcher, :x(1..$l)) -> $m {
                take self.substr($prev-pos, $m.from - $prev-pos);
                take $m if $all;
                $prev-pos = $m.to;
            }
            take self.substr($prev-pos);
        }
    }

    multi method split($delimiter, $limit = *, :$all) {
        my $match-string = $delimiter.Str;
        return if self eq '' && $delimiter eq '';
        my $c = 0;
        my $l = $limit ~~ ::Whatever ?? Inf !! $limit - 1;
        if $l >= 0 {
            gather {
                while $l-- > 0 {
                    if ($match-string eq "") {
                        last unless $c + 1 < self.chars;
                        take self.substr($c, 1);
                        $c++;
                    } else {
                        my $m = self.index($match-string, $c);
                        last unless $m.defined; # CHEAT, but the best I can do for now
                        take self.substr($c, $m - $c);
                        take $match-string if $all;
                        $c = $m + $match-string.chars;
                    }
                }
                take self.substr($c);
            }
        } else {
            Nil;
        }
    }

    our Str multi method substr($start, $length?) is export {
        my $len = $length // self.chars;
        if ($len < 0) {
            if ($start >= 0) {
                $len += self.chars;
            }
            $len -= $start;
        }

        if ($start > self.chars || $start < -self.chars) {
            return Mu;
        }

        ~pir::substr(self, $start, $len);
    }

    my class LSM {
        has Cool $!source is readonly;
        has      @!substitutions;

        has Int  $!index = 0;
        has Int  $!next_match;
        has      $!next_substitution;
        has      $!substitution_length;

        has Str  $.unsubstituted_text;
        has Str  $.substituted_text;

        method add_substitution($key, $value) {
            push @!substitutions, $key => $value;
        }

        submethod compare_substitution($substitution, Int $pos, Int $length) {
            if $!next_match > $pos
               || $!next_match == $pos && $!substitution_length < $length {

                $!next_match = $pos;
                $!substitution_length = $length;
                $!next_substitution = $substitution;
            }
        }

        multi submethod triage_substitution($_ where { .key ~~ Regex }) {
            my $key = .key;
            return unless $!source.substr($!index) ~~ $key;
            self.compare_substitution($_, $!index + $/.from, $/.to - $/.from);
        }

        multi submethod triage_substitution($_ where { .key ~~ Cool }) {
            return unless defined index($!source, .key, $!index);
            self.compare_substitution($_,
                                      index($!source, .key, $!index),
                                      .key.chars);
        }

        multi submethod triage_substitution($_) {
            die "Don't know how to handle a {.WHAT} as a substitution key";
        }

        multi submethod increment_index(Regex $s) {
            $!source.substr($!index) ~~ $s;
            $!index = $!next_match + $/.chars;
        }

        multi submethod increment_index(Cool $s) {
            $!index = $!next_match + $s.chars;
        }

        method next_substitution() {
            $!next_match = $!source.chars;

            for @!substitutions {
                self.triage_substitution($_);
            }

            $!unsubstituted_text
                = $!source.substr($!index, $!next_match - $!index);
            if defined $!next_substitution {
                my $result = $!next_substitution.value;
                $!substituted_text
                    = $result ~~ Callable ?? $result() !! $result;
                self.increment_index($!next_substitution.key);
            }

            return $!next_match < $!source.chars;
        }
    }

    multi method trans(*@changes) {
        my sub expand($s) {
            return $s.list if $s ~~ Iterable|Positional;
            gather for $s.comb(/ (\w) '..' (\w) | . /, :match) {
                if .[0] {
                    take $_ for ~.[0] .. ~.[1];
                } else {
                    take ~$_;
                }
            }
        }

        my $lsm = LSM.new(:source(self));
        for (@changes) -> $p {
            die "$p.perl() is not a Pair" unless $p ~~ Pair;
            if $p.key ~~ Regex {
                $lsm.add_substitution($p.key, $p.value);
            }
            elsif $p.value ~~ Callable {
                my @from = expand $p.key;
                for @from -> $f {
                    $lsm.add_substitution($f, $p.value);
                }
            }
            else {
                my @from = expand $p.key;
                my @to   = expand $p.value;
                if @to {
                    @to = @to xx ceiling(@from / @to);
                } else {
                    @to = '' xx @from;
                }
                for @from Z @to -> $f, $t {
                    $lsm.add_substitution($f, $t);
                }
            }
        }

        my $r = "";
        while $lsm.next_substitution {
            $r ~= $lsm.unsubstituted_text ~ $lsm.substituted_text;
        }
        $r ~= $lsm.unsubstituted_text;

        return $r;
    }

    # S32/Str says that this should always return a StrPos object
    our Int multi method index($substring, $pos = 0) is export {
        if ($substring.chars == 0) {
            my $string_length = self.chars;
            return $pos < $string_length ?? $pos !! $string_length;
        }

        my $result = pir::index__ISSi(self, $substring, $pos);
        fail("Substring '$substring' not found in '{self}'") if $result < 0;
        return $result;

        # also used to be a the following error message, but the condition
        # was never checked:
        # .tailcall '!FAIL'("Attempt to index from negative position")
    }


    # S32/Str says that this should always return a StrPos object
     our Int multi method rindex($substring, $pos?) is export {
         if ($substring.chars == 0) {
             my $string_length = self.chars;
             return $pos.defined && $pos < $string_length ?? $pos !! $string_length;
         }

         my $result = (~self).reverse_index($substring, $pos);
         fail("Substring '$substring' not found in '{self}'") if $result < 0;
         return $result;
     }

    our Str multi method chop() is export {
        self.substr(0, -1)
    }

    our Str multi method fmt(Str $format = '%s') {
        sprintf($format, self)
    }

    our Str multi method lc() {
        ~(pir::downcase__SS(self))
    }

    our Str multi method lcfirst() {
        self gt '' ?? self.substr(0,1).lc ~ self.substr(1) !! ""
    }

    our multi method match(Regex $pat,
                           :c(:$continue),
                           :g(:$global),
                           :pos(:$p),
                           :$x,
                           :st(:nd(:rd(:th(:$nth)))),
                           :ov(:$overlap)) {
        if $continue ~~ Bool {
            note ":c / :continue requires a position in the string";
            fail ":c / :continue requires a position in the string";
        }
        my %opts;
        %opts<p> = $p        if defined $p;
        %opts<c> = $continue // 0 unless defined $p;
        my $x_upper = -1;
        if defined($x) {
            return if $x == 0;
            if $x ~~ Range {
                $x_upper = $x.excludes_max ?? $x.max - 1 !! $x.max;
            } else {
                $x_upper = $x;
            }
        }

        if $global || $nth.defined || $overlap || ($x.defined && $x_upper > 1) {
            my $nth-list = $nth.defined ?? $nth.flat !! $nth;
            my $next-index;
            if $nth-list.defined {
                return if !$nth-list;
                $next-index = $nth-list.shift;
                return if +$next-index < 1;
            }

            my $taken = 0;
            my $i = 1;
            my @r = gather while my $m = Cursor.parse(self, :rule($pat), |%opts) {
                my $m-copy = $m;
                if !$nth-list.defined || $i == $next-index {
                    take $m-copy;
                    $taken++;

                    if ($nth-list.defined) {
                        while ?$nth-list && $next-index <= $i  {
                            $next-index = $nth-list.shift;
                        }
                        last if $next-index <= $i;
                    }
                }
                last if $taken == $x_upper;

                if ($overlap) {
                    %opts<c> = $m.from + 1;
                } else {
                    if $m.to == $m.from {
                        %opts<c> = $m.to + 1;
                        if $p.defined {
                            warn "multiple matches with :p terminated by zero-width match\n";
                            last;
                        }
                    } else {
                        %opts<c> = $m.to;
                        %opts<p> = $m.to if $p.defined;
                    }
                }

                $i++;
            }
            if $x.defined && $taken !~~ $x {
                return;
            }
            return |@r;
        } else {
            Cursor.parse(self, :rule($pat), |%opts);
        }
    }
    multi method match($pat, *%options) {
        self.match(rx{ $pat }, |%options);
    }

    our multi method ord() {
        if self eq "" {
            fail('Can not take ord of empty string');
        }

        pir::box__PI(pir::ord__IS(self));
    }

    our multi method ords() {
        gather for self.comb {
            take $_.ord;
        }
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

    multi method eval() {
        eval(~self);
    }

    multi method flip() is export {
        ~ Q:PIR {
            $P0 = box ''
            $P1 = find_lex 'self'
            $S0 = $P0.'reverse'($P1)
            %r  = box $S0
        }
    }

    # Not yet spec'd, I expect it will be renamed
    multi method trim-leading() is export {
        if ~self ~~ /^\s*:(.*)$/ {
            ~$/[0];
        } else {
            self;
        }
    }

    # Not yet spec'd, I expect it will be renamed
    multi method trim-trailing() is export {
        if ~self ~~ /^(.*\S)\s*$/ {
            ~$/[0];
        } elsif self ~~ /^\s*$/ {
            "";
        }
        else {
            self;
        }
    }

    # TODO: signature not fully specced in S32 yet
    multi method trim() is export {
        self.trim-leading.trim-trailing;
    }

    multi method words(Str $input: Int $limit = *) {
        $input.comb( / \S+ /, $limit );
    }

    our multi method lines(Str $input: Int $limit = Inf) {
        $input.comb( / ^^ \N* /, $limit );
    }

    our Str multi method uc() {
        ~(pir::upcase__SS(self))
    }

    our Str multi method ucfirst() {
        self gt '' ?? self.substr(0,1).uc ~ self.substr(1) !! ""
    }

    our Str multi method sprintf(*@args) {
        my $result;
        try {
            $result = pir::sprintf__SSP(~self, (|@args)!PARROT_POSITIONALS);
        }
        $! ?? die 'Not enough arguments supplied for the given format string' !! $result
    }

    method IO() {
        ::IO.new(path => ~self);
    }
}

proto ord($string) { $string.ord; }
proto ords($string) { $string.ords; }

our Str proto sub infix:<x>($str, $n) {
    $n > 0 ?? ~(pir::repeat__SSI($str, $n)) !!  ''
}

our multi sub infix:<cmp>($a, $b) {
    return Order::Increase if $a eqv -Inf || $b eqv Inf;
    return Order::Decrease if $a eqv Inf || $b eqv -Inf;
    $a lt $b ?? Order::Increase !! ($a gt $b ?? Order::Decrease !! Order::Same);
}

our multi sub infix:<leg>($a, $b) {
    ~$a cmp ~$b
}

multi split ( $delimiter, $input, $limit = *, :$all ) {
    $input.split($delimiter, $limit, :$all);
}

our List multi comb ( Regex $matcher, Str $input, Int $limit = Inf ) {
    $input.comb($matcher , $limit );
}

multi sub sprintf($str as Str, *@args) {
    $str.sprintf(|@args)
}

proto sub uc($string) { $string.uc; }
proto sub ucfirst($string) { $string.ucfirst; }
proto sub lc($string) { $string.lc; }
proto sub lcfirst($string) { $string.lcfirst; }
proto sub capitalize($string) { $string.capitalize; }

# vim: ft=perl6
