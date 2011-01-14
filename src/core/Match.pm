class Match is Regex::Match is Cool does Positional does Associative {

    method ACCEPTS($x) {
       self === Match ?? nextsame() !! self;
    }
    method new(:$from, :$to, :$orig, :$ast, :@positional, :%named) {
        my $new = self.bless(*);
        return $new unless $orig.defined;
        pir::setattribute__vpsp($new, '$!from',   $from);
        pir::setattribute__vpsp($new, '$!to',     $to);
        pir::setattribute__vpsp($new, '$!target', $orig);
        pir::setattribute__vpsp($new, '$!ast',    $ast);

        for @positional.kv -> $k, $v {
            Q:PIR {
                .local pmc self, key, value
                self  = find_lex '$new'
                key   = find_lex '$k'
                value = find_lex '$v'

                self  = descalarref self
                $I0   = key
                self[$I0] = value
            }

        }
        for (%named ||{}).kv -> $k, $v {
            Q:PIR {
                .local pmc self, key, value
                self  = find_lex '$new'
                key   = find_lex '$k'
                value = find_lex '$v'

                self  = descalarref self
                $S0   = key
                self[$S0] = value
            }
        }

        $new;
    }

    method ast() {
        my $x = self.Regex::Match::ast;
        pir::isa__IPs($x, 'Undef') ?? Any !! $x;
    }

    method Str() {
        ~self.Regex::Match::Str;
    }

    method Bool() {
        $.from <= $.to;
    }

    method defined() {
        $.from <= $.to;
    }

    method at_key($key) {
        Q:PIR {
            $P0 = find_lex 'self'
            $P1 = find_lex '$key'
            %r = $P0[$P1]
            unless null %r goto done
            %r = new ['Proxy']
            setattribute %r, '$!base', $P0
            setattribute %r, '$!key', $P1
          done:
        }
    }

    method at_pos($pos) {
        Q:PIR {
            $P0 = find_lex 'self'
            $P1 = find_lex '$pos'
            $I1 = $P1
            %r = $P0[$I1]
            unless null %r goto done
            %r = new ['Proxy']
            setattribute %r, '$!base', $P0
            setattribute %r, '$!key', $P1
          done:
        }
    }

    method of() { Mu }

    multi method hash() {
        # nextsame() dies here with 'Null PMC access in clone()'
        CREATE_HASH_FROM_LOW_LEVEL(pir::descalarref__PP(self).Regex::Match::hash);
    }

    multi method list() {
        Seq.new(pir::descalarref__PP(self).Regex::Match::list);
    }

    multi method keys() {
        (self.list.keys, self.hash.keys).flat;
    }

    multi method values() {
        (self.list.values, self.hash.values).flat;
    }

    multi method kv() {
        (self.list.kv, self.hash.kv).flat;
    }

    multi method pairs() {
        (self.list.pairs, self.hash.pairs).flat;
    }

    multi method caps() {
        my @caps;
        for self.pairs -> $p {
            # in regexes like [(.) ...]+, the capture for (.) is
            # a List. flatten that.
            if $p.value ~~ Array  {
                for $p.value.list {
                    @caps.push: $p.key => $_;
                }
            } else {
                @caps.push: $p;
            }
        }
        @caps.sort({ .value.from });
    }

    multi method chunks() {
        my $prev = $.from;
        gather {
            for @.caps {
                if .value.from > $prev {
                    take '~' => self.substr($prev - $.from, .value.from - $prev)
                }
                take $_;
                $prev = .value.to;
            }
            take ('~' => self.substr($prev - $.from)) if $prev < $.to;
        }
    }

    multi method perl() {
        self!_perl(0);
    }

    method !_perl(Int $indent) {
        return [~] gather {
            my $sp = ' ' x $indent;
            take $sp;
            take "Match.new(\n";
            take "$sp ast  => {$.ast.perl},\n" unless $.ast === Any;
            take "$sp from => $.from,\n";
            take "$sp orig => $.orig.perl(),\n";
            take "$sp to   => $.to,\n";
            if @(self) {
                take "$sp positional => [\n";
                for self.flat {
                    self!_perl_quant($_, $indent);
                    take ",\n";
                }
                take "$sp  ],\n";
            }
            if %(self) {
                take "$sp named => \{\n";
                for %(self).pairs {
                    take "$sp  {.key} => ";
                    self!_perl_quant(.value, $indent);
                    take ",\n";
                }
                take "$sp \},\n";
            }
            take "$sp)";
        }
    }

    method !_perl_quant($obj, $indent) {
        my $sp = ' ' x $indent;
        if !defined($obj) {
            # do nothing; we only get here when something's not quite right...
        } elsif $obj ~~ Match {
            take $obj!_perl($indent + 3);
        } else {
            take "[\n";
            for $obj.list {
                take $_!_perl($indent + 4);
                take ",\n";
            }
            take "$sp  ]";
        }
    }

    method pretty ($d = 0) {
        my $s = ' ' x ($d + 1);
        my $r = "=> <{self}>\n";
        for @.caps {
            $r ~= $s ~ (.key // '?') ~ ' ' ~ .value.pretty($d + 1)
        }
        $r;
    }
}

multi sub infix:<eqv>(Match $a, Match $b) {
       $a.ast eqv $b.ast
    && $a.orig eqv $b.orig
    && $a.from eqv $b.from
    && $a.to   eqv $b.to
    && $a.list eqv $b.list
    && $a.hash eqv $b.hash
}

# A helper function used by Perl6/Actions.pm. The real point is to
# ensure that Parrot objects returned by .ACCEPTS are converted to
# Bools.
our multi coerce-smartmatch-result(Mu $x,    1) { ! $x }
our multi coerce-smartmatch-result(Mu $x,    0) { ? $x }
our multi coerce-smartmatch-result(Match $x, 0) {   $x }

# vim: ft=perl6
