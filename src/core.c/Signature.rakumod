my class X::Cannot::Capture { ... }

my class Signature { # declared in BOOTSTRAP
    # class Signature is Any
    #   has @!params;             # VM's array of parameters
    #   has Mu $!returns;         # return type
    #   has int $!arity;          # arity
    #   has Num $!count;          # count
    #   has Code $!code;
    #   has int $!readonly;       # bit mask indicating read-only positionals

    multi method new(Signature:U:
            :@params,
         Mu :$returns,
      Int:D :$arity = @params.elems,
      Num:D :$count = $arity.Num
    ) {
        nqp::create(self)!SET-SELF(@params, $returns, $arity, $count)
    }

    method !SET-SELF(@params, Mu $returns, $arity, $count) {
        nqp::bind(@!params,nqp::getattr(@params,List,'$!reified'));
        $!returns := $returns;
        $!arity    = $arity;
        $!count   := $count;
        self
    }

    multi method ACCEPTS(Signature:D: Mu \topic) {
        nqp::hllbool(nqp::istrue(try self.ACCEPTS: topic.Capture))
    }
    multi method ACCEPTS(Signature:D: Capture $topic) {
        nqp::hllbool(nqp::p6isbindable(self, nqp::decont($topic)));
    }
    multi method ACCEPTS(Signature:D: Signature:D $topic) {
        my @r-params := self.params;
        my @l-params := $topic.params;

        my $l-params := @l-params.elems;
        my $todo     := $l-params;

        my @r-pos-queue;
        my %r-named-queue;

        my $r-pos-sink   := False;
        my $r-named-sink := False;

        for @r-params -> $r-param is raw {
            if $r-param.positional {
                if $r-param.slurpy {
                    $r-pos-sink := True;
                }
                elsif $todo {
                    # When a required or optional positional parameter exists
                    # in a signature, it will be prepended. Typechecks can be
                    # predicted when such parameters exist in the topic too.
                    my $l-param := @l-params[$l-params - $todo];
                    if $l-param.positional and not $l-param.slurpy {
                        return False unless $l-param ~~ $r-param;
                        $todo := $todo - 1;
                    }
                    else {
                        @r-pos-queue.push: $r-param;
                    }
                }
                else {
                    @r-pos-queue.push: $r-param;
                }
            }
            elsif $r-param.named {
                if $r-param.slurpy {
                    $r-named-sink := True;
                }
                else {
                    %r-named-queue{$_} := $r-param for $r-param.named_names;
                }
            }
            else {
                $r-pos-sink := $r-named-sink := True;
            }
        }

        for @l-params.tail: $todo -> $l-param is raw {
            state %r-to-l-named{Mu};
            if $l-param.positional {
                if $l-param.slurpy {
                    return False unless $r-pos-sink;
                }
                elsif @r-pos-queue {
                    return False unless $l-param ~~ @r-pos-queue.shift;
                }
                else {
                    return False unless $r-pos-sink;
                }
            }
            elsif $l-param.named {
                if $l-param.slurpy {
                    return False unless $r-named-sink;
                }
                elsif %r-named-queue {
                    my $found := False;
                    for $l-param.named_names -> $name is raw {
                        if %r-named-queue{$name}:exists {
                            my $r-param := %r-named-queue{$name}:delete;
                            return False unless $l-param ~~ $r-param;
                            return False
                                if %r-to-l-named{$r-param}:exists and not
                                   %r-to-l-named{$r-param} =:= $l-param;
                            %r-to-l-named{$r-param} := $l-param;
                            $found := True;
                        }
                    }
                    return False unless $found or $l-param.optional && $l-param.untyped;
                }
                else {
                    return False unless $r-named-sink;
                }
            }
            else {
                return False unless $r-pos-sink && $r-named-sink;
            }
        }

        return False unless .optional for @r-pos-queue;

        return False unless .optional && .untyped for %r-named-queue.values;

        self.returns =:= $topic.returns
    }

    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }

    method arity() {
        $!arity
    }

    method count() {
        $!count
    }

    method params() {
        nqp::p6bindattrinvres(nqp::create(List), List, '$!reified',
            nqp::clone(@!params));
    }

    method !gistraku(Signature:D: $raku, Mu:U :$elide-type = Mu) {
        # Opening.
        my $text = $raku ?? ':(' !! '(';

        # Parameters.
        if self.params.Array -> @params {
            if @params[0].invocant {
                my $invocant = @params.shift.raku(:$elide-type);
                $invocant .= chop(2) if $invocant.ends-with(' $');
                $text ~= "$invocant: ";
            }
            $text ~= ';; ' if @params && !@params[0].multi-invocant;

            my $sep = '';
            for @params.kv -> $i, $param {
                $text ~= $sep ~ $_ with $param.raku(:$elide-type);

                # Remove sigils from anon typed scalars, leaving type only
                $text .= subst(/» ' $'$/,'') unless $raku;

                $sep = $param.multi-invocant && !@params[$i+1].?multi-invocant
                  ?? ';; '
                  !! ', '
            }
        }
        if nqp::not_i(nqp::isnull($!returns))
          && nqp::not_i(nqp::eqaddr($!returns,Mu)) {
            $text = $text ~ ' --> ' ~ (nqp::can($!returns, 'raku') ?? $!returns.raku !! $!returns.^name)
        }
        # Closer.
        $text ~ ')'
    }

    method !deftype(Signature:D:) {
         nqp::not_i(nqp::isnull($!code))
           && $!code ~~ Routine ?? Any !! Mu
    }

    multi method raku(Signature:D:) {
        self!gistraku(True, :elide-type(self!deftype))
    }
    multi method gist(Signature:D:) {
        self!gistraku(False, :elide-type(self!deftype))
    }
}

multi sub infix:<eqv>(Signature:D $a, Signature:D $b) {

    # we're us
    return True if nqp::eqaddr($a,$b);

    # different container type
    return False unless nqp::eqaddr($a.WHAT,$b.WHAT);

    # different return
    return False unless nqp::eqaddr($a.returns,$b.returns);

    # arity or count mismatch
    return False if $a.arity != $b.arity || $a.count != $b.count;

    # different number of parameters or no parameters
    my $ap := nqp::getattr($a.params,List,'$!reified');
    my $bp := nqp::getattr($b.params,List,'$!reified');
    my int $elems = nqp::elems($ap);
    return False if nqp::isne_i($elems,nqp::elems($bp));
    return True unless $elems;

    # compare all positionals
    my int $i = -1;
    Nil
      while nqp::islt_i(++$i,$elems)
        && nqp::atpos($ap,$i) eqv nqp::atpos($bp,$i);

    # not all matching positionals
    if nqp::islt_i($i,$elems) {

        # not all same and different number of positionals
        return False
          if (!nqp::atpos($ap,$i).named || !nqp::atpos($bp,$i).named);

        # create lookup table
        my int $j = $i = $i - 1;
        my $lookup := nqp::hash;
        while nqp::islt_i(++$j,$elems) {
            my $p  := nqp::atpos($ap,$j);
            my $nn := nqp::getattr($p,Parameter,'@!named_names');
            my str $key =
              nqp::isnull($nn) ?? '' !! nqp::elems($nn) ?? nqp::atpos_s($nn,0) !! '';
            die "Found named parameter '{
              nqp::chars($key) ?? $key !! '(unnamed)'
            }' twice in signature {$a.raku}: {$p.raku} vs {nqp::atkey($lookup,$key).raku}"
              if nqp::existskey($lookup,$key);
            nqp::bindkey($lookup,$key,$p);
        }

        # named variable mismatch
        while nqp::islt_i(++$i,$elems) {
            my $p  := nqp::atpos($bp,$i);
            my $nn := nqp::getattr($p,Parameter,'@!named_names');
            my str $key = nqp::defined($nn) && nqp::elems($nn)
              ?? nqp::atpos_s($nn,0)
              !! '';

            # named param doesn't exist in other or is not equivalent
            return False
              unless nqp::existskey($lookup,$key)
                && $p eqv nqp::atkey($lookup,$key);
        }
    }

    # it's a match
    True
}

Metamodel::Configuration.set_multi_sig_comparator(
    -> \a, \b { a.signature eqv b.signature }
);

# vim: expandtab shiftwidth=4
