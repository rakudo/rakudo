my class X::Cannot::Capture { ... }

my class Signature { # declared in BOOTSTRAP
    # class Signature is Any
    #   has @!params;             # VM's array of parameters
    #   has Mu $!returns;         # return type
    #   has int $!arity;          # arity
    #   has Num $!count;          # count
    #   has Code $!code;

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
        my $sclass = self.params.classify({.named});
        my $tclass = $topic.params.classify({.named});
        my @spos := $sclass{False} // ();
        my @tpos := $tclass{False} // ();

        while @spos {
            my $s;
            my $t;
            last unless @tpos && ($t = @tpos.shift);
            $s=@spos.shift;
            if $s.slurpy or $s.capture {
                @spos=();
                @tpos=();
                last;
            }
            if $t.slurpy or $t.capture {
                return False unless any(@spos) ~~ {.slurpy or .capture};
                @spos=();
                @tpos=();
                last;
            }
            if not $s.optional {
                return False if $t.optional
            }
            return False unless $t ~~ $s;
        }
        return False if @tpos;
        if @spos {
            return False unless @spos[0].optional or @spos[0].slurpy or @spos[0].capture;
        }

        for flat ($sclass{True} // ()).grep({!.optional and !.slurpy}) -> $this {
            my $other;
            return False unless $other=($tclass{True} // ()).grep(
                {!.optional and $_ ~~ $this });
            return False unless +$other == 1;
        }

        my $here=($sclass{True}:v).SetHash;
        my $hasslurpy=($sclass{True} // ()).grep({.slurpy});
        $here{@$hasslurpy} :delete;
        $hasslurpy .= Bool;
        for flat @($tclass{True} // ()) -> $other {
            my $this;

            if $other.slurpy {
                return False if any($here.keys) ~~ -> Any $_ { !(.type =:= Mu) };
                return $hasslurpy;
            }
            if $this=$here.keys.grep( -> $t { $other ~~ $t }) {
                $here{$this[0]} :delete;
            }
            else {
                return False unless $hasslurpy;
            }
        }
        return False unless self.returns =:= $topic.returns;
        True;
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
        if !nqp::isnull($!returns) && !($!returns =:= Mu) {
            $text = $text ~ ' --> ' ~ (nqp::can($!returns, 'raku') ?? $!returns.raku !! $!returns.^name)
        }
        # Closer.
        $text ~ ')'
    }

    method !deftype(Signature:D:) {
         !nqp::isnull($!code) && $!code ~~ Routine ?? Any !! Mu
    }

    multi method raku(Signature:D:) {
        self!gistraku(True, :elide-type(self!deftype))
    }
    multi method gist(Signature:D:) {
        self!gistraku(False, :elide-type(self!deftype))
    }
}

multi sub infix:<eqv>(Signature:D \a, Signature:D \b) {

    # we're us
    return True if a =:= b;

    # different container type
    return False unless a.WHAT =:= b.WHAT;

    # different return
    return False unless a.returns =:= b.returns;

    # arity or count mismatch
    return False if a.arity != b.arity || a.count != b.count;

    # different number of parameters or no parameters
    my $ap := nqp::getattr(a.params,List,'$!reified');
    my $bp := nqp::getattr(b.params,List,'$!reified');
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
            }' twice in signature {a.raku}: {$p.raku} vs {nqp::atkey($lookup,$key).raku}"
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

Perl6::Metamodel::Configuration.set_multi_sig_comparator(
    -> \a, \b { a.signature eqv b.signature }
);

# vim: expandtab shiftwidth=4
