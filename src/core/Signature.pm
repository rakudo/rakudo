my class Signature { # declared in BOOTSTRAP
    # class Signature is Any {
    #   has Mu $!params;          # VM's array of parameters
    #   has Mu $!returns;         # return type
    #   has Mu $!arity;           # arity
    #   has Mu $!count;           # count
    #   has Mu $!code;

    multi method ACCEPTS(Signature:D: Capture $topic) {
        nqp::p6bool(nqp::p6isbindable(self, nqp::decont($topic)));
    }

    multi method ACCEPTS(Signature:D: @topic) {
        self.ACCEPTS(@topic.Capture)
    }

    multi method ACCEPTS(Signature:D: %topic) {
        self.ACCEPTS(%topic.Capture)
    }

    multi method ACCEPTS(Signature:D: Signature:D $topic) {
        my $sclass = self.params.classify({.named});
        my $tclass = $topic.params.classify({.named});
        my @spos := $sclass{False} // ();
        my @tpos := $tclass{False} // ();

        while @spos {
            my $s;
            my $t;
            last unless $t=@tpos.shift;
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

        my $here=$sclass{True}.SetHash;
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

    method arity() {
        $!arity
    }

    method count() {
        $!count
    }

    method params() {
        nqp::p6bindattrinvres(nqp::create(List), List, '$!reified',
            nqp::clone($!params));
    }

    method !gistperl(Signature:D: $perl, Mu:U :$elide-type = Mu,
                     :&where = -> $ { 'where { ... }' } ) {
        # Opening.
        my $text = $perl ?? ':(' !! '(';

        # Parameters.
        if self.params.Array -> @params {
            $text ~= @params.shift.perl(:$elide-type) ~ ': '
                if @params[0].invocant;
            $text ~= ';; '
                if !@params[0].multi-invocant;

            my $sep = '';
            for @params.kv -> $i, $param {
                my $parmstr = $param.perl(:$elide-type, :&where);
                return Nil without $parmstr;
                $text ~= $sep ~ $parmstr;
                $text .= subst(/' $'$/,'') unless $perl;
                $sep = $param.multi-invocant && !@params[$i+1].?multi-invocant
                  ?? ';; '
                  !! ', '
            }
        }
        if !nqp::isnull($!returns) && !($!returns =:= Mu) {
            $text = $text ~ ' --> ' ~ $!returns.perl
        }
        # Closer.
        $text ~ ')'
    }

    method !deftype(Signature:D:) {
         !nqp::isnull($!code) && $!code ~~ Routine ?? Any !! Mu
    }

    multi method perl(Signature:D:) {
        self!gistperl(True, :elide-type(self!deftype))
    }
    multi method gist(Signature:D:) {
        self!gistperl(False, :elide-type(self!deftype))
    }

    method returns() { $!returns }
}

multi sub infix:<eqv>(Signature \a, Signature \b) {

    # we're us
    return True if a =:= b;

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
      while nqp::islt_i($i = nqp::add_i($i,1),$elems)
        && nqp::atpos($ap,$i) eqv nqp::atpos($bp,$i);

    # not all matching positionals
    if nqp::islt_i($i,$elems) {

        # not all same and different number of positionals
        return False
          if (!nqp::atpos($ap,$i).named || !nqp::atpos($bp,$i).named);

        # create lookup table
        my int $j = $i = $i - 1;
        my $lookup := nqp::hash;
        while nqp::islt_i($j = nqp::add_i($j,1),$elems) {
            my $p  := nqp::atpos($ap,$j);
            my $nn := nqp::getattr($p,Parameter,'$!named_names');
            my str $key =
              nqp::isnull($nn) ?? '' !! nqp::elems($nn) ?? nqp::atpos($nn,0) !! '';
            die "Found named parameter '{
              nqp::chars($key) ?? $key !! '(unnamed)'
            }' twice in signature {a.perl}: {$p.perl} vs {nqp::atkey($lookup,$key).perl}"
              if nqp::existskey($lookup,$key);
            nqp::bindkey($lookup,$key,$p);
        }

        # named variable mismatch
        while nqp::islt_i($i = nqp::add_i($i,1),$elems) {
            my $p  := nqp::atpos($bp,$i);
            my $nn := nqp::getattr($p,Parameter,'$!named_names');
            my str $key = nqp::defined($nn) && nqp::elems($nn)
              ?? nqp::atpos($nn,0)
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

# vim: ft=perl6 expandtab sw=4
