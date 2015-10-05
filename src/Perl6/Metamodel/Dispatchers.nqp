class Perl6::Metamodel::BaseDispatcher {
    has @!candidates;
    has $!idx;

    method candidates() { @!candidates }
    
    method exhausted() { $!idx >= +@!candidates }
    
    method last()      { @!candidates := [] }
    
    method call_with_args(*@pos, *%named) {
        my $call := @!candidates[$!idx];
        $!idx := $!idx + 1;
        if self.has_invocant {
            my $inv := self.invocant;
            nqp::setdispatcher(self);
            $call($inv, |@pos, |%named);
        }
        else {
            nqp::setdispatcher(self);
            $call(|@pos, |%named);
        }
    }
    
    method call_with_capture($capture) {
        my $call := @!candidates[$!idx];
        $!idx := $!idx + 1;
        nqp::setdispatcher(self);
        nqp::invokewithcapture(nqp::decont($call), $capture)
    }
}

class Perl6::Metamodel::MethodDispatcher is Perl6::Metamodel::BaseDispatcher {
    has $!obj;

    method new(:@candidates, :$idx, :$obj) {
        my $disp := nqp::create(self);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '@!candidates', @candidates);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '$!idx', $idx);
        nqp::bindattr($disp, Perl6::Metamodel::MethodDispatcher, '$!obj', $obj);
        $disp
    }
    
    method vivify_for($sub, $lexpad, $args) {
        my $obj      := $lexpad<self>;
        my $name     := $sub.name;
        my @mro      := nqp::can($obj.HOW, 'mro_unhidden')
            ?? $obj.HOW.mro_unhidden($obj)
            !! $obj.HOW.mro($obj);
        my @methods;
        for @mro {
            my %mt := $_.HOW.method_table($_);
            if nqp::existskey(%mt, $name) {
                @methods.push(%mt{$name});
            }
        }
        self.new(:candidates(@methods), :obj($obj), :idx(1))
    }
    
    method has_invocant() { 1 }
    method invocant()     { $!obj }
}

class Perl6::Metamodel::MultiDispatcher is Perl6::Metamodel::BaseDispatcher {
    has $!has_invocant;
    has $!invocant;

    method new(:@candidates, :$idx, :$invocant, :$has_invocant) {
        my $disp := nqp::create(self);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '@!candidates', @candidates);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '$!idx', $idx);
        nqp::bindattr($disp, Perl6::Metamodel::MultiDispatcher, '$!invocant', $invocant);
        nqp::bindattr($disp, Perl6::Metamodel::MultiDispatcher, '$!has_invocant', $has_invocant);
        $disp
    }
    
    method vivify_for($sub, $lexpad, $args) {
        my $disp         := $sub.dispatcher();
        my $has_invocant := nqp::existskey($lexpad, 'self');
        my $invocant     := $has_invocant && $lexpad<self>;
        my @cands        := $disp.find_best_dispatchee($args, 1);
        self.new(:candidates(@cands), :idx(1), :invocant($invocant),
            :has_invocant($has_invocant))
    }

    method has_invocant() { $!has_invocant }
    method invocant() { $!invocant }
}

class Perl6::Metamodel::WrapDispatcher is Perl6::Metamodel::BaseDispatcher {
    method new(:@candidates, :$idx, :$invocant, :$has_invocant) {
        my $disp := nqp::create(self);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '@!candidates', @candidates);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '$!idx', 1);
        $disp
    }

    method has_invocant() { 0 }
    
    method add($wrapper) {
        self.candidates.unshift($wrapper)
    }
    
    method remove($wrapper) {
        my @cands := self.candidates;
        my $i := 0;
        while $i < +@cands {
            if nqp::decont(@cands[$i]) =:= nqp::decont($wrapper) {
                nqp::splice(@cands, [], $i, 1);
                return 1;
            }
            $i := $i + 1;
        }
        return 0;
    }
    
    method enter(*@pos, *%named) {
        my $fresh := nqp::clone(self);
        my $first := self.candidates[0];
        nqp::setdispatcher($fresh);
        $first(|@pos, |%named)
    }
}
