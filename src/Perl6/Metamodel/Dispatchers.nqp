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
            pir::perl6_set_dispatcher_for_callee__vP(self);
            $call($inv, |@pos, |%named);
        }
        else {
            pir::perl6_set_dispatcher_for_callee__vP(self);
            $call(|@pos, |%named);
        }
    }
    
    method call_with_capture($capture) {
        # Extract parts of the capture.
        my @pos;
        my %named;
        my $i := 0;
        while $i < nqp::elems($capture) {
            @pos[$i] := $capture[$i];
            $i := $i + 1;
        }
        my @nameds := pir::getattribute__PPs($capture, 'named');
        unless nqp::isnull(@nameds) {
            for @nameds {
                %named{$_} := $capture{$_};
            }
        }
        
        # Call.
        my $call := @!candidates[$!idx];
        $!idx := $!idx + 1;
        pir::perl6_set_dispatcher_for_callee__vP(self);
        $call(|@pos, |%named);
    }
}

class Perl6::Metamodel::MethodDispatcher is Perl6::Metamodel::BaseDispatcher {
    has $!obj;
    
    method vivify_for($sub, $lexpad) {
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
    
    method vivify_for($sub, $lexpad) {
        my $disp         := $sub.dispatcher();
        my $args         := $lexpad<call_sig>;
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
    method new() {
        self.bless(:candidates([]), :idx(1))
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
        pir::perl6_set_dispatcher_for_callee__vP($fresh);
        $first(|@pos, |%named)
    }
}
