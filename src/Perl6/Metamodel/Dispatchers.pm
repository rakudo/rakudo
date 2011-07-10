class Perl6::Metamodel::BaseDispatcher {
    has @!candidates;
    has $!idx;
    
    method exhausted() { $!idx >= +@!candidates }
    
    method last()      { @!candidates := [] }
    
    method call_next(*@pos, *%named) {
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
}

class Perl6::Metamodel::MethodDispatcher is Perl6::Metamodel::BaseDispatcher {
    has $!obj;
    
    method vivify_for($sub, $lexpad) {
        my $obj      := $lexpad<self>;
        my $name     := $sub.name;
        my @mro      := $obj.HOW.mro($obj);
        my @methods;
        for @mro {
            my %mt := $_.HOW.method_table($_);
            if pir::exists(%mt, $name) {
                @methods.push(%mt{$name});
            }
        }
        self.new(:candidates(@methods), :obj($obj), :idx(1))
    }
    
    method has_invocant() { 1 }
    method invocant()     { $!obj }
}

class Perl6::Metamodel::MultiDispatcher is Perl6::Metamodel::BaseDispatcher {
    method vivify_for($sub, $lexpad) {
        my $disp  := $sub.dispatcher();
        my $args  := $lexpad<callsig>;
        my @cands; # XXX := pir::XXX($disp, $args);
        self.new(:candidates(@cands), :idx(1))
    }

    method has_invocant() { 0 }
}

class Perl6::Metamodel::WrapDispatcher is Perl6::Metamodel::BaseDispatcher {
    method new(@wrappers) {
        self.bless(:candidates(@wrappers), :idx(0))
    }

    method has_invocant() { 0 }
}
