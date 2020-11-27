class Perl6::Metamodel::BaseDispatcher {
    has @!candidates;
    has $!idx;
    has $!next_dispatcher; # The dispatcher we must pass control to when own queue exhausts

    method candidates() { @!candidates }

    method exhausted() { $!idx >= +@!candidates && (!nqp::isconcrete($!next_dispatcher) || $!next_dispatcher.exhausted()) }

    method last()      { @!candidates := [] }

    method set_next_dispatcher($next_dispatcher) { $!next_dispatcher := $next_dispatcher }

    # Wrapper-like dispatchers don't set dispatcher for the last candidate.
    method is_wrapper_like() { 0 }

    method get_call() { # Returns [$call, $is_dispatcher]
        my $call := @!candidates[$!idx++];

        my $disp;
        try {
            # XXX Are there any better way to determine a invocation handler with own dispatcher in $!dispatcher?
            $disp := nqp::getattr($call, nqp::what($call), '$!dispatcher'); # If $call is a handler. But there must be better way to deal with this.
            $disp := nqp::null() unless nqp::istype($disp, Perl6::Metamodel::BaseDispatcher); # Protect from multi-Routine dispatcher attribute
        }
        if nqp::isconcrete($disp) {
            return [$disp, 1];
        }
        else {
            my $last_candidate := $!idx >= +@!candidates;
            if  $last_candidate && nqp::isconcrete($!next_dispatcher) {
                nqp::setdispatcherfor($!next_dispatcher, $call);
                $!next_dispatcher := nqp::null();
            }
            else {
                nqp::setdispatcherfor(self, $call) unless $last_candidate && self.is_wrapper_like;
            }
        }
        [$call, 0]
    }

    method call_with_args(*@pos, *%named) {
        my @call := self.get_call;
        if @call[1] {
            return @call[0].enter_with_args(@pos, %named, :next_dispatcher(self));
        }
        if self.has_invocant {
            my $inv := self.invocant;
            @call[0]($inv, |@pos, |%named);
        }
        else {
            @call[0](|@pos, |%named);
        }
    }

    method call_with_capture($capture) {
        my @call := self.get_call;
        if @call[1] { # Got a dispatcher
            return @call[0].enter_with_capture($capture, :next_dispatcher(self));
        }
        nqp::invokewithcapture(@call[0], $capture);
    }

    method shift_callee() {
        my $callee := @!candidates[$!idx];
        $!idx := $!idx + 1;
        nqp::decont($callee)
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
            my %mt := nqp::hllize($_.HOW.method_table($_));
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

    method is_wrapper_like() { 1 }

    method add($wrapper) {
        self.candidates.unshift($wrapper)
    }

    method remove($wrapper) {
        my @cands := self.candidates;
        my int $i := -1;
        while ++$i < +@cands {
            if nqp::decont(@cands[$i]) =:= nqp::decont($wrapper) {
                nqp::splice(@cands, [], $i, 1);
                return 1;
            }
        }
        return 0;
    }

    method get_first($next_dispatcher) {
        my $fresh := nqp::clone(self);
        $fresh.set_next_dispatcher($next_dispatcher) if $next_dispatcher;
        my $first := self.candidates[0];
        nqp::setdispatcherfor($fresh, $first);
        $first
    }

    # This method is a bridge between Perl6 and NQP.
    method enter(*@pos, *%named) {
        self.enter_with_args(@pos, %named);
    }

    method enter_with_args(@pos, %named, :$next_dispatcher?) {
        self.get_first($next_dispatcher)(|@pos, |%named)
    }

    method enter_with_capture($capture, :$next_dispatcher?) {
        my $first := self.get_first($next_dispatcher);
        nqp::invokewithcapture($first, $capture);
    }
}

# vim: expandtab sw=4
