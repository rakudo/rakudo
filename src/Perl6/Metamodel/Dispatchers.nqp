class Perl6::Metamodel::BaseDispatcher {
    has @!candidates;
    has $!idx;
    has $!next_dispatcher; # The dispatcher we must pass control to when own queue exhausts

    method candidates()     { @!candidates }

    method exhausted()      { $!idx >= +@!candidates && (!nqp::defined($!next_dispatcher) || $!next_dispatcher.exhausted()) }

    method last_candidate() { $!idx >= +@!candidates }

    method last()           { @!candidates := [] }

    method set_next_dispatcher($next_dispatcher)
                            { $!next_dispatcher := $next_dispatcher }

    # Wrapper-like dispatchers handle their last candidate differently.
    method is_wrapper_like() { 0 }

    method get_call() {
        my $call := @!candidates[$!idx];
        ++$!idx;
        if self.is_wrapper_like && self.last_candidate {
            self.set_last_call_dispatchers($call);
        }
        else {
            if (nqp::can($call, 'is_dispatcher') && $call.is_dispatcher) {
                nqp::nextdispatcherfor(self, $call);
            }
            else {
                nqp::setdispatcherfor(self, $call);
            }
        }
        $call
    }

    method call_with_args(*@pos, *%named) {
        if self.last_candidate {
            if nqp::defined($!next_dispatcher) {
                $!next_dispatcher.call_with_args(|@pos, |%named);
            }
            else {
                die(self.HOW.shortname(self) ~ " is already exhausted");
            }
        }
        else {
            my $call := self.get_call;
            if self.has_invocant {
               $call(self.invocant, |@pos, |%named);
            }
            else {
                $call(|@pos, |%named);
            }
        }
    }

    method call_with_capture($capture) {
        if self.last_candidate {
            if nqp::defined($!next_dispatcher) {
                $!next_dispatcher.call_with_capture($capture)
            }
            else {
                die(self.HOW.shortname(self) ~ " is already exhausted");
            }
        }
        else {
            my $call := self.get_call;
            nqp::invokewithcapture($call, $capture);
        }
    }

    method shift_callee() {
        if self.last_candidate {
            if nqp::defined($!next_dispatcher) {
                $!next_dispatcher.shift_callee;
            }
            else {
                nqp::null()
            }
        }
        else {
            self.get_call;
        }
    }

    method add_from_mro(@methods, $class, $sub, :$skip_first = 0) {
        my @mro := nqp::can($class.HOW, 'mro_unhidden')
                        ?? $class.HOW.mro_unhidden($class)
                        !! $class.HOW.mro($class);
        my $name := $sub.name;
        my %seen;
        for @mro {
            my $mt := nqp::hllize($_.HOW.method_table($_));
            if nqp::existskey($mt, $name) {
                my $meth := nqp::atkey($mt, $name);
                if $meth.is_dispatcher {
                    my $proto_pkg_id := nqp::objectid($meth.package);
                    # Skip proto if it's been seen before. Prevents from multiple dispatching over the same multi
                    # candidates.
                    $meth := nqp::null() if %seen{$proto_pkg_id};
                    %seen{$proto_pkg_id} := 1
                }
                # Skipping the first method obtained from MRO because it should have been invoked already
                nqp::if(
                    nqp::isgt_i($skip_first, 0),
                    (--$skip_first),
                    nqp::unless(
                        nqp::isnull($meth),
                        nqp::push(@methods, $meth)
                    )
                )
            }
        }
        @methods
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
        my $obj      := nqp::decont($lexpad<self>);
        my @methods  := [];
        self.add_from_mro(@methods, $obj, $sub);
        self.new(:candidates(@methods), :obj($obj), :idx(1))
    }

    method has_invocant() { 1 }
    method invocant()     { $!obj }
}

class Perl6::Metamodel::MultiDispatcher is Perl6::Metamodel::BaseDispatcher {
    has $!has_invocant;
    has $!invocant;

    method new(:@candidates, :$idx, :$invocant, :$has_invocant, :$next_dispatcher) {
        my $disp := nqp::create(self);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '@!candidates', @candidates);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '$!idx', $idx);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '$!next_dispatcher', $next_dispatcher);
        nqp::bindattr($disp, Perl6::Metamodel::MultiDispatcher, '$!invocant', $invocant);
        nqp::bindattr($disp, Perl6::Metamodel::MultiDispatcher, '$!has_invocant', $has_invocant);
        $disp
    }

    method vivify_for($sub, $lexpad, $args) {
        my $disp         := $sub.dispatcher();
        my $has_invocant := nqp::existskey($lexpad, 'self');
        my @cands        := $disp.find_best_dispatchee($args, 1);
        my $invocant     := $has_invocant && $lexpad<self>;
        my $next_dispatcher := nqp::existskey($lexpad, '$*NEXT-DISPATCHER')
                                ?? nqp::atkey($lexpad, '$*NEXT-DISPATCHER')
                                !! nqp::null();

        # The first candidate has already been invoked, throw it away from the list;
        # If called in a method then only take control if MethodDispatcher is in charge.
        if $has_invocant && !nqp::defined($next_dispatcher) {
            self.add_from_mro(@cands, $invocant, $sub, :skip_first(1));
            Perl6::Metamodel::MethodDispatcher.new(:candidates(@cands), :idx(1), :obj($invocant))
        }
        else {
            self.new(:candidates(@cands), :idx(1), :$invocant, :$has_invocant, :$next_dispatcher)
        }
    }

    method has_invocant() { $!has_invocant }
    method invocant() { $!invocant }
}

class Perl6::Metamodel::WrapDispatcher is Perl6::Metamodel::BaseDispatcher {
    has $!stashed_dispatcher;

    method new(:@candidates, :$idx, :$invocant, :$has_invocant, :$next_dispatcher, :$stashed_dispatcher) {
        my $disp := nqp::create(self);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '@!candidates', @candidates);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '$!idx', 1);
        nqp::bindattr($disp, Perl6::Metamodel::BaseDispatcher, '$!next_dispatcher', $next_dispatcher);
        nqp::bindattr($disp, Perl6::Metamodel::WrapDispatcher, '$!stashed_dispatcher', $stashed_dispatcher);
        $disp
    }

    method vivify_for($sub, $lexpad, $capture) {
        my @candidates      := $sub.wrappers;
        my $next_dispatcher := nqp::existskey($lexpad, '$*NEXT-DISPATCHER')
                                ?? nqp::atkey($lexpad, '$*NEXT-DISPATCHER')
                                !! nqp::null();
        my $stashed_dispatcher := nqp::existskey($lexpad, '$*DISPATCHER')
                                    ?? nqp::atkey($lexpad, '$*DISPATCHER')
                                    !! nqp::null();
        self.new(:@candidates, :idx(1), :$next_dispatcher, :$stashed_dispatcher)
    }

    method has_invocant() { 0 }
    method invocant() { NQPMu }
    method is_wrapper_like() { 1 }

    method set_last_call_dispatchers($call) {
        nqp::setdispatcherfor($!stashed_dispatcher, $call) unless nqp::isnull($!stashed_dispatcher);
        my $next_dispatcher := nqp::getattr(self, Perl6::Metamodel::BaseDispatcher, '$!next_dispatcher');
        nqp::nextdispatcherfor($next_dispatcher, $call) unless nqp::isnull($next_dispatcher);
    }
}
