my class X::Routine::Unwrap { ... }

my class Routine {
    method of() { self.signature.returns }
    method returns() { self.signature.returns }
    method rw() { $!rw }
    
    method assuming($r: *@curried_pos, *%curried_named) {
        return sub CURRIED (*@pos, *%named) {
            $r(|@curried_pos, |@pos, |%curried_named, |%named)
        }
    }
    
    method candidates() {
        self.is_dispatcher ??
            pir::perl6ize_type__PP($!dispatchees) !!
            (self,)
    }
    
    method candidates_matching(|$c) {
        my $disp;
        if self.is_dispatcher {
            $disp := self;
        }
        else {
            $disp := nqp::create(self);
            nqp::bindattr($disp, Routine, '$!dispatchees', nqp::list(self));
        }
        # Call this lexical sub to get rid of 'self' in the signature.
        sub checker(|$) {
            my Mu $cap := pir::find_lex__Ps('call_sig');
            pir::perl6ize_type__PP(pir::perl6_get_matching_multis__PPP($disp, $cap))
        }
        checker(|$c);
    }
    
    method multi() {
        self.dispatcher.defined
    }
    
    multi method perl(Routine:D:) {
        my $perl = self.^name.lc();
        if self.name() -> $n {
            $perl ~= " $n";
        }
        $perl ~= self.signature().perl.substr(1);
        $perl ~= ' { ... }';
        $perl
    }
    
    method wrap(&wrapper) {
        my class WrapHandle {
            has $!dispatcher;
            has $!wrapper;
            method restore() {
                nqp::p6bool($!dispatcher.remove($!wrapper));
            }
        }
        my role Wrapped {
            has $!dispatcher;
            method UNSHIFT_WRAPPER(&wrapper) {
                # Add candidate.
                $!dispatcher := WrapDispatcher.new()
                    unless nqp::isconcrete($!dispatcher);
                $!dispatcher.add(&wrapper);
                
                # Return a handle.
                my $handle := nqp::create(WrapHandle);
                nqp::bindattr($handle, WrapHandle, '$!dispatcher', $!dispatcher);
                nqp::bindattr($handle, WrapHandle, '$!wrapper', &wrapper);
                $handle
            }
            method postcircumfix:<( )>($c) {
                $!dispatcher.enter(|$c);
            }
        }
        
        # If we're not wrapped already, do the initial dispatcher
        # creation.
        unless nqp::istype(self, Wrapped) {
            my $orig = self.clone();
            self does Wrapped;
            self.UNSHIFT_WRAPPER($orig);
        }

        # Add this wrapper.
        self.UNSHIFT_WRAPPER(&wrapper);
    }
    
    method unwrap($handle) {
        $handle.can('restore') && $handle.restore() ||
            die(X::Routine::Unwrap.new())
    }
    
    method yada() {
        nqp::p6bool(nqp::getattr_i(self, Routine, '$!yada'))
    }

    method package() { $!package }
}
