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
            pir::perl6ize_type__PP(nqp::getattr(self, Code, '$!dispatchees')) !!
            (self,)
    }
    
    method candidates_matching(|$c) {
        my $disp;
        if self.is_dispatcher {
            $disp := self;
        }
        else {
            $disp := nqp::create(self);
            nqp::bindattr($disp, Code, '$!dispatchees', nqp::list(self));
        }
        sub checker(|$) {
            my Mu $cap := pir::find_lex__Ps('call_sig');
            pir::perl6ize_type__PP(pir::perl6_get_matching_multis__PPP($disp, $cap))
        }
        checker(|$c);
    }
    
    method multi() {
        self.dispatcher.defined
    }
}
